# BioStat Studio: local R statistical engine. No remote data transmission.
# Only user-selected variables enter models; reference levels are set in app.R.
infer_type <- function(x,name) {
 a<-na.omit(x);k<-length(unique(a));n<-length(a)
 if(!n)return(list(type='nominal',reason='全欄缺失，無法判定；請確認。'))
 if(grepl('(^id$|_id$|^id_|識別|編號)',name,ignore.case=TRUE)&&k>1)return(list(type='id',reason=if(k==n)'欄名與唯一值符合識別碼特徵。'else'欄名符合識別碼特徵；重複值可能表示個案重複測量或群聚，請確認其層級。'))
 if(is.ordered(x))return(list(type='ordinal',reason='來源檔已指定順序類別。'))
 if(is.factor(x)||is.logical(x))return(list(type='nominal',reason='來源為類別或邏輯欄位；不猜設類別順序。'))
 if(is.numeric(x)) {
  if(k<=2)return(list(type='nominal',reason='數值僅一至兩級，建議作類別。'))
  if(k<=7&&all(a==floor(a))&&min(a)>=0&&max(a)<=7&&grepl('(^q[0-9]|item|likert|題)',name,ignore.case=TRUE))return(list(type='ordinal',reason='題項名稱及少級整數符合 Likert 特徵；需確認。'))
  if(k<=10&&all(a==floor(a)))return(list(type='nominal',reason='少級整數可能為類別編碼或計數，請確認。'))
  return(list(type='continuous',reason='數值欄位且具多個取值；請確認是否為計數、日期或識別碼。'))
 }
 list(type='nominal',reason='文字／日期字串不自動轉連續數值；請確認。')
}
`%||%` <- function(x,y) if(is.null(x)||length(x)==0) y else x
need <- function(ok,msg) if(!isTRUE(ok)) stop(msg,call.=FALSE)
f2 <- function(x) ifelse(is.finite(x),sprintf('%.2f',x),NA_character_)
fp <- function(p) ifelse(is.na(p),NA_character_,ifelse(p<5e-8,'<5×10⁻⁸',ifelse(p<1e-4,'<0.0001',sprintf('%.4f',p))))
out <- function(title,tables=list(),plots=list(),notes=character()) list(title=title,tables=tables,plots=plots,notes=notes)
mtab <- function(m) data.frame(Term=rownames(m),as.data.frame(m),row.names=NULL,check.names=FALSE)
cc <- function(d,vars) {
 need(length(vars)>0 && all(vars %in% names(d)),'請選擇有效變項。')
 z<-d[,unique(vars),drop=FALSE]; z<-droplevels(z[complete.cases(z),,drop=FALSE])
 need(nrow(z)>=3,'完整個案不足 3 筆。'); z
}
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
form <- function(y,x,inter=NULL) reformulate(c(x,if(length(inter)==2) paste(inter,collapse=':')),response=y,env=parent.frame())
checkfit <- function(fit) {
 if(inherits(fit,'glm')) need(isTRUE(fit$converged),'模型未收斂；停止推論。')
 if(!is.null(fit$convergence))need(fit$convergence==0,'模型最佳化未收斂；停止推論。')
 b<-coef(fit); need(all(is.finite(b)),'模型係數不可識別，請檢查共線性、空組別或變項數。')
 if(inherits(fit,'glm') && fit$family$family=='binomial') need(!any(fitted(fit)<1e-8 | fitted(fit)>1-1e-8),'預測接近 0／1，可能完全或近完全分離；停止一般 Logistic 推論。')
 invisible(fit)
}
coeftable <- function(b,V,ratio=FALSE,df=Inf,label='Estimate') {
 se<-sqrt(diag(V)); crit<-qt(.975,df); p<-2*pt(-abs(b/se),df)
 lo<-b-crit*se; hi<-b+crit*se
 if(ratio){b<-exp(b);lo<-exp(lo);hi<-exp(hi)}
 z<-data.frame(Term=names(b),Estimate=b,SE_link=se,CI_low=lo,CI_high=hi,p_value=p,row.names=NULL)
 names(z)[2]<-label;z
}
forest <- function(t,estimate=names(t)[2],null=1) {
 z<-t[t$Term!='(Intercept)' & is.finite(t$CI_low)&is.finite(t$CI_high),,drop=FALSE]
 ggplot2::ggplot(z,ggplot2::aes(x=.data[[estimate]],y=reorder(Term,.data[[estimate]])))+
 ggplot2::geom_vline(xintercept=null,lty=2,color='grey50')+ggplot2::geom_errorbar(ggplot2::aes(xmin=CI_low,xmax=CI_high),orientation='y',width=.15,color='#315bea')+ggplot2::geom_point(color='#315bea',size=2)+ggplot2::labs(x=paste(estimate,'(95% CI)'),y=NULL)+ggplot2::theme_minimal(base_size=13)
}
boot_cases <- function(d,FUN,B=5000,seed=20260910,progress=function(...)NULL) {
 need(B>=20 && B<=20000,'Bootstrap 次數需介於 20 至 20,000。')
 point<-FUN(d);set.seed(seed);iteration_seeds<-sample.int(.Machine$integer.max,B);samples<-matrix(NA_real_,B,length(point)); colnames(samples)<-names(point)
 errors<-character(B)
 for(i in seq_len(B)) {
  # Third-party estimators may change RNG state internally. Give each replicate
  # its own deterministic seed so this never duplicates all later resamples.
  set.seed(iteration_seeds[i])
  # Materialize before FUN: R promises otherwise allow FUN to reset RNG before
  # the sampling expression is evaluated (even with per-iteration seeds).
  sampled<-d[sample.int(nrow(d),replace=TRUE),,drop=FALSE]
  v<-tryCatch(withCallingHandlers(FUN(sampled),warning=function(w)stop(conditionMessage(w))),error=function(e){errors[i]<<-conditionMessage(e);NULL})
  if(length(v)==length(point)) samples[i,]<-v
  if(i%%25==0) progress(i/B)
 }
 good<-colSums(is.finite(samples)); ci<-sapply(seq_along(point),function(j) if(good[j]>=max(20,.95*B)) quantile(samples[,j],c(.025,.975),na.rm=TRUE) else c(NA,NA))
 list(table=data.frame(Term=names(point),Estimate=point,CI_low=ci[1,],CI_high=ci[2,],Successful=good,Requested=B,row.names=NULL),samples=samples,errors=sort(table(errors[nzchar(errors)]),decreasing=TRUE))
}
bootnote <- 'Bootstrap 以獨立個案有放回抽樣；95% CI 為百分位法。每項成功率未達 95% 或少於 20 次時不提供 CI。這不是 BCa；重複測量／群集資料不可使用此個案抽樣。'
analysis_note <- '結果為所選資料及模型下的估計。警示：此處推論可能存在資訊偏誤或混雜風險；亦須評估選擇性偏誤、時間先後與模型假設，不能單憑顯著性或 Hill 準則認定因果關係。'

descriptive <- function(d,vars,group='',summary='mean',continuous_test='none',categorical_test='none',p_adjust='none',compute_smd=FALSE) {
 need(p_adjust%in%c('none','holm','bonferroni'),'不支援此多重檢定校正。')
 need(length(vars)>0,'請選擇描述變項。'); group<-group %||% ''; tabs<-list(); rows<-list()
 if(continuous_test!='none'||categorical_test!='none')need(nzchar(group),'執行組間檢定前，請先選擇分組變項。')
 groups<-list(Overall=rep(TRUE,nrow(d)))
 if(nzchar(group)) {need(is.factor(d[[group]]),'分組變項需為名目或順序類別。'); for(l in levels(d[[group]])) groups[[paste0(group,'=',l)]]<-!is.na(d[[group]]) & d[[group]]==l}
 pub<-list();kinds<-character();addpub<-function(label,values,kind){pub[[length(pub)+1]]<<-as.data.frame(c(list(Characteristic=label),values),check.names=FALSE);kinds<<-c(kinds,kind)}
 addpub('n',lapply(groups,function(g)format(sum(g),big.mark=',',trim=TRUE)),'total')
 for(v in vars) {
  x<-d[[v]]
  if(is.numeric(x)) {
   r<-list(Characteristic=paste(v,if(summary=='mean')'mean ± SD' else 'median (Q1, Q3)'))
   for(g in names(groups)){a<-x[groups[[g]] & !is.na(x)]; r[[g]]<-if(!length(a))'—' else if(summary=='mean') paste(f2(mean(a)),'±',f2(sd(a))) else paste0(f2(median(a)),' (',f2(quantile(a,.25)),', ',f2(quantile(a,.75)),')')}
   rows[[length(rows)+1]]<-as.data.frame(r,check.names=FALSE)
   addpub(v,r[-1],'continuous')
  } else {addpub(v,setNames(as.list(rep('',length(groups))),names(groups)),'heading');for(l in levels(factor(x))) {
   r<-list(Characteristic=paste(v,l,'n (%)'))
   for(g in names(groups)){a<-x[groups[[g]] & !is.na(x)]; n<-sum(a==l);r[[g]]<-if(length(a))sprintf('%d (%.2f)',n,100*n/length(a)) else '—'}
   rows[[length(rows)+1]]<-as.data.frame(r,check.names=FALSE)
   addpub(l,r[-1],'level')
  }}
  r<-list(Characteristic=paste(v,'Missing n')); for(g in names(groups))r[[g]]<-as.character(sum(is.na(x[groups[[g]]])))
  rows[[length(rows)+1]]<-as.data.frame(r,check.names=FALSE)
  if(any(vapply(r[-1],function(a)as.numeric(a)>0,logical(1))))addpub('Missing',r[-1],'level')
 }
 ans<-out('描述性統計 / Table 1',list(Table1=dplyr::bind_rows(rows)),notes=c('比例分母為各組各變項的非缺失人數；缺失數另列。分組變項缺失者僅納入 Overall。摘要方法由使用者選擇，不以常態檢定自動決定。'))
 pd<-dplyr::bind_rows(pub);tests<-list()
 if(nzchar(group)&&(continuous_test!='none'||categorical_test!='none'))for(v in setdiff(vars,group)){
  method<-if(is.numeric(d[[v]]))continuous_test else categorical_test
  if(method=='none')next
  pv<-NA_real_;detail<-''
  tryCatch({z<-cc(d,c(v,group));g<-droplevels(factor(z[[group]]));x<-z[[v]];need(nlevels(g)>=2,'至少需要兩組。')
   fit<-switch(method,welch={need(nlevels(g)==2,'Welch t 僅適用兩組。');t.test(x~g)},anova=oneway.test(x~g,var.equal=FALSE),wilcox={need(nlevels(g)==2,'Wilcoxon 僅適用兩組。');wilcox.test(x~g,exact=FALSE)},kruskal=kruskal.test(x~g),fisher=fisher.test(table(x,g)),chisq=chisq.test(table(x,g),correct=FALSE))
   pv<-fit$p.value;detail<-fit$method
   if(method=='chisq'&&any(fit$expected<5))detail<-paste(detail,'；存在期望次數 < 5，卡方近似需審慎評估。')
  },error=function(e)detail<<-conditionMessage(e))
  tests[[length(tests)+1]]<-data.frame(Variable=v,Method=method,p_value=pv,Detail=detail)
 }
 if(length(tests)){
  tt<-dplyr::bind_rows(tests);pd[['p-value']]<-''
  adj_name<-switch(p_adjust,holm='Holm',bonferroni='Bonferroni','None');adj_col<-paste0('p_',adj_name);adj_header<-paste(adj_name,'adj. p-value')
  if(p_adjust!='none'){tt[[adj_col]]<-p.adjust(tt$p_value,p_adjust,n=nrow(tt));pd[[adj_header]]<-''}
  for(i in seq_len(nrow(tt))){j<-which(pd$Characteristic==tt$Variable[i]&kinds%in%c('continuous','heading'))[1];pd[j,'p-value']<-fp(tt$p_value[i]);if(p_adjust!='none')pd[j,adj_header]<-fp(tt[[adj_col]][i])}
  ans$tables$Group_tests<-tt
  ans$tables$Multiplicity<-data.frame(Method=adj_name,Planned_tests=nrow(tt),Available_p=sum(!is.na(tt$p_value)),Family='本次 Table 1 所選變項的整體組間檢定；無法估计的已選檢定仍計入數量')
  ans$notes<-c(ans$notes,'每個 p 值為該變項的整體組間檢定，非逐一類別比較；分組變項本身不檢定。',if(p_adjust!='none')paste(adj_name,'多重檢定校正涵蓋本表',nrow(tt),'個預定檢定；原始 p 保留，與納入干擾因子的模型調整不同。'))
 }
 ans$publication<-list(data=pd,kinds=kinds,group=group,summary=summary);if(isTRUE(compute_smd))ans<-add_table1_smd(ans,d,vars,group);ans
}
comparison <- function(d,y,g,method) {
 z<-cc(d,c(y,g));x<-z[[y]];a<-droplevels(factor(z[[g]]));need(nlevels(a)>=2,'至少需要兩組。')
 if(method=='fisher') {t<-table(x,a); fit<-fisher.test(t); tabs<-list(Counts=mtab(t),Test=data.frame(Method=fit$method,p_value=fit$p.value)); p<-ggplot2::ggplot(z,ggplot2::aes(x=.data[[g]],fill=factor(.data[[y]])))+ggplot2::geom_bar(position='fill')+ggplot2::labs(y='Proportion',fill=y)}
 else {need(is.numeric(x),'此方法需要連續數值結果。');fit<-switch(method,welch={need(nlevels(a)==2,'Welch t 檢定需要兩組。');t.test(x~a)},anova=oneway.test(x~a,var.equal=FALSE),wilcox={need(nlevels(a)==2,'Wilcoxon 檢定需要兩組。');wilcox.test(x~a,exact=FALSE)},kruskal=kruskal.test(x~a))
 tabs<-list(Test=data.frame(Method=fit$method,Statistic=unname(fit$statistic),p_value=fit$p.value,N=nrow(z)))
 if(!is.null(fit$conf.int))tabs$CI<-data.frame(Lower=fit$conf.int[1],Upper=fit$conf.int[2])
 p<-ggplot2::ggplot(z,ggplot2::aes(x=.data[[g]],y=.data[[y]]))+ggplot2::geom_boxplot(fill='#eef2ff')+ggplot2::geom_jitter(width=.12,alpha=.25)
 }
 out('獨立組別比較',tabs,list(Distribution=p+ggplot2::theme_minimal(base_size=13)),c(sprintf('分析 N=%d；排除缺失 %d。',nrow(z),nrow(d)-nrow(z)),'以上為獨立樣本檢定；配對或重複測量請勿視為獨立個案。',analysis_note))
}
correlation <- function(d,vars,method,p_adjust='holm') {
 need(p_adjust%in%c('none','holm','bonferroni'),'不支援此多重檢定校正。')
 z<-cc(d,vars);z[]<-lapply(z,num);need(all(vapply(z,function(a)all(is.finite(a))&&sd(a)>0,logical(1))),'相關分析需要非恆定數值變項。')
 k<-ncol(z);need(k>=2,'至少兩個變項。'); rows<-list()
 for(i in 1:(k-1)) for(j in (i+1):k){f<-cor.test(z[[i]],z[[j]],method=method,exact=FALSE); rows[[length(rows)+1]]<-data.frame(X=names(z)[i],Y=names(z)[j],r=unname(f$estimate),p_value=f$p.value,N=nrow(z))}
 t<-dplyr::bind_rows(rows);adj_name<-switch(p_adjust,holm='Holm',bonferroni='Bonferroni','None');if(p_adjust!='none')t[[paste0('p_',adj_name)]]<-p.adjust(t$p_value,p_adjust,n=nrow(t));r<-cor(z,method=method);long<-as.data.frame(as.table(r))
 p<-ggplot2::ggplot(long,ggplot2::aes(Var1,Var2,fill=Freq))+ggplot2::geom_tile()+ggplot2::geom_text(ggplot2::aes(label=sprintf('%.2f',Freq)))+ggplot2::scale_fill_gradient2(low='#c84b55',mid='white',high='#315bea',limits=c(-1,1))+ggplot2::theme_minimal()+ggplot2::labs(x=NULL,y=NULL,fill=method)
 out(paste(method,'相關'),list(Pairs=t,Matrix=mtab(r),Multiplicity=data.frame(Method=adj_name,Planned_tests=nrow(t),Available_p=sum(!is.na(t$p_value)),Family='所選變項所有不重複成對相關；不含對角線')),list(Correlation=p),sprintf('完整個案 N=%d；所有配對使用相同樣本。Spearman p 值為漸近近似；多重檢定方法 %s，涵蓋 %d 個配對。原始 p 值保留。',nrow(z),adj_name,nrow(t)))
}

# Pairwise full maximum likelihood, including thresholds. Any optimizer warning
# fails that matrix; never silently substitute Pearson or smooth the matrix.
poly_matrix <- function(x) {
 k<-ncol(x);r<-diag(k);dimnames(r)<-list(names(x),names(x))
 for(i in 1:(k-1))for(j in (i+1):k){v<-withCallingHandlers(polycor::polychor(x[[i]],x[[j]],ML=TRUE,std.err=FALSE),warning=function(w)stop(conditionMessage(w)));need(is.finite(v)&&abs(v)<.999,'多分相關達估計邊界。');r[i,j]<-r[j,i]<-v}
 r
}
matrix_diag <- function(r) {
 e<-eigen(r,symmetric=TRUE,only.values=TRUE)$values
 data.frame(Min_eigen=min(e),Condition=if(min(e)>0)max(e)/min(e) else Inf,Extreme_pairs=sum(abs(r[upper.tri(r)])>.9),Positive_definite=min(e)>1e-8)
}
smc_eigen <- function(r) {need(matrix_diag(r)$Positive_definite,'相關矩陣非正定；未自動平滑。');diag(r)<-1-1/diag(solve(r));eigen(r,symmetric=TRUE,only.values=TRUE)$values}
retain <- function(obs,ref) {a<-obs>ref;if(any(!a))which(!a)[1]-1L else length(a)}
parallel_analysis <- function(x,B=500,seed=20260910,method='poly',progress=function(...)NULL) {
 cf<-if(method=='poly')poly_matrix else function(z)cor(z,method=method)
 r<-cf(x); obs<-smc_eigen(r); full<-eigen(r,symmetric=TRUE,only.values=TRUE)$values
 set.seed(seed);iteration_seeds<-sample.int(.Machine$integer.max,B);sim<-simp<-matrix(NA_real_,B,ncol(x));err<-character(B)
 for(b in seq_len(B)) {
  set.seed(iteration_seeds[b])
  permuted<-as.data.frame(lapply(x,sample))
  rr<-tryCatch(cf(permuted),error=function(e){err[b]<<-conditionMessage(e);NULL})
  if(!is.null(rr)) {if(matrix_diag(rr)$Positive_definite){sim[b,]<-smc_eigen(rr);simp[b,]<-eigen(rr,symmetric=TRUE,only.values=TRUE)$values}else err[b]<-'Non-positive definite'}
  if(b%%10==0)progress(b/B)
 }
 ok<-complete.cases(sim);need(sum(ok)>=20,'成功置換不足 20 次，無法估計第 95 百分位。')
 ref<-apply(sim[ok,,drop=FALSE],2,quantile,.95);refp<-apply(simp[ok,,drop=FALSE],2,quantile,.95)
 list(r=r,table=data.frame(Factor=seq_along(obs),Observed_SMC=obs,Null_95=ref,Observed_full=full,Null_full_95=refp),n=if(mean(ok)>=.95)retain(obs,ref) else NA_integer_,nfull=if(mean(ok)>=.95)retain(full,refp) else NA_integer_,successful=sum(ok),requested=B,errors=sort(table(err[nzchar(err)]),decreasing=TRUE))
}
raw_alpha <- function(x) {k<-ncol(x);need(k>=2,'α 至少兩題。');v<-var(rowSums(x));if(!is.finite(v)||v<=0)return(NA_real_); k/(k-1)*(1-sum(vapply(x,var,numeric(1)))/v)}
questionnaire <- function(d,items,reverse=character(),lower=NULL,upper=NULL,method='poly',B=500,boot=TRUE,bootB=5000,seed=20260910,nfactor=0,efa=TRUE,progress=function(...)NULL) {
 x<-cc(d,items);x[]<-lapply(x,num);need(all(is.finite(as.matrix(x))),'題項需為數值評分，請先確認編碼。')
 need(all(vapply(x,sd,numeric(1))>0),'含零變異題項，請移除或檢查。')
 if(length(reverse)) {need(length(lower)==1&&length(upper)==1&&is.finite(lower)&&is.finite(upper)&&lower<upper,'請填寫反向題的理論最低與最高分。');for(v in reverse){need(v %in% items,'反向題須包含於選取題項。');need(all(x[[v]]>=lower&x[[v]]<=upper),'反向題資料超出指定量尺範圍。');x[[v]]<-lower+upper-x[[v]]}}
 if(method=='poly')need(all(vapply(x,function(v)all(v==floor(v))&&length(unique(v))<=15,logical(1))),'多分相關需為離散順序評分（此介面至多 15 級）。')
 al<-raw_alpha(x);k<-ncol(x);notes<-c(sprintf('完整個案 %d / %d；反向題 %s。未使用個人平均插補。',nrow(x),nrow(d),paste(reverse,collapse=', ')),'α 是內部一致性，不能單獨證明單向度或效度。不得僅憑 α if deleted 自動刪題。')
 diagtab<-data.frame(Item=items,Mean=vapply(x,mean,numeric(1)),SD=vapply(x,sd,numeric(1)),Corrected_item_total=vapply(seq_len(k),function(j)cor(x[[j]],rowSums(x[,-j,drop=FALSE])),numeric(1)),Alpha_if_deleted=vapply(seq_len(k),function(j)if(k>2)raw_alpha(x[,-j,drop=FALSE])else NA_real_,numeric(1)))
 tabs<-list(Reliability=data.frame(N=nrow(x),Items=k,Alpha=al),Items=diagtab)
 tabs$Frequencies<-dplyr::bind_rows(lapply(items,function(v){a<-table(x[[v]]);data.frame(Item=v,Level=names(a),n=as.integer(a),Percent=100*as.integer(a)/nrow(x))}))
 if(boot){bt<-boot_cases(x,function(z)c(Alpha=raw_alpha(z)),bootB,seed,progress);tabs$Alpha_bootstrap<-bt$table;notes<-c(notes,bootnote)}
 plots<-list()
 if(!efa)return(out('問卷信度',tabs,plots,notes))
 rr<-tryCatch(if(method=='poly')poly_matrix(x) else cor(x,method=method),error=function(e)e)
 if(inherits(rr,'error'))return(out('問卷信度；效度分析停止',tabs,plots,c(notes,conditionMessage(rr))))
 tabs$Correlation<-mtab(rr);tabs$Matrix_diagnostics<-matrix_diag(rr)
 pairs<-combn(items,2,simplify=FALSE);tabs$Sparse_cells<-dplyr::bind_rows(lapply(pairs,function(v){t<-table(x[[v[1]]],x[[v[2]]]);data.frame(Item1=v[1],Item2=v[2],Empty_cells=sum(t==0),Cells=length(t))}))
 if(k==2){r<-rr[1,2];tabs$Two_item<-data.frame(Correlation=r,Spearman_Brown=2*r/(1+r));return(out('兩題量表診斷',tabs,plots,c(notes,'兩題不執行 EFA。Spearman–Brown 係數依所選相關尺度計算；多分相關版本為潛在尺度。')))}
 if(!matrix_diag(rr)$Positive_definite)return(out('問卷信度；EFA 停止',tabs,plots,c(notes,'相關矩陣非正定；請檢查稀疏格、重複題與極端相關。未自動平滑。')))
 km<-psych::KMO(rr);tabs$KMO<-data.frame(Item=c('Overall',items),MSA=c(km$MSA,km$MSAi))
 if(method=='pearson'){b<-psych::cortest.bartlett(rr,n=nrow(x));tabs$Bartlett<-data.frame(Chi_square=b$chisq,df=b$df,p_value=b$p.value)}else notes<-c(notes,'不對多分／Spearman 相關套用以連續多變量常態為前提的 Bartlett χ² 推論。')
 pa<-parallel_analysis(x,B,seed,method,progress);tabs$Parallel<-pa$table;tabs$PA_status<-data.frame(Successful=pa$successful,Requested=B,Factors_SMC=pa$n,Factors_full=pa$nfull)
 if(length(pa$errors))tabs$PA_failures<-data.frame(Reason=names(pa$errors),Count=as.integer(pa$errors))
 notes<-c(notes,'逐題独立置換保留各題邊際分布；每次重估相關矩陣。觀察與置換皆以 SMC 縮減；由第一因素起連續保留高於第 95 百分位者。失敗次數另列；成功率低於 95% 不建議因素數。')
 plotdata<-tidyr::pivot_longer(pa$table[,1:3],-Factor,names_to='Series',values_to='Eigenvalue')
 plots$Parallel<-ggplot2::ggplot(plotdata,ggplot2::aes(Factor,Eigenvalue,color=Series))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::theme_minimal(base_size=13)
 nf<-if(nfactor>0)nfactor else pa$n
 if(k==3)notes<-c(notes,'三題 EFA 的因素結構證據有限；三題一因素 CFA 通常剛好識別。')
 if(!is.na(nf)&&nf>0) {
  need(nf<=k-1 && ((k-nf)^2-k-nf)>=0,'所選因素數使共同因素模型自由度為負，請减少因素數。')
  fa<-psych::fa(rr,nfactors=nf,n.obs=nrow(x),fm='pa',SMC=TRUE,rotate=if(nf==1)'none' else 'promax',smooth=FALSE,max.iter=1000)
  tabs$Loadings<-mtab(unclass(fa$loadings));tabs$Communalities<-data.frame(Item=items,h2=fa$communality);if(!is.null(fa$Phi))tabs$Factor_correlations<-mtab(fa$Phi)
  if(any(fa$communality>1)||any(fa$communality<0))notes<-c(notes,'Heywood case：共同性超出 [0,1]，因素解不可作常規解讀。')
  ld<-as.data.frame(as.table(unclass(fa$loadings)));plots$Loadings<-ggplot2::ggplot(ld,ggplot2::aes(Var2,Var1,fill=Freq))+ggplot2::geom_tile()+ggplot2::geom_text(ggplot2::aes(label=sprintf('%.2f',Freq)))+ggplot2::scale_fill_gradient2(low='#c84b55',high='#315bea',limits=c(-1,1))+ggplot2::labs(x=NULL,y=NULL)+ggplot2::theme_minimal()
 }
 if(method!='spearman') {
  spa<-parallel_analysis(x,B,seed,'spearman',progress);tabs$Spearman_PA<-spa$table;tabs$Spearman_status<-data.frame(Factors=spa$n,Successful=spa$successful,Requested=B)
  if(!is.na(spa$n)&&spa$n>0&&((k-spa$n)^2-k-spa$n)>=0){sf<-psych::fa(spa$r,nfactors=spa$n,n.obs=nrow(x),fm='pa',SMC=TRUE,rotate=if(spa$n==1)'none' else 'promax',smooth=FALSE,max.iter=1000);tabs$Spearman_loadings<-mtab(unclass(sf$loadings))}
  notes<-c(notes,'Spearman 敏感度分析另列；因素可交換順序及正負號，應對齊後比較題項歸屬、跨負荷與方向。')
 }
 out('問卷信度與探索性因素分析',tabs,plots,notes)
}
