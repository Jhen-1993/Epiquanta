source('R/sem_diagram.R',encoding='UTF-8')
source('R/sem_types.R',encoding='UTF-8')
source('R/sem_outcome.R',encoding='UTF-8')
# General models, design-aware effects, interactions, survival and SEM.
model_fit <- function(z,y,x,model,inter=NULL,offsetvar='') {
 f<-form(y,x,inter)
 if(nzchar(offsetvar)) {need(all(z[[offsetvar]]>0),'人時／offset 必須為正。');f<-update(f,paste('. ~ . + offset(log(',offsetvar,'))'))}
 fit<-withCallingHandlers(switch(model,linear=lm(f,z),logistic=glm(f,z,family=binomial()),rr=glm(f,z,family=poisson()),poisson=glm(f,z,family=poisson()),negbin=MASS::glm.nb(f,z),ordinal=MASS::polr(f,z,Hess=TRUE,method='logistic'),multinomial=nnet::multinom(f,z,trace=FALSE,Hess=TRUE)),warning=function(w)stop(paste('模型估計警示，停止推論：',conditionMessage(w)),call.=FALSE))
 checkfit(fit);fit
}
vc <- function(fit,model) if(model %in% c('rr','linear'))sandwich::vcovHC(fit,type=if(model=='rr')'HC0' else 'HC3') else vcov(fit)
regression <- function(d,y,x,model,design='unspecified',event='',inter=NULL,offsetvar='',boot=FALSE,B=5000,seed=20260910,progress=function(...)NULL) {
 need(length(x)>0&&!y %in% x,'請選擇預測變項；結果不能同時作為預測變項。')
 if(length(inter))need(length(inter)==2&&all(inter %in% x)&&length(unique(inter))==2,'交互作用必須選擇兩個不同且已納入模型的預測變項。')
 z<-cc(d,c(y,x,offsetvar[nzchar(offsetvar)]));n0<-nrow(z)
 need(!nzchar(offsetvar)||model %in% c('poisson','negbin'),'人時 offset 僅適用此介面的 Poisson／負二項計數模型。')
 for(v in x)if(is.factor(z[[v]])){ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1];need(ref%in%levels(z[[v]]),paste(v,'指定參考組在完整個案中無資料。'));z[[v]]<-relevel(factor(z[[v]],levels=levels(z[[v]]),ordered=FALSE),ref=ref)}
 if(model %in% c('rr','logistic')) {need(length(unique(z[[y]]))==2&&event %in% as.character(z[[y]]),'二元結果需恰好兩組，且需指定事件組。');z[[y]]<-as.integer(as.character(z[[y]])==event)}
 if(model=='rr')need(design %in% c('cohort','rct','cross'),'RR 需要世代／RCT；橫斷性應報 PR。病例對照不可直接估計 RR。')
 if(model %in% c('poisson','negbin'))need(is.numeric(z[[y]])&&all(z[[y]]>=0&z[[y]]==floor(z[[y]])),'計數模型結果需為非負整數。')
 if(model=='linear')need(is.numeric(z[[y]]),'線性迴歸結果需為連續數值。')
 if(model=='ordinal') {need(is.ordered(z[[y]])&&nlevels(z[[y]])>=3,'順序 Logistic 結果需已設定至少三級的順序型態與次序。')}
 if(model=='multinomial') {z[[y]]<-factor(z[[y]]);need(nlevels(z[[y]])>=3,'多項 Logistic 需至少三個類別。')}
 fit<-model_fit(z,y,x,model,inter,offsetvar);notes<-c(sprintf('完整個案 N=%d；缺失排除 %d。',nrow(z),nrow(d)-nrow(z)),analysis_note)
 notes<-c(notes,'類別預測變項（含順序類別）以 treatment contrasts 對照首個級別；若需線性趨勢，請先明確改為連續型態。')
 tables<-list();plots<-list()
 refs<-lapply(c(y,x),function(v)if(is.factor(z[[v]]))data.frame(Variable=v,Reference=levels(z[[v]])[1],Levels=paste(levels(z[[v]]),collapse=' → ')))
 tables$Reference<-dplyr::bind_rows(refs)
 if(model=='multinomial') {
  s<-summary(fit);b<-s$coefficients;se<-s$standard.errors;rows<-list();for(i in seq_len(nrow(b))) rows[[i]]<-data.frame(Outcome=rownames(b)[i],Term=colnames(b),Relative_odds=exp(b[i,]),CI_low=exp(b[i,]-1.96*se[i,]),CI_high=exp(b[i,]+1.96*se[i,]),p_value=2*pnorm(-abs(b[i,]/se[i,])))
  tables$Coefficients<-dplyr::bind_rows(rows);notes<-c(notes,'多項 Logistic：每個結果類別相對於參考結果類別的勝算比。')
  if(length(inter)==2){f0<-model_fit(z,y,x,model);lr<-2*as.numeric(logLik(fit)-logLik(f0));df<-attr(logLik(fit),'df')-attr(logLik(f0),'df');tables$Interaction_LRT<-data.frame(Chi_square=lr,df=df,p_value=pchisq(lr,df,lower.tail=FALSE))}
  if(boot){bt<-boot_cases(z,function(a){ff<-model_fit(a,y,x,model,inter);bb<-coef(ff);v<-as.vector(t(exp(bb)));names(v)<-as.vector(t(outer(rownames(bb),colnames(bb),paste,sep=':')));v},B,seed,progress);tables$Bootstrap<-bt$table;notes<-c(notes,bootnote)}
  return(attach_diagnostics(out('多項 Logistic 迴歸',tables,plots,notes),fit,'multinomial'))
 }
 b<-coef(fit);V<-vc(fit,model);if(model=='ordinal')V<-V[names(b),names(b),drop=FALSE]
 label<-switch(model,linear='Beta',logistic='OR',rr=if(design=='cross')'PR' else 'RR',poisson=if(nzchar(offsetvar))'IRR' else 'Count_ratio',negbin=if(nzchar(offsetvar))'IRR' else 'Count_ratio',ordinal='Cumulative_OR')
 tables$Coefficients<-coeftable(b,V,model!='linear',df=if(model=='linear')df.residual(fit) else Inf,label=label)
 plots$Forest<-forest(tables$Coefficients,label,if(model=='linear')0 else 1)
 if(model=='rr')notes<-c(notes,'Modified Poisson 使用 HC0 穩健變異數；Wald 檢定有效性依獨立樣本與大樣本近似。不可將 Poisson 一般 LRT 當作二元結果之穩健檢定。')
 if(model=='rr'&&any(fitted(fit)>1))notes<-c(notes,'此 modified Poisson 模型出現預測值 >1；log link 不限制在機率範圍，請檢查模型設定，勿直接視為合法絕對風險。')
 if(model=='linear') {notes<-c(notes,'線性迴歸採 HC3 穩健標準誤；交互作用在結果的加法尺度。'); dd<-data.frame(Fitted=fitted(fit),Residual=residuals(fit));plots$Residuals<-ggplot2::ggplot(dd,ggplot2::aes(Fitted,Residual))+ggplot2::geom_point(alpha=.5)+ggplot2::geom_hline(yintercept=0,lty=2)+ggplot2::theme_minimal()}
 if(model %in% c('poisson','negbin'))tables$Dispersion<-data.frame(Pearson_dispersion=sum(residuals(fit,type='pearson')^2)/df.residual(fit))
 if(model=='ordinal') {
  # Threshold-specific models plus a joint covariance Wald diagnostic.
  lev<-levels(z[[y]]);rows<-list();fits<-list()
  for(j in seq_len(length(lev)-1)){zz<-z;zz[[y]]<-as.integer(z[[y]])>j;ff<-glm(form(y,x,inter),zz,family=binomial());checkfit(ff);fits[[j]]<-ff;tt<-coeftable(coef(ff),vcov(ff),TRUE,label='OR_above_cut');tt$Cut<-lev[j];rows[[j]]<-tt}
  tables$Threshold_diagnostic<-dplyr::bind_rows(rows)
  # Joint covariance of threshold-specific logistic estimates accounts for
  # their dependence within respondents; test equality of slopes, not intercepts.
  q<-length(coef(fits[[1]]));K<-length(fits);bread<-matrix(0,q*K,q*K);score<-matrix(0,nrow(z),q*K);bb<-unlist(lapply(fits,coef),use.names=FALSE)
  for(j in seq_len(K)){ii<-((j-1)*q+1):(j*q);xx<-model.matrix(fits[[j]]);mu<-fitted(fits[[j]]);bread[ii,ii]<-solve(crossprod(xx*sqrt(mu*(1-mu))));score[,ii]<-xx*as.numeric(fits[[j]]$y-mu)}
  vv<-bread%*%crossprod(score)%*%bread;D<-matrix(0,(K-1)*(q-1),K*q);row<-0
  for(j in 2:K)for(k in 2:q){row<-row+1;D[row,k]<-1;D[row,(j-1)*q+k]<- -1}
  dv<-D%*%vv%*%t(D);diff<-D%*%bb;need(qr(dv)$rank==nrow(dv),'比例勝算聯合檢定共變異矩陣不可逆；樣本或級別不足。');stat<-drop(t(diff)%*%solve(dv,diff));tables$Proportional_odds_Wald<-data.frame(Chi_square=stat,df=nrow(D),p_value=pchisq(stat,nrow(D),lower.tail=FALSE))
  notes<-c(notes,'比例勝算假設：以各累積切點二元 Logistic 斜率相等之联合 Wald 檢定評估，sandwich 共變異數保留同一受試者跨切點相關；屬大樣本近似。另列切點係數。polr 正向係數表示較高類別的累積勝算較大。')
 }
 if(boot) {
  bt<-boot_cases(z,function(a){ff<-model_fit(a,y,x,model,inter,offsetvar);v<-coef(ff);if(model!='linear')v<-exp(v);v},B,seed,progress);tables$Bootstrap<-bt$table;notes<-c(notes,bootnote)
 }
 if(length(inter)==2 && !model %in% c('ordinal')) {
  idx<-grep(':',names(b));need(length(idx)>0,'模型中未能建立交互作用項。')
  stat<-drop(t(b[idx])%*%solve(V[idx,idx,drop=FALSE],b[idx]));tables$Interaction_Wald<-data.frame(Scale=if(model=='linear')'Additive outcome' else 'Multiplicative link',Chi_square=stat,df=length(idx),p_value=pchisq(stat,length(idx),lower.tail=FALSE))
  if(!model %in% c('rr','linear')){f0<-model_fit(z,y,x,model,NULL,offsetvar);lr<-2*as.numeric(logLik(fit)-logLik(f0));tables$Interaction_LRT<-data.frame(Chi_square=lr,df=length(idx),p_value=pchisq(lr,length(idx),lower.tail=FALSE))}
  # Prediction at explicit reference values; no silent adjustment to invented covariates.
  baseline<-z[1,,drop=FALSE];for(v in x)baseline[[v]]<-if(is.factor(z[[v]]))factor(levels(z[[v]])[1],levels=levels(z[[v]])) else median(z[[v]])
  if(nzchar(offsetvar))baseline[[offsetvar]]<-1
  grids<-lapply(inter,function(v)if(is.factor(z[[v]]))levels(z[[v]]) else seq(min(z[[v]]),max(z[[v]]),length.out=30));names(grids)<-inter
  grid<-expand.grid(grids,stringsAsFactors=FALSE);nd<-baseline[rep(1,nrow(grid)),,drop=FALSE];for(v in inter)nd[[v]]<-if(is.factor(z[[v]]))factor(grid[[v]],levels=levels(z[[v]])) else grid[[v]]
  mm<-model.matrix(delete.response(terms(fit)),nd);eta<-drop(mm%*%b);se<-sqrt(rowSums((mm%*%V)*mm));inv<-if(model=='linear')identity else fit$family$linkinv
  grid$Estimate<-inv(eta);grid$CI_low<-inv(eta-1.96*se);grid$CI_high<-inv(eta+1.96*se)
  tables$Prediction_reference<-data.frame(Variable=x,Value=vapply(x,function(v)as.character(baseline[[v]][1]),character(1)))
  tables$Interaction_predictions<-grid
  if(is.factor(z[[inter[1]]])&&is.factor(z[[inter[2]]]))p<-ggplot2::ggplot(grid,ggplot2::aes(x=.data[[inter[1]]],y=Estimate,color=factor(.data[[inter[2]]])))+ggplot2::geom_pointrange(ggplot2::aes(ymin=CI_low,ymax=CI_high),position=ggplot2::position_dodge(.4))
  else {xx<-inter[if(is.numeric(z[[inter[1]]]))1 else 2];gg<-setdiff(inter,xx);p<-ggplot2::ggplot(grid,ggplot2::aes(x=.data[[xx]],y=Estimate,color=factor(.data[[gg]]),fill=factor(.data[[gg]])))+ggplot2::geom_line()+ggplot2::geom_ribbon(ggplot2::aes(ymin=CI_low,ymax=CI_high),alpha=.12,color=NA)}
  legend_var<-if(is.factor(z[[inter[1]]])&&is.factor(z[[inter[2]]]))inter[2] else gg
  prediction_label<-switch(model,linear='Predicted mean',logistic='Predicted probability',rr=if(design=='cross')'Predicted prevalence'else'Predicted risk',poisson=if(nzchar(offsetvar))'Predicted events per unit time'else'Predicted count',negbin=if(nzchar(offsetvar))'Predicted events per unit time'else'Predicted count')
  plots$Interaction<-p+ggplot2::labs(color=legend_var,y=prediction_label)+ggplot2::theme_minimal(base_size=13)
  notes<-c(notes,'交互作用圖固定其他連續變項於中位數、類別於參考組；表列固定值。offset 固定為 1 人時。95% CI 為點估計區間，非同時信賴帶。')
  # Conditional stratum contrasts include covariance between main and interaction terms.
  if(is.factor(z[[inter[2]]])) {
   zz<-inter[2];xx<-inter[1];ctr<-list()
   for(l in levels(z[[zz]])) {
    nd0<-baseline;nd1<-baseline;nd0[[zz]]<-nd1[[zz]]<-factor(l,levels=levels(z[[zz]]))
    if(is.factor(z[[xx]])){need(nlevels(z[[xx]])==2,'分層效果圖目前要求暴露為二類別或連續。');nd0[[xx]]<-factor(levels(z[[xx]])[1],levels=levels(z[[xx]]));nd1[[xx]]<-factor(levels(z[[xx]])[2],levels=levels(z[[xx]]))}else nd1[[xx]]<-nd0[[xx]]+1
    delta<-drop(model.matrix(delete.response(terms(fit)),nd1)-model.matrix(delete.response(terms(fit)),nd0));bb<-sum(delta*b);vv<-drop(t(delta)%*%V%*%delta);ctr[[l]]<-coeftable(setNames(bb,paste(zz,l)),matrix(vv,1,1),model!='linear',label=label)
   }
   tables$Stratified<-dplyr::bind_rows(ctr);plots$Stratified_forest<-forest(tables$Stratified,label,if(model=='linear')0 else 1)
  }
  if(model %in% c('rr','logistic')&&design %in% c('cohort','rct','cross')&&all(vapply(z[inter],function(v)is.factor(v)&&nlevels(v)==2,logical(1)))) {
   # Standardize individual predicted probabilities under four exposure settings.
   # Logistic provides bounded probabilities; RR from standardized risks is
   # computed directly, never by interpreting OR/HR as RR.
   riskfun<-function(a) {
    ff<-model_fit(a,y,x,'logistic',inter)
    risks<-numeric(4);comb<-expand.grid(A=0:1,Z=0:1)
    for(j in 1:4){nd<-a;for(i in 1:2)nd[[inter[i]]]<-factor(levels(z[[inter[i]]])[comb[j,i]+1],levels=levels(z[[inter[i]]]));risks[j]<-mean(predict(ff,nd,type='response'))}
    rr<-risks/risks[1];reri<-rr[4]-rr[2]-rr[3]+1;den<-rr[2]+rr[3]-2
    c(Risk00=risks[1],Risk10=risks[2],Risk01=risks[3],Risk11=risks[4],RD11_00=risks[4]-risks[1],RERI=reri,AP=reri/rr[4],S=if(rr[2]>1&&rr[3]>1&&den>0)(rr[4]-1)/den else NA_real_)
   }
   pointfit<-model_fit(z,y,x,'logistic',inter)
   comb<-expand.grid(A=0:1,Z=0:1)
   mms<-lapply(1:4,function(j){nd<-z;for(i in 1:2)nd[[inter[i]]]<-factor(levels(z[[inter[i]]])[comb[j,i]+1],levels=levels(z[[inter[i]]]));model.matrix(delete.response(terms(pointfit)),nd)})
   from_beta<-function(b){risks<-vapply(mms,function(mm)mean(plogis(drop(mm%*%b))),numeric(1));rr<-risks/risks[1];reri<-rr[4]-rr[2]-rr[3]+1;den<-rr[2]+rr[3]-2;c(Risk00=risks[1],Risk10=risks[2],Risk01=risks[3],Risk11=risks[4],RD11_00=risks[4]-risks[1],RERI=reri,AP=reri/rr[4],S=if(rr[2]>1&&rr[3]>1&&den>0)(rr[4]-1)/den else NA_real_)}
   pt<-from_beta(coef(pointfit));se<-rep(NA_real_,length(pt))
   for(j in which(is.finite(pt))){gr<-numDeriv::grad(function(b)from_beta(b)[j],coef(pointfit));se[j]<-sqrt(drop(t(gr)%*%vcov(pointfit)%*%gr))}
   low<-pt-1.96*se;high<-pt+1.96*se
   for(j in 1:4){sl<-se[j]/(pt[j]*(1-pt[j]));low[j]<-plogis(qlogis(pt[j])-1.96*sl);high[j]<-plogis(qlogis(pt[j])+1.96*sl)}
   pv<-rep(NA_real_,length(pt));pv[5:7]<-2*pnorm(-abs(pt[5:7]/se[5:7]))
   if(is.finite(pt[8])&&pt[8]>0){sl<-se[8]/pt[8];low[8]<-exp(log(pt[8])-1.96*sl);high[8]<-exp(log(pt[8])+1.96*sl);pv[8]<-2*pnorm(-abs(log(pt[8])/sl))}else{low[8]<-high[8]<-NA_real_}
   tabs<-data.frame(Term=names(pt),Estimate=pt,CI_low=low,CI_high=high,p_value=pv,row.names=NULL)
   if(boot){bt<-boot_cases(z,riskfun,B,seed,progress);tabs<-bt$table;tabs$p_value<-pv}
   tables$Additive_interaction<-tabs
   notes<-c(notes,'標準化風險的 delta 推論以觀察到的共變項分布為固定；個案 Bootstrap 則同時反映重抽樣下的共變項分布變動。')
   rd<-tabs[grepl('^Risk',tabs$Term),];rp<-ggplot2::ggplot(rd,ggplot2::aes(Term,Estimate))+ggplot2::geom_col(fill='#315bea');if('CI_low'%in%names(rd))rp<-rp+ggplot2::geom_errorbar(ggplot2::aes(ymin=CI_low,ymax=CI_high),width=.15)
   plots$Absolute_risk<-rp+ggplot2::labs(y=if(design=='cross')'Standardized prevalence' else 'Standardized risk')+ggplot2::theme_minimal()
   notes<-c(notes,'加法交互作用另以 Logistic 標準化四組風險計算 RERI、AP、S；未把 OR／HR 當 RR。00 由兩變項參考組決定。S 僅在兩個單獨暴露均增加風險時輸出，log S 推論另需 S>0。RERI／AP／RD 使用 delta Wald p（虛無值 0），S 使用 log-delta Wald p（虛無值 1）。預設 CI 為 delta 法，風險在 logit 尺度計算；勾選 Bootstrap 時 CI 改為百分位法，p 值仍為 delta Wald。未對四組風險檢定為零。橫斷性為盛行比例尺度。')
  }
 }
 ans<-out(paste('迴歸',label),tables,plots,notes);ans$publication<-model_publication(tables$Coefficients,z,x,label,y);attach_diagnostics(add_publication_forest(ans),fit,model)
}

effects2x2 <- function(d,y,e,event,exposed,design) {
 z<-cc(d,c(y,e));need(length(unique(z[[y]]))==2&&length(unique(z[[e]]))==2,'二乘二分析需要二元結果及暴露。')
 need(event %in% as.character(z[[y]])&&exposed %in% as.character(z[[e]]),'請指定有效事件與暴露組。')
 yy<-as.character(z[[y]])==event;ee<-as.character(z[[e]])==exposed
 a<-sum(ee&yy);b<-sum(ee&!yy);c<-sum(!ee&yy);dd<-sum(!ee&!yy)
 tab<-matrix(c(dd,c,b,a),2,2,byrow=TRUE,dimnames=list(Exposure=c('Reference','Exposed'),Outcome=c('No event','Event')))
 fisher<-fisher.test(tab);t<-data.frame(Term='OR (conditional exact)',Estimate=unname(fisher$estimate),CI_low=fisher$conf.int[1],CI_high=fisher$conf.int[2],p_value=fisher$p.value)
 notes<-c('OR 使用 Fisher 條件最大概似估計與精確 CI；p 值為 Fisher 精確檢定。沒有自動加 0.5。',analysis_note)
 if(design %in% c('cohort','rct','cross')) {
  p1<-a/(a+b);p0<-c/(c+dd);rd<-p1-p0
  # Newcombe hybrid score interval for difference of independent proportions.
  wilson<-function(k,n){z<-qnorm(.975);p<-k/n;center<-(p+z*z/(2*n))/(1+z*z/n);half<-z*sqrt(p*(1-p)/n+z*z/(4*n*n))/(1+z*z/n);c(center-half,center+half)}
  w1<-wilson(a,a+b);w0<-wilson(c,c+dd);lo<-rd-sqrt((p1-w1[1])^2+(w0[2]-p0)^2);hi<-rd+sqrt((w1[2]-p1)^2+(p0-w0[1])^2)
  rt<-if(design=='cross')'PR' else 'RR';rr<-p1/p0;sr<-if(a>0&&c>0)sqrt(1/a-1/(a+b)+1/c-1/(c+dd))else NA_real_
  t<-rbind(t,data.frame(Term=rt,Estimate=rr,CI_low=exp(log(rr)-1.96*sr),CI_high=exp(log(rr)+1.96*sr),p_value=NA_real_),data.frame(Term=if(design=='cross')'Prevalence difference' else 'RD (attributable risk)',Estimate=rd,CI_low=lo,CI_high=hi,p_value=NA_real_))
  risk<-data.frame(Group=c('Reference','Exposed'),Risk=c(p0,p1),CI_low=c(w0[1],w1[1]),CI_high=c(w0[2],w1[2]))
  plots<-list(Risk=ggplot2::ggplot(risk,ggplot2::aes(Group,Risk))+ggplot2::geom_pointrange(ggplot2::aes(ymin=CI_low,ymax=CI_high),color='#315bea')+ggplot2::theme_minimal())
  notes<-c(notes,'AR 容易混淆：此處明確分開 absolute risk（各組風險）與 attributable risk＝RD。RR 使用 Katz log CI；零事件時不輸出該近似 CI。RD 使用 Newcombe score CI。世代／RCT 需有可比較的固定追蹤期間與完整結果；有設限請使用存活分析。')
 } else {risk<-data.frame();plots<-list();notes<-c(notes,'研究設計未指定或病例對照：僅報 OR，不以樣本中病例比例推估風險、RR 或 RD。')}
 out('二元暴露效果量',list(Counts=mtab(tab),Effects=t,Absolute_risks=risk),plots,notes)
}

survival_analysis <- function(d,time,event,eventlevel,x=character(),group='',id='',start='',ag=FALSE,inter=NULL) {
 vars<-c(time,event,x,group[nzchar(group)],id[nzchar(id)],start[nzchar(start)]);z<-cc(d,vars)
 if(length(inter))need(length(inter)==2&&all(inter%in%x)&&length(unique(inter))==2,'Cox 交互作用需兩個已選取的不同共變項。')
 for(v in x)if(is.factor(z[[v]])){ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1];need(ref%in%levels(z[[v]]),paste(v,'指定參考組在完整個案中無資料。'));z[[v]]<-relevel(factor(z[[v]],levels=levels(z[[v]]),ordered=FALSE),ref=ref)}
 need(is.numeric(z[[time]])&&all(z[[time]]>=0),'追蹤時間需為非負數值。')
 need(length(unique(z[[event]]))==2&&eventlevel %in% as.character(z[[event]]),'請確認事件指標與事件組；需同時有事件與設限。');z$.event<-as.integer(as.character(z[[event]])==eventlevel)
 tables<-list();plots<-list();notes<-c(sprintf('完整個案 N=%d；事件 %d。',nrow(z),sum(z$.event)),analysis_note)
 if(ag) {
  need(nzchar(id)&&nzchar(start),'AG 需要 ID 與起始時間。');need(is.numeric(z[[start]])&&all(z[[start]]>=0&z[[start]]<z[[time]]),'需滿足 0 ≤ start < stop。')
  for(ii in split(seq_len(nrow(z)),z[[id]])){zz<-z[ii,,drop=FALSE];zz<-zz[order(zz[[start]],zz[[time]]),,drop=FALSE];if(nrow(zz)>1)need(all(zz[[start]][-1]>=head(zz[[time]],-1)),'同一 ID 的事件區間重疊。')}
  response<-paste0('survival::Surv(',start,',',time,',.event)')
  fit<-survival::coxph(form(response,x,inter),z,cluster=z[[id]],robust=TRUE,x=TRUE,model=TRUE)
  notes<-c(notes,'AG：以受試者 ID 聚集的穩健 sandwich 標準誤；不提供一般 LRT。需另評估獨立增量、事件過程與資訊性設限。')
 }else {
  if(nzchar(id))need(!anyDuplicated(z[[id]]),'一般存活分析需每位受試者一列；重複區間請使用 AG。')
  response<-paste0('survival::Surv(',time,',.event)')
  km<-survival::survfit(form(response,if(nzchar(group))group else '1'),z)
  ss<-summary(km);kmdata<-data.frame(Time=ss$time,Survival=ss$surv,CI_low=ss$lower,CI_high=ss$upper,At_risk=ss$n.risk,Events=ss$n.event,Group=if(is.null(ss$strata))'Overall' else as.character(ss$strata))
  # Include t=0 baseline so the KM plot does not start at the first event.
  base<-data.frame(Time=0,Survival=1,CI_low=1,CI_high=1,At_risk=as.numeric(km$n),Events=0,Group=if(is.null(km$strata))'Overall' else names(km$strata));pd<-rbind(base,kmdata)
  tables$KM<-kmdata;plots$KM<-ggplot2::ggplot(pd,ggplot2::aes(Time,Survival,color=Group))+ggplot2::geom_step()+ggplot2::geom_step(ggplot2::aes(y=CI_low),lty=3)+ggplot2::geom_step(ggplot2::aes(y=CI_high),lty=3)+ggplot2::coord_cartesian(ylim=c(0,1))+ggplot2::theme_minimal(base_size=13)
  if(nzchar(group)){lr<-survival::survdiff(form(response,group),z);tables$Logrank<-data.frame(Chi_square=lr$chisq,df=length(lr$n)-1,p_value=pchisq(lr$chisq,length(lr$n)-1,lower.tail=FALSE))}
  if(!length(x))return(out('Kaplan–Meier / log-rank',tables,plots,notes))
  fit<-survival::coxph(form(response,x,inter),z,x=TRUE,model=TRUE)
 }
 checkfit(fit);tables$Cox<-coeftable(coef(fit),vcov(fit),TRUE,label='HR');plots$Forest<-forest(tables$Cox,'HR')
 if(!ag){ph<-survival::cox.zph(fit);tables$PH_test<-mtab(ph$table);notes<-c(notes,'HR 是瞬時危險比，不是 RR。比例危險假設以 Schoenfeld 檢定評估，亦需配合殘差圖與研究知識。')}
 if(length(inter)==2){idx<-grep(':',names(coef(fit)));need(length(idx)>0,'無有效交互作用。');bb<-coef(fit)[idx];VV<-vcov(fit)[idx,idx,drop=FALSE];w<-drop(t(bb)%*%solve(VV,bb));tables$Interaction_Wald<-data.frame(Chi_square=w,df=length(idx),p_value=pchisq(w,length(idx),lower.tail=FALSE))
  if(!ag){f0<-survival::coxph(form(response,x),z);lr<-2*as.numeric(logLik(fit)-logLik(f0));tables$Interaction_LRT<-data.frame(Chi_square=lr,df=length(idx),p_value=pchisq(lr,length(idx),lower.tail=FALSE))
   nd<-z[1,,drop=FALSE];for(v in x)nd[[v]]<-if(is.factor(z[[v]]))factor(levels(z[[v]])[1],levels=levels(z[[v]]))else median(z[[v]])
   grid<-expand.grid(lapply(z[inter],function(a)if(is.factor(a))levels(a)else as.numeric(quantile(a,c(.25,.75)))),stringsAsFactors=FALSE);nn<-nd[rep(1,nrow(grid)),,drop=FALSE];for(v in inter)nn[[v]]<-if(is.factor(z[[v]]))factor(grid[[v]],levels=levels(z[[v]]))else grid[[v]]
   sf<-survival::survfit(fit,newdata=nn);tt<-dplyr::bind_rows(lapply(seq_len(nrow(nn)),function(j)data.frame(Time=c(0,sf$time),Survival=c(1,sf$surv[,j]),Group=paste(paste(inter,grid[j,],sep='='),collapse=', '))))
   plots$Adjusted_survival<-ggplot2::ggplot(tt,ggplot2::aes(Time,Survival,color=Group))+ggplot2::geom_step()+ggplot2::theme_minimal();tables$Curve_profiles<-nn[,x,drop=FALSE]
  }
 }
 ans<-out(if(ag)'Andersen–Gill 復發事件' else '存活分析',tables,plots,notes);ans$publication<-model_publication(tables$Cox,z,x,'HR',event);attach_diagnostics(add_publication_forest(ans),fit,if(ag)'ag'else'cox')
}

iv_analysis <- function(d,y,exposure,instruments,covars=character()) {
 need(length(instruments)>0&&length(unique(c(y,exposure,instruments,covars)))==length(c(y,exposure,instruments,covars)),'結果、內生暴露、工具與外生共變項不得重複。')
 z<-cc(d,c(y,exposure,instruments,covars));need(is.numeric(z[[y]])&&is.numeric(z[[exposure]]),'此 2SLS 模組限定連續結果與連續內生暴露。')
 X<-model.matrix(form(y,c(exposure,covars)),z);Z<-model.matrix(form(y,c(instruments,covars)),z);Y<-z[[y]]
 need(qr(X)$rank==ncol(X)&&qr(Z)$rank==ncol(Z),'工具或模型矩陣不滿秩。')
 Xh<-qr.fitted(qr(Z),X);A<-crossprod(Xh);need(qr(A)$rank==ncol(X),'弱識別／模型不可識別。')
 b<-drop(solve(A,crossprod(Xh,Y)));names(b)<-colnames(X);u<-Y-drop(X%*%b);V<-solve(A)%*%crossprod(Xh*as.numeric(u))%*%solve(A)*nrow(z)/(nrow(z)-ncol(X))
 first<-lm(form(exposure,c(instruments,covars)),z);first0<-lm(form(exposure,if(length(covars))covars else '1'),z);ft<-anova(first0,first)
 tt<-coeftable(b,V,FALSE,Inf,'Beta');out('工具變項 / 2SLS',list(Coefficients=tt,First_stage=data.frame(F=ft$F[2],df1=ft$Df[2],df2=df.residual(first),p_value=ft$`Pr(>F)`[2])),list(Forest=forest(tt,'Beta',0)),c('2SLS 變異數以原始結構殘差與投影設計矩陣計算 HC1；不是將第二階段普通 OLS 標準誤當作有效推論。第一階段 F 為同方差部分 F 診斷；不是群集穩健弱工具檢定。','工具相關性、排除限制與獨立性需由研究設計支持。未實作 Hansen J／Wu–Hausman；不顯示虛構診斷。',analysis_note))
}
roc_analysis <- function(d,y,predictor,event,direction='<',B=5000,seed=20260910,boot=FALSE) {
 z<-cc(d,c(y,predictor));need(is.numeric(z[[predictor]])&&length(unique(z[[y]]))==2&&event %in% as.character(z[[y]]),'ROC 需數值預測指標、二元結果與有效事件組。')
 response<-as.integer(as.character(z[[y]])==event);r<-pROC::roc(response,z[[predictor]],levels=c(0,1),direction=direction,quiet=TRUE);set.seed(seed)
 ci<-pROC::ci.auc(r,method=if(boot)'bootstrap' else 'delong',boot.n=B,boot.stratified=TRUE)
 best<-as.data.frame(pROC::coords(r,'best',best.method='youden',ret=c('threshold','sensitivity','specificity'),transpose=FALSE))
 pd<-data.frame(FPR=1-r$specificities,TPR=r$sensitivities)
 out('ROC 分析',list(AUC=data.frame(AUC=as.numeric(pROC::auc(r)),CI_low=ci[1],CI_high=ci[3],Method=if(boot)paste('Stratified bootstrap',B)else'DeLong'),Youden=best),list(ROC=ggplot2::ggplot(pd,ggplot2::aes(FPR,TPR))+ggplot2::geom_line(color='#315bea')+ggplot2::geom_abline(lty=2)+ggplot2::coord_equal()+ggplot2::theme_minimal()),c('事件方向由使用者指定，不以 AUC 自動翻轉。Youden 切點為同一資料的探索結果，可能樂觀；需獨立驗證，並非臨床最佳決策閾值。'))
}

sem_analysis <- function(d,syntax,ordered_vars=character(),estimator='MLR',B=5000,boot=FALSE,seed=20260910,validation='same',progress=function(...)NULL,node_labels=NULL,outcome=NULL,measurement_report=FALSE) {
 need(nzchar(trimws(syntax)),'請填寫事先指定的 lavaan CFA／SEM 模型；系統不猜設構面。')
 pt<-lavaan::lavaanify(syntax)
 d<-sem_outcome_data(d,outcome)
 type_spec<-sem_variable_spec(d,pt,ordered_vars,estimator)
 if(!is.null(outcome)&&outcome$kind=='observed')type_spec$table$Source[type_spec$table$Variable==outcome$variable]<-'Y 獨立設定'
 vars<-type_spec$variables;ordered_vars<-type_spec$ordered;z<-cc(d,vars)
 for(v in ordered_vars){
  # Complete-case exclusion may remove a level; preserve the relative order of
  # the remaining levels, and record both configured and analyzed orders.
  lev<-type_spec$levels[[v]];lev<-lev[lev%in%as.character(z[[v]])]
  need(length(lev)>=2,paste(v,'在完整個案中少於兩個有效類別，無法估計順序題。'))
  z[[v]]<-ordered(z[[v]],levels=lev)
 }
 type_spec$table$Analyzed_level_order<-vapply(vars,function(v)if(v%in%ordered_vars)paste(levels(z[[v]]),collapse=' < ')else'',character(1))
 fitfun<-function(a,se=TRUE){f<-lavaan::sem(syntax,data=a,ordered=if(length(ordered_vars))ordered_vars else NULL,estimator=estimator,missing='listwise',se=if(se)'standard' else 'none',test=if(se)'standard' else 'none');need(lavaan::lavInspect(f,'converged'),'SEM 未收斂。');need(lavaan::lavInspect(f,'post.check'),'SEM 有負變異數或非正定潛在矩陣。');f}
 # Do not override estimator-specific robust SE/test defaults on the main fit.
 fit<-lavaan::sem(syntax,data=z,ordered=if(length(ordered_vars))ordered_vars else NULL,estimator=estimator,missing='listwise')
 need(lavaan::lavInspect(fit,'converged'),'SEM 未收斂。');need(lavaan::lavInspect(fit,'post.check'),'SEM 出現不適當解（例如負變異數），停止推論。')
 pe<-lavaan::parameterEstimates(fit,standardized=TRUE,ci=TRUE);names(pe)[names(pe)=='pvalue']<-'p_value'
 fm<-lavaan::fitMeasures(fit);keep<-intersect(names(fm),c('chisq','df','pvalue','cfi','tli','rmsea','rmsea.ci.lower','rmsea.ci.upper','srmr','chisq.scaled','df.scaled','pvalue.scaled','cfi.robust','tli.robust','rmsea.robust','rmsea.ci.lower.robust','rmsea.ci.upper.robust'))
 tabs<-list(Parameters=pe,Fit=data.frame(Index=keep,Value=unname(fm[keep])),SEM_variable_types=type_spec$table)
 notes<-c(sprintf('N=%d；估計法 %s；完整個案分析。',nrow(z),estimator),'CFA／SEM 驗證使用者指定的模型，不自動證實因果。請依自由度、估計法與理論綜合判讀配適度。',if(validation=='same')'EFA 與 CFA 若使用同一資料屬內部驗證，配適度可能樂觀。' else '使用者聲明使用獨立驗證樣本；系統無法替代研究流程查證。')
 if(fm['df']==0)notes<-c(notes,'模型剛好識別（df=0），整體配適度不能用來證明模型正確。')
 measurement<-paste(syntax,collapse='\n');lf<-lavaan::lavNames(fit,'lv')
 if(length(lf)) {
  av<-tryCatch(semTools::AVE(fit),error=function(e)NULL);cr<-tryCatch(semTools::compRelSEM(fit),error=function(e)NULL)
  if(!is.null(av))tabs$AVE<-data.frame(Factor=names(av),AVE=as.numeric(av))
  if(!is.null(cr))tabs$Composite_reliability<-data.frame(Factor=names(cr),Reliability=as.numeric(cr))
  if(length(lf)>=2){cfa_syntax<-paste(vapply(lf,function(f)paste(f,'=~',paste(pt$rhs[pt$op=='=~'&pt$lhs==f],collapse=' + ')),character(1)),collapse='\n')
   ht_ordered<-intersect(ordered_vars,pt$rhs[pt$op=='=~'])
   ht<-semTools::htmt(cfa_syntax,data=z,ordered=if(length(ht_ordered))ht_ordered else NULL,htmt2=TRUE);tabs$HTMT2<-mtab(ht)
   fc<-lavaan::lavInspect(fit,'cor.lv');if(!is.null(av)&&all(lf %in% names(av)))diag(fc)<-sqrt(av[lf]);tabs$Fornell_Larcker<-mtab(fc)
   notes<-c(notes,'區辨效度列 HTMT2（幾何平均版本）及 Fornell–Larcker：對角線為 √AVE、非對角線為潛在相關；門檻僅供參考，非自動通過／不通過。')
  }
 }
 if(boot) {
  # External respondent-level bootstrap also supports WLSMV, unlike asking
  # lavaan for its internal ML bootstrap under an incompatible estimator.
  key<-paste(pe$lhs,pe$op,pe$rhs)
  bf<-function(a){f<-fitfun(a,FALSE);p<-lavaan::parameterEstimates(f);v<-p$est[match(key,paste(p$lhs,p$op,p$rhs))];names(v)<-key
   if(length(lf)>=2){h<-semTools::htmt(cfa_syntax,data=a,ordered=if(length(ht_ordered))ht_ordered else NULL,htmt2=TRUE);ii<-which(lower.tri(h),arr.ind=TRUE);hv<-h[lower.tri(h)];names(hv)<-paste('HTMT2',rownames(h)[ii[,1]],colnames(h)[ii[,2]]);v<-c(v,hv)}
   v}
  bt<-boot_cases(z,bf,B,seed,progress);tabs$Bootstrap_parameters<-bt$table
  if(length(bt$errors))tabs$Bootstrap_failures<-data.frame(Reason=names(bt$errors),Count=as.integer(bt$errors))
  notes<-c(notes,bootnote,'SEM Bootstrap 每次重新配適相同估計法；表列未標準化參數、自訂間接效果，以及多構面 HTMT2。原始 p 值仍來自主模型漸近推論，非 Bootstrap p 值。')
 }
 paths<-pe[pe$op %in% c('~','=~'),];pd<-data.frame(Term=paste(paths$lhs,paths$op,paths$rhs),Estimate=paths$est,CI_low=paths$ci.lower,CI_high=paths$ci.upper)
 plots<-list(Paths=forest(pd,'Estimate',0))
 if(nrow(paths)>0){
  figure<-tryCatch(sem_publication_diagram(fit,node_labels),error=function(e)e)
  if(inherits(figure,'error'))notes<-c(notes,paste('路徑圖未產生：',conditionMessage(figure)))else{plots$SEM_diagram<-figure$plot;tabs$SEM_fit_summary<-figure$fit_table;tabs$SEM_diagram_edges<-figure$edge_table;tabs$SEM_residual_variances<-figure$residual_table}
 }
 cr<-lavaan::lavResiduals(fit,type='cor')$cov;tabs$Residual_correlations<-mtab(cr);tabs$SEM_assumptions<-data.frame(Assessment=c('已檢查收斂及不適當解；此檢查不等於模型正確。','ML 假定的條件分布與 MLR／WLSMV 穩健修正需配合資料型態；單變項常態不等於多變項常態。','檢查殘差相關及整體配適；不依 modification indices 自動新增路徑。','獨立性、測量等值性、時間順序與因果識別需另行評估，未由配適度驗證。'))
 if(length(ordered_vars))notes<-c(notes,paste('實際以順序類別估計：',paste(ordered_vars,collapse='、'),'；題型來源與級別順序見 SEM_variable_types。'))
 if(!is.null(outcome)){tabs$SEM_outcome<-sem_outcome_table(outcome);notes<-c(notes,if(outcome$type=='binary')paste0('二元 Y：',outcome$variable,'；事件組 ',outcome$event,'；非事件組 ',outcome$reference,'。結果採類別反應的 probit 潛在反應尺度，不是 Logistic OR。')else paste('終點結果 Y：',outcome$label,'；型態',outcome$type))}
 report<-if(measurement_report)cfa_result_report(fit,tabs,node_labels,ordered_vars)else NULL
 if(!is.null(report)){tabs<-c(tabs,report$tables);notes<-c(notes,report$notes)}
 ans<-out('CFA / 結構方程模型',tabs,plots,c(notes,analysis_note));ans$sem_type_spec<-type_spec;ans$sem_outcome<-outcome
 if(!is.null(report))ans$measurement_publications<-report$publications
 ans
}
