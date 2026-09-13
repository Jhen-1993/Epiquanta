# Correlated observations: explicit grouping and complete-case provenance.
# IDs identify dependence; they never enter the fixed effects automatically.
correlated_models<-function(mode,kind,design){
 if(design=='casecontrol')return(character())
 if(mode=='gee')switch(kind,continuous=c('Gaussian／identity → β'='gaussian'),binary=c('Binomial／logit → OR'='logistic',setNames('modified_poisson',if(design=='cross')'Modified Poisson／log → PR'else'Modified Poisson／log → RR')),count=c('Poisson／log → 計數比或 IRR'='poisson'),character())
 else switch(kind,continuous=c('線性混合模型 LMM → β'='gaussian'),binary=c('Logistic 混合模型 GLMM → 條件 OR'='logistic'),count=c('Poisson 混合模型 GLMM → 條件計數比或 IRR'='poisson'),character())
}

correlated_prepare<-function(d,y,x,id,structure,time='',model='gaussian',event='',offsetvar='',inter=NULL){
 need(structure%in%c('repeated','clustered','matched'),'GEE／混合模型請指定重複測量、配對或單一層級群聚結構。')
 need(length(y)==1&&length(id)==1&&nzchar(y)&&nzchar(id)&&y!=id,'請指定不同的結果與個案／群聚 ID。')
 need(length(x)>0&&!any(c(y,id,offsetvar[nzchar(offsetvar)])%in%x),'請勾選固定效果；結果、ID 與 offset 不可同時作為固定效果。')
 need(!nzchar(offsetvar)||model=='poisson','offset 僅供計數 Poisson；二元 Modified Poisson 不使用人時 offset。')
 need(!nzchar(offsetvar)||!offsetvar%in%c(y,id,time),'offset 不可與結果、ID 或時間欄相同。')
 if(length(inter))need(length(inter)==2&&length(unique(inter))==2&&all(inter%in%x),'交互作用須指定兩個已納入的固定效果。')
 if(structure=='repeated')need(length(time)==1&&nzchar(time)&&!time%in%c(y,id),'長格式重複測量需指定與結果、ID 不同的時間／訪視欄。')
 if(structure!='repeated')time<-''
 vars<-unique(c(y,x,id,time[nzchar(time)],offsetvar[nzchar(offsetvar)]));need(all(vars%in%names(d)),'所選欄位不存在。')
 # Duplicate keys are checked BEFORE excluding missing outcome/covariate rows.
 if(nzchar(time)){
  keys<-d[complete.cases(d[,c(id,time),drop=FALSE]),c(id,time),drop=FALSE]
  need(!anyDuplicated(keys),'同一 ID＋時間／訪視出現重複列。請先釐清資料層級或合併規則；程式不會自動平均。')
 }
 z<-cc(d,vars)
 for(v in vars)if(is.numeric(z[[v]]))need(all(is.finite(z[[v]])),paste(v,'含非有限值。'))
 for(v in x)if(is.factor(z[[v]])){
  ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1]
  need(ref%in%levels(z[[v]]),paste(v,'指定參考組在完整資料中不存在。'))
  z[[v]]<-relevel(factor(z[[v]],ordered=FALSE),ref=ref)
 }
 if(model=='gaussian')need(is.numeric(z[[y]])&&length(unique(z[[y]]))>1,'連續結果需為可變動的有限數值。')
 if(model%in%c('logistic','modified_poisson')){
  need(length(unique(z[[y]]))==2&&length(event)==1&&event%in%as.character(z[[y]]),'二元結果需恰好兩類，並指定事件組。')
  z[[y]]<-as.integer(as.character(z[[y]])==event)
 }
 if(model=='poisson')need(is.numeric(z[[y]])&&all(z[[y]]>=0&z[[y]]==floor(z[[y]]))&&any(z[[y]]>0),'Poisson 結果需為非負整數，且不可全部為零。')
 if(nzchar(offsetvar))need(is.numeric(z[[offsetvar]])&&all(z[[offsetvar]]>0),'人時／offset 必須為正的有限數值。')
 z[[id]]<-factor(z[[id]]);z<-z[if(nzchar(time))order(z[[id]],z[[time]])else order(z[[id]]),,drop=FALSE]
 sizes<-table(z[[id]]);need(length(sizes)>=2&&any(sizes>1),'至少需要兩個獨立 ID，且必須有 ID 具有多筆觀察值。')
 f<-form(y,x,inter);if(nzchar(offsetvar))f<-update(f,paste('. ~ . + offset(log(',offsetvar,'))'))
 mm<-model.matrix(f,z);need(qr(mm)$rank==ncol(mm),'固定效果不可識別：請檢查共線性或類別空組。')
 refs<-dplyr::bind_rows(lapply(x,function(v)if(is.factor(z[[v]]))data.frame(Variable=v,Reference=levels(z[[v]])[1],Levels=paste(levels(z[[v]]),collapse=' | '))))
 list(z=z,formula=f,sizes=sizes,p=ncol(mm),time=time,reference=refs,
      structure=data.frame(Input_rows=nrow(d),Analyzed_rows=nrow(z),Excluded_rows=nrow(d)-nrow(z),Input_IDs=length(unique(na.omit(d[[id]]))),Analyzed_IDs=length(sizes),Min_rows_per_ID=min(sizes),Median_rows_per_ID=median(sizes),Max_rows_per_ID=max(sizes),Singleton_IDs=sum(sizes==1),ID=id,Time=if(nzchar(time))time else 'Not applicable'))
}

correlated_analysis<-function(d,y,x,id,mode='gee',model='gaussian',design='cohort',structure='repeated',time='',event='',corstr='exchangeable',slope='',offsetvar='',inter=NULL){
 need(mode%in%c('gee','mixed')&&design%in%c('cohort','rct','cross'),'此 GEE／混合模型入口支援世代研究、RCT、橫斷性；配對病例對照請用條件式 Logistic。')
 need(model%in%c('gaussian','logistic','poisson',if(mode=='gee')'modified_poisson'),'此模型尚未支援。')
 prep<-correlated_prepare(d,y,x,id,structure,time,model,event,offsetvar,inter);z<-prep$z;f<-prep$formula;environment(f)<-environment();G<-length(prep$sizes)
 family<-switch(model,gaussian=gaussian(),logistic=binomial(),poisson=poisson(),modified_poisson=poisson())
 label<-switch(model,gaussian='Beta',logistic='OR',modified_poisson=if(design=='cross')'PR'else'RR',poisson=if(nzchar(offsetvar))'IRR'else'Count_ratio')
 tables<-list(Data_structure=prep$structure,Reference=prep$reference);plots<-list()
 notes<-c(sprintf('以 %s 表示相依性；%d 個獨立 ID、%d 筆觀察值。完整個案排除 %d 列。',id,G,nrow(z),nrow(d)-nrow(z)),
          '此處支援單一群聚層級。受試者另隸屬醫院、多階層或交叉群聚資料需另建模型；此入口不使用複雜抽樣權重、分層或 FPC。',
          '類別固定效果以指定參考組建立 k−1 個虛擬變項；未勾選的共變項不會自動加入。',analysis_note)
 if(nzchar(prep$time)&&!prep$time%in%x)notes<-c(notes,'時間欄僅用於資料結構與相關順序；未加入固定效果。若要估計時間趨勢，請明確勾選時間變項。')
 if(model%in%c('logistic','modified_poisson'))notes<-c(notes,paste('事件組＝',event,'；其餘類別＝非事件。'))
 if(mode=='gee'){
  need(requireNamespace('geepack',quietly=TRUE),'請先安裝 geepack。')
  need(corstr%in%c('independence','exchangeable','ar1'),'請選擇已支援的工作相關結構。')
  need(G>prep$p,'獨立群聚數須大於固定效果係數數量，否則穩健共變異矩陣無法完整識別。')
  # geeglm requires contiguous clusters; sorted above, preserve scheduled gaps.
  gid<-as.integer(z[[id]]);wv<-NULL
  if(corstr=='ar1'){
   need(structure=='repeated','AR(1) 只供具有明確訪視順序的重複測量。')
   w<-z[[prep$time]];need(is.numeric(w)&&all(is.finite(w))&&all(w==floor(w)),'AR(1) 需數值整數的等距訪視編號（例如 0、1、2）；不可用類別代碼順序或不等距時間猜設。')
   need(diff(range(w))<=1000,'AR(1) 訪視編號間距超過 1000；請使用明確等距且合理範圍的訪視編號。')
   # geeglm internally factorizes waves. Explicit levels preserve scheduled
   # gaps even when a visit is absent from EVERY cluster.
   wv<-factor(w-min(w)+1,levels=seq_len(diff(range(w))+1));need(length(unique(wv))>=2,'AR(1) 至少需要兩個訪視時間。')
  }
  # scale.value defaults to 1; do not pass it through geeglm's model.frame.
  fit<-geepack::geeglm(f,data=z,id=gid,waves=wv,family=family,corstr=corstr,std.err='san.se',scale.fix=model!='gaussian')
  need(fit$geese$error==0,'GEE 未收斂，停止推論。')
  need(all(is.finite(coef(fit))),'GEE 係數不可識別。')
  if(model=='logistic')need(!any(fitted(fit)<1e-8|fitted(fit)>1-1e-8),'GEE 預測接近 0／1，可能存在完全或近完全分離，停止一般 Wald 推論。')
  b<-coef(fit);V<-fit$geese$vbeta;dimnames(V)<-list(names(b),names(b))
  need(all(is.finite(V))&&all(diag(V)>0)&&qr(V)$rank==length(b),'GEE 穩健共變異矩陣不可識別。')
  tables$Coefficients<-coeftable(b,V,model!='gaussian',label=label)
  tables$Working_correlation<-if(length(fit$geese$alpha))data.frame(Structure=corstr,Parameter=names(fit$geese$alpha) %||% paste0('alpha',seq_along(fit$geese$alpha)),Estimate=as.numeric(fit$geese$alpha))else data.frame(Structure=corstr,Parameter='Fixed working independence',Estimate=0)
  tables$Estimation<-data.frame(Estimator='GEE: robust sandwich, asymptotic Wald',Family=family$family,Link=family$link,Converged=TRUE)
  notes<-c(notes,'GEE 估計群體平均（population-averaged）效果；使用群聚 sandwich 穩健標準誤與大樣本 Wald 95% CI／p 值。工作相關設定不等於已證實真實相關結構。',
           '目前為未加權完整個案 GEE。結果缺失常需 MCAR 或額外條件；MAR 不自動保證本估計有效，失訪可能需要加權 GEE／多重插補。')
  if(G<40)notes<-c(notes,'獨立群聚少於 40：此為提醒門檻，非適用性的定理；未施加小樣本 sandwich 校正，Wald CI／p 值可能不穩定。')
  if(corstr=='ar1')notes<-c(notes,'AR(1) 依指定訪視編號差距計算相關，保留缺訪間隔；請確認每一編號步長有相同時間意義。')
  if(model=='modified_poisson'&&any(fitted(fit)>1))notes<-c(notes,'Modified Poisson 出現預測值 > 1；不可直接將其視為合法個人絕對風險。')
  resid<-residuals(fit,type='pearson');fitval<-fitted(fit)
 }else{
  need(requireNamespace('lme4',quietly=TRUE)&&requireNamespace('lmerTest',quietly=TRUE),'請先安裝 lme4、lmerTest。')
  need(length(slope)<=1,'目前每個模型支援一個數值隨機斜率。')
  if(nzchar(slope)){
   need(slope%in%x&&is.numeric(z[[slope]]),'隨機斜率須為已納入固定效果的連續數值變項。')
   varied<-tapply(z[[slope]],z[[id]],function(v)length(unique(v))>1)
   need(sum(varied)>=3,'至少三個 ID 內需有隨機斜率變項的變動。')
  }
  rf<-as.formula(paste(paste(deparse(f),collapse=' '),'+ (1',if(nzchar(slope))paste('+',slope)else'', '|',id,')'))
  fit<-if(model=='gaussian')lmerTest::lmer(rf,data=z,REML=TRUE)else lme4::glmer(rf,data=z,family=family,nAGQ=1,control=lme4::glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=100000)))
  b<-lme4::fixef(fit);V<-as.matrix(vcov(fit));opt<-fit@optinfo
  need(all(is.finite(b))&&all(is.finite(V))&&all(diag(V)>0),'混合模型係數或共變異矩陣不可識別。')
  msgs<-opt$conv$lme4$messages;hardmsgs<-msgs[!grepl('singular',msgs,ignore.case=TRUE)]
  need((is.null(opt$conv$opt)||all(opt$conv$opt==0))&&!length(hardmsgs),paste('混合模型未收斂：',paste(hardmsgs,collapse='；')))
  singular<-lme4::isSingular(fit)
  if(model=='gaussian'){
   sm<-coef(summary(fit,ddf='Satterthwaite'));df<-sm[names(b),'df']
   tables$Coefficients<-coeftable(b,V,FALSE,df=df,label=label);tables$Coefficients$df<-df
  }else tables$Coefficients<-coeftable(b,V,TRUE,label=label)
  tables$Random_effects<-as.data.frame(lme4::VarCorr(fit))
  tables$Estimation<-data.frame(Estimator=if(model=='gaussian')'REML; Satterthwaite t'else'ML Laplace; asymptotic Wald z',Family=family$family,Link=family$link,Converged=TRUE,Singular=singular,Random_formula=paste(deparse(rf),collapse=' '))
  notes<-c(notes,if(model=='gaussian')'LMM 採 REML，固定效果 p 值與 t 型 CI 使用 Satterthwaite 自由度近似。'else'GLMM 採 Laplace ML，固定效果使用大樣本 Wald z CI／p 值；效果為給定隨機效果下的個案／群聚條件效果，與 GEE 群體平均效果不同。',
           '混合模型假設隨機效果分布適當，並依模型處理條件相關。LMM 另假設條件殘差常態與等變異；隨機截距不等於已處理所有時間相關。缺失可在模型正確與 MAR 等條件下由概似處理，但共變項缺失仍按列排除。')
  if(singular){tables$Coefficients$CI_low<-tables$Coefficients$CI_high<-tables$Coefficients$p_value<-NA_real_;notes<-c(notes,'隨機效果奇異擬合：保留點估計與變異成分供檢查，暫不顯示固定效果 CI／p 值；請依研究設計調整隨機效果，而非自動刪除。')}
  resid<-residuals(fit,type=if(model=='gaussian')'response'else'pearson');fitval<-fitted(fit)
  if(model=='gaussian'){
   idx<-unique(round(seq(1,length(resid),length.out=min(5000,length(resid)))))
   qq<-data.frame(Residual=sort(resid)[idx]);plots$Conditional_residual_QQ<-ggplot2::ggplot(qq,ggplot2::aes(sample=Residual))+ggplot2::stat_qq()+ggplot2::stat_qq_line()+ggplot2::theme_minimal()
   notes<-c(notes,'條件殘差 QQ 圖最多 5000 個等分位點；殘差不獨立，未套用獨立樣本 Shapiro–Wilk p 值。')
  }
 }
 idx<-unique(round(seq(1,nrow(z),length.out=min(5000,nrow(z)))))
 pd<-data.frame(Fitted=fitval[idx],Residual=resid[idx])
 plots$Residuals<-ggplot2::ggplot(pd,ggplot2::aes(Fitted,Residual))+ggplot2::geom_point(alpha=.35)+ggplot2::geom_hline(yintercept=0,lty=2)+ggplot2::theme_minimal()
 if(model=='poisson')tables$Dispersion<-data.frame(Pearson_dispersion=sum(resid^2)/max(1,nrow(z)-length(b)),Interpretation='Exploratory; no formal p-value. Mixed-model denominator approximate.')
 ans<-out(paste(if(mode=='gee')'GEE：群體平均效果'else'混合模型：個案／群聚條件效果',label),tables,plots,notes)
 ans$publication<-model_publication(tables$Coefficients,z,x,label,y)
 ans$publication$note<-paste(publication_note(ans$publication),if(mode=='gee')'GEE，群聚 sandwich Wald 推論。'else if(model=='gaussian')'LMM，Satterthwaite t 推論；奇異擬合時不提供 CI／p 值。'else'GLMM，條件效果及 Wald z 推論；奇異擬合時不提供 CI／p 值。')
 add_publication_forest(ans)
}

correlated_demo<-function(){
 set.seed(20260911);G<-120L;n<-G*5L;id<-rep(seq_len(G),each=5);visit<-rep(0:4,G)
 treatment<-factor(rep(rbinom(G,1,.5),each=5),levels=0:1);age<-rep(round(runif(G,30,75),1),each=5)
 ri<-rep(rnorm(G,0,1.2),each=5);rs<-rep(rnorm(G,0,.25),each=5)
 d<-data.frame(Subject_ID=id,Visit=visit,Treatment=treatment,Age=age)
 d$Score<-3+.6*num(treatment)+.4*visit+.15*num(treatment)*visit+.02*age+ri+rs*visit+rnorm(n)
 d$Binary<-factor(rbinom(n,1,plogis(-1+.5*num(treatment)+.15*visit+ri)),levels=0:1)
 d$Person_time<-runif(n,.5,2);d$Count<-rpois(n,exp(.1+.2*num(treatment)+.1*visit+.3*ri)*d$Person_time)
 d
}

paired_analysis<-function(d,y,id,occasion,a0,a1,method='paired_t',event=''){
 need(method%in%c('paired_t','signed_rank','mcnemar'),'請指定配對方法。')
 need(length(unique(c(y,id,occasion)))==3&&all(c(y,id,occasion)%in%names(d)),'結果、配對 ID、條件／訪視須為不同欄位。')
 need(length(a0)==1&&length(a1)==1&&a0!=a1&&all(c(a0,a1)%in%as.character(d[[occasion]])),'請選擇不同且存在的兩個比較條件。')
 z<-d[!is.na(d[[occasion]])&as.character(d[[occasion]])%in%c(a0,a1),c(y,id,occasion),drop=FALSE]
 keyed<-z[complete.cases(z[,c(id,occasion),drop=FALSE]),,drop=FALSE]
 need(!anyDuplicated(keyed[,c(id,occasion)]),'同一配對 ID 在同一條件出現多筆。配對檢定需一組一筆；不會自動平均。')
 nIDs<-length(unique(keyed[[id]]));z<-keyed[complete.cases(keyed),,drop=FALSE]
 z0<-z[as.character(z[[occasion]])==a0,,drop=FALSE];z1<-z[as.character(z[[occasion]])==a1,,drop=FALSE]
 ids<-intersect(as.character(z0[[id]]),as.character(z1[[id]]));need(length(ids)>=3,'完整配對不足 3 組。')
 v0<-z0[[y]][match(ids,as.character(z0[[id]]))];v1<-z1[[y]][match(ids,as.character(z1[[id]]))]
 tables<-list(Data_structure=data.frame(Input_rows=nrow(d),Rows_in_selected_conditions=nrow(keyed),IDs_in_selected_conditions=nIDs,Complete_pairs=length(ids),Excluded_incomplete_pairs=nIDs-length(ids),Reference_condition=a0,Comparison_condition=a1))
 notes<-c('長格式：每列為一個條件／訪視的觀察；依配對 ID 對齊，不依資料列順序配對。僅使用兩個選定條件，缺失或未成對者整組排除。',analysis_note)
 if(method=='mcnemar'){
  vals<-unique(as.character(c(v0,v1)));need(length(vals)==2&&event%in%vals,'McNemar 需二元結果及有效事件組。')
  b<-sum(as.character(v0)!=event&as.character(v1)==event);c0<-sum(as.character(v0)==event&as.character(v1)!=event)
  p<-if(b+c0==0)1 else binom.test(b,b+c0,p=.5)$p.value
  tables$Paired_test<-data.frame(Method='Exact McNemar (two-sided binomial)',Complete_pairs=length(ids),Non_event_to_event=b,Event_to_non_event=c0,p_value=p)
  tables$Paired_counts<-as.data.frame.matrix(table(Reference=factor(as.character(v0)==event,levels=c(FALSE,TRUE)),Comparison=factor(as.character(v1)==event,levels=c(FALSE,TRUE))))
  tables$Paired_counts<-data.frame(Reference=rownames(tables$Paired_counts),tables$Paired_counts,row.names=NULL)
  notes<-c(notes,'McNemar 使用不一致配對的雙側精確二項 p 值；無不一致配對時 p＝1。此處不推算調整 OR。')
 }else{
  need(is.numeric(v0)&&is.numeric(v1)&&all(is.finite(c(v0,v1))),'配對 t／Wilcoxon 需有限連續數值。')
  delta<-v1-v0;need(length(unique(delta))>1,'所有配對差值相同，無法進行此配對推論。')
  tt<-if(method=='paired_t')t.test(v1,v0,paired=TRUE)else wilcox.test(v1,v0,paired=TRUE,exact=FALSE,correct=TRUE)
  tables$Paired_test<-data.frame(Method=tt$method,Complete_pairs=length(ids),Mean_difference=mean(delta),CI_low=if(method=='paired_t')tt$conf.int[1]else NA_real_,CI_high=if(method=='paired_t')tt$conf.int[2]else NA_real_,Statistic=unname(tt$statistic),p_value=tt$p.value)
  tables$Difference_summary<-data.frame(Mean=mean(delta),SD=sd(delta),Median=median(delta),Q1=unname(quantile(delta,.25)),Q3=unname(quantile(delta,.75)))
  if(method=='paired_t')tables$Difference_normality<-shapiro_row(delta,'Within-pair difference')
  notes<-c(notes,paste('差異方向：',a1,'−',a0),if(method=='paired_t')'配對 t 的常態假設針對配對差值；獨立單位為配對，而非每一資料列。'else'Wilcoxon signed-rank 使用連續性校正的大樣本近似；位置差異解讀需差值分布對稱。Mean_difference 僅供描述，不是 signed-rank 的估計量。')
 }
 out('配對／兩次重複測量比較',tables,notes=notes)
}

conditional_logistic<-function(d,y,x,id,event,inter=NULL){
 need(length(x)>0&&!any(c(y,id)%in%x)&&y!=id,'請選擇預測變項；結果與配對組 ID 不可作為預測變項。')
 if(length(inter))need(length(inter)==2&&length(unique(inter))==2&&all(inter%in%x),'交互作用須為兩個已納入變項。')
 z<-cc(d,c(y,x,id));need(length(unique(z[[y]]))==2&&event%in%as.character(z[[y]]),'條件式 Logistic 需二元病例狀態及有效病例組。')
 for(v in x)if(is.factor(z[[v]])){ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1];need(ref%in%levels(z[[v]]),paste(v,'參考組無資料。'));z[[v]]<-relevel(factor(z[[v]],ordered=FALSE),ref)}
 z[[y]]<-as.integer(as.character(z[[y]])==event);z[[id]]<-factor(z[[id]])
 informative<-tapply(z[[y]],z[[id]],function(v)length(unique(v))==2)
 dropped<-sum(!informative);z<-droplevels(z[as.character(z[[id]])%in%names(informative)[informative],,drop=FALSE])
 need(nlevels(z[[id]])>=3,'同時含病例與對照的有效配對組不足 3 組。')
 for(v in x)need(any(vapply(split(z[[v]],z[[id]]),function(a)length(unique(a))>1,logical(1))),paste(v,'在所有配對組內均不變，條件式 Logistic 無法估計其效果。'))
 f<-form(y,c(x,paste0('strata(',id,')')),inter)
 # Local binding allows survival's formula special to be resolved explicitly.
 strata<-survival::strata;coxph<-survival::coxph;Surv<-survival::Surv
 fit<-withCallingHandlers(survival::clogit(f,data=z,method='exact'),warning=function(w)stop(paste('條件式 Logistic 估計警示：',conditionMessage(w)),call.=FALSE))
 b<-coef(fit);V<-vcov(fit);need(all(is.finite(b))&&all(is.finite(V))&&all(diag(V)>0),'模型不可識別或有分離，停止推論。')
 tab<-coeftable(b,V,TRUE,label='OR')
 tables<-list(Coefficients=tab,Data_structure=data.frame(Input_rows=nrow(d),Complete_case_rows=nrow(cc(d,c(y,x,id))),Analyzed_rows=nrow(z),Informative_sets=nlevels(z[[id]]),Excluded_noninformative_sets=dropped,Cases=sum(z[[y]]),Controls=sum(1-z[[y]])))
 ans<-out('配對病例對照：條件式 Logistic',tables,notes=c('每列為一位受試者；配對組 ID 以 strata 條件化，可處理 1:1 或 1:m。缺失按列排除後，無病例或無對照的配對組不提供資訊，另列排除數。','採 exact conditional likelihood；固定效果 CI／p 值仍為 Wald 近似，不是精確信賴區間。配對變項若組內完全相同，其係數不能另行估計。','OR 不可直接解讀為 RR；此入口未處理抽樣權重、重複使用對照或跨組相依。',analysis_note))
 ans$publication<-model_publication(tab,z,x,'OR',y);ans$publication$note<-paste(publication_note(ans$publication),'依配對組條件化的 OR；Wald 95% CI／p 值。')
 add_publication_forest(ans)
}
