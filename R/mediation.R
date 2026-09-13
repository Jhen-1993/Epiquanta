# Single-mediator analyses. Causal effects use regmedint's published formulae.
# No automatic covariate selection and no claim that identification can be tested.
mediation_covariates<-function(z,covars,values=NULL){
 if(!length(covars))return(list(data=data.frame(row.names=seq_len(nrow(z))),at=NULL,map=data.frame()))
 for(v in covars){need(is.numeric(z[[v]])||is.factor(z[[v]]),'共變項需為數值或已確認的類別。');if(is.factor(z[[v]]))z[[v]]<-factor(z[[v]],levels=levels(z[[v]]),ordered=FALSE)}
 mm<-model.matrix(reformulate(covars),z)[,-1,drop=FALSE];at<-NULL
 if(!is.null(values)){
  nd<-z[1,,drop=FALSE]
  for(v in covars){value<-values[[v]];need(length(value)==1&&!is.na(value)&&nzchar(as.character(value)),paste('請設定',v,'的效果評估值。'))
   if(is.factor(z[[v]])){need(as.character(value)%in%levels(z[[v]]),paste(v,'評估層級無有效資料。'));nd[[v]]<-factor(value,levels=levels(z[[v]]))}
   else {value<-suppressWarnings(as.numeric(value));need(is.finite(value)&&value>=min(z[[v]])&&value<=max(z[[v]]),paste(v,'評估值需在有效觀察範圍內。'));nd[[v]]<-value}
  }
  at<-drop(model.matrix(reformulate(covars),nd)[,-1,drop=FALSE])
 }
 nm<-paste0('C',seq_len(ncol(mm)));mp<-data.frame(Code=nm,Original=colnames(mm));colnames(mm)<-nm
 list(data=as.data.frame(mm),at=if(is.null(at))NULL else unname(at),map=mp)
}
mediation_prepare<-function(d,a,m,y,covars,a0,a1,mreg='linear',yreg='linear',m_event='',y_event='',eventvar='',c_values=NULL,exposure_scale='values',exposure_change=1){
 vars<-c(a,m,y,covars,eventvar[nzchar(eventvar)]);need(!anyDuplicated(vars),'暴露、中介、結果、事件及干擾因子不能重複。')
 z<-cc(d,vars);ncc<-nrow(z);coding<-list();add<-function(v,rule)coding[[length(coding)+1]]<<-data.frame(Variable=v,Coding=rule)
 if(is.factor(z[[a]])){
  need(length(a0)==1&&length(a1)==1&&a0!=a1&&all(c(a0,a1)%in%levels(z[[a]])),'請選擇不同且有資料的暴露參考組及對照組。')
  z<-z[as.character(z[[a]])%in%c(a0,a1),,drop=FALSE];A<-as.integer(as.character(z[[a]])==a1);aa0<-0;aa1<-1
  add(a,paste0('0 = ',a0,'；1 = ',a1,'；僅保留這兩組（其餘組別不參與本次估計）'))
  effect_change<-1;scale<-'groups';contrast_note<-paste0(a,'：',a0,' → ',a1,' 的組間對比。')
  contrast<-data.frame(Variable=a,Scale=scale,Change=1,From=as.character(a0),To=as.character(a1),Analyzed_min=NA_real_,Analyzed_max=NA_real_)
 }else{
  need(is.numeric(z[[a]]),'暴露需為數值或類別。');A<-z[[a]]
  need(all(is.finite(A))&&diff(range(A))>0,'連續 X 需有至少兩個不同的有限數值。')
  need(exposure_scale%in%c('unit','values'),'請選擇有效的連續 X 效果單位。');scale<-exposure_scale
  if(scale=='unit'){
   # In this additive linear model the slope is constant. Scaling by a change
   # in X requires no arbitrary reference value or categorization of X.
   effect_change<-suppressWarnings(as.numeric(exposure_change))
   need(length(effect_change)==1&&is.finite(effect_change)&&effect_change>0,'請填寫大於 0 的 X 增加單位，例如 1 或 10。')
   aa0<-aa1<-NA_real_;contrast_note<-paste0(a,' 每增加 ',effect_change,' 個原始單位的效果；X 保留連續數值，不分組。')
  }else{
   aa0<-suppressWarnings(as.numeric(a0));aa1<-suppressWarnings(as.numeric(a1))
   need(length(aa0)==1&&length(aa1)==1&&all(is.finite(c(aa0,aa1))),paste0('X = ',a,'：請填寫參考數值 a0 與比較數值 a1；完整個案的觀察範圍為 ',min(A),' 至 ',max(A),'。'))
   need(aa0!=aa1,'參考數值 a0 與比較數值 a1 不可相同。')
   need(min(aa0,aa1)>=min(A)&&max(aa0,aa1)<=max(A),paste0('X = ',a,'：a0 與 a1 需在完整個案的觀察範圍 ',min(A),' 至 ',max(A),' 內。'))
   effect_change<-aa1-aa0;contrast_note<-paste0(a,' 從 ',aa0,' 變為 ',aa1,'（ΔX = ',effect_change,'）的效果；X 保留連續數值，不分組。')
  }
  add(a,contrast_note)
  contrast<-data.frame(Variable=a,Scale=scale,Change=effect_change,From=if(scale=='unit')''else as.character(aa0),To=if(scale=='unit')''else as.character(aa1),Analyzed_min=min(A),Analyzed_max=max(A))
 }
 binary<-function(v,level){lev<-unique(as.character(z[[v]]));need(length(lev)==2&&level%in%lev,paste(v,'需二元且需指定編碼 1 的類別。'));add(v,paste0('1 = ',level,'；0 = ',setdiff(lev,level)));as.integer(as.character(z[[v]])==level)}
 M<-if(mreg=='logistic')binary(m,m_event)else {need(is.numeric(z[[m]]),'線性中介模型需要連續數值中介。');z[[m]]}
 Y<-if(yreg%in%c('logistic','loglinear'))binary(y,y_event)else {need(is.numeric(z[[y]]),'此結果模型需要數值結果／追蹤時間。');z[[y]]}
 zz<-data.frame(A=A,M=M,Y=Y)
 if(startsWith(yreg,'surv')){need(all(Y>0),'存活時間必須 > 0。');zz$D<-binary(eventvar,y_event)}
 for(v in covars)if(is.factor(z[[v]])){ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1];need(ref%in%as.character(z[[v]]),paste(v,'參考組在分析樣本中無資料。'));z[[v]]<-relevel(droplevels(factor(z[[v]],ordered=FALSE)),ref)}
 cv<-mediation_covariates(z,covars,c_values);zz<-cbind(zz,cv$data)
 need(all(vapply(zz,function(v)all(is.finite(v)),logical(1))),'模型變項含非有限值。')
 list(data=zz,cvar=names(cv$data),at=cv$at,map=cv$map,coding=dplyr::bind_rows(coding),a0=aa0,a1=aa1,effect_change=effect_change,contrast=contrast,contrast_note=contrast_note,N=nrow(zz),Missing_excluded=nrow(d)-ncc,Other_exposure_excluded=ncc-nrow(zz))
}
mediation_publication<-function(tab,label,outcome,note){
 pd<-data.frame(Characteristic=tab$Term,Estimate=paste0(f2(tab$Estimate),' (',f2(tab$CI_low),', ',f2(tab$CI_high),')'),`p-value`=fp(tab$p_value),check.names=FALSE)
 names(pd)[2]<-paste0(label,' (95% CI)');pd[is.na(pd)]<-'—'
 list(data=pd,kinds=rep('continuous',nrow(pd)),group='',summary='mean',model=TRUE,outcome=outcome,note=note)
}
ordinary_mediation<-function(d,a,m,y,covars=character(),a0=NULL,a1=NULL,boot=FALSE,B=5000,seed=20260910,progress=function(...)NULL,exposure_scale='values',exposure_change=1){
 pp<-mediation_prepare(d,a,m,y,covars,a0,a1,exposure_scale=exposure_scale,exposure_change=exposure_change);z<-pp$data
 syntax<-paste0('M ~ a*A',if(length(pp$cvar))paste0(' + ',paste(pp$cvar,collapse=' + ')),'\nY ~ cprime*A + b*M',if(length(pp$cvar))paste0(' + ',paste(pp$cvar,collapse=' + ')),'\nindirect := a*b\ntotal := cprime + a*b')
 ffun<-function(z){f<-lavaan::sem(syntax,data=z,estimator='MLR',fixed.x=TRUE);need(lavaan::lavInspect(f,'converged')&&lavaan::lavInspect(f,'post.check'),'中介路徑模型未收斂或有不適當解。');f}
 fit<-ffun(z);pe<-lavaan::parameterEstimates(fit,ci=TRUE);ix<-match(c('cprime','indirect','total'),pe$label)
 delta<-pp$effect_change;tt<-data.frame(Term=c('Direct (c′)','Indirect (a × b)','Total (c′ + a × b)'),Estimate=pe$est[ix]*delta,CI_low=pmin(pe$ci.lower[ix]*delta,pe$ci.upper[ix]*delta),CI_high=pmax(pe$ci.lower[ix]*delta,pe$ci.upper[ix]*delta),p_value=pe$pvalue[ix])
 tabs<-list(Exposure_contrast=pp$contrast,Mediation=tt,Paths=pe,Coding=pp$coding,Covariate_codes=pp$map,Sample=data.frame(N=pp$N,Missing_excluded=pp$Missing_excluded,Other_exposure_excluded=pp$Other_exposure_excluded))
 if(boot){bt<-boot_cases(z,function(zz){p<-lavaan::parameterEstimates(ffun(zz));setNames(p$est[match(c('cprime','indirect','total'),p$label)]*delta,tt$Term)},B,seed,progress);tabs$Bootstrap<-bt$table;tt$CI_low<-bt$table$CI_low;tt$CI_high<-bt$table$CI_high;tabs$Mediation<-tt}
 ans<-out('一般中介分析：單一連續中介與連續結果',tabs,list(Effects=forest(tt,'Estimate',0)+ggplot2::labs(subtitle=pp$contrast_note)),c(pp$contrast_note,'線性路徑模型使用 lavaan MLR；報告未標準化效果，依所選 X 增加單位或 a1 − a0 對比縮放。這是統計中介，不自動識別因果。',if(boot)c(bootnote,'CI 為 Bootstrap；p 值仍為主模型 MLR delta 推論。')else'CI 與 p 值為主模型 MLR delta 推論。','此入口支援單一連續中介／連續結果，無暴露×中介交互作用；多重／序列或潛在變項路徑請使用 SEM 自訂語法。',analysis_note))
 ans$publication<-mediation_publication(tt,'Difference',y,paste('統計中介效果；',a,'→',m,'→',y,'。',pp$contrast_note,'共變項：',paste(covars,collapse='、'),'。'))
 ans<-attach_diagnostics(ans,lm(reformulate(c('A',pp$cvar),'M'),z),'linear','Mediator')
 attach_diagnostics(ans,lm(reformulate(c('A','M',pp$cvar),'Y'),z),'linear','Outcome')
}
causal_mediation<-function(d,a,m,y,covars=character(),a0,a1,mreg,yreg,m_event='',y_event='',eventvar='',m_cde,c_values=list(),interaction=TRUE,rare=FALSE,design='cohort',reviewed=FALSE,boot=FALSE,B=5000,seed=20260910,progress=function(...)NULL){
 need(design%in%c('cohort','rct'),'此因果中介入口限世代／RCT 獨立個案；其他抽樣設計尚未實作。')
 need(isTRUE(reviewed),'請先閱讀並確認已評估因果中介識別條件。')
 need(mreg%in%c('linear','logistic')&&yreg%in%c('linear','logistic','loglinear','survCox','survAFT_weibull'),'不支援此中介／結果模型組合。')
 if(yreg%in%c('logistic','survCox'))need(isTRUE(rare),'此 Logistic／Cox 因果中介公式依賴罕見事件近似；請先評估適用性，或選擇其他適用模型。')
 pp<-mediation_prepare(d,a,m,y,covars,a0,a1,mreg,yreg,m_event,y_event,eventvar,if(length(covars))c_values else NULL);z<-pp$data
 need(length(m_cde)==1&&is.finite(m_cde)&&m_cde>=min(z$M)&&m_cde<=max(z$M),'CDE 的中介固定值需為有效觀察範圍內數值。')
 if(mreg=='logistic')need(m_cde%in%c(0,1),'二元中介固定值需為 0 或 1。')
 fitfun<-function(zz){
  f<-withCallingHandlers(regmedint::regmedint(zz,yvar='Y',avar='A',mvar='M',cvar=if(length(pp$cvar))pp$cvar else NULL,eventvar=if(startsWith(yreg,'surv'))'D'else NULL,a0=pp$a0,a1=pp$a1,m_cde=m_cde,c_cond=pp$at,mreg=mreg,yreg=yreg,interaction=interaction,casecontrol=FALSE,na_omit=FALSE),warning=function(w)stop(paste('中介模型警示：',conditionMessage(w)),call.=FALSE))
  checkfit(f$mreg_fit);checkfit(f$yreg_fit);f
 }
 fit<-fitfun(z);ss<-summary(fit)$summary_myreg;ratio<-yreg!='linear'
 coef_matrix<-function(f){s<-summary(f);if(inherits(f,'survreg'))s$table else s$coefficients}
 effect<-switch(yreg,linear='Difference',logistic='OR (rare outcome approximation)',loglinear='RR',survCox='HR (rare event approximation)',survAFT_weibull='Time ratio')
 ix<-match(c('cde','pnde','tnie','tnde','pnie','te'),rownames(ss));names_effect<-c('CDE','PNDE','TNIE','TNDE','PNIE','TE')
 tt<-data.frame(Term=names_effect,Estimate=ss[ix,'est'],CI_low=ss[ix,'lower'],CI_high=ss[ix,'upper'],p_value=ss[ix,'p'])
 if(ratio)tt[,2:4]<-exp(tt[,2:4])
 pm<-data.frame(Term='PM (%)',Estimate=100*ss['pm','est'],CI_low=100*ss['pm','lower'],CI_high=100*ss['pm','upper'],p_value=ss['pm','p'])
 tabs<-list(Causal_effects=tt,Proportion_mediated=pm,Link_scale=mtab(ss),Coding=pp$coding,Covariate_codes=pp$map,Evaluation=data.frame(Variable=c('a0','a1','m_cde',covars),Value=as.character(c(a0,a1,m_cde,unlist(c_values[covars])))),Sample=data.frame(N=pp$N,Missing_excluded=pp$Missing_excluded,Other_exposure_excluded=pp$Other_exposure_excluded),Mediator_coefficients=mtab(coef_matrix(fit$mreg_fit)),Outcome_coefficients=mtab(coef_matrix(fit$yreg_fit)))
 if(startsWith(yreg,'surv')||yreg%in%c('logistic','loglinear'))tabs$Event_frequency<-data.frame(N=nrow(z),Events=sum(if(startsWith(yreg,'surv'))z$D else z$Y),Fraction=mean(if(startsWith(yreg,'surv'))z$D else z$Y))
 if(boot){bt<-boot_cases(z,function(zz){v<-coef(fitfun(zz));v[ix]<-if(ratio)exp(v[ix])else v[ix];v['pm']<-100*v['pm'];v},B,seed,progress);tabs$Bootstrap<-bt$table;ii<-match(tolower(tt$Term),bt$table$Term);tt$CI_low<-bt$table$CI_low[ii];tt$CI_high<-bt$table$CI_high[ii];tabs$Causal_effects<-tt;pm$CI_low<-bt$table$CI_low[bt$table$Term=='pm'];pm$CI_high<-bt$table$CI_high[bt$table$Term=='pm'];tabs$Proportion_mediated<-pm}
 note<-paste('單一中介條件效果；共變項固定於 Evaluation 所列使用者指定值，非人口平均效果。尺度：',effect,'。TE 分解為 PNDE + TNIE 或 TNDE + PNIE（比值尺度為相乘）。TNDE 與 TNIE 不能直接合成 TE。')
 ans<-out('因果中介分析：反事實自然效果',tabs,list(Effects=forest(tt,'Estimate',if(ratio)1 else 0)),c(note,'CDE＝控制直接效果；PNDE／TNDE＝純／總自然直接效果；PNIE／TNIE＝純／總自然間接效果；TE＝總效果；PM＝中介比例。PM 依套件定義，在總效果接近虛無或直接、間接方向相反時可能不穩定／超出 0–100%，不裁切。',if(boot)c(bootnote,'CI 為百分位 Bootstrap；p 值仍是主模型 delta Wald。')else'95% CI 與 p 值使用 regmedint delta Wald 推論。',if(yreg%in%c('logistic','survCox'))'本模型使用罕見事件近似。事件比例僅列作資訊，不能單憑觀察比例判定近似成立。',if(!length(covars))'未指定干擾因子：即使暴露隨機化，中介通常未隨機化，仍需評估中介—結果混雜。',analysis_note))
 ans$publication<-mediation_publication(tt,effect,y,note)
 ans$tables$Identification<-data.frame(Requirement=c('一致性與明確的暴露／中介介入定義','正值性／共同支持','暴露—結果、暴露—中介、中介—結果無未測量混雜','沒有受暴露影響的中介—結果干擾因子','暴露→中介→結果時間順序、正確模型、合理缺失／設限機制'),Assessment='需由研究設計與領域知識支持；此軟體不能由 p 值驗證')
 ans<-attach_diagnostics(ans,fit$mreg_fit,mreg,'Mediator')
 attach_diagnostics(ans,fit$yreg_fit,switch(yreg,loglinear='rr',survCox='cox',survAFT_weibull='aft',yreg),'Outcome')
}
