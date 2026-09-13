# Design-based inference; never turn survey weights into row replications.
survey_options<-function(cfg){
 lonely<-cfg$lonely %||% 'fail';need(lonely%in%c('fail','adjust','average'),'請選擇有效的單一 PSU 處理方式。')
 options(survey.lonely.psu=lonely,survey.adjust.domain.lonely=lonely!='fail')
}
survey_prepare<-function(d,vars,cfg){
 need(requireNamespace('survey',quietly=TRUE),'請先安裝 survey 套件。')
 wt<-cfg$weight %||% '';psu<-cfg$psu %||% '';st<-cfg$strata %||% '';fp<-cfg$fpc %||% '';fm<-cfg$fpc_mode %||% 'none';dom<-cfg$domain %||% '';dv<-cfg$domain_values %||% character();sid<-cfg$sample_id %||% ''
 need(nzchar(wt)&&nzchar(psu),'請指定抽樣權重及 PSU；無群聚時須明確選擇「每列為獨立抽樣單位」。')
 need(fm%in%c('none','population','fraction'),'FPC 型態不正確。');if(fm=='none')fp<-''else need(nzchar(fp),'已啟用 FPC，請指定欄位。')
 idvars<-c(wt,if(psu!='__individual__')psu,st[nzchar(st)],fp[nzchar(fp)],sid[nzchar(sid)])
 need(all(c(vars,idvars,dom[nzchar(dom)])%in%names(d)),'所選分析或抽樣設計欄位不存在。')
 need(!anyDuplicated(idvars),'權重、PSU、分層、FPC、樣本 ID 須為不同欄位。')
 need(!anyNA(d[,idvars,drop=FALSE]),'抽樣設計欄位含缺失，不能默默排除。請先確認權重、PSU、分層及 FPC。')
 need(is.numeric(d[[wt]])&&all(is.finite(d[[wt]])&d[[wt]]>0),'抽樣權重需為正的有限數值；不接受 0、負值或缺失。')
 if(nzchar(sid))need(!anyDuplicated(d[[sid]]),'樣本 ID 重複：此入口每列須為一名抽樣個案；複雜抽樣縱向資料需另立設計。')
 z<-d[,unique(c(vars,idvars,dom[nzchar(dom)])),drop=FALSE]
 stratum<-if(nzchar(st))as.character(z[[st]])else rep('All',nrow(z));psukey<-if(psu=='__individual__')seq_len(nrow(z))else z[[psu]]
 counts<-tapply(psukey,stratum,function(v)length(unique(v)));need(nrow(z)>=3,'抽樣資料不足 3 列。')
 fpc_formula<-NULL
 if(nzchar(fp)){
  fv<-z[[fp]];need(is.numeric(fv)&&all(is.finite(fv)&fv>0),'FPC 須為正的有限數值。')
  need(all(vapply(split(fv,stratum),function(a)length(unique(a))==1,logical(1))),'此單階段 FPC 入口要求同一分層內 FPC 相同。')
  sample_n<-as.numeric(counts[stratum])
  if(fm=='fraction'){need(all(fv<=1),'FPC 抽樣比例須介於 0 與 1。');fv<-sample_n/fv}
  else need(all(fv==floor(fv))&&all(fv>=sample_n),'FPC 母體抽樣單位數須為整數，且不得小於各層已抽取 PSU 數。')
  nm<-tail(make.unique(c(names(z),'.survey_fpc_population')),1);z[[nm]]<-fv;fpc_formula<-reformulate(nm)
 }
 args<-list(ids=if(psu=='__individual__')~1 else reformulate(psu),weights=reformulate(wt),data=z,nest=TRUE)
 if(nzchar(st))args$strata<-reformulate(st);if(!is.null(fpc_formula))args$fpc<-fpc_formula
 des<-do.call(survey::svydesign,args);full_df<-survey::degf(des)
 need(full_df>0,'抽樣設計自由度不足；請確認 PSU 與分層。')
 if(nzchar(dom)){
  need(length(dv)>0&&all(dv%in%as.character(na.omit(z[[dom]]))),'請指定有效的子母體類別。')
  keep<-!is.na(z[[dom]])&as.character(z[[dom]])%in%dv
  # Subset the SURVEY DESIGN, never rebuild from an already filtered dataset.
  des<-des[keep,];need(nrow(des$variables)>=3,'選定子母體不足 3 筆。')
 }
 info<-data.frame(Input_rows=nrow(d),Domain_rows=nrow(des$variables),Full_PSU=sum(counts),Full_strata=length(counts),Full_design_df=full_df,Domain_design_df=survey::degf(des),Weight_sum=sum(weights(des)),Weight_min=min(weights(des)),Weight_max=max(weights(des)),Lonely_strata=sum(counts==1),Lonely_PSU=cfg$lonely %||% 'fail')
 logic<-c(sprintf('以 survey::svydesign 建立設計：weights＝%s；PSU＝%s；strata＝%s；nest＝TRUE。',wt,if(psu=='__individual__')'每列一個抽樣單位'else psu,if(nzchar(st))st else'未分層'),
          if(fm=='none')'未提供 FPC：使用最高層 PSU 的有放回／ultimate-cluster 變異數估計；可搭配資料提供者指定的分析權重、分層與 PSU。'else paste('單階段 FPC：',fp,'；輸入型態＝',fm,'。不是所有多階段或 PPS 不放回設計的通用設定。'),
          if(nzchar(dom))paste('先建立完整抽樣設計，再以設計物件取子母體：',dom,'＝',paste(dv,collapse='、'),'；未先刪除其他樣本重建設計。')else'未限制子母體。',
          paste('單一 PSU 分層處理：',cfg$lonely %||% 'fail','；若選 adjust／average，同時啟用對子母體 lonely PSU 的對應調整。'),
          '權重總和不是實際樣本數；推估母體總數的意義取決於權重的定義與尺度。PSU、權重、分層只用於設計，不自動作為結果模型的共變項。')
 list(design=des,info=info,logic=logic,design_vars=idvars)
}

survey_complete<-function(prep,vars){
 des<-prep$design;z<-des$variables;ok<-complete.cases(z[,vars,drop=FALSE])
 for(v in vars)if(is.numeric(z[[v]]))need(all(is.finite(z[[v]][!is.na(z[[v]])])),paste(v,'含非有限值。'))
 des<-des[ok,];need(nrow(des$variables)>=3,'所選分析的完整個案不足 3 筆。')
 info<-prep$info;info$Analyzed_rows<-nrow(des$variables);info$Excluded_missing_rows<-sum(!ok);info$Analysis_design_df<-survey::degf(des)
 list(design=des,info=info)
}

survey_descriptive<-function(d,vars,cfg,group='',continuous_test=FALSE,categorical_test=FALSE,p_adjust='none'){
 old<-survey_options(cfg);on.exit(options(old),add=TRUE)
 need(length(vars)>0,'請勾選要摘要的變項。');need(p_adjust%in%c('none','holm','bonferroni'),'不支援此多重檢定方法。')
 prep<-survey_prepare(d,unique(c(vars,group[nzchar(group)])),cfg);des<-prep$design
 if(nzchar(group)){need(is.factor(des$variables[[group]]),'分組需先設定為類別型態。');groups<-levels(droplevels(des$variables[[group]]));need(length(groups)>=2&&length(groups)<=20,'分組需為 2 至 20 組。')}else groups<-character()
 rows<-list();tests<-list()
 for(v in vars){
  if(is.numeric(des$variables[[v]]))need(all(is.finite(des$variables[[v]][!is.na(des$variables[[v]])])),paste(v,'含非有限值。'))
  for(gr in c(NA_character_,groups)){
   ds<-des;if(!is.na(gr))ds<-ds[!is.na(ds$variables[[group]])&as.character(ds$variables[[group]])==gr,]
   obs<-ds$variables[[v]];N<-sum(!is.na(obs));if(!N)next
   ds<-ds[!is.na(obs),];val<-ds$variables[[v]];ddf<-survey::degf(ds);need(ddf>0,paste(v,'子群的設計自由度不足。'))
   name<-if(is.na(gr))'Overall'else paste0(group,'=',gr)
   if(is.numeric(val)){
    stat<-survey::svymean(reformulate(v),ds,na.rm=TRUE);mn<-as.numeric(coef(stat));se<-as.numeric(survey::SE(stat));ci<-mn+c(-1,1)*qt(.975,ddf)*se
    sdw<-sqrt(as.numeric(coef(survey::svyvar(reformulate(v),ds,na.rm=TRUE))))
    rows[[length(rows)+1]]<-data.frame(Variable=v,Level='',Group=name,Unweighted_N=N,Unweighted_level_n=NA_integer_,Weighted_mean=mn,Weighted_SD=sdw,Weighted_percent=NA_real_,SE=se,CI_low=ci[1],CI_high=ci[2],df=ddf)
   }else{
    lev<-if(is.factor(val))levels(val)else sort(unique(as.character(val)));need(length(lev)<=100,paste(v,'類別過多，請確認型態。'))
    for(l in lev){tmp<-ds;nm<-tail(make.unique(c(names(tmp$variables),'.survey_indicator')),1);tmp$variables[[nm]]<-as.numeric(as.character(val)==l)
     # Logit CI remains in [0,1]; boundary proportions have no estimated CI.
     mn<-as.numeric(coef(survey::svymean(reformulate(nm),tmp)));se<-as.numeric(survey::SE(survey::svymean(reformulate(nm),tmp)))
     ci<-if(mn>0&&mn<1)as.numeric(confint(survey::svyciprop(reformulate(nm),tmp,method='logit',df=ddf)))else c(NA_real_,NA_real_)
     rows[[length(rows)+1]]<-data.frame(Variable=v,Level=l,Group=name,Unweighted_N=N,Unweighted_level_n=sum(as.character(val)==l),Weighted_mean=NA_real_,Weighted_SD=NA_real_,Weighted_percent=100*mn,SE=100*se,CI_low=100*ci[1],CI_high=100*ci[2],df=ddf)
    }
   }
  }
  if(nzchar(group)&&v!=group&&((is.numeric(des$variables[[v]])&&continuous_test)||(!is.numeric(des$variables[[v]])&&categorical_test))){
   testrow<-tryCatch({
    cc0<-survey_complete(prep,c(v,group));ds<-cc0$design;ds$variables[[group]]<-droplevels(ds$variables[[group]])
    if(is.numeric(ds$variables[[v]])){
     fit<-survey::svyglm(form(v,group),ds,family=gaussian());tt<-survey::regTermTest(fit,reformulate(group),method='Wald');data.frame(Variable=v,Method='Design-based Wald F (survey linear model)',p_value=as.numeric(tt$p),Status='OK')
    }else{
     ds$variables[[v]]<-droplevels(factor(ds$variables[[v]]));tt<-survey::svychisq(reformulate(c(v,group)),ds,statistic='F');data.frame(Variable=v,Method='Rao-Scott second-order F',p_value=tt$p.value,Status='OK')
    }
   },error=function(e)data.frame(Variable=v,Method='Requested survey group test',p_value=NA_real_,Status=conditionMessage(e)))
   tests[[length(tests)+1]]<-testrow
  }
 }
 table<-dplyr::bind_rows(rows);need(nrow(table)>0,'沒有可供摘要的資料。')
 tt<-dplyr::bind_rows(tests);tabs<-list(Survey_summary=table,Survey_design=prep$info)
 if(nrow(tt)){if(p_adjust!='none')tt[[if(p_adjust=='holm')'p_Holm'else'p_Bonferroni']]<-p.adjust(tt$p_value,p_adjust,n=nrow(tt));tabs$Group_tests<-tt;tabs$Multiplicity<-data.frame(Method=p_adjust,Planned_tests=nrow(tt),Available_p=sum(is.finite(tt$p_value)),Family='Selected overall variable-by-group tests')}
 out('複雜抽樣：加權描述與設計校正比較',tabs,notes=c(prep$logic,'連續資料：加權平均、加權 SD、平均數設計 SE 與 t 型 95% CI。類別：未加權人數、加權百分比、百分點 SE 與 logit 95% CI；0%／100% 邊界不提供 logit CI。','每個變項以非缺失列估計，分母明列；SD 描述個案變異，SE 描述估計不確定性。群組／子母體均由設計物件取子集。','連續組間比較使用 survey 線性模型整體 Wald F；類別使用二階 Rao–Scott F。若選多重校正，以本次預定整體變項檢定為家族，失敗檢定仍計入數量。',analysis_note))
}

survey_regression<-function(d,y,x,cfg,model='gaussian',design='cross',event='',offsetvar='',inter=NULL){
 old<-survey_options(cfg);on.exit(options(old),add=TRUE)
 need(length(x)>0&&!y%in%x,'請選擇固定效果，結果不能同時作為預測變項。')
 need(model%in%c('gaussian','logistic','modified_poisson','poisson'),'此 survey 模型尚未支援。')
 if(design=='casecontrol')need(model=='logistic','病例對照 survey 入口目前只提供 Logistic OR；權重需符合實際病例抽樣設計。')
 if(nzchar(offsetvar))need(model=='poisson'&&!offsetvar%in%c(y,x),'offset 僅供計數模型，不能同時當作結果或固定效果。')
 if(length(inter))need(length(inter)==2&&length(unique(inter))==2&&all(inter%in%x),'交互作用需兩個已選變項。')
 vars<-unique(c(y,x,offsetvar[nzchar(offsetvar)]));prep<-survey_prepare(d,vars,cfg)
 need(!any(c(y,x,offsetvar)%in%prep$design_vars),'抽樣設計欄位不得同時指定為分析結果、固定效果或 offset。')
 cc0<-survey_complete(prep,vars);ds<-cc0$design;z<-ds$variables
 for(v in x)if(is.factor(z[[v]])){ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1];need(ref%in%as.character(z[[v]]),paste(v,'參考組在分析樣本中不存在。'));z[[v]]<-relevel(droplevels(factor(z[[v]],ordered=FALSE)),ref)}
 if(model%in%c('logistic','modified_poisson')){need(length(unique(z[[y]]))==2&&event%in%as.character(z[[y]]),'請指定二元結果及事件組。');z[[y]]<-as.integer(as.character(z[[y]])==event)}
 if(model=='gaussian')need(is.numeric(z[[y]])&&length(unique(z[[y]]))>1,'線性模型結果須為連續數值且有變異。')
 if(model=='poisson')need(is.numeric(z[[y]])&&all(z[[y]]>=0&z[[y]]==floor(z[[y]]))&&any(z[[y]]>0),'計數結果須為非負整數且至少一筆大於零。')
 if(nzchar(offsetvar))need(is.numeric(z[[offsetvar]])&&all(z[[offsetvar]]>0),'offset 必須為正的有限數值。')
 ds$variables<-z;f<-form(y,x,inter);if(nzchar(offsetvar))f<-update(f,paste('. ~ . + offset(log(',offsetvar,'))'))
 mm<-model.matrix(f,z);need(qr(mm)$rank==ncol(mm),'模型矩陣不滿秩，請檢查重複變項或完全共線性。')
 family<-switch(model,gaussian=gaussian(),logistic=quasibinomial(),modified_poisson=quasipoisson(),poisson=quasipoisson())
 fit<-survey::svyglm(f,ds,family=family);need(isTRUE(fit$converged)&&all(is.finite(coef(fit))),'survey 模型未收斂或係數不可識別。')
 need(fit$df.residual>0,'分析設計剩餘自由度不足，不能進行此 Wald t 推論。')
 if(model=='logistic')need(!any(fitted(fit)<1e-8|fitted(fit)>1-1e-8),'預測接近 0／1，可能分離；停止一般 Logistic 推論。')
 b<-coef(fit);V<-vcov(fit);need(all(is.finite(V))&&all(diag(V)>0),'survey 模型變異數不可識別。')
 label<-switch(model,gaussian='Beta',logistic='OR',modified_poisson=if(design=='cross')'PR'else'RR',poisson=if(nzchar(offsetvar))'IRR'else'Count_ratio')
 tab<-coeftable(b,V,model!='gaussian',df=fit$df.residual,label=label);tab$df<-fit$df.residual
 tables<-list(Coefficients=tab,Survey_design=cc0$info,Estimation=data.frame(Estimator='Survey-weighted GLM; design SE; residual-design-df Wald t',Formula=paste(deparse(f),collapse=' '),Family=family$family,Link=family$link,df=fit$df.residual))
 if(length(inter)){term<-reformulate(paste(inter,collapse=':'));tt<-survey::regTermTest(fit,term,method='Wald');tables$Interaction_Wald<-data.frame(F=as.numeric(tt$Ftest),df=tt$df,df2=tt$ddf,p_value=as.numeric(tt$p))}
 notes<-c(prep$logic,'survey::svyglm 使用設計權重與線性化設計標準誤。固定效果 CI 與 p 值使用模型剩餘設計自由度的 Wald t，而非以觀察列數替代設計自由度。','二元與計數採 quasi family，避免非整數權重產生不必要警示；變異數仍由抽樣設計估計。共變項缺失按完整個案處理；權重不自動修正分析變項缺失所造成的偏誤。',if(model%in%c('logistic','modified_poisson'))paste('事件組＝',event),if(model=='modified_poisson'&&any(fitted(fit)>1))'Modified Poisson 預測值有 >1；不可直接解讀為合法個人機率。',analysis_note)
 ans<-out(paste('複雜抽樣：加權迴歸',label),tables,notes=notes);ans$publication<-model_publication(tab,z,x,label,y);ans$publication$note<-paste(publication_note(ans$publication),'Survey 設計權重與設計 SE；Wald t 95% CI／p 值。');add_publication_forest(ans)
}

survey_cox<-function(d,time,eventvar,event,x,cfg,inter=NULL){
 old<-survey_options(cfg);on.exit(options(old),add=TRUE)
 need(length(x)>0&&length(unique(c(time,eventvar,x)))==length(c(time,eventvar,x)),'請指定時間、事件與不同的預測變項。')
 if(length(inter))need(length(inter)==2&&length(unique(inter))==2&&all(inter%in%x),'交互作用需兩個已選變項。')
 vars<-c(time,eventvar,x);prep<-survey_prepare(d,vars,cfg);need(!any(vars%in%prep$design_vars),'抽樣設計欄位不可同時作為模型變項。');cc0<-survey_complete(prep,vars);ds<-cc0$design;z<-ds$variables
 need(is.numeric(z[[time]])&&all(z[[time]]>0),'Cox 追蹤時間需為正的有限數值。');need(length(unique(z[[eventvar]]))==2&&event%in%as.character(z[[eventvar]]),'請指定二元事件欄與事件組。')
 z[[eventvar]]<-as.integer(as.character(z[[eventvar]])==event)
 for(v in x)if(is.factor(z[[v]])){ref<-attr(d[[v]],'reference') %||% levels(d[[v]])[1];need(ref%in%as.character(z[[v]]),paste(v,'參考組無資料。'));z[[v]]<-relevel(droplevels(factor(z[[v]],ordered=FALSE)),ref)}
 ds$variables<-z;Surv<-survival::Surv
 f<-as.formula(paste0('Surv(',time,',',eventvar,') ~ ',paste(c(x,if(length(inter))paste(inter,collapse=':')),collapse=' + ')))
 mm<-model.matrix(reformulate(c(x,if(length(inter))paste(inter,collapse=':'))),z);need(qr(mm)$rank==ncol(mm),'Cox 模型矩陣不滿秩，請檢查共線性。')
 fit<-withCallingHandlers(survey::svycoxph(f,ds),warning=function(w)stop(paste('Survey Cox 估計警示：',conditionMessage(w)),call.=FALSE))
 b<-coef(fit);V<-vcov(fit);need(all(is.finite(b))&&all(is.finite(V))&&all(diag(V)>0),'Survey Cox 係數或變異數不可識別。')
 tab<-coeftable(b,V,TRUE,label='HR');tables<-list(Cox=tab,Survey_design=cc0$info,Estimation=data.frame(Estimator='Survey-weighted Cox; design SE; asymptotic Wald z',Formula=paste(deparse(f),collapse=' '),Events=sum(z[[eventvar]])))
 ans<-out('複雜抽樣：加權 Cox HR',tables,notes=c(prep$logic,'使用 survey::svycoxph 的設計標準誤；係數 exp(β) 為 HR，95% CI／p 為漸近 Wald z 推論。抽樣分層不等於模型中允許不同基準風險的 strata。','此入口每位個案一筆、正追蹤時間、右設限。未支援 start–stop、復發事件、競爭風險或 survey 設計校正的 PH 檢定；不可把一般 cox.zph 當成此設計的正式檢定。',analysis_note))
 ans$publication<-model_publication(tab,z,x,'HR',eventvar);ans$publication$note<-paste(publication_note(ans$publication),'Survey 加權 Cox 與設計 SE；Wald z 推論。');add_publication_forest(ans)
}

survey_demo<-function(){
 set.seed(219);N<-600;d<-data.frame(Sample_ID=seq_len(N),Stratum=factor(rep(1:4,each=150)),PSU=rep(rep(1:10,each=15),4));d$Weight<-runif(N,10,50);d$Group<-factor(rbinom(N,1,.5));re<-rep(rnorm(40),each=15);d$Age<-round(runif(N,20,80),1);d$Score<-2+.5*num(d$Group)+.03*d$Age+.5*re+rnorm(N);d$Binary<-factor(rbinom(N,1,plogis(-1+.3*num(d$Group)+.1*re)));d$Count<-rpois(N,exp(.3+.2*num(d$Group)+.1*re));d$Time<-rexp(N,.02*exp(.2*num(d$Group)));d$Event<-factor(rbinom(N,1,.6));d
}
