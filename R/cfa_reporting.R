# CFA reports summarize the fitted measurement model. Standardized intervals
# must come from standardizedSolution(), never the unstandardized parameter CI.
cfa_result_report<-function(fit,tables,labels=NULL,ordered_vars=character()){
 ss<-lavaan::standardizedSolution(fit,type='std.all',se=TRUE,ci=TRUE,level=.95)
 ld<-ss[ss$op=='=~',,drop=FALSE];factors<-unique(ld$lhs)
 label<-function(x){v<-unname(labels[x]);if(is.null(labels))return(x);ifelse(is.na(v)|!nzchar(v),x,v)}
 n<-as.integer(lavaan::lavInspect(fit,'nobs'));need(length(n)==1,'CFA 報表目前僅支援單群組模型。')
 get_metric<-function(key,col){t<-tables[[key]];v<-setNames(rep(NA_real_,length(factors)),factors);if(!is.null(t))v[t$Factor]<-t[[col]];v}
 av<-get_metric('AVE','AVE');cr<-get_metric('Composite_reliability','Reliability')
 scope<-if(length(factors)>1)'聯合 CFA：所有構面於同一模型估計'else'單構面 CFA'
 loading<-data.frame(Construct=label(ld$lhs),Question=ld$rhs,Std_loading=ld$est.std,CI_low=ld$ci.lower,CI_high=ld$ci.upper,
  Assessment=ifelse(!is.finite(ld$se)|!is.finite(ld$ci.lower)|!is.finite(ld$ci.upper),'標準誤／CI 不可用，不作推論',ifelse(abs(ld$est.std)>1,'負荷超出標準化範圍，需檢查模型',ifelse(ld$est.std<0,'負荷為負，確認計分與因素定向','配合題目內容與整體配適度判讀'))),check.names=FALSE)
 scale<-vapply(factors,function(f){v<-ld$rhs[ld$lhs==f];k<-sum(v%in%ordered_vars);if(k==0)'連續觀察分數'else if(k==length(v))'順序觀察分數（Green–Yang 修正）'else'混合題型；CR 可能無法計算'},character(1))
 convergent<-data.frame(Construct=label(factors),Items=vapply(factors,function(f)sum(ld$lhs==f),integer(1)),N=rep(n,length(factors)),CR=unname(cr[factors]),AVE=unname(av[factors]),Sqrt_AVE=sqrt(unname(av[factors])),CR_scale=scale,check.names=FALSE)
 report<-list(CFA_scope=data.frame(Model=scope,Factors=length(factors),N=n,Missing='所有模型題項的完整個案'),CFA_loadings=loading,CFA_convergent=convergent)
 note_loading<-'Std.all 標準化負荷量及 95% CI：lavaan::standardizedSolution 的 delta method／對稱常態近似，使用主模型的估計法與變異數矩陣。此處不是未標準化負荷量 CI，也不是百分位 Bootstrap CI。未自動刪題。'
 note_convergent<-'CR：semTools::compRelSEM，tau.eq=FALSE、obs.var=TRUE、ord.scale=TRUE（ω 類型組合信度）。順序題的 CR 回到觀察順序分數尺度；AVE 使用 semTools::AVE，順序題以潛在反應尺度計算。CR 與 AVE 應依各自尺度解讀，參考門檻不能單獨證明效度。'
 publications<-list()
 make_publication<-function(t,title,note,headers){
  blocks<-list();blocks[[scope]]<-display_table(t)
  p<-measurement_publication(blocks,headers,integer(),title,note);p
 }
 # Preserve 3-decimal presentation without rounding the raw result tables.
 format3<-function(t,cols){for(v in intersect(cols,names(t)))t[[v]]<-measurement_number(t[[v]]);t}
 publications$CFA_loadings<-make_publication(format3(loading,c('Std_loading','CI_low','CI_high')),'CFA 標準化因素負荷量',note_loading,c('構面','題目','標準化 λ','95% CI 下限','95% CI 上限','判讀事項'))
 publications$CFA_convergent<-make_publication(format3(convergent,c('CR','AVE','Sqrt_AVE')),'CFA 收斂效度：CR 與 AVE',note_convergent,c('構面','題數','N','CR','AVE','√AVE','CR 計算尺度'))
 note_discriminant<-'潛在相關及其 95% CI 來自同一聯合 CFA 的 Std.all／delta method。Fornell–Larcker 比較兩構面的 √AVE 是否均大於 |r|；固定為 0 的相關不作此判讀。HTMT2 為 semTools::htmt 的幾何平均版本（absolute=TRUE），順序題採 polychoric／混合相關，完全連續題採 Pearson；不是 Spearman HTMT。HTMT2 CI 僅在實際要求 Bootstrap 且有效次數足夠時提供。'
 if(length(factors)>1){
  pairs<-combn(factors,2,simplify=FALSE);pt<-lavaan::parTable(fit);ht<-tables$HTMT2;bt<-tables$Bootstrap_parameters
  get_ht<-function(a,b){if(is.null(ht)||!b%in%names(ht)||!a%in%ht$Term)return(NA_real_);as.numeric(ht[match(a,ht$Term),b])}
  rows<-lapply(pairs,function(p){a<-p[1];b<-p[2];ii<-which(ss$op=='~~'&((ss$lhs==a&ss$rhs==b)|(ss$lhs==b&ss$rhs==a)));j<-which(pt$op=='~~'&((pt$lhs==a&pt$rhs==b)|(pt$lhs==b&pt$rhs==a)))
   r<-if(length(ii))ss$est.std[ii[1]]else NA_real_;lo<-if(length(ii))ss$ci.lower[ii[1]]else NA_real_;hi<-if(length(ii))ss$ci.upper[ii[1]]else NA_real_;free<-length(j)>0&&pt$free[j[1]]>0
   fl<-if(!free)'固定相關，不作判讀'else if(!all(is.finite(c(r,av[a],av[b]))))'指標不可用'else if(all(sqrt(av[c(a,b)])>abs(r)))'符合數值準則；非效度結論'else'未符合數值準則；需檢視'
   bi<-if(is.null(bt))integer()else which(bt$Term%in%c(paste('HTMT2',a,b),paste('HTMT2',b,a)))
   data.frame(Construct_1=label(a),Construct_2=label(b),Latent_r=r,r_CI_low=if(free)lo else NA_real_,r_CI_high=if(free)hi else NA_real_,Fornell_Larcker=fl,HTMT2=get_ht(a,b),HTMT2_CI_low=if(length(bi))bt$CI_low[bi[1]]else NA_real_,HTMT2_CI_high=if(length(bi))bt$CI_high[bi[1]]else NA_real_,Successful=if(length(bi))bt$Successful[bi[1]]else NA_integer_,Requested=if(length(bi))bt$Requested[bi[1]]else NA_integer_,check.names=FALSE)
  })
  disc<-do.call(rbind,rows);report$CFA_discriminant<-disc
  ci_text<-function(e,l,h)ifelse(is.finite(l)&is.finite(h),measurement_ci(e,l,h),paste0(measurement_number(e),'（CI 未提供）'))
  pretty<-data.frame(A=disc$Construct_1,B=disc$Construct_2,r=ci_text(disc$Latent_r,disc$r_CI_low,disc$r_CI_high),FL=disc$Fornell_Larcker,HTMT=ci_text(disc$HTMT2,disc$HTMT2_CI_low,disc$HTMT2_CI_high),Bootstrap=ifelse(is.na(disc$Requested),'未要求 Bootstrap',paste0(disc$Successful,' / ',disc$Requested)),check.names=FALSE)
  publications$CFA_discriminant<-make_publication(pretty,'CFA 區辨效度',note_discriminant,c('構面 1','構面 2','潛在 r（95% CI）','F–L 數值檢查','HTMT2（95% CI）','有效／要求次數'))
 }
 df<-as.numeric(lavaan::fitMeasures(fit,'df'));pe<-lavaan::parameterEstimates(fit);pt<-lavaan::parTable(fit)
 free_rows<-paste(pe$lhs,pe$op,pe$rhs)%in%paste(pt$lhs[pt$free>0],pt$op[pt$free>0],pt$rhs[pt$free>0])
 report$CFA_diagnostics<-data.frame(Check=c('模型範圍','估計收斂','不適當解','自由參數標準誤','模型自由度'),Result=c(scope,'已收斂','已通過 lavaan post.check',if(all(is.finite(pe$se[free_rows])))'可計算'else'存在不可用值',as.character(df)),Interpretation=c('不將分構面或配對估計拼成聯合模型。','收斂不代表模型正確或估計完全穩定。','已檢查負變異數與模型矩陣；仍需檢視負荷、標準誤及配適度。','標準誤很大時仍需檢視識別與樣本資訊。',if(df==0)'剛好識別，不以整體配適度判定模型。'else'配適度需結合理論、題目內容及估計法。'),check.names=FALSE)
 list(tables=report,publications=publications,notes=c(scope,note_loading,note_convergent,if(length(factors)>1)note_discriminant))
}

# Independent fits use each card's complete cases. Never pool fit indices or
# invent between-factor correlations from separately fitted one-factor models.
cfa_separate_analysis<-function(d,config,ordered_vars=character(),estimator='auto',B=5000,boot=FALSE,seed=20260910,validation='same',progress=function(...)NULL){
 results<-list();failures<-list();specs<-list();total<-length(config$cards)
 for(i in seq_along(config$cards)){
  g<-config$cards[[i]];syn<-paste(g$code,'=~',paste(g$items,collapse=' + '));ord<-intersect(ordered_vars,g$items)
  est<-if(estimator=='auto'){if(length(ord)||any(vapply(d[g$items],is.ordered,logical(1))))'WLSMV'else'MLR'}else estimator
  group_seed<-as.integer((as.double(seed)+i-2)%%2147483646+1)
  n<-sum(complete.cases(d[g$items]));label<-unname(config$labels[g$code])
  a<-tryCatch(cfa_analysis(d,syn,ord,est,B,boot,group_seed,validation,function(p)progress((i-1+p)/total),config$labels),error=function(e)e)
  good<-!inherits(a,'error');specs[[i]]<-data.frame(Construct=label,Items=length(g$items),N=n,Estimator=est,Seed=group_seed,Status=if(good)'完成'else'未產生推論',Scope='單構面分開估計；非聯合模型')
  if(good)results[[g$code]]<-a else failures[[g$code]]<-data.frame(Construct=label,N=n,Reason=gsub('SEM','CFA',conditionMessage(a),fixed=TRUE))
  progress(i/total)
 }
 tabs<-list(CFA_scope=dplyr::bind_rows(specs));pubs<-list();plots<-list()
 for(key in c('CFA_loadings','CFA_convergent','CFA_diagnostics','SEM_variable_types','Parameters','Bootstrap_parameters','Residual_correlations')){
  blocks<-lapply(names(results),function(code){t<-results[[code]]$tables[[key]];if(is.null(t))return(NULL);if(!'Construct'%in%names(t))t<-cbind(Construct=unname(config$labels[code]),t);t})
  t<-dplyr::bind_rows(blocks);if(nrow(t))tabs[[key]]<-t
 }
 fits<-lapply(names(results),function(code){t<-results[[code]]$tables$SEM_fit_summary;if(is.null(t))t<-results[[code]]$tables$Fit;cbind(Construct=unname(config$labels[code]),t)})
 tabs$CFA_group_fit<-dplyr::bind_rows(fits)
 if(length(failures))tabs$CFA_failures<-dplyr::bind_rows(failures)
 for(key in c('CFA_loadings','CFA_convergent')){
  pp<-lapply(names(results),function(code){p<-results[[code]]$measurement_publications[[key]];p$data[p$kinds=='heading',1]<-paste0(config$labels[code],' · 單構面分開估計');p})
  if(length(pp)){p<-pp[[1]];p$data<-dplyr::bind_rows(lapply(pp,`[[`,'data'));p$kinds<-unlist(lapply(pp,`[[`,'kinds'),use.names=FALSE);p$note<-paste('分構面 CFA；各題組使用自己的完整作答樣本，不代表聯合測量模型。',p$note);pubs[[key]]<-p}
 }
 for(code in names(results))if(!is.null(results[[code]]$plots$SEM_diagram))plots[[paste0('CFA_diagram_',code)]]<-results[[code]]$plots$SEM_diagram
 ans<-out('分構面 CFA：逐題組檢查',tabs,plots,c('每個構面分別配適單因素 CFA；樣本數依各題組完整作答資料決定。未估計跨構面相關／區辨效度，不產生合併的整體配適度。',
  '失敗構面保留原因；其他題組完成不代表整份量表驗證通過。負荷量與 CI、CR／AVE 依各構面實際估計法與計算尺度列示。',
  unique(unlist(lapply(results,function(a)a$notes[!grepl('N=|單構面 CFA',a$notes)]),use.names=FALSE))))
 ans$measurement_model<-TRUE;ans$cfa_scope<-'separate';ans$measurement_publications<-pubs;ans$cfa_group_settings<-tabs$CFA_scope
 ans
}
