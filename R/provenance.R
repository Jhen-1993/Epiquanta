# Build from the completed analysis and frozen inputs, never from later inputs.
analysis_provenance<-function(ans,module,settings=list()){
 val<-function(n,default='未指定')paste(settings[[n]] %||% default,collapse=', ')
 packages<-switch(module,normality='stats',descriptive='stats',comparison='stats',paired='stats',correlation='stats',reliability='stats',efa=c('psych',if(any(ans$tables$EFA_summary$Fitted_factors>1))'GPArotation',if(val('cor_method')=='poly'||any(ans$efa_spec$methods=='poly'))'polycor'),cfa=c('lavaan','semTools'),sem=c('lavaan','semTools'),mediation=c('lavaan','stats'),causal_mediation=c('regmedint','stats'),regression=c('stats',switch(val('model'),linear=c('sandwich','lmtest'),rr='sandwich',negbin='MASS',ordinal='MASS',multinomial='nnet',character())),effects='stats',survival='survival',ag='survival',iv='stats',roc='pROC',gee='geepack',mixed=c('lme4',if(val('corr_model')=='gaussian')'lmerTest'),clogit='survival',survey_desc='survey',survey_regression='survey',survey_cox=c('survey','survival'),character())
 statistics<-unique(c('stats',packages));plotting<-if(length(ans$plots))c('ggplot2','grid','grDevices')else character()
 packages<-unique(c(statistics,plotting))
 versions<-vapply(packages,function(p)tryCatch(as.character(packageVersion(p)),error=function(e)'無法確認'),character(1))
 steps<-switch(module,
  normality=c('逐一檢查連續變項的樣本數與分布；Shapiro–Wilk 僅支援 3–5000 筆。',paste('超過 5000 筆抽樣檢定：',val('normality_sample','FALSE'),'；未啟用時不以全部大樣本回報 Shapiro p 值。'),'QQ／直方圖與檢定表的實際使用樣本數見結果。'),
  descriptive=c(paste('連續摘要＝',val('summary'),'；類別＝n 與組內百分比。'),paste('連續檢定＝',if(identical(settings$structure,'independent'))val('continuous_test','none')else'none', '；類別檢定＝',if(identical(settings$structure,'independent'))val('categorical_test','none')else'none','；多重校正＝',if(identical(settings$structure,'independent'))val('p_adjust','none')else'none','。'),if(identical(settings$structure,'independent'))'SMD（若啟用）以標準化平均差／比例差／多類別 Mahalanobis 距離計算；不由 p 值換算。'else'非獨立結構實際停用獨立組間檢定與 SMD；摘要單位為資料列。'),
  comparison=c(paste('指定方法＝',val('test'),'；使用所選結果與分組的完整資料。'),'依方法計算 Welch、秩次或精確檢定；不是自動選擇最小 p 值。'),
  paired=c(paste('方法＝',val('pair_method'),'；比較方向＝',val('pair_a1'),'−',val('pair_a0'),'。'),'依配對 ID 對齊兩條件；重複鍵停止、不完整配對整組排除。配對 t 檢查差值，McNemar 使用不一致配對的精確二項檢定。'),
  correlation=c(paste('相關方法＝',val('cor_method'),'；所有所選變項使用相同的完整個案資料。'),paste('不重複成對檢定的多重校正＝',val('cor_p_adjust','holm'),'；保留原始 p。')),
  reliability=c('依指定理論上下界處理反向題；α＝k／(k−1) × [1−各題變異數總和／總分變異數]。以 stats::var 計算，另列校正題總相關及刪題 α。','α 衡量內部一致性，並不直接證明單一構面或效度。'),
  efa=c(paste('相關矩陣＝',val('cor_method'),'；置換式平行分析次數＝',val('permutations'),'。'),'以資料置換形成虛無特徵值分布；因素萃取採 PAF＋SMC、Promax 轉軸，多因素使用 Promax，單因素不轉軸；非 Spearman 主分析另列 Spearman 敏感度。失敗迭代明列。'),
  cfa=c(paste('lavaan 測量模型（CFA）；估計法＝',val('estimator'),'。'),'所有指定構面於同一模型估計；構面相關不代表方向性影響路徑。標準化負荷量及 CI 由 standardizedSolution(type="std.all") 計算，使用 delta method／對稱常態近似。','CR＝semTools::compRelSEM（obs.var=TRUE、tau.eq=FALSE、ord.scale=TRUE）；AVE＝semTools::AVE。區辨效度列同一模型潛在相關、Fornell–Larcker 及幾何平均 HTMT2；HTMT2 CI 僅於實際啟用 Bootstrap 且成功次數足夠時提供。'),
  sem=c(paste('lavaan SEM；估計法＝',val('estimator'),'。'),'依指定測量／結構路徑估計參數；:= 表達式計算自訂間接效果。未僅憑配適度宣稱因果。'),
  mediation=c('一般線性中介：估計 X→M 的 a 路徑、M→Y 的 b 路徑及 X→Y 直接路徑。',if(val('med_exposure_scale')=='unit')paste0('效果單位：連續 X 每增加 ',val('med_exposure_change'),' 個原始單位；X 不分組。')else paste0('效果對比：',val('med_a0'),' → ',val('med_a1'),'。'),'間接效果＝a×b×ΔX；直接效果＝c′×ΔX；總效果＝直接＋間接。估計值與信賴區間同時縮放，p 值仍為原模型推論。採 lavaan MLR delta 推論，另依設定提供 Bootstrap CI。'),
  causal_mediation=c(paste('regmedint：中介模型＝',val('med_mreg'),'；結果模型＝',val('med_yreg'),'。'),'在指定暴露對比、干擾因子評估值與交互作用設定下，估計 CDE、PNDE、TNIE、TNDE、PNIE、TE。','總效果分解為 PNDE＋TNIE 或 TNDE＋PNIE；比值尺度使用乘法。PM 與效果比值分開處理；條件效果不是自動標準化的母體平均效果。'),
  regression=c(paste('模型＝',val('model'),'；固定效果僅含勾選變項及指定交互作用。'),switch(val('model'),linear='lm 線性係數；HC3 穩健共變異矩陣與 t 型推論。',rr='Modified Poisson；HC0 穩健變異數，exp(β) 為 RR／PR。',logistic='二元 Logistic，exp(β) 為 OR；Wald 推論。',negbin='負二項迴歸；依指定人時 offset 報 IRR 或計數比。',ordinal='比例勝算 Logistic；依已指定順序估計累積 OR。',multinomial='多項 Logistic；各結果類別相對參考結果的勝算比。','Poisson log link；依有無人時 offset 報 IRR 或計數比。')),
  effects=c('由指定二元暴露與事件建立 2×2 表；OR 採 Fisher 條件估計與精確 CI。','研究設計允許時才計算 RR／PR、風險／盛行比例差；零格不自動加 0.5，OR 不當作 RR。'),
  survival=c('依追蹤時間與事件／設限估計 Kaplan–Meier；勾選共變項時另配適 Cox。','Cox exp(β)＝HR；並提供適用的比例風險診斷。'),
  ag=c('Andersen–Gill 使用 start–stop 風險區間與事件指標。','以個案 ID 計算群聚穩健變異數；不把同一人的多個區間當成獨立個案。'),
  iv=c('兩階段最小平方法：以工具矩陣投影預測矩陣，再估計結果係數；使用原始結構殘差計算 HC1 變異數，Wald z 推論。','工具的排除限制與獨立性須有研究設計支持；第一階段強度並不證明全部識別條件。'),
  roc=c('依指定事件類別與方向建立 ROC，計算 AUC。',paste('AUC CI＝',if(isTRUE(settings$bootstrap))'依事件分層 Bootstrap'else'DeLong','；使用同一分析樣本，不是外部驗證或完整校準。')),
  gee=c(paste('geeglm：',val('corr_model'),'；工作相關＝',val('corr_corstr','exchangeable'),'。'),'依 ID 排序並保留訪視間隔；群體平均效果，群聚 sandwich SE 與漸近 Wald CI／p。'),
  mixed=c(paste('模型＝',val('corr_model'),'；群聚 ID＝',val('corr_id'),'；數值隨機斜率＝',val('corr_slope','無'),'。'),'LMM 使用 REML＋Satterthwaite；GLMM 使用 Laplace ML＋Wald z。奇異擬合不發布固定效果 CI／p。'),
  clogit=c('依配對組 strata 條件化，使用 survival::clogit 的 exact conditional likelihood。','exp(β)＝條件 OR；CI／p 仍為 Wald 近似，exact 不代表精確信賴區間。'),
  survey_desc=c('先用完整資料的權重、分層與 PSU 建立 survey 設計，再取子母體與非缺失觀察。','加權平均＝Σ(w×y)／Σw；加權比例將 y 換成該類別的 0／1 指標。SE 由抽樣分層及 PSU 線性化估計；連續 CI 為 t 型，比例 CI 為 logit。',paste('組間檢定：連續 Wald F＝',val('svy_cont_test','FALSE'),'；類別 Rao–Scott 二階 F＝',val('svy_cat_test','FALSE'),'；未指定分組時不執行。'),paste('多重校正＝',val('svy_p_adjust','none'),'；已預定但失敗的檢定仍計入數量。')),
  survey_regression=c('先建立 survey 抽樣設計；使用 svyglm 加權估計係數與線性化設計標準誤。',paste('模型＝',val('svy_model'),'；固定效果 CI／p 使用剩餘設計自由度的 Wald t。'),'二元／計數採 quasi family；exp(β) 的意義依 OR、RR／PR、計數比／IRR 分開標示。'),
  survey_cox=c('svycoxph 使用抽樣權重與設計標準誤，處理分層及 PSU。','exp(β)＝HR；使用漸近 Wald z CI／p。未把一般 Cox 的 PH 檢定當成設計校正檢定。'),
  '依本結果的方法與資料紀錄計算。')
 if(isTRUE(settings$bootstrap)&&module%in%c('cfa','sem','mediation','causal_mediation','regression','roc'))steps<-c(steps,paste('已要求 Bootstrap',val('B'),'次；seed＝',val('seed'),'。實際 CI 方法／可用成功次數見對應結果表，p 值不會自動改成 Bootstrap p。'))
 if(module=='efa'&&!is.null(ans$efa_spec))steps<-c(paste('各題組相關矩陣：',paste(paste(names(ans$efa_spec$methods),ans$efa_spec$methods,sep='='),collapse='；')),paste('逐題獨立置換平行分析',ans$efa_spec$B,'次；SMC 虛無特徵值第 95 百分位作保留參考；各題組種子見 EFA_summary。'),'PAF 萃取，單因素不轉軸、多因素 Promax；列出 KMO、MSA、完整 pattern loadings、共同性及 off-diagonal RMS。','非 Spearman 主分析另列 Spearman 敏感度；多因素欄位屬獨立解，未宣稱已作因素對齊。')
 if(module=='cfa'&&identical(ans$cfa_scope,'separate'))steps<-c('各題組分開配適單因素 CFA，使用各題組自己的完整作答樣本；估計法、N 與種子見本次估計範圍表。','不估計跨構面相關或聯合區辨效度，不將各組配適度合併。標準化負荷量 CI 採 standardizedSolution 的 delta method；CR／AVE 的尺度說明見各表。')
 if(module=='reliability')steps<-c('以各題組完整作答個案、原計分共變異數計算原始 α；以 Pearson 相關矩陣計算標準化 α。0／1 題的原始 α 等同 KR-20；順序計分題未改稱 ordinal alpha。','3 題以上計算該題與其餘題總分的 CITC 及刪題原始 α；兩題列 r 與 Spearman–Brown＝2r／(1+r)，不計算 CITC／刪題 α。',paste0('受試者整組作答有放回 Bootstrap ',ans$reliability_spec$B,' 次；有效值百分位 CI（quantile type=7），可定義負值保留。每係數有效比例 <95% 停止整次分析。'),paste('題組種子：',paste(paste(ans$tables$Reliability$Scale,ans$tables$Reliability$Seed,sep='='),collapse='；'),'；RNG：',paste(ans$reliability_spec$RNGkind,collapse=' / ')))
 if(module%in%c('sem','cfa')&&!is.null(ans$sem_type_spec))steps<-c(steps,
  if(length(ans$sem_type_spec$ordered))paste('順序題／結果＝',paste(ans$sem_type_spec$ordered,collapse='、'),'；合併資料中已確認的順序型態與本次另指定欄位，保留級別順序。WLSMV 以 DWLS 估計參數，搭配穩健標準誤與平均及變異數調整檢定。')else'本次未指定順序題，依原數值配適。',
  '僅納入模型所需變項的完整個案；SEM_variable_types 記錄欄位角色、題型來源、原級別順序及實際分析級別順序。')
 pkgtable<-data.frame(Package=packages,Version=unname(versions),Role=ifelse(packages%in%statistics,'Statistical','Plotting'),row.names=NULL)
 metadata<-data.frame(Item=c('R version','Platform','Analysis','Study design','Data structure','Calculated at'),Value=c(R.version.string,R.version$platform,ans$title,val('design'),val('structure'),format(Sys.time(),'%Y-%m-%d %H:%M:%S %Z')))
 ans$provenance<-list(metadata=metadata,packages=pkgtable,statistics=pkgtable[pkgtable$Role=='Statistical',c('Package','Version'),drop=FALSE],plotting=pkgtable[pkgtable$Role=='Plotting',c('Package','Version'),drop=FALSE],logic=steps)
 ans
}
package_lines<-function(p,kind){
 tab<-p[[kind]];if(is.null(tab))tab<-p$packages[p$packages$Package %in% if(kind=='plotting')c('ggplot2','grid','grDevices')else setdiff(p$packages$Package,c('ggplot2','grid','grDevices')),c('Package','Version'),drop=FALSE]
 if(!nrow(tab))return('本次未產生圖形，未使用繪圖套件。')
 paste(tab$Package,tab$Version)
}
provenance_text<-function(p){if(is.null(p))return(character());c(paste(p$metadata$Item,p$metadata$Value,sep='：'),'統計套件：',paste('•',package_lines(p,'statistics')),'繪圖套件：',paste('•',package_lines(p,'plotting')),p$logic)}
provenance_ui<-function(p){if(is.null(p))return(NULL);div(class='card calculation-card',h4('計算方法與版本'),tags$p(strong(p$metadata$Value[p$metadata$Item=='R version'])),
 div(class='package-columns',div(h5('統計套件'),tags$ul(lapply(package_lines(p,'statistics'),tags$li))),div(h5('繪圖套件'),tags$ul(lapply(package_lines(p,'plotting'),tags$li)))),
 tags$ol(lapply(p$logic,tags$li)),tags$details(tags$summary('完整環境與計算時間'),tags$ul(lapply(paste(p$metadata$Item,p$metadata$Value,sep='：'),tags$li))),tags$p(class='small-note','此資訊取自這次已完成的計算；具體公式、參考組、樣本數與假設限制見下方估計表及「分析方法與資料紀錄」。匯出會保留版本與計算邏輯。'))}
