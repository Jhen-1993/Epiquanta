# These are limits of the implemented estimators, not universal prohibitions.
regression_models<-function(design,kind){
 if(design=='casecontrol')return(if(kind=='binary')c('Binary Logistic → OR'='logistic')else character())
 switch(kind,continuous=c('線性迴歸 → β（HC3）'='linear'),binary=c('Binary Logistic → OR'='logistic',setNames('rr',if(design=='cross')'Modified Poisson → PR'else'Modified Poisson → RR')),count=c('Poisson → 計數比／IRR'='poisson','負二項 → 計數比／IRR'='negbin'),ordinal=c('順序 Logistic → 累積 OR'='ordinal'),nominal=c('多項 Logistic → 相對勝算比'='multinomial'),character())
}
available_modules<-function(design,structure){
 if(!design%in%c('cohort','rct','casecontrol','cross')||!structure%in%c('independent','matched','repeated','clustered','survey','recurrent'))return(character())
 if(structure=='recurrent')return(if(design%in%c('cohort','rct'))c('descriptive','ag')else'descriptive')
 if(structure=='survey')return(c('survey_desc','survey_regression',if(design%in%c('cohort','rct'))'survey_cox'))
 if(structure=='matched')return(c('descriptive','paired',if(design=='casecontrol')'clogit'else c('gee','mixed')))
 if(structure%in%c('repeated','clustered'))return(c('descriptive',if(structure=='repeated')'paired',if(design!='casecontrol')c('gee','mixed')))
 m<-c('normality','mediation','descriptive','comparison','correlation','reliability','efa','cfa','sem','regression','effects','iv','roc')
 if(design%in%c('cohort','rct'))m<-c(m,'survival','causal_mediation')
 m
}
design_guidance<-function(design,structure){
 d<-switch(design,cohort='世代研究：可依結果與追蹤資訊估計 RR、風險差或 HR；回溯性資料也需確認明確的時間起點。',rct='RCT：需另確認隨機化單位、ITT／依計畫分析族群及失訪；隨機化不表示可忽略群聚或重複測量。',casecontrol='病例對照：此版本支援非配對 Logistic OR，配對資料使用條件式 Logistic；不由病例抽樣比例估計絕對風險或 RR。巢式病例對照與 case-cohort 需另設專用模型。',cross='橫斷性：估計盛行比例、PR／盛行勝算比；缺乏時間順序，不能直接解讀為發生風險或因果效果。','請先指定研究設計。')
 s<-switch(structure,independent='每人一筆且組別獨立。請另確認結果型態與模型假設。',matched='配對／匹配：兩條件比較提供配對 t、Wilcoxon signed-rank、精確 McNemar；配對病例對照另提供條件式 Logistic。需明確配對組 ID；非病例對照另可選 GEE／混合模型。',repeated='重複測量：每列為一次訪視，需個案 ID＋時間。世代／RCT／橫斷性提供 GEE、LMM／GLMM；兩次比較可選配對檢定。',clustered='單一層級群聚：每列為一位個案，以醫院／學校等 ID 指定相依群聚，提供 GEE、LMM／GLMM。若另有個案內重複測量，需多層模型；目前入口尚未支援。',survey='複雜抽樣：需抽樣權重、分層、PSU（必要時 FPC）；使用 survey 的加權描述、設計校正比較、加權迴歸；世代／RCT 另提供加權 Cox。先建立完整設計，再取子母體。',recurrent='復發事件：選擇 Andersen–Gill，需 start、stop、事件指標與 ID；描述統計以區間列為單位。','請指定資料結構。')
 paste(d,s)
}
