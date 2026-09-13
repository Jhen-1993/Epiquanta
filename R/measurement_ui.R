factor_controls<-function(mode,pick){
 tagList(if(mode=='efa')tagList(radioButtons('efa_entry','題組設定方式',c('構面卡片選題'='builder','進階文字設定'='text')),conditionalPanel("input.efa_entry === 'builder'",actionButton('open_builder_efa','設定 EFA 構面題組 →',class='btn-primary btn-block')),conditionalPanel("input.efa_entry === 'text'",pick('items','納入題項',TRUE)))else pick('items','同一量表／構面題項',TRUE),pick('reverse','反向題',TRUE),numericInput('lower','反向題理論最低分',NA),numericInput('upper','反向題理論最高分',NA),
 if(mode=='efa')tagList(conditionalPanel("input.efa_entry === 'text'",textAreaInput('efa_groups','題組／構面（可選；每行一組）',rows=3,placeholder='構面A = Q1, Q2, Q3')),textAreaInput('efa_labels','題目簡稱（可選）',rows=3,placeholder='Q1 = 題目簡稱'),numericInput('efa_seed','平行分析基礎種子',20260910,min=1,max=2147483646),selectInput('cor_method','題項相關',c('依已確認題型自動選擇'='auto','Polychoric ML（順序）'='poly','Pearson（連續）'='pearson','Spearman'='spearman')),selectInput('efa_preset','計算規模',c('預設：500 次'='500','先試跑：200 次'='200','自訂'='custom')),numericInput('permutations','平行分析置換次數',500,min=20,max=10000),numericInput('nfactor','因素數（0＝依平行分析）',0,min=0,max=20),p(class='small-note','自動模式：連續題用 Pearson；順序／0–1 題用 Polychoric。混合題型需明確指定方法。EFA：置換式平行分析、PAF 與 Promax。非 Spearman 主分析另做 Spearman 敏感度分析，因此有兩輪置換。'))
 else tagList(textAreaInput('rel_groups','題組／構面（可選；每行一組）',rows=3,placeholder='構面A = Q1, Q2, Q3\n構面B = Q4, Q5'),p(class='small-note','留空時，所有所選題項視為同一題組。請依研究定義分組；不會自動猜構面。'),textAreaInput('rel_labels','題目簡稱（可選；每行一題）',rows=3,placeholder='Q1 = 題目簡稱'),textInput('rel_subject_id','受試者 ID 欄名（可選；檢查每人一列）',''),numericInput('rel_seed','信度 Bootstrap 基礎種子',20260910,min=1,max=2147483646),p(class='small-note','每題組採完整作答個案；固定 5,000 次受試者層級百分位 Bootstrap。逐係數記錄有效次數，任何有效比例低於 95% 即停止分析。'),p(class='small-note','原計分／Pearson 信度：連續及有數值計分的順序題；0／1 題原始 α 等同 KR-20。未計分的名目題不可直接分析。兩題改列題間 r 與 Spearman–Brown，保留原始 α。')))
}
source('R/cfa_reporting.R',encoding='UTF-8')
cfa_analysis<-function(d,syntax,ordered_vars=character(),estimator='MLR',B=5000,boot=FALSE,seed=20260910,validation='same',progress=function(...)NULL,node_labels=NULL){
 pt<-lavaan::lavaanify(syntax);need(any(pt$op=='=~'),'CFA 需要至少一個事先指定的因素測量模型（=~）。');need(!any(pt$op=='~'),'CFA 入口不包含結構迴歸路徑（~）；請選 SEM。')
 ans<-sem_analysis(d,syntax,ordered_vars,estimator,B,boot,seed,validation,progress,node_labels=node_labels,measurement_report=TRUE);ans$title<-'驗證性因素分析 CFA';ans$measurement_model<-TRUE
 ans$plots$Paths<-NULL
 if(!is.null(ans$plots$SEM_diagram))ans$plots$SEM_diagram<-ans$plots$SEM_diagram+ggplot2::labs(title='Confirmatory factor analysis')
 ans
}
efa_analysis<-function(d,items,reverse=character(),lower=NULL,upper=NULL,method='poly',B=500,seed=20260910,nfactor=0,progress=function(...)NULL){
 ans<-questionnaire(d,items,reverse,lower,upper,method,B,FALSE,20,seed,nfactor,TRUE,progress)
 ans$tables<-ans$tables[setdiff(names(ans$tables),c('Reliability','Items','Alpha_bootstrap'))]
 ans$title<-if('Loadings'%in%names(ans$tables))'探索性因素分析 EFA'else'EFA 診斷（未產生可解讀的負荷量）'
 ans$notes<-ans$notes[!grepl('α',ans$notes,fixed=TRUE)];ans
}
