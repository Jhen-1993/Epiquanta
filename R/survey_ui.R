survey_config<-function(input){list(weight=input$svy_weight,psu=input$svy_psu,strata=input$svy_strata %||% '',fpc=input$svy_fpc %||% '',fpc_mode=input$svy_fpc_mode %||% 'none',lonely=input$svy_lonely %||% 'fail',domain=input$svy_domain %||% '',domain_values=input$svy_domain_values %||% character(),sample_id=input$svy_sample_id %||% '')}
survey_controls<-function(mode,ch,d){
 sel<-function(id,label,values=names(d),optional=FALSE)selectInput(id,label,c(setNames('',if(optional)'不使用'else'請選擇'),values))
 tagList(p(class='method-guidance','每列為一名抽樣個案；使用資料提供者指定的分析權重、分層及 PSU。此入口為線性化設計推論；未使用重複權重或 PPS 不放回專用設計。'),
  sel('svy_weight','抽樣／分析權重'),selectInput('svy_psu','主要抽樣單位 PSU',c('請選擇'='','每列為獨立抽樣單位（ids = ~1）'='__individual__',names(d))),sel('svy_strata','抽樣分層 strata',optional=TRUE),sel('svy_sample_id','樣本 ID（可選，用於檢查每人一筆）',optional=TRUE),
  selectInput('svy_fpc_mode','有限母體校正 FPC',c('不使用：最高層 PSU 有放回變異數'='none','單階段：母體抽樣單位總數'='population','單階段：抽樣比例'='fraction')),
  conditionalPanel("input.svy_fpc_mode !== 'none'",sel('svy_fpc','FPC 欄位'),p(class='small-note','分母／總數指抽樣單位，不一定是受試者人數。每個抽樣分層內須相同；多階段 FPC 需另立設計。')),
  selectInput('svy_lonely','單一 PSU 分層的處理',c('停止並要求確認設計'='fail','Adjust：以整體中心調整'='adjust','Average：以其他層的平均變異替代'='average')),
  sel('svy_domain','子母體欄位（可選；保留完整抽樣設計）',optional=TRUE),uiOutput('svy_domain_values'),
  if(mode=='survey_desc')tagList(div(class='variable-checklist',checkboxGroupInput('svy_vars','加權描述變項（勾選）',ch)),sel('svy_group','比較組別',ch[vapply(d[ch],is.factor,logical(1))],TRUE),checkboxInput('svy_cont_test','連續變項：設計校正整體 Wald F',FALSE),checkboxInput('svy_cat_test','類別變項：Rao–Scott 二階 F',FALSE),selectInput('svy_p_adjust','多重檢定校正',c('不校正'='none','Holm'='holm','Bonferroni'='bonferroni')))
  else tagList(if(mode=='survey_regression')tagList(selectInput('svy_kind','結果資料型態',c('請選擇'='','連續'='continuous','二元'='binary','計數'='count')),uiOutput('svy_outcome'))else tagList(sel('svy_time','追蹤時間',ch[vapply(d[ch],is.numeric,logical(1))]),sel('svy_y','事件指標',ch[vapply(d[ch],function(v)length(unique(na.omit(v)))==2,logical(1))]),uiOutput('svy_event')),
   div(class='variable-checklist',checkboxGroupInput('svy_x','固定效果／干擾因子（勾選納入）',ch),covariate_buttons('svy_x')),
   div(class='variable-checklist',checkboxGroupInput('svy_inter','交互作用（兩個已選固定效果）',ch)),
   if(mode=='survey_regression')conditionalPanel("input.svy_model === 'poisson'",sel('svy_offset','人時／觀察量 offset',ch[vapply(d[ch],is.numeric,logical(1))],TRUE)))
 )
}
setup_survey_ui<-function(input,output,data,choices){
 output$svy_domain_values<-renderUI({req(nzchar(input$svy_domain %||% ''));lev<-unique(as.character(na.omit(data()[[input$svy_domain]])));need(length(lev)<=100,'子母體欄位超過 100 類，請先建立研究定義的子母體類別。');selectizeInput('svy_domain_values','納入的子母體類別（可複選）',lev,multiple=TRUE)})
 output$svy_outcome<-renderUI({req(input$module=='survey_regression',nzchar(input$svy_kind %||% ''));kind<-input$svy_kind;d<-data();ch<-choices();values<-ch[vapply(d[ch],function(v)if(kind=='binary')length(unique(na.omit(v)))==2 else is.numeric(v),logical(1))]
  models<-if(input$design=='casecontrol'){if(kind=='binary')c('Survey Logistic → OR'='logistic')else character()}else switch(kind,continuous=c('Survey 線性模型 → β'='gaussian'),binary=c('Survey Logistic → OR'='logistic',setNames('modified_poisson',if(input$design=='cross')'Survey Modified Poisson → PR'else'Survey Modified Poisson → RR')),count=c('Survey Poisson → 計數比或 IRR'='poisson'))
  if(!length(models))return(p('此研究設計與結果型態目前沒有支援的 survey 迴歸。'))
  tagList(selectInput('svy_model','模型與效果量',models),selectInput('svy_y','結果變項',c('請選擇'='',values)),if(kind=='binary')uiOutput('svy_event'))
 })
 output$svy_event<-renderUI({req(nzchar(input$svy_y %||% ''));lev<-unique(as.character(na.omit(data()[[input$svy_y]])));selectInput('svy_event','事件組（其餘類別＝非事件）',c('請指定事件'='',lev))})
}
