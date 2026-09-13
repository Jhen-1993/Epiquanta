mediation_controls<-function(causal=FALSE,ch,d){
 pick<-function(id,label,values=ch,multiple=FALSE)selectizeInput(id,label,if(multiple)values else c('請選擇'='',values),selected=if(multiple)character()else'',multiple=multiple)
 numeric<-ch[vapply(d[ch],is.numeric,logical(1))]
 tagList(p(class='small-note',if(causal)'單一中介；請明確指定暴露對比與干擾因子。多類別暴露將僅保留所選兩組。'else'一般線性路徑：單一連續中介、連續結果；多重／潛在中介請用 SEM 語法。'),
  pick('med_a','暴露 X'),uiOutput('med_contrast'),
  if(causal)selectInput('med_mreg','中介模型',c('連續中介 → Linear'='linear','二元中介 → Logistic'='logistic')),
  pick('med_m','中介 M',if(causal)ch else numeric),uiOutput('med_m_options'),
  if(causal)selectInput('med_yreg','結果模型與效果尺度',c('請選擇'='','存活時間 → Cox HR（罕見事件近似）'='survCox','存活時間 → Weibull AFT 時間比'='survAFT_weibull','二元結果 → Modified Poisson RR'='loglinear','二元結果 → Logistic OR（罕見事件近似）'='logistic','連續結果 → Linear 差值'='linear')),
  pick('med_y',if(causal)'結果 Y／存活追蹤時間'else'連續結果 Y',if(causal)ch else numeric),uiOutput('med_y_options'),
  pick('med_covars','納入的干擾因子／共變項（可搜尋複選；不自動加入）',multiple=TRUE),covariate_buttons('med_covars'),
  if(causal)tagList(uiOutput('med_c_values'),checkboxInput('med_interaction','納入暴露 × 中介交互作用',TRUE),
   conditionalPanel("['logistic','survCox'].includes(input.med_yreg)",checkboxInput('med_rare','已評估罕見事件近似適用於目前研究',FALSE)),
   tags$details(tags$summary('因果識別條件'),tags$ul(tags$li('正確時間順序、一致性與正值性。'),tags$li('暴露—結果、暴露—中介及中介—結果沒有未控制混雜。'),tags$li('不存在受暴露影響的中介—結果混雜。'),tags$li('合理缺失／設限機制及正確模型；以上不能由顯著性檢定證明。'))),
   checkboxInput('med_reviewed','已評估上述條件；了解軟體無法驗證因果識別',FALSE))
 )
}
covariate_buttons<-function(id){div(class='field-actions',lapply(c(all='全選',clear='清除'),function(label){action<-if(label=='全選')'all'else'clear';tags$button(type='button',class='btn btn-default btn-sm',onclick=paste0("Shiny.setInputValue('covariate_bulk',{id:'",id,"',action:'",action,"'},{priority:'event'})"),label)}))}
setup_mediation_ui<-function(input,output,data,choices){
 output$med_contrast<-renderUI({req(data(),nzchar(input$med_a %||% ''));v<-data()[[input$med_a]]
  if(is.factor(v))tagList(selectInput('med_a0','參考暴露 a0',c('請選擇'='',levels(v))),selectInput('med_a1','比較暴露 a1（只保留 a0 與 a1 兩組）',c('請選擇'='',levels(v))))
  else {
   limits<-range(v[is.finite(v)]);range_note<-p(class='small-note',paste0(input$med_a,' 的觀察範圍：',limits[1],' 至 ',limits[2],'。X 會保留連續數值，不會依比較數值分組。'))
   values<-tagList(numericInput('med_a0','參考暴露數值 a0（必填）',NA),numericInput('med_a1','比較暴露數值 a1（必填）',NA),p(class='small-note','估計 X 從 a0 變為 a1 的效果，例如年齡由 50 歲變為 60 歲。請依研究問題指定；最終範圍以模型完整個案為準。'))
   if(input$module=='mediation')tagList(range_note,
    radioButtons('med_exposure_scale','連續 X 的效果單位',c('每增加指定單位（預設 1）'='unit','指定兩個數值比較（a0 → a1）'='values'),selected='unit'),
    conditionalPanel("input.med_exposure_scale === 'unit'",numericInput('med_exposure_change','X 每增加幾個單位',1,min=0,step=1),p(class='small-note','例如 Age 每增加 1 歲或 10 歲；直接、間接及總效果與其信賴區間會使用相同單位。此模式不需要填 a0、a1。')),
    conditionalPanel("input.med_exposure_scale === 'values'",values))
   else tagList(range_note,values)
  }
 })
 output$med_m_options<-renderUI({req(input$module=='causal_mediation',data(),nzchar(input$med_m %||% ''))
  if(input$med_mreg=='logistic'){v<-sort(unique(as.character(na.omit(data()[[input$med_m]]))));tagList(selectInput('med_m_event','中介編碼 1 的類別（另一組＝0）',c('請選擇'='',v)),selectInput('med_m_cde','CDE 固定中介狀態',c('請選擇'='', '0：非事件中介'='0','1：事件中介'='1')))}
  else numericInput('med_m_cde','CDE 固定中介數值 m（必填）',NA)
 })
 output$med_y_options<-renderUI({req(input$module=='causal_mediation',data());kind<-input$med_yreg %||% '';d<-data()
  if(startsWith(kind,'surv'))tagList(selectInput('med_eventvar','死亡／事件指標',c('請選擇'='',names(d)[vapply(d,function(v)length(unique(na.omit(v)))==2,logical(1))])),uiOutput('med_surv_event'))
  else if(kind%in%c('logistic','loglinear')&&nzchar(input$med_y %||% ''))selectInput('med_y_event','結果事件組（另一組＝非事件）',c('請選擇'='',sort(unique(as.character(na.omit(d[[input$med_y]]))))))
 })
 output$med_surv_event<-renderUI({req(data(),nzchar(input$med_eventvar %||% ''));selectInput('med_y_event','事件類別（另一組＝設限）',c('請選擇'='',sort(unique(as.character(na.omit(data()[[input$med_eventvar]]))))))})
 output$med_c_values<-renderUI({req(input$module=='causal_mediation',data());covars<-input$med_covars %||% character();if(!length(covars))return(p(class='small-note','尚未選擇干擾因子；隨機化暴露也不能排除中介—結果混雜。'))
  tagList(h5('指定效果評估條件（必填）'),p(class='small-note','在下列干擾因子條件下報告效果。例如年齡 50、性別女性，表示該組條件下的直接／間接效果。模型仍使用全部符合條件的分析樣本；目前不對全體樣本平均。'),lapply(seq_along(covars),function(i){v<-data()[[covars[i]]];id<-paste0('med_cval_',i);if(is.factor(v))selectInput(id,covars[i],c('請選擇'='',levels(v)))else numericInput(id,covars[i],NA)}))
 })
}
