# Each card exports only its own frozen result, with shared formatting.
result_piece<-function(r,section,index=1L){
 need(!is.null(r)&&r$title!='分析未完成','請先成功執行分析。')
 if(section=='report')return(r)
 if(section=='publication'){
  need(!is.null(r$publication),'尚無模型表。')
  r$tables<-list();r$plots<-list();r$title<-if(isTRUE(r$publication$model))paste('模型結果',r$publication$outcome)else if(isTRUE(r$publication$custom))r$publication$title else'Table 1'
 }else if(section=='table'){
  need(index>=1&&index<=length(r$tables),'找不到此表格。');r$title<-names(r$tables)[index];key<-names(r$tables)[index];r$tables<-r$tables[index];r$plots<-list();r$publication<-r$measurement_publications[[key]]
 }else stop('不支援的匯出項目。')
 r
}
plot_size<-function(p){list(width=attr(p,'export_width') %||% 9,height=attr(p,'export_height') %||% 5)}
write_plot<-function(p,path,format){s<-plot_size(p);ggplot2::ggsave(path,p,width=s$width,height=s$height,dpi=300,bg='white',limitsize=FALSE,device=if(format=='pdf')grDevices::cairo_pdf else 'png')}
setup_result_outputs<-function(input,output,session,result){
 saved<-reactiveVal(character())
 output$export_status<-renderUI({paths<-saved();if(!length(paths))return(NULL);div(class='export-feedback',role='status',strong('已儲存到本機'),lapply(paths,tags$p))})
 # Raw event buttons avoid dynamic actionButton re-binding firing old clicks.
 local_button<-function(section,index,format){payload<-jsonlite::toJSON(list(section=section,index=index,format=format),auto_unbox=TRUE);tags$button(type='button',class='btn btn-default btn-sm',onclick=paste0("Shiny.setInputValue('save_piece',",payload,",{priority:'event'})"),paste('儲存',toupper(format),'至本機'))}
 controls<-function(section,index,formats){div(class='result-actions',lapply(formats,function(f)downloadButton(paste('dl',section,index,f,sep='_'),paste('下載',switch(f,docx='Word',xlsx='Excel',toupper(f))),class='btn-sm')),tags$details(tags$summary('下載無反應？儲存到本機'),lapply(formats,function(f)local_button(section,index,f))))}
 observeEvent(input$save_piece,{
  v<-input$save_piece;r<-isolate(result());idx<-as.integer(v$index);format<-v$format
  tryCatch(withProgress(message='正在匯出',value=.4,{
   need(!is.null(r)&&r$title!='分析未完成','請先成功執行分析。');need(format%in%c('docx','xlsx','png','pdf'),'不支援的格式。')
   dir.create('exports',showWarnings=FALSE);path<-tempfile(paste0('BioStat_',v$section,'_',format(Sys.time(),'%Y%m%d_%H%M%S'),'_'),tmpdir=normalizePath('exports'),fileext=paste0('.',format))
   if(v$section=='plot'){need(format%in%c('png','pdf')&&idx>=1&&idx<=length(r$plots),'圖形不存在。');write_plot(r$plots[[idx]],path,format)}
   else {need(format%in%c('docx','xlsx'),'此表格需要 Word 或 Excel 格式。');piece<-result_piece(r,v$section,idx);if(format=='docx')export_docx(piece,path)else export_xlsx(piece,path)}
   need(file.exists(path)&&file.info(path)$size>0,'檔案未建立。');saved(c(tail(saved(),3),normalizePath(path,winslash='/')));showNotification(paste('已儲存：',basename(path)),duration=15)
  }),error=function(e)showNotification(paste('匯出失敗：',conditionMessage(e)),type='error',duration=NULL))
 })
 output$results<-renderUI({
  r<-result();if(is.null(r))return(div(class='card',h3('尚無分析結果'),p('完成左側設定並按「送出並執行分析」。')))
  if(r$title=='分析未完成')return(div(class='card',h3(r$title),tags$ul(lapply(r$notes,tags$li))))
  publication<-if(!is.null(r$publication))div(class='card',div(class='result-heading',h3(if(isTRUE(r$publication$model))'模型結果'else if(isTRUE(r$publication$custom))r$publication$title else'Table 1'),controls('publication',1,c('docx','xlsx'))),HTML(publication_html(r$publication)),p(class='small-note',publication_note(r$publication)))
  cfa_main<-c('CFA_scope','CFA_failures','CFA_loadings','CFA_convergent','CFA_discriminant','CFA_group_fit','SEM_fit_summary','CFA_diagnostics','CFA_constructs')
  audit<-which(names(r$tables)%in%c(if(isTRUE(r$measurement_model))setdiff(names(r$tables),cfa_main),'Settings','Dictionary',if(!is.null(r$publication))c('Coefficients','Cox','Reference')))
  detailed<-setdiff(seq_along(r$tables),if(!is.null(r$publication))which(names(r$tables)%in%c('Table1','Coefficients','Cox','Reference'))else integer())
  table_card<-function(i)div(class='card',div(class='result-heading',h4(switch(names(r$tables)[i],Exposure_contrast='X 的效果單位與比較設定',SEM_outcome='終點結果 Y 與事件定義',SEM_fit_summary=if(isTRUE(r$measurement_model))'CFA 模型配適度'else'SEM 模型配適度',CFA_constructs='CFA 構面與題項',CFA_scope='本次 CFA 的估計範圍',CFA_failures='未完成構面與原因',CFA_group_fit='各構面的配適度（分開估計）',CFA_loadings='CFA 標準化因素負荷量',CFA_convergent='CFA 收斂效度：CR 與 AVE',CFA_discriminant='CFA 區辨效度',CFA_diagnostics='CFA 估計檢查與報告限制',SEM_residual_variances='測量誤差與擾動項',names(r$tables)[i])),controls('table',i,c('docx','xlsx'))),if(!is.null(r$measurement_publications[[names(r$tables)[i]]])){p<-r$measurement_publications[[names(r$tables)[i]]];tagList(HTML(publication_html(p)),p(class='small-note',publication_note(p)))}else div(class='table-scroll',tableOutput(paste0('table_',i))))
  plot_order<-c(which(names(r$plots)=='SEM_diagram'),which(names(r$plots)!='SEM_diagram'))
  plot_cards<-lapply(plot_order,function(i){sz<-plot_size(r$plots[[i]]);semplot<-names(r$plots)[i]=='SEM_diagram'||startsWith(names(r$plots)[i],'CFA_diagram_');div(class='card',div(class='result-heading',h3(if(semplot){if(startsWith(names(r$plots)[i],'CFA_diagram_'))paste('單構面測量圖',sub('CFA_diagram_','',names(r$plots)[i],fixed=TRUE))else if(isTRUE(r$measurement_model))'CFA 測量模型圖'else'SEM 結構與測量路徑圖'}else if(names(r$plots)[i]=='Paths')'路徑係數與 95% 信賴區間'else if(grepl('Publication_forest',names(r$plots)[i]))'效果量森林圖'else names(r$plots)[i]),controls('plot',i,c('png','pdf'))),div(class='figure-scroll',plotOutput(paste0('plot_',i),width=if(semplot)paste0(max(820,round(sz$width*65)),'px')else'100%',height=paste0(max(420,round(sz$height*65)),'px'))))})
  priority<-na.omit(match(if(isTRUE(r$measurement_model))cfa_main else c('SEM_outcome','SEM_fit_summary'),names(r$tables)))
  tagList(provenance_ui(r$provenance),div(class='card',h3(r$title),p(class='small-note','以下為上次執行的結果；修改設定後請重新執行。'),tags$details(tags$summary('分析方法與資料紀錄'),tags$ul(lapply(r$notes,tags$li))),uiOutput('export_status')),publication,lapply(priority,table_card),plot_cards,lapply(setdiff(detailed,c(audit,priority)),table_card),tags$details(class='result-audit',tags$summary('完整分析紀錄與原始係數'),controls('report',1,c('docx','xlsx')),downloadButton('record_rds','下載分析紀錄 RDS'),lapply(audit,table_card)))
 })
 observeEvent(result(),{
  r<-result();req(r,r$title!='分析未完成')
  bind_export<-function(section,index,format){local({sec<-section;j<-index;fmt<-format;snapshot<-r
   output[[paste('dl',sec,j,fmt,sep='_')]]<-downloadHandler(filename=function()paste0('BioStat_',if(sec=='table')names(snapshot$tables)[j]else if(sec=='plot')names(snapshot$plots)[j]else sec,'_',Sys.Date(),'.',fmt),contentType=switch(fmt,docx='application/vnd.openxmlformats-officedocument.wordprocessingml.document',xlsx='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',png='image/png',pdf='application/pdf'),content=function(path){if(sec=='plot')write_plot(snapshot$plots[[j]],path,fmt)else {piece<-result_piece(snapshot,sec,j);if(fmt=='docx')export_docx(piece,path)else export_xlsx(piece,path)}})
  })}
  for(sec in c('report',if(!is.null(r$publication))'publication'))for(fmt in c('docx','xlsx'))bind_export(sec,1,fmt)
  for(i in seq_along(r$tables)){local({j<-i;tab<-r$tables[[i]];output[[paste0('table_',j)]]<-renderTable(display_table(tab),striped=TRUE,rownames=FALSE)});for(fmt in c('docx','xlsx'))bind_export('table',i,fmt)}
  for(i in seq_along(r$plots)){local({j<-i;p<-r$plots[[i]];output[[paste0('plot_',j)]]<-renderPlot(p,res=110)});for(fmt in c('png','pdf'))bind_export('plot',i,fmt)}
  output$record_rds<-downloadHandler(filename=function()paste0('BioStat_record_',Sys.Date(),'.rds'),content=function(path){record<-r;record$plots<-NULL;saveRDS(record,path)})
 },ignoreNULL=TRUE)
}
