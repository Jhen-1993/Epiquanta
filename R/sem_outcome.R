# A dedicated endpoint is part of the guided SEM specification. Its coding is
# applied to the model copy only; the imported data dictionary stays unchanged.
sem_outcome_levels<-function(x){
 if(is.factor(x))levels(x)[levels(x)%in%as.character(x)]else as.character(sort(unique(na.omit(x))))
}
sem_outcome_type<-function(x){
 if(length(sem_outcome_levels(x))==2)'binary'else if(is.ordered(x))'ordinal'else if(is.numeric(x))'continuous'else'nominal'
}
sem_outcome_spec<-function(d,kind='observed',variable='',type='auto',event='',level_text='',items=character(),label='',reserved=character()){
 label<-trimws(label);if(!nzchar(label))label<-'Y'
 need(kind%in%c('observed','latent'),'請選擇 Y 的結果形式。')
 if(kind=='latent'){
  items<-unique(items);need(length(items)>=3,'Y 為潛在構面：請在「終點結果 Y」選擇至少 3 個測量題項。')
  need(all(items%in%names(d)),'Y 含未納入或不存在的題項。')
  # Avoid a name collision with an observed column called Y or another factor.
  symbol<-'Y_latent';while(symbol%in%c(names(d),reserved))symbol<-paste0(symbol,'_')
  return(list(kind=kind,type='latent',variable='',symbol=symbol,items=items,label=label,levels=character(),event='',reference=''))
 }
 need(length(variable)==1&&variable%in%names(d),'請先在「終點結果 Y」選擇結果欄位。')
 x<-d[[variable]];lev<-sem_outcome_levels(x)
 if(type=='auto')type<-sem_outcome_type(x)
 need(type%in%c('continuous','binary','ordinal','nominal'),'請選擇有效的 Y 結果型態。')
 need(type!='nominal','Y 為多類別名目結果，目前這個 lavaan SEM 不支援；請依研究定義選擇合適模型，不可直接當作順序結果。')
 ref<-'';ord<-character()
 if(type=='binary'){
  need(length(lev)==2,'Y 設為二元結果，但資料不是兩個有效類別；請確認結果型態。')
  need(length(event)==1&&nzchar(event)&&event%in%lev,'請在「終點結果 Y」選擇事件組；另一類會自動設為非事件組。')
  ref<-setdiff(lev,event);ord<-c(ref,event)
 }else if(type=='ordinal'){
  ord<-if(nzchar(trimws(level_text)))trimws(strsplit(level_text,'|',fixed=TRUE)[[1]])else if(is.ordered(x))levels(x)else character()
  need(length(ord)>=2&&all(nzchar(ord))&&!anyDuplicated(ord)&&all(lev%in%ord),'請在「終點結果 Y」填寫完整且不重複的級別順序，以 | 分隔，由低至高。')
 }else need(is.numeric(x),'連續 Y 需要數值欄位；請在「資料與變項」確認型態與計分，不會直接把類別代碼當連續分數。')
 list(kind=kind,type=type,variable=variable,symbol=variable,items=character(),label=label,levels=ord,event=if(type=='binary')event else'',reference=ref)
}
sem_outcome_data<-function(d,s){
 if(!is.null(s)&&s$type%in%c('binary','ordinal'))d[[s$variable]]<-ordered(d[[s$variable]],levels=s$levels)
 d
}
sem_outcome_table<-function(s){data.frame(Outcome='Y',Label=s$label,Variable=if(s$kind=='latent')s$symbol else s$variable,
 Type=s$type,Event=s$event,Non_event=s$reference,Level_order=paste(s$levels,collapse=' < '),Items=paste(s$items,collapse=', '),row.names=NULL)}
setup_sem_outcome<-function(input,output,session,data,choices,raw){
 output$sem_outcome_controls<-renderUI({
  # New data starts a new endpoint definition; changes in other cards never
  # rebuild these inputs or restore stale event choices.
  raw();isolate(tagList(
   div(class='outcome-grid',textInput('sem_y_label','結果名稱（選填）','',placeholder='例如：衛生所轉型'),radioButtons('sem_y_kind','結果形式',c('觀察結果（單一欄位）'='observed','潛在結果（多個題項）'='latent'))),
   conditionalPanel("input.sem_y_kind === 'observed'",selectizeInput('sem_y_variable','Y 對應結果欄位',choices=c('請選擇結果欄位'='',choices()),selected='',options=list(placeholder='搜尋結果欄位')),uiOutput('sem_y_type_control'),uiOutput('sem_y_detail_control')),
   conditionalPanel("input.sem_y_kind === 'latent'",selectizeInput('sem_y_items','Y 的測量題項',choices=choices(),selected=character(),multiple=TRUE,options=list(plugins=list('remove_button'),placeholder='搜尋並選擇至少三題')),p(class='small-note','Y 的題項在這裡選擇；已確認的順序題會自動套用。')),
   uiOutput('sem_y_status')))
 })
 observeEvent(choices(),{
  ch<-choices();v<-isolate(input$sem_y_variable) %||% '';updateSelectizeInput(session,'sem_y_variable',choices=c('請選擇結果欄位'='',ch),selected=if(v%in%ch)v else'')
  updateSelectizeInput(session,'sem_y_items',choices=ch,selected=intersect(isolate(input$sem_y_items),ch))
 },ignoreInit=TRUE)
 output$sem_y_type_control<-renderUI({
  req(input$sem_y_variable%in%names(data()));x<-data()[[input$sem_y_variable]]
  guess<-sem_outcome_type(x);types<-c(continuous='連續數值',binary='二元結果',ordinal='順序類別',nominal='多類別名目（目前不支援）')
  tagList(p(class='small-note',paste('辨識結果：',types[[guess]],'；請確認是否符合研究定義。')),
   selectInput('sem_y_type','Y 結果型態',c(setNames('auto',paste0('自動辨識：',types[[guess]])),setNames(names(types),types)),selected='auto'))
 })
 y_type<-reactive({req(input$sem_y_variable%in%names(data()));t<-input$sem_y_type %||% 'auto';if(t=='auto')sem_outcome_type(data()[[input$sem_y_variable]])else t})
 output$sem_y_detail_control<-renderUI({
  req(input$sem_y_variable%in%names(data()));x<-data()[[input$sem_y_variable]];t<-y_type()
  if(t=='binary')tagList(selectInput('sem_y_event','Y 事件組（必選）',choices=c('請選擇要估計的事件類別'='',sem_outcome_levels(x)),selected=''),p(class='small-note','另一類自動作為非事件組。二元 Y 使用 WLSMV；不需到「補充順序題設定」重複勾選。'))
  else if(t=='ordinal')tagList(textAreaInput('sem_y_levels','Y 級別順序（由低至高，以 | 分隔）',paste(if(is.ordered(x))levels(x)else sem_outcome_levels(x),collapse=' | '),rows=2),p(class='small-note','請確認級別順序，再以 WLSMV 估計；不需另行勾選 Y。'))
  else if(t=='continuous')p(class='small-note','連續 Y 可搭配 ML／MLR；若其他測量題項為順序，整體模型仍需 WLSMV。')
  else p(class='builder-validation','目前不支援多類別名目 Y；請選擇合適的結果模型。')
 })
 spec<-reactive(sem_outcome_spec(data()[,choices(),drop=FALSE],input$sem_y_kind %||% 'observed',input$sem_y_variable %||% '',input$sem_y_type %||% 'auto',input$sem_y_event %||% '',input$sem_y_levels %||% '',input$sem_y_items %||% character(),input$sem_y_label %||% ''))
 output$sem_y_status<-renderUI(tryCatch({s<-spec();p(class='builder-valid',if(s$type=='binary')paste0('Y = ',s$variable,'；事件組 = ',s$event,'；非事件組 = ',s$reference,'。')else if(s$type=='latent')paste('Y 已選',length(s$items),'個測量題項。')else paste0('Y = ',s$variable,if(s$type=='ordinal')paste0('；順序：',paste(s$levels,collapse=' < '))else'；連續數值。'))},error=function(e)p(class='builder-validation',conditionMessage(e))))
 list(spec=spec)
}
