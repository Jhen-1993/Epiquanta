# Guided specifications generate the same lavaan syntax as the advanced editor.
# The builder never guesses indicators, adds paths, or chooses covariates.
source('R/sem_outcome.R',encoding='UTF-8')
source('R/measurement_builder.R',encoding='UTF-8')
sem_build_spec<-function(nodes,edges,available){
 need(length(nodes)>=2,'請新增至少兩個構面／觀察變項，再設定路徑。')
 codes<-vapply(nodes,function(n)trimws(n$code),character(1))
 need(all(grepl('^[A-Za-z][A-Za-z0-9_]*$',codes)),'構面代碼請用英文字母開頭，可含數字與底線，例如 F1、F2、Y。')
 need(!anyDuplicated(codes),'構面／變項代碼不可重複。')
 ids<-vapply(nodes,function(n)n$id,character(1));symbols<-character(length(nodes));labels<-character(length(nodes));items<-character();lines<-character()
 for(i in seq_along(nodes)){
  n<-nodes[[i]];label<-trimws(n$label);if(!nzchar(label))label<-codes[i]
  if(n$kind=='latent'){
   need(!codes[i]%in%available,paste(codes[i],'與資料欄名相同，請換一個構面代碼。'))
   v<-unique(n$items);need(length(v)>=3,paste(label,'請選至少 3 題；需要兩題構面及識別限制時，請改用進階語法。'))
   need(all(v%in%available),paste(label,'含未納入或不存在的題項，請重新選擇。'))
   need(!any(v%in%items),'同一題不可分配到兩個構面；跨負荷模型請使用進階語法。')
   items<-c(items,v);symbols[i]<-codes[i];lines<-c(lines,paste(codes[i],'=~',paste(v,collapse=' + ')))
  }else{
   need(n$variable%in%available,paste(label,'請選擇對應的觀察變項。'));symbols[i]<-n$variable
  }
  labels[i]<-if(label==codes[i])label else paste0(label,' (',codes[i],')')
 }
 need(!anyDuplicated(symbols),'不同節點不可對應同一個構面／觀察變項。')
 need(!any(symbols%in%items),'題項不可同時作為獨立觀察節點；請確認模型或改用進階語法。')
 need(length(edges)>0,'請按「＋新增路徑」，指定起點與終點。')
 ed<-do.call(rbind,lapply(edges,function(e)data.frame(From=e$from,To=e$to,Type=e$type,stringsAsFactors=FALSE)))
 need(all(ed$From%in%ids)&all(ed$To%in%ids),'每條路徑都需要指定起點與終點。')
 need(all(ed$From!=ed$To),'路徑起點與終點不可相同。')
 need(all(ed$Type%in%c('regression','covariance')),'請選擇有效的路徑種類。')
 key<-ifelse(ed$Type=='covariance',paste(pmin(ed$From,ed$To),pmax(ed$From,ed$To)),paste(ed$From,ed$To))
 need(!anyDuplicated(paste(ed$Type,key)),'路徑不可重複；雙向共變異關係只需設定一次。')
 directed<-ed[ed$Type=='regression',,drop=FALSE];need(nrow(directed)>0,'SEM 請至少指定一條單向結構路徑；純測量模型請選 CFA。')
 # Kahn traversal rejects feedback loops in the guided editor only.
 remaining<-ids
 while(length(remaining)){
  roots<-setdiff(remaining,directed$To[directed$From%in%remaining & directed$To%in%remaining])
  need(length(roots)>0,'逐步設定不支援循環／互為因果路徑；請確認方向，或改用含識別限制的進階語法。')
  remaining<-setdiff(remaining,roots)
 }
 need(all(ids%in%c(ed$From,ed$To)),'有節點尚未連接路徑；請補上路徑或移除該節點。')
 ed$From_symbol<-symbols[match(ed$From,ids)];ed$To_symbol<-symbols[match(ed$To,ids)]
 for(i in seq_len(nrow(ed)))lines<-c(lines,if(ed$Type[i]=='regression')paste(ed$To_symbol[i],'~',ed$From_symbol[i])else paste(ed$From_symbol[i],'~~',ed$To_symbol[i]))
 syntax<-paste(lines,collapse='\n')
 # Let the actual lavaan parser validate field-name syntax before execution.
 parsed<-lavaan::lavaanify(syntax)
 list(syntax=syntax,labels=setNames(labels,symbols),nodes=nodes,edges=ed,variables=lavaan::lavNames(parsed,'ov'))
}

efa_build_spec<-function(groups,available){
 need(length(groups)>0,'請新增至少一個構面題組。')
 labels<-vapply(groups,function(g)trimws(g$label),character(1))
 need(all(nzchar(labels))&&!anyDuplicated(labels),'各構面題組請填寫不同的名稱。')
 need(!any(grepl('[=\n\r]',labels)),'構面名稱不可包含等號或換行。')
 allitems<-character()
 for(i in seq_along(groups)){
  v<-unique(groups[[i]]$items);need(length(v)>=3,paste(labels[i],'EFA 請選至少 3 題。'))
  need(all(v%in%available),paste(labels[i],'有未納入或不存在的欄位。'))
  need(!any(v%in%allitems),'不同構面題組含重複題項，請確認分組。');allitems<-c(allitems,v)
 }
 list(items=allitems,groups=paste(vapply(seq_along(groups),function(i)paste(labels[i],'=',paste(groups[[i]]$items,collapse=', ')),character(1)),collapse='\n'))
}

builder_button<-function(action,id,label,class='btn btn-default btn-sm'){
 payload<-jsonlite::toJSON(list(action=action,id=as.character(id)),auto_unbox=TRUE)
 tags$button(type='button',class=class,onclick=paste0("Shiny.setInputValue('builder_action',",payload,",{priority:'event'})"),label)
}
sem_estimation_controls<-function(pick,cfa=FALSE){tagList(
 p(class='small-note',if(cfa)'已確認順序的測量題項會自動套用；請於資料與變項確認實際級別次序。'else'已確認為順序的模型題項與結果會自動套用，不需重複勾選。類別順序可到「資料與變項 → 修改」調整。'),
 tags$details(tags$summary('補充順序題設定（選填）'),p(class='small-note',if(cfa)'只列出本模型尚需確認題型的題項；已確認順序的題項自動帶入。'else'只列出本模型尚未設定的題項／內生結果。Y 與已確認的順序題自動帶入，不需重複勾選。'),uiOutput('sem_ordered_control')),
 selectInput('estimator','估計法',c(if(cfa)c('依已確認題型自動選擇'='auto'),'MLR（連續，穩健）'='MLR','ML（連續）'='ML','WLSMV（順序／二元題項）'='WLSMV')),uiOutput('sem_type_preview'),
 selectInput('validation','驗證樣本',c('同一／尚未確認'='same','獨立驗證樣本'='independent')))}
sem_controls<-function(pick){tagList(
 radioButtons('sem_entry','模型設定方式',c('逐步設定構面與路徑'='builder','進階 lavaan 語法'='syntax')),
 conditionalPanel("input.sem_entry === 'builder'",actionButton('open_builder_sem','設定構面與路徑 →',class='btn-primary btn-block'),p(class='small-note','在右側建立節點、逐條連接路徑，並為各潛在構面選擇題項。')),
 conditionalPanel("input.sem_entry === 'syntax'",textAreaInput('syntax','lavaan SEM 語法',value='',rows=8,placeholder='F1 =~ Q1 + Q2 + Q3\nF2 =~ Q4 + Q5 + Q6\nF2 ~ F1')),
 sem_estimation_controls(pick))}

setup_model_builder<-function(input,output,session,data,choices,raw){
 outcome<-setup_sem_outcome(input,output,session,data,choices,raw)
 cfa_cards<-setup_measurement_cards(input,output,session,data,choices,raw,'cfa')
 rel_cards<-setup_measurement_cards(input,output,session,data,choices,raw,'rel')
 cfa_correlation_saved<-reactiveVal('correlated')
 observeEvent(input$cfa_correlation,{if(input$cfa_correlation%in%c('correlated','orthogonal'))cfa_correlation_saved(input$cfa_correlation)},priority=200)
 cfa_scope_saved<-reactiveVal('joint')
 observeEvent(input$cfa_scope,{if(input$cfa_scope%in%c('joint','separate'))cfa_scope_saved(input$cfa_scope)},priority=200)
 state<-reactiveValues(nodes=list(),edges=list(),groups=list(),next_node=1L,next_edge=1L,next_group=1L)
 getv<-function(id,fallback){x<-input[[id]];if(endsWith(id,'_items')&&id%in%names(input))return(x %||% character());if(is.null(x))fallback else x}
 # Snapshot all existing fields before adding/removing a card, so edits survive.
 nodes_now<-reactive(lapply(state$nodes,function(n){p<-paste0('sem_node_',n$id,'_');list(id=n$id,code=getv(paste0(p,'code'),n$code),label=getv(paste0(p,'label'),n$label),kind=getv(paste0(p,'kind'),n$kind),items=getv(paste0(p,'items'),n$items),variable=getv(paste0(p,'variable'),n$variable))}))
 edges_now<-reactive(lapply(state$edges,function(e){p<-paste0('sem_edge_',e$id,'_');list(id=e$id,from=getv(paste0(p,'from'),e$from),to=getv(paste0(p,'to'),e$to),type=getv(paste0(p,'type'),e$type))}))
 groups_now<-reactive(lapply(state$groups,function(g){p<-paste0('efa_group_',g$id,'_');list(id=g$id,label=getv(paste0(p,'label'),g$label),items=getv(paste0(p,'items'),g$items))}))
 addnode<-function(){i<-state$next_node;state$next_node<-i+1L;state$nodes<-c(state$nodes,list(list(id=as.character(i),code=paste0('F',i),label='',kind='latent',items=character(),variable='')))}
 addgroup<-function(){i<-state$next_group;state$next_group<-i+1L;state$groups<-c(state$groups,list(list(id=as.character(i),label=paste0('構面 ',i),items=character())))}
 observeEvent(raw(),{state$nodes<-list();state$edges<-list();state$groups<-list();addnode();addgroup()},ignoreNULL=TRUE)
 observeEvent(input$open_builder_sem,updateTabsetPanel(session,'workspace',selected='builder'))
 observeEvent(input$open_builder_efa,updateTabsetPanel(session,'workspace',selected='builder'))
 observeEvent(input$module,{if(input$module%in%c('sem','efa','cfa','reliability'))updateTabsetPanel(session,'workspace',selected='builder')})
 observeEvent(input$builder_action,{
  a<-input$builder_action;id<-a$id
  if(a$action%in%c('add_node','remove_node')){
   state$nodes<-isolate(nodes_now());state$edges<-isolate(edges_now())
   if(a$action=='add_node')addnode()else{
    state$nodes<-Filter(function(n)n$id!=id,state$nodes)
    state$edges<-Filter(function(e)!id%in%c(e$from,e$to),state$edges)
   }
  }else if(a$action%in%c('add_edge','remove_edge')){
   state$edges<-isolate(edges_now())
   if(a$action=='add_edge'){i<-state$next_edge;state$next_edge<-i+1L;state$edges<-c(state$edges,list(list(id=as.character(i),from='',to='',type='regression')))}else state$edges<-Filter(function(e)e$id!=id,state$edges)
  }else if(a$action%in%c('add_group','remove_group')){
   state$groups<-isolate(groups_now());if(a$action=='add_group')addgroup()else state$groups<-Filter(function(g)g$id!=id,state$groups)
  }else if(a$action%in%c('all_sem','clear_sem','all_efa','clear_efa')){
   prefix<-if(grepl('sem$',a$action))'sem_node_'else'efa_group_'
   validids<-if(prefix=='sem_node_')vapply(state$nodes,`[[`,character(1),'id')else vapply(state$groups,`[[`,character(1),'id')
   if(id%in%validids)updateSelectizeInput(session,paste0(prefix,id,'_items'),selected=if(startsWith(a$action,'all'))choices()else character())
  }
 })
 output$builder_workspace<-renderUI({
  req(data());m<-input$module
  if(m=='sem')tagList(div(class='builder-intro',span(class='eyebrow','SEM MODEL BUILDER'),h3('先定義終點，再建立構面與路徑'),p('Y 的結果型態、事件組與測量題項集中在第一區。其他構面可作為預測因子或中介，再逐條連接到 Y。')),
   div(class='card outcome-card',h4('01　終點結果 Y'),p(class='small-note','先完成這一區。Y 會自動加入路徑終點清單，不需另外新增 Y 節點。'),uiOutput('sem_outcome_controls')),
   div(class='card',div(class='result-heading',h4('02　其他構面與觀察變項'),builder_button('add_node','','＋新增構面／變項')),uiOutput('sem_node_definitions')),
   div(class='card',div(class='result-heading',h4('03　逐條設定路徑'),builder_button('add_edge','','＋新增路徑')),p(class='small-note','例如 F1 → F2、F2 → Y；需要直接效果時，另加 F1 → Y。每條路徑均由你指定。'),uiOutput('sem_edge_cards')),
   div(class='card',h4('04　其他構面選擇題項'),uiOutput('sem_item_cards')),
   div(class='card',h4('模型檢查與語法預覽'),uiOutput('sem_builder_preview'),p(class='small-note','設定完成後，確認左側的題型與估計法，再按「送出並執行分析」。')))
  else if(m=='efa')tagList(div(class='builder-intro',span(class='eyebrow','EFA ITEM GROUPS'),h3('新增構面題組，直接選擇欄位'),p('每張卡片分開執行一次 EFA；因素數由平行分析或左側指定值決定。若要探索整份量表的因素結構，請把所有相關題項放在同一張卡片。')),
   div(class='result-heading',h4('構面題組'),builder_button('add_group','','＋新增構面',class='btn btn-primary')),
   uiOutput('efa_group_cards'),div(class='card',uiOutput('efa_builder_preview'),p(class='small-note','完成後到左側設定相關矩陣、平行分析與反向題，再按「送出並執行分析」。')))
  else if(m%in%c('cfa','reliability'))measurement_workspace(m,isolate(cfa_correlation_saved()),isolate(cfa_scope_saved()))
  else div(class='card',h3('構面與題項設定'),p('選擇 CFA、信度、SEM 或 EFA 後，這裡會顯示對應的操作介面。'))
 })
 output$sem_node_definitions<-renderUI({
  state$nodes;ch<-choices();ns<-isolate(nodes_now())
  if(!length(ns))return(p('按「＋新增構面／變項」開始。'))
  tagList(lapply(ns,function(n){p<-paste0('sem_node_',n$id,'_');div(class='builder-node-definition',
   textInput(paste0(p,'code'),'代碼',n$code),textInput(paste0(p,'label'),'名稱',n$label,placeholder='例如：工作環境'),selectInput(paste0(p,'kind'),'節點類型',c('潛在構面（多題）'='latent','觀察變項（單一欄位）'='observed'),selected=n$kind),
   builder_button('remove_node',n$id,'移除'),conditionalPanel(paste0("input.",p,"kind === 'observed'"),selectizeInput(paste0(p,'variable'),'對應資料欄位',choices=c('請選擇'='',ch),selected=n$variable,options=list(placeholder='搜尋欄位'))))}))
 })
 output$sem_item_cards<-renderUI({
  state$nodes;ch<-choices();ns<-isolate(nodes_now())
  tagList(lapply(ns,function(n){p<-paste0('sem_node_',n$id,'_');conditionalPanel(paste0("input.",p,"kind === 'latent'"),div(class='builder-item-card',uiOutput(paste0('sem_item_heading_',n$id)),selectizeInput(paste0(p,'items'),'測量題項（可搜尋、多選）',choices=ch,selected=n$items,multiple=TRUE,options=list(plugins=list('remove_button'),placeholder='輸入欄名後選擇；可重複加入多題')),div(class='builder-item-actions',builder_button('all_sem',n$id,'全選欄位'),builder_button('clear_sem',n$id,'清除'))))}))
 })
 observe({ns<-nodes_now();for(n in ns)local({id<-n$id;caption<-paste0(n$code,if(nzchar(n$label))paste0(' · ',n$label));output[[paste0('sem_item_heading_',id)]]<-renderUI(h4(caption))})})
 node_choices<-reactive({ns<-nodes_now();c(setNames(vapply(ns,`[[`,character(1),'id'),vapply(ns,function(n)paste0(n$code,if(nzchar(n$label))paste0(' · ',n$label)),character(1))),setNames('outcome',paste0('Y · 終點結果',if(nzchar(input$sem_y_label %||% ''))paste0(' · ',input$sem_y_label))))})
 output$sem_edge_cards<-renderUI({
  state$edges;es<-isolate(edges_now());ch<-isolate(node_choices())
  if(!length(es))return(p('尚未設定路徑。'))
  tagList(lapply(es,function(e){p<-paste0('sem_edge_',e$id,'_');div(class='builder-edge-row',selectInput(paste0(p,'from'),'起點',c('請選擇'='',ch[ch!='outcome']),selected=e$from),selectInput(paste0(p,'type'),'關係',c('→ 單向路徑'='regression','↔ 共變異'='covariance'),selected=e$type),selectInput(paste0(p,'to'),'終點',c('請選擇'='',ch),selected=e$to),builder_button('remove_edge',e$id,'移除'))}))
 })
 observeEvent(node_choices(),{ch<-node_choices();for(e in isolate(edges_now())){p<-paste0('sem_edge_',e$id,'_');for(k in c('from','to')){opts<-if(k=='from')ch[ch!='outcome']else ch;updateSelectInput(session,paste0(p,k),choices=c('請選擇'='',opts),selected=if(e[[k]]%in%unname(opts))e[[k]]else'')}}},ignoreInit=TRUE)
 output$efa_group_cards<-renderUI({state$groups;ch<-choices();gs<-isolate(groups_now());tagList(lapply(gs,function(g){p<-paste0('efa_group_',g$id,'_');div(class='card builder-item-card',div(class='result-heading',textInput(paste0(p,'label'),'構面／題組名稱',g$label),builder_button('remove_group',g$id,'移除此構面')),selectizeInput(paste0(p,'items'),'納入題項（可搜尋、多選）',choices=ch,selected=g$items,multiple=TRUE,options=list(plugins=list('remove_button'),placeholder='搜尋並選擇這個題組的欄位')),div(class='builder-item-actions',builder_button('all_efa',g$id,'全選欄位'),builder_button('clear_efa',g$id,'清除')))}))})
 sem_spec<-reactive({
  y<-outcome$spec();ns<-nodes_now();es<-edges_now()
  need(!any(vapply(ns,function(n)n$code=='Y',logical(1))),'Y 請在「終點結果 Y」獨立設定；其他構面請使用不同代碼。')
  if(y$kind=='latent')while(y$symbol%in%vapply(ns,`[[`,character(1),'code'))y$symbol<-paste0(y$symbol,'_')
  need(!any(vapply(es,function(e)e$from=='outcome'||(e$to=='outcome'&&e$type!='regression'),logical(1))),'Y 是終點結果，請以單向路徑連向 Y；多終點或其他複雜結構請使用進階語法。')
  need(any(vapply(es,function(e)e$to=='outcome'&&e$type=='regression',logical(1))),'請新增至少一條通往「Y · 終點結果」的單向路徑。')
  yn<-list(id='outcome',code=if(y$kind=='latent')y$symbol else'Y',kind=y$kind,label=y$label,items=y$items,variable=y$variable)
  s<-sem_build_spec(c(ns,list(yn)),es,choices());s$labels[y$symbol]<-if(y$label=='Y')'Y'else paste0(y$label,' (Y)');s$outcome<-y;s
 })
 efa_spec<-reactive(efa_build_spec(groups_now(),choices()))
 model_input<-reactive({
  req(data(),input$module%in%c('sem','cfa'))
  s<-if(input$module=='sem'&&(input$sem_entry %||% 'builder')=='builder')sem_spec()else if(input$module=='cfa'&&(input$cfa_entry %||% 'builder')=='builder')cfa_cards$spec()else NULL
  syntax<-if(!is.null(s))s$syntax else if(input$module=='cfa')input$cfa_syntax %||% ''else input$syntax %||% ''
  need(nzchar(trimws(syntax)),'完成模型設定後，這裡會顯示實際使用的題型。')
  list(pt=lavaan::lavaanify(syntax),data=sem_outcome_data(data(),s$outcome),outcome=s$outcome)
 })
 ordered_candidates<-reactive({m<-model_input();vars<-lavaan::lavNames(m$pt,'ov.nox');setdiff(vars[!vapply(m$data[vars],is.ordered,logical(1))],m$outcome$variable)})
 output$sem_ordered_control<-renderUI(tryCatch({ch<-ordered_candidates();if(!length(ch))return(p(class='small-note','本模型不需要補充順序題設定。'));checkboxGroupInput('ordered','補充指定為順序的欄位',choices=ch,selected=intersect(isolate(input$ordered),ch))},error=function(e)p(class='small-note','完成模型設定後，會顯示本模型可補設定的欄位。')))
 effective_ordered<-reactive(intersect(input$ordered %||% character(),ordered_candidates()))
 effective_estimator<-reactive({e<-input$estimator %||% if(input$module=='cfa')'auto'else'MLR';if(e!='auto')return(e);need(input$module=='cfa','此分析請指定估計法。');m<-model_input();vars<-lavaan::lavNames(m$pt,'ov.nox');if(length(effective_ordered())||any(vapply(m$data[vars],is.ordered,logical(1))))'WLSMV'else'MLR'})
 model_types<-reactive({
  m<-model_input();sem_variable_spec(m$data,m$pt,effective_ordered(),effective_estimator())
 })
 output$sem_type_preview<-renderUI(tryCatch({t<-model_types();if(length(t$ordered))tagList(p(class='builder-valid',paste0('本模型使用 ',length(t$ordered),' 個順序題／結果（WLSMV）。')),tags$details(tags$summary('查看題項與級別順序'),tags$ul(lapply(t$ordered,function(v)tags$li(paste0(v,'：',paste(t$levels[[v]],collapse=' < '),if(v%in%t$automatic)'（自動套用）'else'（另指定）'))))))else p(class='small-note','本模型使用數值變項，未指定順序題。')},error=function(e)p(class='builder-validation',conditionMessage(e))))
 output$sem_builder_preview<-renderUI(tryCatch({s<-sem_spec();check<-tryCatch({model_types();p(class='builder-valid','路徑、題型與估計法設定已完整，可以送出估計；模型識別與收斂將在估計時檢查。')},error=function(e)p(class='builder-validation',conditionMessage(e)));tagList(check,tags$details(tags$summary('查看自動產生的 lavaan 語法'),tags$pre(s$syntax)))},error=function(e)p(class='builder-validation',conditionMessage(e))))
 output$efa_builder_preview<-renderUI(tryCatch({s<-efa_spec();p(class='builder-valid',paste('已設定',length(state$groups),'個題組，共',length(s$items),'個題項。'))},error=function(e)p(class='builder-validation',conditionMessage(e))))
 output$cfa_builder_preview<-renderUI(tryCatch({s<-cfa_cards$spec();t<-model_types();separate<-identical(input$cfa_scope,'separate');tagList(p(class='builder-valid',paste0('已設定 ',length(s$cards),' 個構面、',length(s$items),' 個題項；',if(separate)'各構面分開估計，估計法依所選題型逐組決定。'else paste0('聯合估計法：',effective_estimator(),'。'))),
  if((separate&&any(vapply(s$cards,function(g)length(g$items)==3,logical(1))))||(length(s$cards)==1&&length(s$items)==3))p(class='builder-validation','單構面三題通常剛好識別；即使能估計，也不能用整體配適度證明構面正確。'),
  p(class='small-note','基本設定固定各構面第一題負荷為 1；收斂與不適當解會在估計時檢查。'),tags$details(tags$summary('查看自動產生的 CFA 語法'),tags$pre(if(separate)paste(vapply(s$cards,function(g)paste(g$code,'=~',paste(g$items,collapse=' + ')),character(1)),collapse='\n# 下一構面分開配適\n')else s$syntax)))},error=function(e)p(class='builder-validation',conditionMessage(e))))
 output$rel_reverse_control<-renderUI({ch<-if((input$rel_entry %||% 'builder')=='builder')intersect(rel_cards$items(),choices())else intersect(input$items %||% character(),choices());selectizeInput('rel_reverse','反向題（僅列已選題項）',choices=ch,selected=intersect(isolate(input$rel_reverse),ch),multiple=TRUE,options=list(plugins=list('remove_button')))})
 list(sem=sem_spec,efa=efa_spec,cfa=cfa_cards$spec,reliability=rel_cards$spec,ordered=effective_ordered,estimator=effective_estimator,outcome=outcome$spec)
}
