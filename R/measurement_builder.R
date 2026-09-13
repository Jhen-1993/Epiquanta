# Separate card state for CFA and reliability. Cards only organize explicit
# selections; the existing statistical engines remain responsible for fitting.
measurement_group_spec<-function(groups,available,mode=c('cfa','reliability'),correlation='correlated'){
 mode<-match.arg(mode);need(length(groups)>0,'請按「＋新增構面」建立至少一個構面題組。')
 labels<-vapply(groups,function(g)trimws(g$label),character(1))
 if(mode=='cfa'){
  codes<-vapply(groups,function(g)trimws(g$code),character(1))
  need(all(grepl('^[A-Za-z][A-Za-z0-9_]*$',codes))&&!anyDuplicated(codes),'CFA 構面代碼需以英文字母開頭且不可重複，例如 F1、F2。')
  need(!any(codes%in%available),'CFA 構面代碼不可與資料欄名相同。')
  labels[!nzchar(labels)]<-codes[!nzchar(labels)]
 }else need(all(nzchar(labels))&&!anyDuplicated(labels)&&!any(grepl('[=\n\r]',labels)),'信度構面名稱不可空白、重複，或包含等號／換行。')
 items<-character();lines<-character()
 for(i in seq_along(groups)){
  v<-unique(groups[[i]]$items);minimum<-if(mode=='cfa')3 else 2
  need(length(v)>=minimum,paste0(labels[i],'：請選至少 ',minimum,' 題。',if(mode=='cfa')'兩題構面或特殊識別限制請使用進階語法。'else''))
  need(all(v%in%available),paste(labels[i],'含已排除或不存在的題項，請重新選擇。'))
  if(mode=='cfa')need(!any(v%in%items),paste0('題項 ',paste(intersect(v,items),collapse='、'),' 已分配至其他 CFA 構面；跨負荷模型請使用進階語法。'))
  items<-union(items,v)
  lines<-c(lines,if(mode=='cfa')paste(codes[i],'=~',paste(v,collapse=' + '))else paste(labels[i],'=',paste(v,collapse=', ')))
 }
 if(mode=='reliability')return(list(items=items,groups=paste(lines,collapse='\n'),cards=groups))
 need(correlation%in%c('correlated','orthogonal'),'請選擇有效的 CFA 構面相關設定。')
 pairs<-if(length(codes)>1)combn(codes,2,simplify=FALSE)else list()
 for(pair in pairs)lines<-c(lines,paste(pair[1],'~~',if(correlation=='orthogonal')paste0('0*',pair[2])else pair[2]))
 syntax<-paste(lines,collapse='\n');pt<-lavaan::lavaanify(syntax)
 list(syntax=syntax,labels=setNames(ifelse(labels==codes,labels,paste0(labels,' (',codes,')')),codes),items=items,variables=lavaan::lavNames(pt,'ov'),cards=groups,correlation=correlation,
  table=data.frame(Construct=codes,Label=labels,Items=vapply(groups,function(g)paste(g$items,collapse=', '),character(1)),Item_count=vapply(groups,function(g)length(unique(g$items)),integer(1))))
}

cfa_controls<-function(pick){tagList(
 radioButtons('cfa_entry','CFA 設定方式',c('構面卡片選題'='builder','進階 lavaan 語法'='syntax')),
 conditionalPanel("input.cfa_entry === 'builder'",actionButton('open_builder_cfa','設定 CFA 構面與題項 →',class='btn-primary btn-block'),p(class='small-note','在右側新增構面，直接搜尋並選取其測量題項。')),
 conditionalPanel("input.cfa_entry === 'syntax'",textAreaInput('cfa_syntax','CFA 測量模型語法（=~）',value='',rows=7,placeholder='F1 =~ Q1 + Q2 + Q3\nF2 =~ Q4 + Q5 + Q6')),
 sem_estimation_controls(pick,cfa=TRUE))}

measurement_workspace<-function(mode,correlation='correlated',scope='joint'){
 cfa<-mode=='cfa';prefix<-if(cfa)'cfa'else'rel'
 body<-tagList(div(class='builder-intro',span(class='eyebrow',if(cfa)'CFA MEASUREMENT MODEL'else'RELIABILITY SCALES'),h3(if(cfa)'定義構面，直接選擇測量題項'else'依量表與構面，安排信度題組'),
  p(if(cfa)'檢驗題目是否支持預定的測量構面。先選分構面檢查或聯合驗證，再指定各構面的題項；此處不設定方向性影響路徑。'else'每張卡片獨立計算信度。整體量表與分量表可以共用題項，請用不同名稱區分。')),
  if(cfa)div(class='card',h4('估計範圍'),radioButtons('cfa_scope','CFA 估計範圍',c('聯合 CFA：驗證整體測量模型'='joint','分構面 CFA：逐題組檢查'='separate'),selected=scope),p(class='small-note','分構面各自使用完整作答樣本，逐組列出問題；不合併產生整體配適度或跨構面區辨效度。驗證樣本來源在左側另外設定。')),
  div(class='result-heading',h4('01　構面與題項'),builder_button(paste0('add_',prefix),'','＋新增構面',class='btn btn-primary')),
  uiOutput(paste0(prefix,'_group_cards')),
  if(cfa)conditionalPanel("input.cfa_scope !== 'separate'",div(class='card',h4('02　構面之間的關係'),radioButtons('cfa_correlation','構面相關設定',c('允許各構面相關（預設）'='correlated','各構面不相關（固定為 0）'='orthogonal'),selected=correlation),p(class='small-note','依理論選擇；預設估計構面間共變異。特殊限制、跨負荷或高階因素可使用進階語法。'))),
  div(class='card',h4(if(cfa)'03　確認模型與題型'else'02　確認題組與計分'),uiOutput(paste0(prefix,'_builder_preview')),p(class='small-note',if(cfa)'確認左側估計法與驗證樣本，再按「送出並執行分析」。'else'有反向題時，請在左側選題並填量尺的理論最低／最高分，再按「送出並執行分析」。')))
 tagList(conditionalPanel(paste0('input.',prefix,"_entry === 'builder'"),body),conditionalPanel(paste0('input.',prefix,"_entry !== 'builder'"),div(class='card',h3('進階設定'),p('請於左側完成文字設定與估計選項，再按「送出並執行分析」。切回構面卡片時會保留原有選題。'))))
}

reliability_controls<-function(pick,d){tagList(
 radioButtons('rel_entry','信度題組設定方式',c('構面卡片選題'='builder','進階文字設定'='text')),
 conditionalPanel("input.rel_entry === 'builder'",actionButton('open_builder_rel','設定信度構面與題項 →',class='btn-primary btn-block')),
 conditionalPanel("input.rel_entry === 'text'",pick('items','納入信度題項',TRUE),textAreaInput('rel_groups','題組／構面（每行一組；留空為同一組）',rows=4,placeholder='構面A = Q1, Q2, Q3\n構面B = Q4, Q5')),
 uiOutput('rel_reverse_control'),conditionalPanel("input.rel_reverse && input.rel_reverse.length > 0",numericInput('rel_lower','反向題理論最低分',NA),numericInput('rel_upper','反向題理論最高分',NA),p(class='small-note','反向後分數＝最低分＋最高分−原分數；請填量尺定義的上下界。')),
 tags$details(tags$summary('題目簡稱與受試者識別（選填）'),textAreaInput('rel_labels','題目簡稱（每行一題）',rows=3,placeholder='Q1 = 題目簡稱'),selectizeInput('rel_subject_id','受試者 ID 欄位（檢查每人一列）',choices=c('不指定'='',names(d)),selected='')),
 numericInput('rel_seed','信度 Bootstrap 基礎種子',20260910,min=1,max=2147483646),
 p(class='small-note','固定 5,000 次受試者層級 Bootstrap；各題組依題數及可用的原始計分資料計算。三題以上提供 CITC／刪題 α；兩題提供題間 r 與 Spearman–Brown。'))}

setup_measurement_cards<-function(input,output,session,data,choices,raw,prefix){
 mode<-if(prefix=='cfa')'cfa'else'reliability'
 state<-reactiveValues(groups=list(),next_id=1L)
 value<-function(id,fallback){x<-input[[id]];if(endsWith(id,'_items')&&id%in%names(input))x %||% character()else x %||% fallback}
 groups_now<-reactive(lapply(state$groups,function(g){p<-paste0(prefix,'_group_',g$id,'_');list(id=g$id,code=value(paste0(p,'code'),g$code),label=value(paste0(p,'label'),g$label),items=value(paste0(p,'items'),g$items))}))
 # A renderUI output can be remounted from its cached HTML. Freeze edits while
 # leaving the module, before the browser removes inputs, to invalidate that
 # old HTML without rebuilding cards after every keystroke.
 was_active<-FALSE
 observeEvent(input$module,{
  if(was_active)state$groups<-isolate(groups_now())
  was_active<<-identical(input$module,mode)
 },priority=200)
 add<-function(){i<-state$next_id;state$next_id<-i+1L;state$groups<-c(state$groups,list(list(id=as.character(i),code=paste0('F',i),label=if(mode=='cfa')''else paste0('構面 ',i),items=character())))}
 observeEvent(raw(),{state$groups<-list();add()},ignoreNULL=TRUE)
 observeEvent(input[[paste0('open_builder_',prefix)]],updateTabsetPanel(session,'workspace',selected='builder'))
 observeEvent(input$builder_action,{
  a<-input$builder_action;id<-a$id
  if(a$action%in%paste0(c('add_','remove_'),prefix)){
   state$groups<-isolate(groups_now());if(a$action==paste0('add_',prefix))add()else state$groups<-Filter(function(g)g$id!=id,state$groups)
  }else if(a$action%in%paste0(c('all_','clear_'),prefix)&&id%in%vapply(state$groups,`[[`,character(1),'id')){
   other<-unlist(lapply(Filter(function(g)g$id!=id,isolate(groups_now())),`[[`,'items'),use.names=FALSE)
   ch<-if(mode=='cfa')setdiff(choices(),other)else choices()
   updateSelectizeInput(session,paste0(prefix,'_group_',id,'_items'),selected=if(startsWith(a$action,'all'))ch else character())
  }
 })
 output[[paste0(prefix,'_group_cards')]]<-renderUI({
  state$groups;ch<-choices();gs<-isolate(groups_now())
  if(!length(gs))return(div(class='card',p('尚未建立構面，請按「＋新增構面」。')))
  tagList(lapply(gs,function(g){p<-paste0(prefix,'_group_',g$id,'_');div(class='card measurement-group-card',
   div(class=if(mode=='cfa')'measurement-card-header cfa-card-header'else'measurement-card-header',
    if(mode=='cfa')textInput(paste0(p,'code'),'構面代碼',g$code),textInput(paste0(p,'label'),'構面名稱',g$label,placeholder='例如：工作環境'),builder_button(paste0('remove_',prefix),g$id,'移除此構面')),
   selectizeInput(paste0(p,'items'),'測量題項（可搜尋、多選）',choices=ch,selected=g$items,multiple=TRUE,options=list(plugins=list('remove_button'),placeholder='輸入欄名搜尋，逐一加入這個構面的題項')),
   div(class='builder-item-actions',builder_button(paste0('all_',prefix),g$id,if(mode=='cfa')'加入未分配欄位'else'全選欄位'),builder_button(paste0('clear_',prefix),g$id,'清除選題')),
   uiOutput(paste0(prefix,'_group_status_',g$id))) }))
 })
 observe({for(g in groups_now())local({id<-g$id;k<-length(g$items);caption<-paste0('已選 ',k,' 題。 ',if(mode=='cfa'){if(k<3)'請選至少 3 題。'else'送出前會核對題型與構面配置。'}else if(k<2)'請選至少 2 題。'else if(k==2)'兩題題組：題間 r、Spearman–Brown，保留原始 α。'else'三題以上：α、CITC 與刪題後 α。');output[[paste0(prefix,'_group_status_',id)]]<-renderUI(p(class='small-note',caption))})})
 spec<-reactive(measurement_group_spec(groups_now(),choices(),mode,input$cfa_correlation %||% 'correlated'))
 if(mode=='reliability')output$rel_builder_preview<-renderUI(tryCatch({s<-spec();tagList(p(class='builder-valid',paste0('已設定 ',length(s$cards),' 個題組，共 ',length(s$items),' 個不同題項。每題組使用自己的完整作答樣本。')),p(class='small-note','原始 α 與兩題 Spearman–Brown 的 95% CI 使用固定 5,000 次受試者 Bootstrap；每係數有效比例須達 95%。'))},error=function(e)p(class='builder-validation',conditionMessage(e))))
 list(spec=spec,items=reactive(unique(unlist(lapply(groups_now(),`[[`,'items'),use.names=FALSE))))
}
