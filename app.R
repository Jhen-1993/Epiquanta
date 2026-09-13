upload_mb<-suppressWarnings(as.numeric(Sys.getenv('BIOSTAT_UPLOAD_MB','512')));if(!is.finite(upload_mb)||upload_mb<1||upload_mb>1900)upload_mb<-512
options(shiny.maxRequestSize=upload_mb*1024^2,contrasts=c('contr.treatment','contr.poly'))
local_lib<-Sys.getenv('BIOSTAT_LIBRARY','library');if(dir.exists(local_lib)).libPaths(c(normalizePath(local_lib),.libPaths()))
suppressPackageStartupMessages(library(shiny))
source('R/core.R',encoding='UTF-8');source('R/models.R',encoding='UTF-8');source('R/export.R',encoding='UTF-8')
source('R/smd.R',encoding='UTF-8');source('R/import.R',encoding='UTF-8');source('R/diagnostics.R',encoding='UTF-8');source('R/mediation.R',encoding='UTF-8');source('R/mediation_ui.R',encoding='UTF-8');source('R/measurement_ui.R',encoding='UTF-8');source('R/design.R',encoding='UTF-8');source('R/result_ui.R',encoding='UTF-8');source('R/correlated.R',encoding='UTF-8');source('R/correlated_ui.R',encoding='UTF-8');source('R/survey.R',encoding='UTF-8');source('R/survey_ui.R',encoding='UTF-8');source('R/guide_ui.R',encoding='UTF-8');source('R/reliability.R',encoding='UTF-8');source('R/efa_publication.R',encoding='UTF-8');source('R/model_builder.R',encoding='UTF-8');source('R/sem_diagram.R',encoding='UTF-8')
modules<-c('Survey：加權描述與設計校正比較'='survey_desc','Survey：加權迴歸'='survey_regression','Survey：加權 Cox'='survey_cox','GEE：群體平均效果'='gee','混合模型：LMM／GLMM'='mixed','配對／兩次重複測量比較'='paired','配對病例對照：條件式 Logistic'='clogit','連續變項：分布與常態檢查'='normality','一般中介'='mediation','因果中介'='causal_mediation','Table 1：描述與組間檢定'='descriptive','單一結果的組間比較'='comparison','兩個以上變項的相關'='correlation','信度分析：Cronbach α'='reliability','探索性因素分析 EFA'='efa','驗證性因素分析 CFA'='cfa','結構方程式（SEM）'='sem','迴歸模型：納入共變項'='regression','2×2 表：未調整比較'='effects','追蹤至首次事件：KM／Cox'='survival','復發事件：Andersen–Gill'='ag','工具變項：2SLS'='iv','ROC／AUC'='roc')
purposes<-c('描述樣本與比較組別'='describe','估計暴露與結果的關聯'='association','路徑與機制分析'='path','量表品質與效度'='measurement','評估分類判別表現'='discrimination')
purpose_modules<-list(describe=c('survey_desc','descriptive','comparison','paired','normality'),association=c('survey_regression','survey_cox','gee','mixed','clogit','effects','regression','survival','ag','correlation','iv'),path=c('mediation','causal_mediation','sem'),measurement=c('reliability','efa','cfa'),discrimination='roc')

ui<-fluidPage(tags$head(
 tags$title('Epiquanta · 流行病學與生物統計'),
 tags$link(rel='icon',type='image/svg+xml',href=paste0('epiquanta-favicon.svg?v=',as.integer(file.info('www/epiquanta-favicon.svg')$mtime))),
 tags$meta(name='theme-color',content='#101d33'),
 tags$link(rel='stylesheet',href=paste0('studio.css?v=',as.integer(file.info('www/studio.css')$mtime)))
 ),
 tags$header(class='studio-header',
  div(class='epiquanta-brand',HTML(paste(readLines('www/epiquanta-mark.svg',encoding='UTF-8',warn=FALSE),collapse='\n')),
   div(class='epiquanta-wordmark',h2(HTML('Epi<span>quanta</span>')),p('流行病學・生物統計'))),
  div(class='header-controls',span(class='local-badge',span(class='local-status-dot','aria-hidden'='true'),'資料在本機處理'),actionButton('shutdown','關閉本機服務',class='btn-sm'))),
 sidebarLayout(sidebarPanel(width=3,
  h4('01　匯入與確認'),radioButtons('import_mode','資料來源',c('瀏覽器上傳'='upload','本機大檔（免上傳）'='local'),inline=TRUE),conditionalPanel("input.import_mode !== 'local'",fileInput('file','CSV / Excel / SPSS / SAS / Stata',accept=c('.csv','.tsv','.xlsx','.xls','.sav','.sas7bdat','.dta'))),conditionalPanel("input.import_mode === 'local'",textInput('local_path','檔案完整路徑',placeholder='在檔案總管右鍵 → 複製為路徑，再貼上'),actionButton('inspect_local','讀取欄名與檔案資訊'),uiOutput('local_info')),
  selectInput('encoding','CSV 編碼',c('UTF-8','CP950')),textInput('na_strings','CSV 缺失標記（逗號分隔）','NA,N/A'),
  actionButton('submit_data','送出資料 →',class='btn-primary btn-block'),actionButton('demo','使用合成示範資料',class='btn-default'),actionButton('demo_repeated','重複測量示範資料',class='btn-default'),actionButton('demo_survey','複雜抽樣示範資料',class='btn-default'),hr(),
  conditionalPanel(condition="output.data_ready",
  h4('02　研究設計與資料結構'),
  selectInput('design','研究設計',c('請指定'='unspecified','世代研究 Cohort'='cohort','隨機對照試驗 RCT'='rct','病例對照 Case-control'='casecontrol','橫斷性 Cross-sectional'='cross')),
  selectInput('structure','資料結構',c('請指定'='unspecified','獨立個案'='independent','配對／匹配'='matched','重複測量'='repeated','群聚資料'='clustered','複雜抽樣'='survey','復發事件'='recurrent')),uiOutput('design_guidance'),
  h4('03　選擇分析'),selectInput('purpose','分析目的',c('請先指定研究設計與資料結構'='')),selectInput('module','分析方式',c('請先選擇分析目的'='')),uiOutput('method_guidance'),

  uiOutput('controls'),conditionalPanel(condition="output.data_confirmed && ['cfa','sem','regression','roc','mediation','causal_mediation'].includes(input.module)",hr(),h4('04　估計設定'),
  checkboxInput('bootstrap_ci','個案 Bootstrap 95% CI（支援模組）',FALSE),numericInput('B','Bootstrap 次數',5000,min=20,max=20000,step=100),numericInput('seed','Random seed',20260910,min=1,max=2147483646),
  p(class='small-note','Bootstrap 僅於問卷 α、CFA／SEM、中介分析、一般迴歸與 ROC 使用；不等同 EFA 置換式平行分析。')
  ),conditionalPanel(condition="input.module !== ''",conditionalPanel(condition="!output.data_confirmed",div(class='alert alert-info','請先在右側確認自動辨識的變項型態，再按「送出資料」，即可選擇分析變項。')),actionButton('run','送出並執行分析',class='btn-primary btn-block')),hr(),

 )
 ),mainPanel(width=9,
  uiOutput('status'),tabsetPanel(id='workspace',selected='help',
   tabPanel('方法與操作',value='help',user_guide(upload_mb)),
   tabPanel('資料與變項',value='data',conditionalPanel("!output.data_ready",div(class='card empty-data',h3('先匯入研究資料'),p('在左側選擇資料來源，完成設定後按「送出資料 →」。也可以先用合成示範資料熟悉流程。'))),conditionalPanel("output.data_ready",div(class='card',h3('變項型態'),p('已自動辨識並套用，可直接分析。若型態、類別順序或參考組不符合研究定義，請按該欄的「修改」。'),div(class='field-toolbar',textInput('field_search','搜尋欄位','',placeholder='輸入欄位名稱或原始名稱'),selectInput('field_filter','型態篩選',c('全部'='', '連續'='continuous','名目'='nominal','順序'='ordinal','識別碼'='id')),selectInput('field_page_size','每頁欄位',c(10,20,50),selected=20)),div(class='field-actions',actionButton('fields_select','納入全部搜尋結果'),actionButton('fields_clear','排除全部搜尋結果'),actionButton('fields_prev','上一頁'),actionButton('fields_next','下一頁'),textOutput('field_count',inline=TRUE)),uiOutput('variable_editor'),uiOutput('dictionary'),h4('目前資料預覽（前 10 筆）'),div(class='preview-scroll',role='region','aria-label'='資料預覽，可水平與垂直捲動',tabindex='0',tableOutput('preview'))))),
   tabPanel('構面與路徑',value='builder',uiOutput('builder_workspace')),
   tabPanel('分析結果',value='result',uiOutput('results'))
  )
 )))
server<-function(input,output,session){
 observeEvent(input$guide_start,{updateTabsetPanel(session,'workspace',selected='data')})
 allowed<-reactive(available_modules(input$design %||% 'unspecified',input$structure %||% 'unspecified'))
 output$design_guidance<-renderUI(tags$details(class='design-help',tags$summary('查看適用方法與資料需求'),p(class='small-note',design_guidance(input$design %||% 'unspecified',input$structure %||% 'unspecified'))))
 observeEvent(input$shutdown,{showNotification('本機服務已停止；下次請雙擊 Start_BioStat.cmd。',duration=NULL);later::later(function()stopApp(),.5)})
 observeEvent(list(input$design,input$structure),{
  a<-allowed();valid<-names(purpose_modules)[vapply(purpose_modules,function(m)any(m%in%a),logical(1))]
  updateSelectInput(session,'purpose',choices=c('請選擇分析目的'='',purposes[purposes%in%valid]),selected=if((input$purpose %||% '')%in%valid)input$purpose else '')
  result(NULL)
 },ignoreInit=FALSE)
 observeEvent(list(input$purpose,input$design,input$structure),{
  a<-intersect(purpose_modules[[input$purpose %||% '']] %||% character(),allowed())
  updateSelectInput(session,'module',choices=c('請選擇分析方式'='',modules[match(a,modules)]),selected=if((input$module %||% '')%in%a)input$module else '')
 })
 output$method_guidance<-renderUI({text<-switch(input$module %||% '',cfa='驗證題目與預定構面的測量關係。結果以因素負荷、收斂／區辨效度及配適度為主；構面相關不是方向性影響路徑。',sem='評估指定的方向性結構路徑；先定義 Y，再連接預測構面與中介。',effects='先看二元暴露與二元結果的未調整關聯。OR、RR／PR、風險差依研究設計計算；若需納入共變項，改選迴歸模型。',regression='先指定結果型態，再選擇模型與效果量。僅納入你勾選的變項；交互作用在同一模型中設定。',survival='用追蹤時間與事件／設限資料：不選共變項時產出 KM，納入共變項時另估 Cox HR。',iv='工具變項需要研究設計支持相關性、排除限制與獨立性；2SLS 不自動保證因果識別。',roc='目前提供同一樣本 ROC 與 AUC，不包含預測模型訓練、校準或外部驗證。',descriptive='一次產生樣本摘要與所選的組間檢定；p 值不應用來自動篩選干擾因子。','');if(nzchar(text))p(class='method-guidance',text)})

 raw<-reactiveVal(NULL);data<-reactiveVal(NULL);dict<-reactiveVal(NULL);result<-reactiveVal(NULL);data_label<-reactiveVal('尚未匯入資料');confirmed<-reactiveVal(FALSE)
 ingest<-function(d,label) {
  need(nrow(d)>0&&ncol(d)>0,'資料不可為空。');need(!anyDuplicated(names(d))&&all(nzchar(names(d))),'欄名不得空白或重複。')
  d<-as.data.frame(d,check.names=FALSE);original<-names(d);names(d)<-make.names(original,unique=TRUE)
  for(v in names(d)) {if(inherits(d[[v]],'haven_labelled'))d[[v]]<-haven::as_factor(d[[v]]);if(inherits(d[[v]],c('Date','POSIXct','POSIXlt')))d[[v]]<-as.character(d[[v]]);if(is.character(d[[v]]))d[[v]][trimws(d[[v]])=='']<-NA}
  raw(d);data(NULL);confirmed(FALSE);result(NULL);data_label(label)
  inferred<-lapply(names(d),function(v)infer_type(d[[v]],v));typ<-vapply(inferred,`[[`,character(1),'type')
  dd<-data.frame(Variable=names(d),Original=original,Type=typ,Missing=vapply(d,function(x)sum(is.na(x)),integer(1)),Unique=vapply(d,function(x)length(unique(na.omit(x))),integer(1)))
  dd$Reason<-vapply(inferred,`[[`,character(1),'reason');dd$Levels<-vapply(d,function(a)if(length(unique(na.omit(a)))<=30)paste(if(is.factor(a))levels(a)else sort(unique(na.omit(a))),collapse=' | ')else'',character(1));dict(dd);auto<-d;for(i in seq_along(auto))if(dd$Type[i]%in%c('nominal','ordinal'))auto[[i]]<-factor(auto[[i]],levels=if(is.factor(d[[i]]))levels(d[[i]])else sort(unique(na.omit(d[[i]]))),ordered=dd$Type[i]=='ordinal');data(auto);confirmed(TRUE);updateSelectInput(session,'module',selected='');updateTabsetPanel(session,'workspace',selected='data')
 }
 output$data_ready<-reactive(!is.null(raw()));outputOptions(output,'data_ready',suspendWhenHidden=FALSE)
 output$data_confirmed<-reactive(confirmed());outputOptions(output,'data_confirmed',suspendWhenHidden=FALSE)
 local_info<-reactiveVal(NULL)
 output$local_info<-renderUI({info<-local_info();if(is.null(info))return(p(class='small-note','先讀欄名，再選擇需要的欄位。'))
  tagList(p(sprintf('已讀取欄名 · %d 欄（尚未讀取完整資料）',length(info$names))),radioButtons('local_columns_mode','匯入欄位',c('僅指定欄位（大檔建議）'='selected','全部欄位'='all')),conditionalPanel("input.local_columns_mode === 'selected'",selectizeInput('local_columns','搜尋並選擇欄位',info$names,multiple=TRUE,options=list(placeholder='輸入欄名加入，可複選'))),p(class='small-note','CSV／TSV 可直接跳過未選欄位；Excel 仍需讀取活頁簿，所需記憶體可能遠高於檔案大小。'))
 })
 observeEvent(input$inspect_local,{tryCatch({path<-local_data_path(input$local_path);na<-c('',trimws(strsplit(input$na_strings %||% 'NA,N/A',',',fixed=TRUE)[[1]]));h<-read_dataset(path,encoding=input$encoding,na=na,header_only=TRUE);local_info(list(path=path,names=names(h),size=file.info(path)$size,mtime=file.info(path)$mtime,encoding=input$encoding))},error=function(e)showNotification(conditionMessage(e),type='error',duration=NULL))})
 observeEvent(input$submit_data,{
  tryCatch(withProgress(message='正在匯入資料',value=.05,{
   columns<-NULL
   if(identical(input$import_mode,'local')){info<-local_info();need(!is.null(info),'請先讀取本機檔案欄名。');path<-local_data_path(input$local_path);need(identical(path,info$path)&&identical(input$encoding,info$encoding)&&file.info(path)$mtime==info$mtime,'路徑、編碼或來源檔已變更，請重新讀取欄名。');name<-basename(path)
    if(!identical(input$local_columns_mode,'all')){columns<-input$local_columns;need(length(columns)>0&&all(columns%in%info$names),'請先選擇要匯入的欄位，或指定全部欄位。')}
   }else{need(!is.null(input$file),'請先選擇檔案。');path<-input$file$datapath;name<-input$file$name}
   na<-c('',trimws(strsplit(input$na_strings %||% 'NA,N/A',',',fixed=TRUE)[[1]]));setProgress(.15,detail='讀取所有資料列；大檔可能需要較長時間')
   d<-read_dataset(path,name,input$encoding,na,columns);setProgress(.65,detail='辨識變項型態');ingest(d,name);setProgress(1)
  }),error=function(e)showNotification(paste('匯入失敗：',conditionMessage(e)),type='error',duration=NULL))
 })
 observeEvent(input$demo,{
  set.seed(104);n<-360;f1<-rnorm(n);f2<-rnorm(n)*.8+.4*f1;d<-data.frame(ID=seq_len(n))
  for(j in 1:6)d[[paste0('Q',j)]]<-as.integer(cut((if(j<=3)f1 else f2)+rnorm(n,sd=.65),breaks=c(-Inf,-1,-.3,.3,1,Inf)))
  d$Age<-round(runif(n,25,80),1);d$Exposure<-factor(rbinom(n,1,.5));d$Modifier<-factor(rbinom(n,1,.5));d$Y<-rbinom(n,1,plogis(-2+.5*num(d$Exposure)+.4*num(d$Modifier)+.4*num(d$Exposure)*num(d$Modifier)))
  d$Biomarker<-d$Y+rnorm(n);d$Time<-rexp(n,.02*exp(.3*num(d$Exposure)));d$Event<-rbinom(n,1,.7);d$Count<-rpois(n,exp(.4+.2*num(d$Exposure)));d$Instrument<-rnorm(n);d$Endogenous<-.8*d$Instrument+rnorm(n);d$Continuous<-.7*d$Endogenous+.03*d$Age+rnorm(n)
  ingest(d,'合成示範資料（非真實受試者）')
 })
 observeEvent(input$demo_survey,{ingest(survey_demo(),'合成複雜抽樣示範（非真實受試者）')})
 observeEvent(input$demo_repeated,{ingest(correlated_demo(),'合成重複測量示範（非真實受試者）')})
 selected_fields<-reactiveVal(character());field_page<-reactiveVal(1L)
 observeEvent(raw(),{selected_fields(setdiff(names(raw()),dict()$Variable[dict()$Type=='id']));field_page(1L)},ignoreNULL=TRUE)
 filtered_fields<-reactive({req(dict());dd<-dict();idx<-seq_len(nrow(dd));q<-trimws(input$field_search %||% '');if(nzchar(q))idx<-idx[grepl(tolower(q),tolower(paste(dd$Variable[idx],dd$Original[idx])),fixed=TRUE)];tp<-input$field_filter %||% '';if(nzchar(tp))idx<-idx[dd$Type[idx]==tp];idx})
 observeEvent(list(input$field_search,input$field_filter,input$field_page_size),{field_page(1L)})
 observeEvent(input$fields_prev,{field_page(max(1L,field_page()-1L))})
 observeEvent(input$fields_next,{field_page(min(max(1L,ceiling(length(filtered_fields())/as.integer(input$field_page_size %||% 20))),field_page()+1L))})
 observeEvent(input$fields_select,{dd<-dict();v<-dd$Variable[filtered_fields()];selected_fields(union(selected_fields(),setdiff(v,dd$Variable[dd$Type=='id'])))})
 observeEvent(input$fields_clear,{selected_fields(setdiff(selected_fields(),dict()$Variable[filtered_fields()]))})
 observeEvent(input$field_toggle,{v<-input$field_toggle;req(v$index>=1,v$index<=nrow(dict()));name<-dict()$Variable[v$index];if(dict()$Type[v$index]!='id')selected_fields(if(isTRUE(v$checked))union(selected_fields(),name)else setdiff(selected_fields(),name))})
 output$field_count<-renderText({req(dict());sprintf('已納入 %d / %d 欄 · 找到 %d 欄 · 第 %d / %d 頁',length(intersect(selected_fields(),dict()$Variable[dict()$Type!='id'])),nrow(dict()),length(filtered_fields()),field_page(),max(1,ceiling(length(filtered_fields())/as.integer(input$field_page_size %||% 20))))})
 edit_index<-reactiveVal(NULL)
 output$dictionary<-renderUI({req(data());dd<-dict();labels<-c(continuous='連續',nominal='名目',ordinal='順序',id='識別碼');idx<-filtered_fields();size<-as.integer(input$field_page_size %||% 20);idx<-head(idx[seq_along(idx)>(field_page()-1)*size],size)
  if(!length(idx))return(p(class='small-note','找不到符合條件的欄位；請修改搜尋或篩選。'))
  div(class='table-scroll',tags$table(class='table field-table',tags$thead(tags$tr(lapply(c('納入','欄位','型態','參考組','缺失／不同值','設定'),tags$th))),tags$tbody(lapply(idx,function(i){v<-dd$Variable[i];ref<-if(is.factor(data()[[i]]))attr(data()[[i]],'reference') %||% levels(data()[[i]])[1]else '—';tags$tr(tags$td(tags$input(type='checkbox',checked=if(v%in%selected_fields()&&dd$Type[i]!='id')'checked'else NULL,disabled=if(dd$Type[i]=='id')'disabled'else NULL,'aria-label'=paste('納入',v),onchange=sprintf("Shiny.setInputValue('field_toggle', {index:%d, checked:this.checked}, {priority:'event'})",i))),tags$td(strong(v),if(dd$Original[i]!=v)div(class='small-note',dd$Original[i])),tags$td(span(class='type-badge',title=dd$Reason[i],labels[[dd$Type[i]]])),tags$td(ref),tags$td(paste(dd$Missing[i],'/',dd$Unique[i])),tags$td(tags$button(type='button',class='btn btn-default',onclick=sprintf("Shiny.setInputValue('edit_variable', %d, {priority:'event'})",i),'修改')))}))))
 })
 observeEvent(input$edit_variable,{
  i<-as.integer(input$edit_variable);req(dict(),i>=1,i<=nrow(dict()));edit_index(i);dd<-dict();lev<-if(is.factor(data()[[i]]))paste(levels(data()[[i]]),collapse=' | ')else dd$Levels[i]
  output$variable_editor<-renderUI(div(class='card variable-editor',h3(paste('修改變項：',dd$Variable[i])),selectInput('edit_type','變項型態',c('連續'='continuous','名目'='nominal','順序'='ordinal','識別碼（不納入分析）'='id'),selected=dd$Type[i]),conditionalPanel(condition="input.edit_type === 'nominal' || input.edit_type === 'ordinal'",selectInput('edit_reference','參考組（作為類別預測變項時）',choices=if(is.factor(data()[[i]]))levels(data()[[i]])else sort(unique(as.character(na.omit(raw()[[i]])))),selected=attr(data()[[i]],'reference') %||% if(is.factor(data()[[i]]))levels(data()[[i]])[1]else NULL)),textAreaInput('edit_levels','類別順序（以 | 分隔；順序型態由低至高）',lev,rows=4),p(class='small-note','順序型態的參考組僅用於類別預測變項，不改變順序結果的高低次序。二元結果事件組另於分析設定選擇。'),p('修改後按儲存才會套用；取消會保留原設定。'),tagList(actionButton('cancel_variable','取消'),actionButton('save_variable','儲存修改',class='btn-primary'))))
 })
 observeEvent(input$cancel_variable,{req(input$cancel_variable>0);output$variable_editor<-renderUI(NULL)})
 observeEvent(input$save_variable,{
  req(input$save_variable>0)
  tryCatch({i<-edit_index();req(i,raw());d<-data();dd<-dict();a<-raw()[[i]];tp<-input$edit_type;need(tp%in%c('continuous','nominal','ordinal','id'),'請選擇有效型態。')
   if(tp=='continuous'){v<-num(a);need(all(is.na(a)|is.finite(v)),'含非數值，不能轉為連續。');d[[i]]<-v}
   else if(tp=='id')d[[i]]<-a
   else {lev<-trimws(strsplit(input$edit_levels,'|',fixed=TRUE)[[1]]);need(length(lev)>0&&all(nzchar(lev))&&!anyDuplicated(lev),'類別不得空白或重複。');need(all(as.character(na.omit(a))%in%lev),'類別清單未包含所有資料值。');ref<-input$edit_reference %||% lev[1];need(ref%in%lev,'參考組必須在類別清單中。');if(tp=='nominal')lev<-c(ref,setdiff(lev,ref));d[[i]]<-factor(a,levels=lev,ordered=tp=='ordinal');attr(d[[i]],'reference')<-ref}
   dd$Type[i]<-tp;dd$Levels[i]<-if(is.factor(d[[i]]))paste(levels(d[[i]]),collapse=' | ')else input$edit_levels;dd$Reason[i]<-'使用者已修改';data(d);dict(dd);result(NULL);output$variable_editor<-renderUI(NULL);showNotification('已儲存變項設定，請重新執行分析以更新結果。',type='message')
  },error=function(e)showNotification(conditionMessage(e),type='error',duration=10))
 })
 output$status<-renderUI({div(class='status-bar',strong(data_label()),span(if(is.null(raw()))'請先匯入，或載入示範資料。'else paste(nrow(raw()),'筆 ×',ncol(raw()),'欄；資料物件',format(object.size(data() %||% raw()),units='auto'),'（非程序總記憶體）；',if(confirmed())'型態已自動套用，可直接分析'else'已自動辨識型態；請選擇模式並確認變項設定')) )})
 output$preview<-renderTable({req(raw());head(data()%||%raw(),10)},striped=TRUE,rownames=FALSE)
 choices<-reactive({req(data());intersect(setdiff(names(data()),dict()$Variable[dict()$Type=='id']),selected_fields())})
 output$controls<-renderUI({req(confirmed(),nzchar(input$module %||% ''));ch<-choices();d<-data();fac<-ch[vapply(d[ch],is.factor,logical(1))];numeric<-ch[vapply(d[ch],is.numeric,logical(1))];m<-input$module
  pick<-function(id,label,multi=FALSE,values=ch,blank=FALSE){
   if(multi)return(div(class='variable-checklist',checkboxGroupInput(id,paste0(label,'（勾選納入）'),choices=values,selected=character()),if(id=='x')covariate_buttons('x')))
   selectInput(id,label,if(blank)c('不使用'='',values)else values,selected=if(blank)''else values[1])
  }
  switch(m,
   survey_desc=survey_controls(m,ch,d),
   survey_regression=survey_controls(m,ch,d),
   survey_cox=survey_controls(m,ch,d),
   gee=correlated_controls('gee',ch,d,input$structure),
   mixed=correlated_controls('mixed',ch,d,input$structure),
   paired=paired_controls(ch,d),
   clogit=clogit_controls(ch,d),
   normality=tagList(pick('vars','連續變項',TRUE,values=numeric),pick('group','分組',values=fac,blank=TRUE),checkboxInput('normality_sample','N > 5000 時，明確抽取 5000 筆做 Shapiro–Wilk',FALSE),numericInput('normality_seed','抽樣／繪圖 seed',20260910)),
   mediation=mediation_controls(FALSE,ch,d),
   causal_mediation=mediation_controls(TRUE,ch,d),
   descriptive=tagList(pick('vars','描述變項',TRUE),pick('group','分組',values=fac,blank=TRUE),selectInput('summary','連續變項摘要',c('Mean ± SD'='mean','Median (Q1, Q3)'='median')),conditionalPanel("input.structure === 'independent'",selectInput('continuous_test','連續變項組間方法',c('不檢定'='none','Welch t（兩組）'='welch','Welch ANOVA（多組）'='anova','Wilcoxon rank sum（兩組）'='wilcox','Kruskal–Wallis（多組）'='kruskal')),selectInput('categorical_test','類別變項組間方法',c('不檢定'='none','Fisher exact'='fisher','Pearson 卡方'='chisq')),selectInput('p_adjust','多重比較校正',c('不校正'='none','Holm（本表所有已選檢定）'='holm','Bonferroni（本表所有已選檢定）'='bonferroni')),checkboxInput('show_smd','顯示標準化差異 SMD（需分組）',TRUE)),conditionalPanel("input.structure !== 'independent'",p(class='small-note','目前以資料列為單位產生未加權摘要，非受試者層級基線 Table 1；不執行獨立組間檢定或 SMD。請使用專屬配對／群聚模型。')),p(class='small-note','SMD 補充差異大小；多組主表顯示最大成對 |SMD|，詳表列出所有配對。請先選擇分組。p 值為變項整體組間檢定；此處方法適用獨立組別。')),
   comparison=tagList(pick('y','結果'),pick('group','分組',values=fac),selectInput('test','獨立組別方法',c('Welch t'='welch','Welch ANOVA'='anova','Wilcoxon rank sum'='wilcox','Kruskal–Wallis'='kruskal','Fisher exact'='fisher'))),
   correlation=tagList(pick('vars','相關變項',TRUE),selectInput('cor_method','相關方法',c('Spearman'='spearman','Pearson'='pearson')),selectInput('cor_p_adjust','多重檢定校正（所有成對相關）',c('Holm'='holm','Bonferroni'='bonferroni','不校正'='none'))),
   reliability=reliability_controls(pick,d),
   efa=factor_controls('efa',pick),
   cfa=cfa_controls(pick),
   sem=sem_controls(pick),
   regression=tagList(selectInput('outcome_kind','結果資料型態',if(input$design=='casecontrol')c('二元結果'='binary')else c('請選擇'='', '連續數值'='continuous','二元結果'='binary','計數'='count','順序類別'='ordinal','多項名目類別'='nominal')),uiOutput('regression_spec'),pick('x','預測變項／共變項',TRUE),pick('inter','交互作用（選兩個已納入預測變項）',TRUE),conditionalPanel(condition="['poisson','negbin'].includes(input.model)",pick('offsetvar','人時／offset（計數模型）',values=numeric,blank=TRUE))),
   effects=tagList(pick('y','二元結果',values=fac),uiOutput('event_ui'),pick('exposure','二元暴露',values=fac),uiOutput('exposed_ui')),
   survival=tagList(pick('time','追蹤時間',values=numeric),pick('y','事件指標',values=fac),uiOutput('event_ui'),pick('group','KM 分組',values=fac,blank=TRUE),pick('x','Cox 共變項（留空＝KM）',TRUE),pick('inter','Cox 交互作用（兩項）',TRUE),pick('id','受試者 ID（檢查重複）',values=names(d),blank=TRUE)),
   ag=tagList(pick('id','受試者 ID',values=names(d)),pick('start','起始時間',values=numeric),pick('time','終止時間',values=numeric),pick('y','事件指標',values=fac),uiOutput('event_ui'),pick('x','共變項',TRUE),pick('inter','交互作用（兩項）',TRUE)),
   iv=tagList(pick('y','連續結果',values=numeric),pick('exposure','連續內生暴露',values=numeric),pick('instruments','工具變項',TRUE),pick('x','外生共變項',TRUE)),
   roc=tagList(pick('y','二元結果',values=fac),uiOutput('event_ui'),pick('predictor','預測指標',values=numeric),selectInput('direction','事件方向',c('較高值＝事件'='<','較低值＝事件'='>')))
  )
 })
 observeEvent(input$efa_preset,{if(input$efa_preset%in%c('200','500'))updateNumericInput(session,'permutations',value=as.numeric(input$efa_preset))})
 observeEvent(input$covariate_bulk,{
  ev<-input$covariate_bulk;need(ev$id%in%c('x','med_covars','corr_x','clogit_x','svy_x'),'不支援此欄位操作。');ch<-choices()
  exclude<-if(ev$id=='med_covars')c(input$med_a,input$med_m,input$med_y,if(input$module=='causal_mediation'&&startsWith(input$med_yreg %||% '','surv'))input$med_eventvar)else c(input$y,if(input$module%in%c('survival','ag'))c(input$time,input$start,input$id),if(input$module=='iv')c(input$exposure,input$instruments),if(input$module=='regression')input$offsetvar)
  if(ev$id=='corr_x')exclude<-c(input$corr_y,input$corr_id,input$corr_offset)
  if(ev$id=='clogit_x')exclude<-c(input$clogit_y,input$clogit_id)
  if(ev$id=='svy_x')exclude<-c(input$svy_y,input$svy_time,input$svy_weight,input$svy_psu,input$svy_strata,input$svy_fpc,input$svy_sample_id,input$svy_offset)
  selected<-if(ev$action=='all')setdiff(ch,exclude)else character()
  if(ev$id%in%c('x','corr_x','clogit_x','svy_x'))updateCheckboxGroupInput(session,ev$id,selected=selected)else updateSelectizeInput(session,'med_covars',selected=selected)
 })
 output$regression_spec<-renderUI({req(input$module=='regression');kind<-input$outcome_kind %||% '';ch<-choices();d<-data();models<-regression_models(input$design,kind);if(!length(models))return(p('選擇結果型態後，會顯示可用模型。'))
  values<-ch[vapply(d[ch],function(a)switch(kind,continuous=is.numeric(a),count=is.numeric(a),binary=length(unique(na.omit(a)))==2,ordinal=is.ordered(a)&&nlevels(a)>=3,nominal=is.factor(a)&&!is.ordered(a)&&nlevels(a)>=3,FALSE),logical(1))]
  tagList(selectInput('model','模型與效果量',models),selectInput('y','結果變項',c('請選擇'='',values)),uiOutput('event_ui'))
 })
 output$event_ui<-renderUI({req(data(),input$y);if(input$module=='regression'&&!input$model%in%c('logistic','rr'))return(NULL);lev<-sort(unique(as.character(na.omit(data()[[input$y]]))));selectInput('event','事件組（要估計的結果類別）',lev,selected=if('1'%in%lev)'1'else tail(lev,1))})
 output$exposed_ui<-renderUI({req(data(),input$exposure);lev<-levels(factor(data()[[input$exposure]]));selectInput('exposed','暴露組',lev,selected=tail(lev,1))})
 observeEvent(input$run,{
  removeNotification('analysis-error',session=session)
  if(!isTRUE(confirmed())||is.null(data())){showNotification('請先確認右側變項型態，並按「送出資料」，再選擇要分析的變項。',type='warning',duration=12);updateTabsetPanel(session,'workspace',selected='data');return()}
  req(nzchar(input$module %||% ''));result(NULL);updateTabsetPanel(session,'workspace',selected='result')
  tryCatch(withProgress(message='R 正在分析',value=0,{
   d<-data();m<-input$module
   need(m%in%allowed(),'此模式不適用目前指定的研究設計／資料結構，或本版本尚未支援。')
   if(m%in%c('gee','mixed'))need(input$corr_model%in%unname(correlated_models(m,input$corr_kind,input$design)),'模型與結果型態不相符，請重新選擇。')
   if(m=='survey_regression')need(input$svy_model%in%switch(input$svy_kind %||% '',continuous='gaussian',binary=c('logistic','modified_poisson'),count='poisson',character()),'Survey 模型與結果型態不相符，請重新選擇。')
   need(m%in%(purpose_modules[[input$purpose %||% '']] %||% character()),'請從分析目的重新選擇適用的分析方式。')
   if(m=='regression')need(input$model%in%unname(regression_models(input$design,input$outcome_kind %||% '')),'模型與結果型態不相符，請重新選擇。')
   if(m=='regression'&&input$design=='casecontrol')need(input$model=='logistic','此版本病例對照的主要結果模型僅支援 Binary Logistic OR。')
   progress<-function(p)setProgress(value=p,detail=sprintf('目前計算階段 %.0f%%',100*p));warn<-character()
   sem_config<-if(m=='sem'&&(input$sem_entry %||% 'builder')=='builder')builder$sem()else NULL
   efa_config<-if(m=='efa'&&(input$efa_entry %||% 'builder')=='builder')builder$efa()else NULL
   cfa_config<-if(m=='cfa'&&(input$cfa_entry %||% 'builder')=='builder')builder$cfa()else NULL
   rel_config<-if(m=='reliability'&&(input$rel_entry %||% 'builder')=='builder')builder$reliability()else NULL
   ans<-withCallingHandlers(switch(m,
    survey_desc=survey_descriptive(d,input$svy_vars,survey_config(input),input$svy_group %||% '',isTRUE(input$svy_cont_test),isTRUE(input$svy_cat_test),input$svy_p_adjust %||% 'none'),
    survey_regression=survey_regression(d,input$svy_y,input$svy_x,survey_config(input),input$svy_model,input$design,input$svy_event %||% '',if(input$svy_model=='poisson')input$svy_offset %||% ''else'',input$svy_inter),
    survey_cox=survey_cox(d,input$svy_time,input$svy_y,input$svy_event,input$svy_x,survey_config(input),input$svy_inter),
    gee=correlated_analysis(d,input$corr_y,input$corr_x,input$corr_id,'gee',input$corr_model,input$design,input$structure,if(input$structure=='repeated')input$corr_time %||% ''else'',input$corr_event %||% '',input$corr_corstr %||% 'exchangeable',offsetvar=if(input$corr_model=='poisson')input$corr_offset %||% ''else'',inter=input$corr_inter),
    mixed=correlated_analysis(d,input$corr_y,input$corr_x,input$corr_id,'mixed',input$corr_model,input$design,input$structure,if(input$structure=='repeated')input$corr_time %||% ''else'',input$corr_event %||% '',slope=input$corr_slope %||% '',offsetvar=if(input$corr_model=='poisson')input$corr_offset %||% ''else'',inter=input$corr_inter),
    paired=paired_analysis(d,input$pair_y,input$pair_id,input$pair_occasion,input$pair_a0,input$pair_a1,input$pair_method,input$pair_event %||% ''),
    clogit=conditional_logistic(d,input$clogit_y,input$clogit_x,input$clogit_id,input$clogit_event,input$clogit_inter),
    normality=normality_analysis(d,input$vars,input$group,isTRUE(input$normality_sample),input$normality_seed %||% 20260910),
    mediation=ordinary_mediation(d,input$med_a,input$med_m,input$med_y,input$med_covars %||% character(),input$med_a0,input$med_a1,isTRUE(input$bootstrap_ci),input$B %||% 5000,input$seed %||% 20260910,progress,exposure_scale=input$med_exposure_scale %||% 'unit',exposure_change=input$med_exposure_change %||% 1),
    causal_mediation=causal_mediation(d,input$med_a,input$med_m,input$med_y,input$med_covars %||% character(),input$med_a0,input$med_a1,input$med_mreg,input$med_yreg,input$med_m_event %||% '',input$med_y_event %||% '',if(startsWith(input$med_yreg,'surv'))input$med_eventvar else '',as.numeric(input$med_m_cde),setNames(lapply(seq_along(input$med_covars),function(i)input[[paste0('med_cval_',i)]]),input$med_covars),isTRUE(input$med_interaction),isTRUE(input$med_rare),input$design,isTRUE(input$med_reviewed),isTRUE(input$bootstrap_ci),input$B %||% 5000,input$seed %||% 20260910,progress),
    descriptive=descriptive(d,input$vars,input$group,input$summary,if(input$structure=='independent')input$continuous_test %||% 'none'else'none',if(input$structure=='independent')input$categorical_test %||% 'none'else'none',if(input$structure=='independent')input$p_adjust %||% 'none'else'none',input$structure=='independent'&&isTRUE(input$show_smd)),
    comparison=comparison(d,input$y,input$group,input$test),
    correlation=correlation(d,input$vars,input$cor_method,input$cor_p_adjust %||% 'holm'),
    reliability=reliability_analysis(d,if(!is.null(rel_config))rel_config$items else input$items,input$rel_reverse %||% character(),input$rel_lower,input$rel_upper,if(!is.null(rel_config))rel_config$groups else input$rel_groups %||% '',input$rel_labels %||% '',input$rel_seed %||% 20260910,input$rel_subject_id %||% '',progress),
    efa=efa_grouped(d,if(!is.null(efa_config))efa_config$items else input$items,input$reverse,input$lower,input$upper,input$cor_method %||% 'auto',input$permutations,input$efa_seed %||% 20260910,input$nfactor,progress,if(!is.null(efa_config))efa_config$groups else input$efa_groups %||% '',input$efa_labels %||% ''),
    cfa=if(!is.null(cfa_config)&&identical(input$cfa_scope,'separate'))cfa_separate_analysis(d,cfa_config,builder$ordered(),input$estimator %||% 'auto',input$B,input$bootstrap_ci,input$seed,input$validation,progress)else cfa_analysis(d,if(!is.null(cfa_config))cfa_config$syntax else input$cfa_syntax,builder$ordered(),builder$estimator(),input$B,input$bootstrap_ci,input$seed,input$validation,progress,node_labels=if(!is.null(cfa_config))cfa_config$labels else NULL),
    sem=sem_analysis(d,if(!is.null(sem_config))sem_config$syntax else input$syntax,builder$ordered(),input$estimator,input$B,input$bootstrap_ci,input$seed,input$validation,progress,node_labels=if(!is.null(sem_config))sem_config$labels else NULL,outcome=if(!is.null(sem_config))sem_config$outcome else NULL),
    regression=regression(d,input$y,input$x,input$model,input$design,input$event,input$inter,if(input$model%in%c('poisson','negbin'))input$offsetvar %||% ''else'',input$bootstrap_ci,input$B,input$seed,progress),
    effects=effects2x2(d,input$y,input$exposure,input$event,input$exposed,input$design),
    survival=survival_analysis(d,input$time,input$y,input$event,input$x,input$group,input$id,inter=input$inter),
    ag=survival_analysis(d,input$time,input$y,input$event,input$x,id=input$id,start=input$start,ag=TRUE,inter=input$inter),
    iv=iv_analysis(d,input$y,input$exposure,input$instruments,input$x),
    roc=roc_analysis(d,input$y,input$predictor,input$event,input$direction,input$B,input$seed,input$bootstrap_ci)
   ),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')})
   if(m=='descriptive'&&input$structure!='independent'){ans$title<-'未加權資料列描述（非獨立個案基線 Table 1）';ans$notes<-c('以觀察列為單位；多次訪視個案會計入多列，不代表每人一次的基線摘要。不執行獨立樣本檢定或 SMD。',ans$notes);if(!is.null(ans$publication))ans$publication$note<-paste('未加權資料列描述；非受試者層級基線 Table 1。',publication_note(ans$publication))}
   # Freeze provenance alongside the result, independent of subsequent controls.
   allsettings<-reactiveValuesToList(input);allsettings$bootstrap<-isTRUE(input$bootstrap_ci);allsettings$file<-NULL;allsettings$run<-NULL;allsettings$local_path<-NULL;allsettings$local_info<-NULL
   if(!is.null(sem_config)){allsettings$syntax<-sem_config$syntax;ans$builder_spec<-sem_config}
   if(m=='cfa'){allsettings$syntax<-if(!is.null(cfa_config))cfa_config$syntax else input$cfa_syntax;allsettings$estimator_requested<-input$estimator;allsettings$estimator<-if(identical(ans$cfa_scope,'separate'))paste(unique(ans$cfa_group_settings$Estimator),collapse=' / ')else builder$estimator();allsettings$cfa_scope<-ans$cfa_scope %||% 'joint'}
   if(!is.null(cfa_config)){ans$builder_spec<-cfa_config;ans$tables$CFA_constructs<-cfa_config$table;allsettings$cfa_correlation<-cfa_config$correlation;ans$notes<-c(ans$notes,if(identical(ans$cfa_scope,'separate'))'CFA 卡片各自配適；未估計構面相關。'else if(cfa_config$correlation=='correlated')'CFA 卡片設定：構面間共變異自由估計。'else'CFA 卡片設定：構面間共變異固定為 0。')}
   if(!is.null(rel_config)){allsettings$items<-rel_config$items;allsettings$rel_groups<-rel_config$groups;ans$builder_spec<-rel_config}
   if(!is.null(ans$sem_type_spec)){allsettings$ordered_requested<-ans$sem_type_spec$requested;allsettings$ordered<-ans$sem_type_spec$ordered;allsettings$ordered_automatic<-ans$sem_type_spec$automatic}
   if(!is.null(efa_config)){allsettings$items<-efa_config$items;allsettings$efa_groups<-efa_config$groups}
   if(m=='mediation'){
    ec<-ans$tables$Exposure_contrast;allsettings$med_exposure_scale<-ec$Scale;allsettings$med_exposure_change<-ec$Change
    allsettings$med_a0<-if(ec$Scale=='unit')NULL else ec$From;allsettings$med_a1<-if(ec$Scale=='unit')NULL else ec$To
   }
   ans$settings<-allsettings;ans$dictionary<-dict();ans$session<-capture.output(sessionInfo());ans$notes<-c(paste('資料：',data_label()),paste('計算時間：',format(Sys.time())),paste('Random seed：',input$seed),ans$notes,if(length(warn))paste('R 警示：',unique(warn)))
   ans<-analysis_provenance(ans,m,allsettings)
   ans$tables$Settings<-data.frame(Setting=names(allsettings),Value=vapply(allsettings,function(x)paste(x,collapse=', '),character(1)))
   ans$tables$Dictionary<-dict();result(ans);updateTabsetPanel(session,'workspace',selected='result')
  }),error=function(e){result(out('分析未完成',notes=conditionMessage(e)));showNotification(conditionMessage(e),type='error',id='analysis-error',duration=12,session=session)})
 })
 setup_mediation_ui(input,output,data,choices)
 setup_correlated_ui(input,output,data,choices)
 setup_survey_ui(input,output,data,choices)
 builder<-setup_model_builder(input,output,session,data,choices,raw)
 setup_result_outputs(input,output,session,result)

}
shinyApp(ui,server)
