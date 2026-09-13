correlated_controls<-function(mode,ch,d,structure){
 select<-function(id,label,values)selectInput(id,label,c('請選擇'='',values))
 tagList(p(class='method-guidance',if(structure=='repeated')'長格式：每列＝一位個案的一次訪視；需 ID、時間／訪視、結果欄。同一 ID＋訪視只能有一列。寬格式的前測／後測欄需先轉為長格式。'else if(structure=='matched')'每列＝一位匹配成員，以配對組 ID 指定組內相依；不同配對組需獨立。此處適用非病例抽樣的匹配資料；病例對照請用條件式 Logistic。'else'單一群聚層級：每列＝一位個案，ID＝所屬群聚（如醫院）。若同時含個案內重複與醫院層級，需多層模型，此入口尚未支援。'),
  select('corr_id',if(structure=='repeated')'個案 ID（相依單位）'else if(structure=='matched')'配對組 ID（相依單位）'else'群聚 ID（相依單位）',names(d)),
  if(structure=='repeated')select('corr_time','時間／訪視欄（驗證 ID＋訪視唯一）',names(d)),
  selectInput('corr_kind','結果資料型態',c('請選擇'='','連續'='continuous','二元'='binary','計數'='count')),
  uiOutput('corr_outcome'),
  div(class='variable-checklist',checkboxGroupInput('corr_x','固定效果／干擾因子（勾選納入）',ch,selected=character()),covariate_buttons('corr_x')),
  div(class='variable-checklist',checkboxGroupInput('corr_inter','交互作用（兩個已納入固定效果）',ch,selected=character())),
  if(mode=='gee')tagList(selectInput('corr_corstr','工作相關結構',if(structure=='repeated')c('Exchangeable：組內相同相關'='exchangeable','Independence：工作獨立，仍用群聚穩健 SE'='independence','AR(1)：等距訪視編號'='ar1')else c('Exchangeable：組內相同相關'='exchangeable','Independence：工作獨立，仍用群聚穩健 SE'='independence')),p(class='small-note','GEE 估計群體平均效果；使用群聚 sandwich 標準誤。AR(1) 需整數訪視編號，缺訪時保留原編號間隔。'))
  else tagList(selectInput('corr_slope','隨機效果：截距＋可選數值斜率',c('只有隨機截距'='',ch[vapply(d[ch],is.numeric,logical(1))])),p(class='small-note','每個 ID 有隨機截距；選擇斜率時須另勾選該固定效果。LMM 使用 REML＋Satterthwaite；GLMM 使用 Laplace ML。')),
  conditionalPanel("input.corr_model === 'poisson'",selectInput('corr_offset','人時／觀察量 offset',c('不使用'='',ch[vapply(d[ch],is.numeric,logical(1))]))),
  uiOutput('corr_structure_check'))
}

paired_controls<-function(ch,d){
 tagList(p(class='method-guidance','長格式：每列＝一個配對成員或一次訪視。以配對 ID 與條件欄對齊兩組；每個 ID 在各條件只能一筆。三次以上訪視可明確選兩次比較；完整時間趨勢請用 GEE／混合模型。'),
  selectInput('pair_method','配對方法',c('配對 t 檢定（連續差值）'='paired_t','Wilcoxon signed-rank（連續差值）'='signed_rank','McNemar 精確檢定（二元）'='mcnemar')),
  selectInput('pair_id','配對／個案 ID',c('請選擇'='',names(d))),selectInput('pair_occasion','條件／訪視欄',c('請選擇'='',names(d))),uiOutput('pair_levels'),
  selectInput('pair_y','配對結果變項',c('請選擇'='',ch)),uiOutput('pair_event'))
}

clogit_controls<-function(ch,d){
 tagList(p(class='method-guidance','每列＝一位受試者；同一配對組 ID 表示匹配的病例與對照，可為 1:1 或 1:m。組內完全不變的配對因子已由條件化控制，不能再估計其係數。'),
  selectInput('clogit_id','配對組 ID',c('請選擇'='',names(d))),selectInput('clogit_y','病例／對照狀態',c('請選擇'='',ch[vapply(d[ch],function(v)length(unique(na.omit(v)))==2,logical(1))])),uiOutput('clogit_event'),
  div(class='variable-checklist',checkboxGroupInput('clogit_x','預測變項／干擾因子（勾選納入）',ch,selected=character()),covariate_buttons('clogit_x')),
  div(class='variable-checklist',checkboxGroupInput('clogit_inter','交互作用（兩個已納入變項）',ch,selected=character())))
}

setup_correlated_ui<-function(input,output,data,choices){
 output$corr_outcome<-renderUI({req(input$module%in%c('gee','mixed'),nzchar(input$corr_kind %||% ''));d<-data();ch<-choices();kind<-input$corr_kind
  values<-ch[vapply(d[ch],function(v)if(kind=='binary')length(unique(na.omit(v)))==2 else is.numeric(v),logical(1))]
  tagList(selectInput('corr_model','模型與效果量',correlated_models(input$module,kind,input$design)),selectInput('corr_y','結果變項',c('請選擇'='',values)),uiOutput('corr_event'))
 })
 output$corr_event<-renderUI({req(input$corr_kind=='binary',nzchar(input$corr_y %||% ''));lev<-unique(as.character(na.omit(data()[[input$corr_y]])));selectInput('corr_event','事件組（其餘類別＝非事件）',c('請指定事件'='',sort(lev)))})
 output$corr_structure_check<-renderUI({req(nzchar(input$corr_id %||% ''),data());d<-data();id<-input$corr_id;tab<-table(d[[id]]);if(!length(tab))return(p('ID 全部缺失。'))
  txt<-sprintf('匯入資料：%d 列；%d 個 ID；每個 ID %d–%d 列；%d 個 ID 只有一列。模型還會依已選欄位排除缺失並重算。',nrow(d),length(tab),min(tab),max(tab),sum(tab==1))
  time<-input$corr_time %||% '';if(input$structure=='repeated'&&nzchar(time)){keys<-d[complete.cases(d[,c(id,time),drop=FALSE]),c(id,time),drop=FALSE];if(anyDuplicated(keys))return(div(class='alert alert-danger',txt,br(),'發現重複 ID＋時間。請先釐清資料層級；此資料目前不能執行。'))}
  div(class='small-note',txt)
 })
 output$pair_levels<-renderUI({req(nzchar(input$pair_occasion %||% ''));lev<-unique(as.character(na.omit(data()[[input$pair_occasion]])));need(length(lev)<=100,'條件／訪視超過 100 種，請確認欄位。');tagList(selectInput('pair_a0','參考條件／訪視',c('請選擇'='',lev)),selectInput('pair_a1','比較條件／訪視（差值＝比較−參考）',c('請選擇'='',lev)))})
 output$pair_event<-renderUI({req(input$pair_method=='mcnemar',nzchar(input$pair_y %||% ''));lev<-unique(as.character(na.omit(data()[[input$pair_y]])));selectInput('pair_event','事件組',c('請指定事件'='',lev))})
 output$clogit_event<-renderUI({req(nzchar(input$clogit_y %||% ''));lev<-unique(as.character(na.omit(data()[[input$clogit_y]])));selectInput('clogit_event','病例組（非病例＝對照）',c('請指定病例'='',lev))})
}
