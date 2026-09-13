Sys.setlocale('LC_CTYPE','English_United States.utf8');source('app.R',encoding='UTF-8')
dir.create('test-output',showWarnings=FALSE);checks<-character();ok<-function(x,l){stopifnot(isTRUE(x));checks<<-c(checks,l);cat('PASS:',l,'\n')}
set.seed(701);a<-rnorm(350);b<-.3*a+rnorm(350)
d<-as.data.frame(setNames(lapply(1:8,function(i)(if(i<=4)a else b)+rnorm(350,sd=.5)),paste0('Q',1:8)))
d[1:20,1:4]<-NA;d[21:30,5:8]<-NA
cards<-lapply(1:2,function(i)list(id=as.character(i),code=paste0('F',i),label=paste0('構面',i),items=paste0('Q',((i-1)*4+1):(i*4))))
config<-measurement_group_spec(cards,names(d),'cfa')
s<-cfa_separate_analysis(d,config,estimator='auto')
ok(identical(s$tables$CFA_scope$N,c(330L,340L))&&!any(c('CFA_discriminant','SEM_fit_summary','HTMT2')%in%names(s$tables)),'separate CFA uses per-group samples without inventing joint fit or discriminant validity')
for(i in 1:2){ref<-lavaan::cfa(paste(cards[[i]]$code,'=~',paste(cards[[i]]$items,collapse='+')),data=d,estimator='MLR');ld<-lavaan::standardizedSolution(ref);ld<-ld[ld$op=='=~',];mine<-s$tables$CFA_loadings[(i-1)*4+1:4,];ok(max(abs(mine$Std_loading-ld$est.std))<1e-8,paste('separate factor',i,'matches its independent lavaan fit'))}
j<-cfa_analysis(d,config$syntax);ok(j$tables$CFA_scope$N==320&&'CFA_discriminant'%in%names(j$tables),'joint CFA uses the joint complete-case sample and reports factor relationships')
bad<-d;bad$Q1<-0;z<-cfa_separate_analysis(bad,config)
ok(nrow(z$tables$CFA_failures)==1&&all(z$tables$CFA_convergent$Construct=='構面2 (F2)')&&z$tables$CFA_scope$Status[1]=='未產生推論','failed factor is reported without removing the other factor or inventing estimates')
mixed<-d;mixed[5:8]<-lapply(mixed[5:8],function(x)ordered(cut(x,c(-Inf,-1,0,1,Inf),labels=FALSE)))
m<-cfa_separate_analysis(mixed,config);ok(identical(m$tables$CFA_scope$Estimator,c('MLR','WLSMV')),'automatic estimator follows each separate factor item type')
shiny::testServer(server,{
 ingest(d,'synthetic CFA scope');session$setInputs(design='cross',structure='independent',purpose='measurement',module='cfa',cfa_entry='builder',estimator='auto',validation='same',B=20,seed=9,bootstrap_ci=FALSE,cfa_group_1_code='F1',cfa_group_1_label='構面1',cfa_group_1_items=cards[[1]]$items,builder_action=list(action='add_cfa',id=''))
 session$setInputs(cfa_group_2_code='F2',cfa_group_2_label='構面2',cfa_group_2_items=cards[[2]]$items,cfa_scope='separate',run=1)
 r<-result();ok(identical(r$settings$cfa_scope,'separate')&&identical(r$tables$CFA_scope$N,c(330L,340L)),'Shiny runs and freezes the separate CFA selection')
 session$setInputs(cfa_scope='joint',run=2);ok(result()$title=='驗證性因素分析 CFA'&&result()$tables$CFA_scope$N==320,'switching to joint CFA fits the joint model')
})
saveRDS(s,'test-output/CFA_separate_result.rds');writeLines(checks,'test-output/cfa_scope_checks.txt')
