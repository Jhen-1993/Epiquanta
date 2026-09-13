Sys.setlocale('LC_CTYPE','English_United States.utf8');.libPaths(c('library',.libPaths()))
source('R/core.R',encoding='UTF-8');source('R/models.R',encoding='UTF-8')
source('R/export.R',encoding='UTF-8');source('R/result_ui.R',encoding='UTF-8')
dir.create('test-output',showWarnings=FALSE)
checks<-character();ok<-function(x,label){stopifnot(isTRUE(x));checks<<-c(checks,label);cat('PASS:',label,'\n')}
# Independent, reproducible synthetic observations. Compare plot data against
# lavaan directly, including correlated measurement errors and binary outcomes.
set.seed(915);n<-700;f1<-rnorm(n);f2<-.4*f1+rnorm(n)
d<-data.frame(y=.3*f1+.5*f2+rnorm(n));shared<-rnorm(n,sd=.25)
for(i in 1:6)d[[paste0('q',i)]]<-list(f1,f2)[[ceiling(i/3)]]+rnorm(n,sd=.6)
d$q1<-d$q1+shared;d$q2<-d$q2+shared
syntax<-'F1 =~ q1 + q2 + q3\nF2 =~ q4 + q5 + q6\nF2 ~ F1\ny ~ F1 + F2\nq1 ~~ q2'
fit<-lavaan::sem(syntax,d,estimator='MLR');fig<-sem_publication_diagram(fit)
pe<-lavaan::parameterEstimates(fit,standardized=TRUE)
rr<-fig$residual_table;vv<-pe[pe$op=='~~'&pe$lhs==pe$rhs,]
ok(setequal(rr$Node,c(paste0('q',1:6),'F2','y'))&&all(abs(rr$Variance-vv$std.all[match(rr$Node,vv$lhs)])<1e-10),'e/d variances match fitted residuals and exclude exogenous variances')
ed<-fig$edge_table;co<-pe[pe$op=='~~'&pe$lhs!=pe$rhs,]
ok(nrow(ed[ed$Kind=='covariance',])==nrow(co)&&abs(ed$Std[ed$Kind=='covariance']-co$std.all)<1e-10,'correlated measurement error is preserved with its fitted value')
nd<-fig$node_table;me<-pe[pe$op=='=~',]
ok(all(nd$Latent==nd$Node%in%lavaan::lavNames(fit,'lv'))&&all(nd$Y[match(me$rhs,nd$Node)]<nd$Y[match(me$lhs,nd$Node)]),'latent/observed shapes and indicator rows follow the fitted measurement model')
cfa<-lavaan::cfa('F1 =~ q1 + q2 + q3\nF2 =~ q4 + q5 + q6',d,estimator='MLR')
cf<-sem_publication_diagram(cfa)
ok(any(cf$edge_table$Kind=='covariance')&&all(grepl('^e',cf$residual_table$Symbol)),'CFA retains factor covariance and only measurement-error circles')
endpoint<-readRDS('test-output/SEM_endpoint_result.rds')
tab<-result_piece(endpoint,'table',match('SEM_outcome',names(endpoint$tables)))
export_xlsx(tab,'test-output/SEM_endpoint_table.xlsx')
x<-readxl::read_excel('test-output/SEM_endpoint_table.xlsx',sheet='SEM_outcome')
ok(as.character(x$Event)=='1'&&as.character(x$Non_event)=='0'&&x$Variable=='Event','downloaded endpoint Excel table preserves event and non-event definitions')
p<-endpoint$plots$SEM_diagram;write_plot(p,'test-output/SEM_endpoint_diagram.png','png');write_plot(p,'test-output/SEM_endpoint_diagram.pdf','pdf')
ok(file.info('test-output/SEM_endpoint_diagram.png')$size>10000&&file.info('test-output/SEM_endpoint_diagram.pdf')$size>1000,'new binary-endpoint diagram exports PNG and vector PDF')
source('R/provenance.R',encoding='UTF-8');library(shiny)
shiny::testServer(function(input,output,session){setup_result_outputs(input,output,session,reactive(endpoint))},{
 html<-output$results$html
 pos<-vapply(c('終點結果 Y 與事件定義','SEM 模型配適度','SEM 結構與測量路徑圖','路徑係數與 95% 信賴區間'),function(s)regexpr(s,html,fixed=TRUE)[1],integer(1))
 ok(all(pos>0)&&all(diff(pos)>0)&&grepl('dl_plot_2_png',html,fixed=TRUE),'results show endpoint, fit, SEM diagram, then coefficient intervals with matching export IDs')
})
writeLines(checks,'test-output/sem_diagram_checks.txt',useBytes=TRUE)
