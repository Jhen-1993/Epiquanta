Sys.setlocale('LC_CTYPE','English_United States.utf8');source('app.R',encoding='UTF-8')
dir.create('test-output',showWarnings=FALSE)
checks<-character();ok<-function(x,label){stopifnot(isTRUE(x));checks<<-c(checks,label);cat('PASS:',label,'\n')}
set.seed(2801);n<-500;a<-rnorm(n);b<-.45*a+rnorm(n);d<-data.frame(lapply(1:6,function(i)if(i<=3)a+rnorm(n,sd=.7)else b+rnorm(n,sd=.7)));names(d)<-paste0('Q',1:6)
syntax<-'F1 =~ Q1 + Q2 + Q3\nF2 =~ Q4 + Q5 + Q6\nF1 ~~ F2'
labels<-c(F1='工作環境 (F1)',F2='疲勞 (F2)')
for(est in c('MLR','WLSMV')){
 z<-d;if(est=='WLSMV')z[]<-lapply(z,function(x)ordered(cut(x,c(-Inf,-1,-.3,.3,1,Inf),labels=FALSE)))
 ord<-if(est=='WLSMV')names(z)else character()
 ans<-cfa_analysis(z,syntax,ord,estimator=est,node_labels=labels)
 ref<-lavaan::cfa(syntax,z,ordered=if(length(ord))ord else NULL,estimator=est)
 ss<-lavaan::standardizedSolution(ref,type='std.all',ci=TRUE);ss<-ss[ss$op=='=~',]
 ld<-ans$tables$CFA_loadings;cv<-ans$tables$CFA_convergent;dc<-ans$tables$CFA_discriminant
 ok(max(abs(ld$Std_loading-ss$est.std))<1e-8&&max(abs(ld$CI_low-ss$ci.lower))<1e-8&&max(abs(ld$CI_high-ss$ci.upper))<1e-8,paste(est,'loadings and standardized CIs match direct lavaan'))
 ok(max(abs(cv$CR-as.numeric(semTools::compRelSEM(ref))))<1e-8&&max(abs(cv$AVE-as.numeric(semTools::AVE(ref))))<1e-8,paste(est,'CR and AVE match semTools and preserve actual response scale'))
 ok(abs(dc$Latent_r[1]-lavaan::lavInspect(ref,'cor.lv')[1,2])<1e-8&&all(is.na(dc$HTMT2_CI_low))&&all(is.na(dc$Requested)),paste(est,'joint latent correlation is accurate and absent bootstrap CIs remain absent'))
 ok(all(cv$N==n)&&!('Paths'%in%names(ans$plots))&&length(ans$measurement_publications)==3,paste(est,'CFA foregrounds validity reports instead of structural path forest'))
 if(est=='WLSMV')saveRDS(ans,'test-output/CFA_validity_result.rds')
}
ortho<-cfa_analysis(d,sub('F1 ~~ F2','F1 ~~ 0*F2',syntax,fixed=TRUE),node_labels=labels)
ok(ortho$tables$CFA_discriminant$Latent_r==0&&ortho$tables$CFA_discriminant$Fornell_Larcker=='固定相關，不作判讀'&&is.na(ortho$tables$CFA_discriminant$r_CI_low),'fixed-zero factor correlation cannot count as evidence of discriminant validity')
one<-cfa_analysis(d,'F1 =~ Q1 + Q2 + Q3',node_labels=labels)
ok(!'CFA_discriminant'%in%names(one$tables)&&any(grepl('剛好識別',one$tables$CFA_diagnostics$Interpretation)),'single-factor CFA omits nonexistent factor pairs and identifies zero degrees of freedom')
bt<-cfa_analysis(d,syntax,B=30,boot=TRUE,seed=98,node_labels=labels)
bb<-bt$tables$Bootstrap_parameters;bi<-grep('^HTMT2 ',bb$Term);dc<-bt$tables$CFA_discriminant
ok(length(bi)==1&&identical(dc$HTMT2_CI_low,bb$CI_low[bi])&&identical(dc$Successful,bb$Successful[bi])&&dc$Requested==30,'HTMT2 table uses the actual bootstrap interval and valid count')
i<-match('CFA_loadings',names(ans$tables));piece<-result_piece(ans,'table',i)
ok(piece$publication$title=='CFA 標準化因素負荷量'&&grepl('delta method',publication_note(piece$publication),fixed=TRUE)&&grepl('工作環境',publication_html(piece$publication),fixed=TRUE),'individual CFA export retains formatted headers, construct names, and standardized-CI method')
export_docx(piece,'test-output/CFA_loadings_cards.docx');export_xlsx(piece,'test-output/CFA_loadings_cards.xlsx')
ok(file.info('test-output/CFA_loadings_cards.docx')$size>1000&&'MeasurementTable'%in%readxl::excel_sheets('test-output/CFA_loadings_cards.xlsx'),'CFA table exports generate Word and formatted Excel files')
ok(!'CFA_loadings'%in%names(sem_analysis(d,syntax)$tables),'SEM keeps its independent result presentation')
writeLines(checks,'test-output/cfa_reporting_checks.txt')
