if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('app.R',encoding='UTF-8');dir.create('test-output',showWarnings=FALSE)
records<-list();test<-function(name,expr){cat(name,'... ');tryCatch({force(expr);records[[length(records)+1L]]<<-data.frame(Test=name,Status='PASS',Detail='');cat('PASS\n')},error=function(e){records[[length(records)+1L]]<<-data.frame(Test=name,Status='FAIL',Detail=conditionMessage(e));cat('FAIL:',conditionMessage(e),'\n')})}
fails<-function(expr,pattern=NULL){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'error'));if(!is.null(pattern))stopifnot(grepl(pattern,conditionMessage(e),fixed=TRUE))}
close_to<-function(a,b,tol=1e-7)stopifnot(length(a)==length(b),all(is.finite(a)),all(is.finite(b)),max(abs(a-b))<tol)
set.seed(41);N<-250;f<-rnorm(N);g<-rnorm(N);d<-data.frame(ID=1:N,Q1=f+rnorm(N,sd=.7),Q2=2*f+rnorm(N),Q3=f+rnorm(N,sd=.6),Q4=g+rnorm(N,sd=.6),Q5=g+rnorm(N,sd=.6),Q6=g+rnorm(N,sd=.6));items<-paste0('Q',1:6)
test('Raw standardized alpha, CITC and deletion match psych',{
 a<-reliability_analysis(d,items[1:3],B=100);p<-suppressWarnings(psych::alpha(d[items[1:3]],check.keys=FALSE,warnings=FALSE,delete=FALSE))
 close_to(a$tables$Reliability$Alpha,p$total$raw_alpha);close_to(a$tables$Reliability$Standardized_alpha,p$total$std.alpha);close_to(a$tables$Items$CITC,p$item.stats$r.drop);close_to(a$tables$Items$Alpha_if_deleted,p$alpha.drop$raw_alpha)
})
test('Two items report SB and r but omit CITC and deletion',{
 a<-reliability_analysis(d,items[1:2],B=100);s<-a$tables$Reliability;r<-cor(d$Q1,d$Q2);close_to(s$Inter_item_r,r);close_to(s$Spearman_Brown,2*r/(1+r));close_to(s$Standardized_alpha,s$Spearman_Brown)
 stopifnot(abs(s$Alpha-s$Spearman_Brown)>.01,all(is.na(a$tables$Items$CITC)),all(is.na(a$tables$Items$Alpha_if_deleted)))
})
test('Subject bootstrap exactly matches direct row resampling and percentile CI',{
 x<-as.matrix(d[items[1:2]]);b<-reliability_bootstrap(x,100,251);RNGkind('Mersenne-Twister','Inversion','Rejection');set.seed(251)
 direct<-t(replicate(100,{z<-x[sample.int(nrow(x),nrow(x),TRUE),,drop=FALSE];C<-cov(z);r<-cor(z)[1,2];c(2*(1-sum(diag(C))/sum(C)),2*r/(1+r))}))
 close_to(b$replicates,direct);close_to(b$audit$CI_low,apply(direct,2,quantile,.025));close_to(b$audit$CI_high,apply(direct,2,quantile,.975))
})
test('Fixed 5000 replicates and group seeds are reproducible',{
 a<-reliability_analysis(d,items,group_spec='First = Q1, Q2, Q3\nSecond = Q4, Q5, Q6',seed=219,subject_id='ID');stopifnot(all(a$tables$Bootstrap$Requested==5000),identical(a$tables$Reliability$Seed,c(219,220)))
 b<-reliability_analysis(d,items[1:3],seed=219);close_to(a$bootstrap_replicates$First,b$bootstrap_replicates[[1]]);saveRDS(a,'test-output/Reliability.rds')
})
test('Independent complete cases within each scale and labels',{
 dd<-d;dd$Q1[1:10]<-NA;a<-reliability_analysis(dd,items,group_spec='First = Q1, Q2, Q3\nSecond = Q4, Q5, Q6',label_spec='Q1 = Fatigue',B=100)
 stopifnot(identical(a$tables$Reliability$N,c(240L,250L)),a$tables$Items$Item[1]=='Fatigue');stopifnot(any(grepl('rowspan="3"',measurement_html(a$publication),fixed=TRUE)))
})
test('Reverse scoring preserves requested original-score reliability',{
 dd<-d;dd$Q1<-6-dd$Q1;lo<--20;hi<-26;a<-reliability_analysis(dd,items[1:3],reverse='Q1',lower=lo,upper=hi,B=100);b<-reliability_analysis(d,items[1:3],B=100);close_to(a$tables$Reliability$Alpha,b$tables$Reliability$Alpha)
})
test('Negative but defined bootstrap estimates are retained',{
 dd<-d;dd$Q2<--d$Q1+rnorm(N,sd=.8);a<-reliability_analysis(dd,c('Q1','Q2'),B=100);stopifnot(a$tables$Reliability$Alpha<0,any(a$bootstrap_replicates[[1]][,'Raw_alpha']<0),all(a$tables$Bootstrap$Valid==100))
})
test('Less than 95 percent valid replicates stops analysis with counts',{
 x<-matrix(c(0,0,0,0,1,1),ncol=2,byrow=TRUE);fails(reliability_bootstrap(x,500,10),'低於 95%')
})
test('Binary raw alpha equals KR20 using consistent variance convention',{
 set.seed(16);dd<-as.data.frame(matrix(rbinom(600,.size<-1,.5),ncol=3));names(dd)<-c('A','B','C');a<-reliability_analysis(dd,names(dd),B=100);p<-colMeans(dd);vpop<-mean((rowSums(dd)-mean(rowSums(dd)))^2);close_to(a$tables$Reliability$Alpha,3/2*(1-sum(p*(1-p))/vpop));stopifnot(grepl('KR-20',a$tables$Reliability$Score_type))
})
test('Unscored nominal, duplicate subject IDs and invalid group definitions reject',{
 dd<-d;dd$Q1<-factor(rep(c('A','B','C','D','E'),50));fails(reliability_analysis(dd,items[1:3],B=100),'名目')
 dd<-d;dd$ID[1]<-dd$ID[2];fails(reliability_analysis(dd,items[1:3],subject_id='ID',B=100),'重複')
 fails(reliability_groups(items,'A = Q1,Q2'));fails(reliability_groups(items[1:2],'A = Q1,Q1'))
})
test('Measurement Word Excel HTML export retains vertical merges and provenance',{
 a<-readRDS('test-output/Reliability.rds');a<-analysis_provenance(a,'reliability',list(design='cross',structure='independent'));piece<-result_piece(a,'publication');export_docx(piece,'test-output/Reliability.docx');export_xlsx(piece,'test-output/Reliability.xlsx')
 td<-tempfile();dir.create(td);unzip('test-output/Reliability.docx',files='word/document.xml',exdir=td);xml<-paste(readLines(file.path(td,'word/document.xml'),warn=FALSE),collapse='');stopifnot(grepl('w:vMerge w:val="restart"',xml,fixed=TRUE),grepl('5000',xml),grepl(R.version.string,xml,fixed=TRUE))
 unzip('test-output/Reliability.xlsx',files='xl/worksheets/sheet1.xml',exdir=td);xml<-paste(readLines(file.path(td,'xl/worksheets/sheet1.xml'),warn=FALSE),collapse='');stopifnot(grepl('mergeCell ref="A3:A5"',xml,fixed=TRUE));stopifnot('Calculation_info'%in%readxl::excel_sheets('test-output/Reliability.xlsx'))
 writeLines(measurement_html(a$publication),'test-output/Reliability_table.html',useBytes=TRUE)
})
test('EFA auto method follows confirmed item types and rejects mixed automatic choice',{
 stopifnot(efa_method(d,items)=='pearson');dd<-d;dd[items]<-lapply(dd[items],function(x)ordered(cut(x,breaks=c(-Inf,-.5,.5,Inf),labels=1:3)));stopifnot(efa_method(dd,items)=='poly');dd$Q1<-d$Q1;fails(efa_method(dd,items),'混合');stopifnot(efa_method(dd,items,'spearman')=='spearman')
})
test('EFA grouped table KMO MSA loadings and off-diagonal RMS match actual fit',{
 a<-efa_grouped(d,items,method='pearson',B=20,seed=51,nfactor=1,group_spec='First = Q1,Q2,Q3\nSecond = Q4,Q5,Q6');s<-a$tables$EFA_summary;R<-cor(d[items[1:3]]);f<-psych::fa(R,nfactors=1,n.obs=N,fm='pa',SMC=TRUE,rotate='none',smooth=FALSE,max.iter=1000);L<-as.matrix(f$loadings);res<-R-L%*%t(L)
 close_to(s$Off_diagonal_RMS[1],sqrt(mean(res[lower.tri(res)]^2)));close_to(a$tables$KMO$MSA[a$tables$KMO$Scale=='First'],c(psych::KMO(R)$MSA,psych::KMO(R)$MSAi));stopifnot(any(grepl('Pearson loading',names(a$publication$data))),!any(grepl('Polychoric loading',names(a$publication$data))));saveRDS(a,'test-output/EFA.rds')
})
test('Multi-factor EFA table preserves all primary loadings',{
 a<-efa_grouped(d,items,method='spearman',B=20,seed=12,nfactor=2);stopifnot(sum(grepl('Spearman loading',names(a$publication$data)))==2,!any(grepl('sensitivity',names(a$publication$data))))
})
test('EFA and ordinary model exports remain compatible',{
 a<-analysis_provenance(readRDS('test-output/EFA.rds'),'efa',list(design='cross',structure='independent',cor_method='pearson'));export_docx(result_piece(a,'publication'),'test-output/EFA.docx');export_xlsx(result_piece(a,'publication'),'test-output/EFA.xlsx');writeLines(measurement_html(a$publication),'test-output/EFA_table.html',useBytes=TRUE)
 b<-survey_regression(survey_demo(),'Score',c('Group','Age'),list(weight='Weight',psu='PSU',strata='Stratum'));export_xlsx(b,'test-output/Survey_after_measurement.xlsx');stopifnot('ModelTable'%in%readxl::excel_sheets('test-output/Survey_after_measurement.xlsx'))
})
test('Shiny reliability and EFA grouped end-to-end',{
 shiny::testServer(server,{
  ingest(d,'Synthetic measurement');session$setInputs(design='cross',structure='independent',purpose='measurement',module='reliability',rel_entry='text',items=items,reverse=character(),rel_groups='First = Q1,Q2,Q3\nSecond = Q4,Q5,Q6',rel_labels='',rel_seed=310,rel_subject_id='ID',run=1)
  if(result()$title=='分析未完成')stop(paste(result()$notes,collapse='; '));stopifnot(all(result()$tables$Bootstrap$Requested==5000),isTRUE(result()$publication$custom))
  session$setInputs(module='efa',efa_entry='text',efa_groups='First = Q1,Q2,Q3\nSecond = Q4,Q5,Q6',efa_labels='',efa_seed=31,cor_method='auto',permutations=20,nfactor=1,run=2)
  if(result()$title=='分析未完成')stop(paste(result()$notes,collapse='; '));stopifnot(all(result()$tables$EFA_summary$Correlation=='pearson'),!is.null(result()$publication))
 })
})
res<-do.call(rbind,records);write.csv(res,'test-output/信度與EFA表格驗證.csv',row.names=FALSE,fileEncoding='UTF-8');print(res[,1:2]);if(any(res$Status!='PASS'))stop('Measurement tests failed')
