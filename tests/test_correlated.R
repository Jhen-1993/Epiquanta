if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('app.R',encoding='UTF-8');dir.create('test-output',showWarnings=FALSE)
records<-list();test<-function(name,expr){cat(name,'... ');tryCatch({force(expr);records[[length(records)+1L]]<<-data.frame(Test=name,Status='PASS',Detail='');cat('PASS\n')},error=function(e){records[[length(records)+1L]]<<-data.frame(Test=name,Status='FAIL',Detail=conditionMessage(e));cat('FAIL:',conditionMessage(e),'\n')})}
fails<-function(expr,pattern=NULL){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'error'));if(!is.null(pattern))stopifnot(grepl(pattern,conditionMessage(e),fixed=TRUE))}
close_to<-function(a,b,tol=1e-7)stopifnot(max(abs(a-b))<tol)
d<-correlated_demo();x<-c('Treatment','Visit','Age')
test('GEE Gaussian robust SE matches geepack',{
 a<-correlated_analysis(d,'Score',x,'Subject_ID',time='Visit')
 f<-geepack::geeglm(Score~Treatment+Visit+Age,data=d,id=Subject_ID,family=gaussian(),corstr='exchangeable',std.err='san.se')
 close_to(a$tables$Coefficients$Beta,coef(f));close_to(a$tables$Coefficients$SE_link,sqrt(diag(f$geese$vbeta)))
 export_docx(a,'test-output/GEE.docx');export_xlsx(a,'test-output/GEE.xlsx');saveRDS(a,'test-output/GEE.rds')
})
test('Shuffled rows are sorted by ID and wave',{
 a<-correlated_analysis(d,'Score',x,'Subject_ID',time='Visit',corstr='ar1')
 set.seed(9);b<-correlated_analysis(d[sample(nrow(d)),],'Score',x,'Subject_ID',time='Visit',corstr='ar1');close_to(a$tables$Coefficients$Beta,b$tables$Coefficients$Beta)
})
test('AR1 preserves missing scheduled waves',{
 dd<-d[!(d$Subject_ID%%3==0&d$Visit==2),];a<-correlated_analysis(dd,'Score',x,'Subject_ID',time='Visit',corstr='ar1')
 dd$Wave<-dd$Visit+1L;f<-geepack::geeglm(Score~Treatment+Visit+Age,data=dd,id=Subject_ID,waves=Wave,family=gaussian(),corstr='ar1',std.err='san.se')
 close_to(a$tables$Coefficients$Beta,coef(f))
})
test('GEE working independence',{
 a<-correlated_analysis(d,'Score',x,'Subject_ID',time='Visit',corstr='independence');stopifnot(all(is.finite(a$tables$Coefficients$SE_link)))
})
test('GEE binary OR and event reversal',{
 a<-correlated_analysis(d,'Binary',x,'Subject_ID',model='logistic',time='Visit',event='1')
 b<-correlated_analysis(d,'Binary',x,'Subject_ID',model='logistic',time='Visit',event='0')
 close_to(a$tables$Coefficients$OR,1/b$tables$Coefficients$OR,1e-5)
})
test('GEE modified Poisson RR and cross-sectional PR',{
 a<-correlated_analysis(d,'Binary',x,'Subject_ID',model='modified_poisson',time='Visit',event='1')
 b<-correlated_analysis(d,'Binary',x,'Subject_ID',model='modified_poisson',time='Visit',event='1',design='cross')
 close_to(a$tables$Coefficients$RR,b$tables$Coefficients$PR)
})
test('GEE count offset IRR matches geepack',{
 a<-correlated_analysis(d,'Count',x,'Subject_ID',model='poisson',time='Visit',offsetvar='Person_time')
 f<-geepack::geeglm(Count~Treatment+Visit+Age+offset(log(Person_time)),data=d,id=Subject_ID,family=poisson(),corstr='exchangeable',scale.fix=TRUE)
 close_to(log(a$tables$Coefficients$IRR),coef(f))
})
test('LMM random intercept and Satterthwaite inference',{
 a<-correlated_analysis(d,'Score',x,'Subject_ID',mode='mixed',time='Visit')
 f<-lmerTest::lmer(Score~Treatment+Visit+Age+(1|Subject_ID),d,REML=TRUE);s<-coef(summary(f,ddf='Satterthwaite'))
 close_to(a$tables$Coefficients$Beta,s[,1]);close_to(a$tables$Coefficients$p_value,s[,5]);close_to(a$tables$Coefficients$df,s[,3])
})
test('LMM random slope and interaction',{
 a<-correlated_analysis(d,'Score',x,'Subject_ID',mode='mixed',time='Visit',slope='Visit',inter=c('Treatment','Visit'))
 stopifnot(any(a$tables$Coefficients$Term=='Treatment1:Visit'),!a$tables$Estimation$Singular)
 f<-lmerTest::lmer(Score~Treatment+Visit+Age+Treatment:Visit+(1+Visit|Subject_ID),d,REML=TRUE)
 close_to(a$tables$Coefficients$Beta,lme4::fixef(f))
})
test('Binary GLMM conditional OR',{
 a<-correlated_analysis(d,'Binary',x,'Subject_ID',mode='mixed',model='logistic',time='Visit',event='1');dd<-d;dd$Binary<-as.integer(dd$Binary=='1')
 f<-lme4::glmer(Binary~Treatment+Visit+Age+(1|Subject_ID),dd,family=binomial(),nAGQ=1,control=lme4::glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=100000)))
 close_to(log(a$tables$Coefficients$OR),lme4::fixef(f))
})
test('Poisson GLMM with offset',{
 a<-correlated_analysis(d,'Count',c('Treatment','Visit'),'Subject_ID',mode='mixed',model='poisson',time='Visit',offsetvar='Person_time');stopifnot(all(is.finite(a$tables$Coefficients$IRR)))
})
test('Reference group respected',{
 dd<-d;attr(dd$Treatment,'reference')<-'1';a<-correlated_analysis(dd,'Score',x,'Subject_ID',time='Visit');b<-correlated_analysis(d,'Score',x,'Subject_ID',time='Visit')
 close_to(a$tables$Coefficients$Beta[2],-b$tables$Coefficients$Beta[2]);stopifnot(a$publication$data[2,2]=='Ref')
})
test('Structure and ID guards',{
 fails(correlated_analysis(rbind(d,d[1,]),'Score',x,'Subject_ID',time='Visit'),'重複列')
 dup<-rbind(d,d[1,]);dup$Score[nrow(dup)]<-NA;fails(correlated_analysis(dup,'Score',x,'Subject_ID',time='Visit'),'重複列')
 fails(correlated_analysis(d,'Score',x,'Subject_ID',time=''),'時間')
 fails(correlated_analysis(d,'Score',c(x,'Subject_ID'),'Subject_ID',time='Visit'),'固定效果')
 fails(correlated_analysis(d,'Score',x,'Subject_ID',time='Visit',structure='independent'))
 fails(correlated_analysis(d,'Score',x,'Subject_ID',time='Visit',structure='survey'))
 dd<-d;dd$Subject_ID<-seq_len(nrow(d));fails(correlated_analysis(dd,'Score',x,'Subject_ID',time='Visit'),'多筆')
 fails(correlated_analysis(d,'Score',x,'Subject_ID',time='Visit',corstr='ar1',structure='clustered'),'AR(1)')
 dd<-d;dd$Visit<-dd$Visit*.7;fails(correlated_analysis(dd,'Score',x,'Subject_ID',time='Visit',corstr='ar1'),'整數')
 fails(correlated_analysis(d,'Score',x,'Subject_ID',time='Visit',mode='mixed',slope='Treatment'),'數值')
 fails(correlated_analysis(d,'Binary',x,'Subject_ID',model='modified_poisson',time='Visit',event='1',design='casecontrol'))
})
test('Complete-case rows and ID counts reported',{
 dd<-d;dd$Score[1:6]<-NA;a<-correlated_analysis(dd,'Score',x,'Subject_ID',time='Visit')
 stopifnot(a$tables$Data_structure$Excluded_rows==6,a$tables$Data_structure$Analyzed_IDs==119)
})
test('Paired t follows ID not row ordering',{
 set.seed(7);dd<-d[sample(nrow(d)),];a<-paired_analysis(dd,'Score','Subject_ID','Visit','0','4')
 v0<-d$Score[d$Visit==0];v1<-d$Score[d$Visit==4];tt<-t.test(v1,v0,paired=TRUE)
 close_to(a$tables$Paired_test$p_value,tt$p.value);close_to(a$tables$Paired_test$CI_low,tt$conf.int[1])
 b<-paired_analysis(d,'Score','Subject_ID','Visit','4','0');close_to(a$tables$Paired_test$Mean_difference,-b$tables$Paired_test$Mean_difference)
})
test('Paired Wilcoxon matches R',{
 a<-paired_analysis(d,'Score','Subject_ID','Visit','0','4','signed_rank')
 close_to(a$tables$Paired_test$p_value,wilcox.test(d$Score[d$Visit==4],d$Score[d$Visit==0],paired=TRUE,exact=FALSE,correct=TRUE)$p.value)
})
test('Exact McNemar known discordant counts',{
 v0<-c(rep(0,15),rep(1,15));v1<-c(rep(1,12),rep(0,3),rep(0,5),rep(1,10))
 dd<-data.frame(ID=rep(1:30,each=2),Visit=rep(0:1,30),Y=factor(as.vector(rbind(v0,v1))))
 a<-paired_analysis(dd,'Y','ID','Visit','0','1','mcnemar','1')
 close_to(a$tables$Paired_test$p_value,binom.test(12,17,.5)$p.value);stopifnot(a$tables$Paired_test$Non_event_to_event==12)
})
test('Pair incomplete and duplicate guards',{
 dd<-d;dd$Score[1]<-NA;a<-paired_analysis(dd,'Score','Subject_ID','Visit','0','4');stopifnot(a$tables$Data_structure$Complete_pairs==119)
 fails(paired_analysis(rbind(d,d[1,]),'Score','Subject_ID','Visit','0','4'),'多筆')
})
test('Conditional logistic matches survival exact likelihood',{
 dd<-datasets::infert;dd$case<-factor(dd$case);a<-conditional_logistic(dd,'case',c('spontaneous','induced'),'stratum','1')
 strata<-survival::strata;coxph<-survival::coxph;Surv<-survival::Surv;f<-survival::clogit(case~spontaneous+induced+strata(stratum),datasets::infert,method='exact')
 close_to(log(a$tables$Coefficients$OR),coef(f));export_xlsx(a,'test-output/Conditional_Logistic.xlsx')
 fails(conditional_logistic(dd,'case',c('age'),'stratum','1'),'組內均不變')
})
test('AR1 gaps absent across all IDs retained',{
 dd<-d[d$Visit%in%c(0,2,4),];a<-correlated_analysis(dd,'Score',x,'Subject_ID',time='Visit',corstr='ar1');dd$Wave<-factor(dd$Visit+1,levels=1:5)
 f<-geepack::geeglm(Score~Treatment+Visit+Age,data=dd,id=Subject_ID,waves=Wave,family=gaussian(),corstr='ar1')
 close_to(a$tables$Coefficients$Beta,coef(f))
})
test('Singular mixed model does not publish Wald inference',{
 dd<-d;set.seed(8);err<-rnorm(nrow(dd));err<-err-ave(err,dd$Subject_ID);dd$Score<-1+.3*num(dd$Treatment)+.2*dd$Visit+err
 a<-correlated_analysis(dd,'Score',c('Treatment','Visit'),'Subject_ID',mode='mixed',time='Visit')
 stopifnot(a$tables$Estimation$Singular,all(is.na(a$tables$Coefficients$p_value)),all(is.na(a$tables$Coefficients$CI_low)))
})
test('Matched cohort GEE and repeated IDs are recognized',{
 a<-correlated_analysis(d,'Score',x,'Subject_ID',structure='matched');stopifnot(a$tables$Data_structure$Analyzed_IDs==120)
 stopifnot(infer_type(d$Subject_ID,'Subject_ID')$type=='id','gee'%in%available_modules('rct','matched'))
})
test('Design gates and separate path analysis',{
 stopifnot(all(c('gee','mixed','paired')%in%available_modules('cohort','repeated')),'clogit'%in%available_modules('casecontrol','matched'))
 stopifnot(!'regression'%in%available_modules('cohort','repeated'),!'gee'%in%available_modules('cohort','survey'),identical(purpose_modules$path,c('mediation','causal_mediation','sem')))
})
test('Shiny GEE and LMM end-to-end and bulk selection',{
 shiny::testServer(server,{
  ingest(d,'Synthetic repeated verification');session$setInputs(design='cohort',structure='repeated',purpose='association',module='gee',corr_kind='continuous',corr_model='gaussian',corr_y='Score',corr_id='Subject_ID',corr_time='Visit',corr_x=x,corr_inter=character(),corr_corstr='exchangeable',run=1)
  stopifnot(result()$title!='分析未完成',result()$tables$Data_structure$Analyzed_IDs==120)
  session$setInputs(module='mixed',corr_slope='',run=2);if(result()$title=='分析未完成')stop(paste(result()$notes,collapse='; '))
  session$setInputs(covariate_bulk=list(id='corr_x',action='all'));stopifnot(!is.null(output$controls))
 })
})
test('Shiny paired and conditional logistic dispatch',{
 shiny::testServer(server,{
  ingest(d,'Synthetic paired');session$setInputs(design='cohort',structure='repeated',purpose='describe',module='paired',pair_method='paired_t',pair_y='Score',pair_id='Subject_ID',pair_occasion='Visit',pair_a0='0',pair_a1='4',run=1)
  stopifnot(result()$title!='分析未完成')
  dd<-datasets::infert;dd$case<-factor(dd$case);ingest(dd,'Package infert');session$setInputs(design='casecontrol',structure='matched',purpose='association',module='clogit',clogit_y='case',clogit_id='stratum',clogit_x=c('spontaneous','induced'),clogit_event='1',clogit_inter=character(),run=2)
  stopifnot(result()$title!='分析未完成')
 })
})
test('Correlated Table1 never executes independent tests or SMD',{
 shiny::testServer(server,{
  ingest(d,'Synthetic');session$setInputs(design='cohort',structure='repeated',purpose='describe',module='descriptive',vars=c('Age','Treatment'),group='Treatment',summary='mean',continuous_test='welch',categorical_test='fisher',p_adjust='bonferroni',show_smd=TRUE,run=1)
  stopifnot(result()$title!='分析未完成',!'SMD_pairs'%in%names(result()$tables),grepl('資料列',result()$title))
 })
})
res<-do.call(rbind,records);write.csv(res,'test-output/資料結構模型驗證.csv',row.names=FALSE,fileEncoding='UTF-8');print(res[,1:2]);if(any(res$Status!='PASS'))stop('Correlated tests failed')
