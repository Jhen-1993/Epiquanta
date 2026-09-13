if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('app.R',encoding='UTF-8');dir.create('test-output',showWarnings=FALSE)
records<-list();test<-function(name,expr){cat(name,'... ');tryCatch({force(expr);records[[length(records)+1L]]<<-data.frame(Test=name,Status='PASS',Detail='');cat('PASS\n')},error=function(e){records[[length(records)+1L]]<<-data.frame(Test=name,Status='FAIL',Detail=conditionMessage(e));cat('FAIL:',conditionMessage(e),'\n')})}
fails<-function(expr,pattern=NULL){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'error'));if(!is.null(pattern))stopifnot(grepl(pattern,conditionMessage(e),fixed=TRUE))}
close_to<-function(a,b,tol=1e-7)stopifnot(length(a)==length(b),all(is.finite(a)),all(is.finite(b)),max(abs(a-b))<tol)
d<-survey_demo();cfg<-list(weight='Weight',psu='PSU',strata='Stratum');des<-survey::svydesign(~PSU,strata=~Stratum,weights=~Weight,data=d,nest=TRUE)
test('Weighted mean, design SE and t CI match survey',{
 a<-survey_descriptive(d,'Score',cfg);tt<-a$tables$Survey_summary;s<-survey::svymean(~Score,des)
 close_to(tt$Weighted_mean,coef(s));close_to(tt$SE,survey::SE(s));close_to(tt$CI_low,coef(s)-qt(.975,survey::degf(des))*survey::SE(s))
 close_to(tt$Weighted_SD,sqrt(coef(survey::svyvar(~Score,des))));stopifnot(tt$Unweighted_N==600,tt$df==36)
})
test('Weighted percentage and logit CI retain unweighted counts',{
 a<-survey_descriptive(d,'Group',cfg)$tables$Survey_summary;dd<-des;dd$variables$I<-as.integer(d$Group=='1');s<-survey::svyciprop(~I,dd,method='logit',df=36)
 close_to(a$Weighted_percent[a$Level=='1'],100*coef(s));close_to(a$CI_low[a$Level=='1'],100*confint(s)[1]);stopifnot(a$Unweighted_level_n[a$Level=='1']==sum(d$Group=='1'))
})
test('Weight normalization preserves estimates and SE',{
 dd<-d;dd$Weight<-dd$Weight/mean(dd$Weight);a<-survey_descriptive(d,'Score',cfg)$tables$Survey_summary;b<-survey_descriptive(dd,'Score',cfg)$tables$Survey_summary
 close_to(a$Weighted_mean,b$Weighted_mean);close_to(a$SE,b$SE)
})
test('PSU identifiers nest within sampling strata',{
 p<-survey_prepare(d,'Score',cfg);stopifnot(p$info$Full_PSU==40,p$info$Full_strata==4,p$info$Full_design_df==36)
})
test('Explicit independent sampling units use ids ~1',{
 cf<-list(weight='Weight',psu='__individual__');a<-survey_descriptive(d,'Score',cf)$tables$Survey_summary;s<-survey::svymean(~Score,survey::svydesign(~1,weights=~Weight,data=d));close_to(a$SE,survey::SE(s));stopifnot(a$df==599)
})
test('FPC population and sampling fraction give identical SE',{
 dd<-d;dd$Population<-100;dd$Fraction<-.1
 a<-survey_descriptive(dd,'Score',c(cfg,list(fpc='Population',fpc_mode='population')))$tables$Survey_summary
 b<-survey_descriptive(dd,'Score',c(cfg,list(fpc='Fraction',fpc_mode='fraction')))$tables$Survey_summary
 close_to(a$SE,b$SE);s<-survey::svymean(~Score,survey::svydesign(~PSU,strata=~Stratum,weights=~Weight,fpc=~Population,data=dd,nest=TRUE));close_to(a$SE,survey::SE(s))
 dd$Fraction<-1;a<-survey_descriptive(dd,'Score',c(cfg,list(fpc='Fraction',fpc_mode='fraction')))$tables$Survey_summary;close_to(a$SE,0)
})
test('Domain inference subsets original survey design',{
 dd<-d;dd$Domain<-factor(ifelse(dd$PSU<=5&dd$Sample_ID%%2==0,'Target','Other'));ds<-survey::svydesign(~PSU,strata=~Stratum,weights=~Weight,data=dd,nest=TRUE)
 a<-survey_descriptive(dd,'Score',c(cfg,list(domain='Domain',domain_values='Target')))$tables$Survey_summary;s<-survey::svymean(~Score,subset(ds,Domain=='Target'));close_to(a$SE,survey::SE(s));close_to(a$Weighted_mean,coef(s))
 raw<-survey::svydesign(~PSU,strata=~Stratum,weights=~Weight,data=dd[dd$Domain=='Target',],nest=TRUE);stopifnot(abs(survey::SE(s)-survey::SE(survey::svymean(~Score,raw)))>1e-8)
})
test('Missing, nonpositive and infinite design weights are rejected',{
 for(v in c(NA,0,-1,Inf)){dd<-d;dd$Weight[1]<-v;fails(survey_descriptive(dd,'Score',cfg))}
 dd<-d;dd$PSU[1]<-NA;fails(survey_descriptive(dd,'Score',cfg),'設計欄位')
})
test('Design role, duplicate sample ID and invalid FPC guards',{
 fails(survey_prepare(d,'Score',list(weight='Weight',psu='Weight')))
 dd<-d;dd$Sample_ID[1]<-dd$Sample_ID[2];fails(survey_prepare(dd,'Score',c(cfg,list(sample_id='Sample_ID'))),'樣本 ID 重複')
 dd<-d;dd$FPC<-5;fails(survey_prepare(dd,'Score',c(cfg,list(fpc='FPC',fpc_mode='population'))))
 dd$FPC<-1.1;fails(survey_prepare(dd,'Score',c(cfg,list(fpc='FPC',fpc_mode='fraction'))))
 dd$FPC<-100;dd$FPC[1]<-101;fails(survey_prepare(dd,'Score',c(cfg,list(fpc='FPC',fpc_mode='population'))))
})
test('Lonely PSU policy is explicit and options are restored',{
 dd<-d[as.character(d$Stratum)!='1'|d$PSU==1,];old<-getOption('survey.lonely.psu');fails(survey_descriptive(dd,'Score',cfg))
 stopifnot(identical(old,getOption('survey.lonely.psu')))
 for(pol in c('adjust','average')){a<-survey_descriptive(dd,'Score',c(cfg,list(lonely=pol)));stopifnot(is.finite(a$tables$Survey_summary$SE),identical(old,getOption('survey.lonely.psu')))}
})
test('Continuous Wald F and Rao-Scott F match primary package',{
 a<-survey_descriptive(d,c('Score','Binary'),cfg,'Group',TRUE,TRUE,'bonferroni');tt<-a$tables$Group_tests
 s<-survey::regTermTest(survey::svyglm(Score~Group,des),~Group,method='Wald');c<-survey::svychisq(~Binary+Group,des,statistic='F')
 close_to(tt$p_value,c(as.numeric(s$p),c$p.value));close_to(tt$p_Bonferroni,p.adjust(tt$p_value,'bonferroni',n=2));stopifnot(a$tables$Multiplicity$Planned_tests==2)
})
test('Failed group tests remain in planned multiplicity family',{
 dd<-d;dd$Constant<-factor(rep('Only',600));a<-survey_descriptive(dd,c('Score','Constant'),cfg,'Group',TRUE,TRUE,'bonferroni')
 tt<-a$tables$Group_tests;stopifnot(nrow(tt)==2,is.na(tt$p_value[2]),tt$Status[2]!='OK');close_to(tt$p_Bonferroni[1],min(1,2*tt$p_value[1]))
})
check_glm<-function(a,f,label){tt<-a$tables$Coefficients;close_to(if(label=='Beta')tt[[label]]else log(tt[[label]]),coef(f));close_to(tt$SE_link,sqrt(diag(vcov(f))));close_to(tt$p_value,coef(summary(f))[,4]);close_to(if(label=='Beta')tt$CI_low else log(tt$CI_low),coef(f)-qt(.975,f$df.residual)*sqrt(diag(vcov(f))))}
test('Weighted linear coefficients, SE, t p and CI match svyglm',{
 a<-survey_regression(d,'Score',c('Group','Age'),cfg);f<-survey::svyglm(Score~Group+Age,des);check_glm(a,f,'Beta')
})
test('Survey Logistic OR, t inference and reversed event',{
 dd<-des;dd$variables$Binary<-as.integer(d$Binary=='1');a<-survey_regression(d,'Binary',c('Group','Age'),cfg,'logistic',event='1');f<-survey::svyglm(Binary~Group+Age,dd,family=quasibinomial());check_glm(a,f,'OR')
 b<-survey_regression(d,'Binary',c('Group','Age'),cfg,'logistic',event='0');close_to(a$tables$Coefficients$OR,1/b$tables$Coefficients$OR)
})
test('Modified Poisson design SE and PR versus RR labels',{
 dd<-des;dd$variables$Binary<-as.integer(d$Binary=='1');a<-survey_regression(d,'Binary',c('Group','Age'),cfg,'modified_poisson',event='1');f<-survey::svyglm(Binary~Group+Age,dd,family=quasipoisson());check_glm(a,f,'PR')
 b<-survey_regression(d,'Binary',c('Group','Age'),cfg,'modified_poisson',design='cohort',event='1');close_to(a$tables$Coefficients$PR,b$tables$Coefficients$RR)
})
test('Survey count offset produces IRR and matching design SE',{
 a<-survey_regression(d,'Count',c('Group','Age'),cfg,'poisson',offsetvar='Time');f<-survey::svyglm(Count~Group+Age+offset(log(Time)),des,family=quasipoisson());check_glm(a,f,'IRR')
})
test('Reference levels and joint interaction test are respected',{
 dd<-d;attr(dd$Group,'reference')<-'1';a<-survey_regression(dd,'Score',c('Group','Age'),cfg);b<-survey_regression(d,'Score',c('Group','Age'),cfg);close_to(a$tables$Coefficients$Beta[2],-b$tables$Coefficients$Beta[2]);stopifnot(a$publication$data[2,2]=='Ref')
 a<-survey_regression(d,'Score',c('Group','Age'),cfg,inter=c('Group','Age'));f<-survey::svyglm(Score~Group+Age+Group:Age,des);s<-survey::regTermTest(f,~Group:Age,method='Wald');close_to(a$tables$Interaction_Wald$p_value,as.numeric(s$p))
})
test('Missing analysis rows preserve design and report exclusions',{
 dd<-d;dd$Age[1:20]<-NA;a<-survey_regression(dd,'Score',c('Group','Age'),cfg);ds<-survey::svydesign(~PSU,strata=~Stratum,weights=~Weight,data=dd,nest=TRUE);f<-survey::svyglm(Score~Group+Age,subset(ds,!is.na(Age)));check_glm(a,f,'Beta');stopifnot(a$tables$Survey_design$Excluded_missing_rows==20)
})
test('Nonfinite data, singular models and wrong outcome guards',{
 dd<-d;dd$Score[1]<-Inf;fails(survey_descriptive(dd,'Score',cfg),'非有限');fails(survey_regression(dd,'Score',c('Age'),cfg),'非有限')
 fails(survey_regression(d,'Score',c('Weight'),cfg),'抽樣設計');fails(survey_regression(d,'Score',c('Age'),cfg,'poisson'))
 fails(survey_regression(d,'Binary',c('Age'),cfg,'modified_poisson',design='casecontrol',event='1'))
 dd<-d;dd$Age2<-dd$Age;fails(survey_regression(dd,'Score',c('Age','Age2'),cfg))
})
test('Survey Cox HR, design SE and Wald z match svycoxph',{
 dd<-des;dd$variables$Event<-as.integer(d$Event=='1');Surv<-survival::Surv;f<-survey::svycoxph(Surv(Time,Event)~Group+Age,dd)
 a<-survey_cox(d,'Time','Event','1',c('Group','Age'),cfg);close_to(log(a$tables$Cox$HR),coef(f));close_to(a$tables$Cox$SE_link,sqrt(diag(vcov(f))));close_to(a$tables$Cox$p_value,2*pnorm(-abs(coef(f)/sqrt(diag(vcov(f))))))
 dd<-d;dd$Time[1]<-0;fails(survey_cox(dd,'Time','Event','1',c('Group','Age'),cfg))
})
test('Survey structure gates keep methods separate',{
 stopifnot(identical(available_modules('cross','survey'),c('survey_desc','survey_regression')),'survey_cox'%in%available_modules('cohort','survey'),!'gee'%in%available_modules('cohort','survey'))
})
test('Provenance reports actual runtime and persists in individual exports',{
 a<-analysis_provenance(survey_regression(d,'Score',c('Group','Age'),cfg),'survey_regression',list(design='cross',structure='survey',svy_model='gaussian'))
 stopifnot(a$provenance$metadata$Value[1]==R.version.string,a$provenance$packages$Version[a$provenance$packages$Package=='survey']==as.character(packageVersion('survey')))
 html<-as.character(provenance_ui(a$provenance));stopifnot(grepl(R.version.string,html,fixed=TRUE),grepl('Wald t',html,fixed=TRUE))
 piece<-result_piece(a,'publication');export_docx(piece,'test-output/Survey_model.docx');export_xlsx(piece,'test-output/Survey_model.xlsx');saveRDS(a,'test-output/Survey_model.rds')
 sheets<-readxl::excel_sheets('test-output/Survey_model.xlsx');stopifnot(all(c('Calculation_info','Packages','ModelTable')%in%sheets));px<-readxl::read_excel('test-output/Survey_model.xlsx',sheet='Calculation_info');stopifnot(any(grepl(R.version.string,px$Details,fixed=TRUE)))
 td<-tempfile();dir.create(td);unzip('test-output/Survey_model.docx',files='word/document.xml',exdir=td);xml<-paste(readLines(file.path(td,'word/document.xml'),warn=FALSE),collapse='');stopifnot(grepl(R.version.string,xml,fixed=TRUE),grepl('svyglm',xml,fixed=TRUE))
})
test('All module provenance mappings produce meaningful result headers',{
 for(m in unname(modules)){a<-analysis_provenance(out(m),m,list(design='cohort',structure='independent',model='linear',corr_model='gaussian'));stopifnot(nrow(a$provenance$packages)>0,length(a$provenance$logic)>=2,!any(a$provenance$packages$Version=='無法確認'));as.character(provenance_ui(a$provenance))}
})
test('Shiny survey methods and frozen provenance end-to-end',{
 shiny::testServer(server,{
  ingest(d,'Synthetic survey validation');session$setInputs(design='cross',structure='survey',purpose='describe',module='survey_desc',svy_weight='Weight',svy_psu='PSU',svy_strata='Stratum',svy_fpc_mode='none',svy_lonely='fail',svy_vars=c('Score','Binary'),svy_group='Group',svy_cont_test=TRUE,svy_cat_test=TRUE,svy_p_adjust='bonferroni',run=1)
  if(result()$title=='分析未完成')stop(paste(result()$notes,collapse='; '));stopifnot(!is.null(result()$provenance),grepl('計算方法與版本',output$results$html,fixed=TRUE))
  session$setInputs(purpose='association',module='survey_regression',svy_kind='continuous',svy_model='gaussian',svy_y='Score',svy_x=c('Group','Age'),svy_inter=character(),run=2)
  if(result()$title=='分析未完成')stop(paste(result()$notes,collapse='; '));p0<-result()$provenance
  session$setInputs(svy_model='logistic');stopifnot(identical(p0,result()$provenance));session$setInputs(covariate_bulk=list(id='svy_x',action='all'))
  session$setInputs(design='cohort',module='survey_cox',svy_time='Time',svy_y='Event',svy_event='1',svy_x=c('Group','Age'),run=3)
  if(result()$title=='分析未完成')stop(paste(result()$notes,collapse='; '));stopifnot('Cox'%in%names(result()$tables))
 })
})
res<-do.call(rbind,records);write.csv(res,'test-output/複雜抽樣與計算紀錄驗證.csv',row.names=FALSE,fileEncoding='UTF-8');print(res[,1:2]);if(any(res$Status!='PASS'))stop('Survey tests failed')
