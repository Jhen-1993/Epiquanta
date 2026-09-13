# Diagnostic tests assess specific features; a large p value does not prove an assumption.
shapiro_row<-function(x,label,sample_large=FALSE,seed=20260910){
 need(is.numeric(x),'常態檢查僅適用數值變項。');a<-x[is.finite(x)];n<-length(a);tested<-0L;w<-p<-NA_real_
 status<-if(n<3)'有效值不足 3 筆'else if(length(unique(a))<2)'常數欄位，無法檢定'else if(n>5000&&!sample_large)'N > 5000；未執行 Shapiro–Wilk，請看圖或明確勾選抽樣'else '完整有效樣本'
 if(n>=3&&length(unique(a))>1&&(n<=5000||sample_large)){
  if(n>5000){set.seed(seed);a<-sample(a,5000);status<-paste('隨機抽取 5000 筆；seed',seed,'；僅代表此子樣本')}
  s<-shapiro.test(a);tested<-length(a);w<-unname(s$statistic);p<-s$p.value
 }
 data.frame(Variable=label,N=n,Excluded_nonfinite=length(x)-n,Tested_N=tested,W=w,p_value=p,Status=status)
}
normality_analysis<-function(d,vars,group='',sample_large=FALSE,seed=20260910){
 need(length(vars)>0&&all(vars%in%names(d))&&all(vapply(d[vars],is.numeric,logical(1))),'請選擇要檢查的連續數值變項。')
 groups<-if(nzchar(group)){need(is.factor(d[[group]]),'分組需為類別。');split(seq_len(nrow(d)),d[[group]],drop=TRUE)}else list(Overall=seq_len(nrow(d)))
 need(length(groups)<=40,'分組超過 40 組；請改用較少層級的分組。');tabs<-plots<-list();k<-0L
 for(v in vars)for(g in names(groups)){
  k<-k+1L;x<-d[[v]][groups[[g]]];a<-x[is.finite(x)];label<-paste(v,g,sep=' / ')
  tabs[[k]]<-shapiro_row(x,label,sample_large,seed+k-1L)
  if(length(a)>=3&&length(unique(a))>1){
   # Plot only a reproducible sample when large; test sample status stays explicit above.
   n<-length(a);if(n>5000){set.seed(seed+k-1L);a<-sample(a,5000)}
   pd<-data.frame(Value=a);sub<-sprintf('有效 N=%s；繪圖 n=%s%s',n,length(a),if(n>5000)'（固定種子隨機子樣本）'else'')
   plots[[paste0('QQ_',k)]]<-ggplot2::ggplot(pd,ggplot2::aes(sample=Value))+ggplot2::stat_qq(alpha=.35)+ggplot2::stat_qq_line(color='#315bea')+ggplot2::labs(title=label,subtitle=sub,x='Theoretical normal quantile',y='Observed quantile')+ggplot2::theme_minimal()
   plots[[paste0('Histogram_',k)]]<-ggplot2::ggplot(pd,ggplot2::aes(Value))+ggplot2::geom_histogram(bins=35,fill='#315bea',color='white')+ggplot2::labs(title=label,subtitle=sub)+ggplot2::theme_minimal()
  }
 }
 out('連續變項分布與常態檢查',list(Normality=dplyr::bind_rows(tabs)),plots,c('Shapiro–Wilk：虛無假設為常態分布；p≥.05 不等於證明常態。大樣本易偵測很小的偏離，不以本檢定自動決定分析方法。','t／ANOVA 與線性模型關注組內誤差／條件殘差；Logistic、Poisson、Cox 不要求原始結果或共變項常態。分組缺失列不進入分組檢查；非有限值不參與檢定。'))
}
diagnostic_fit<-function(fit,kind,prefix='Model'){
 tabs<-plots<-list();notes<-character()
 add<-function(n,x)tabs[[paste(prefix,n,sep='_')]]<<-x
 if(inherits(fit,'lm')){
  mm<-model.matrix(fit);add('Design',data.frame(N=nrow(mm),Columns=ncol(mm),Rank=qr(mm)$rank,Condition_number=kappa(mm)))
  # VIF here is per design-matrix column, explicitly not a multi-df factor GVIF.
  xx<-mm[,colnames(mm)!='(Intercept)',drop=FALSE]
  if(ncol(xx)>1&&ncol(xx)<=100){v<-vapply(seq_len(ncol(xx)),function(j){yy<-xx[,j];den<-sum((yy-mean(yy))^2);if(den<=0)return(Inf);den/sum(lm.fit(cbind(1,xx[,-j,drop=FALSE]),yy)$residuals^2)},numeric(1));add('VIF_columns',data.frame(Column=colnames(xx),VIF=v))}
  n<-nobs(fit);idx<-seq_len(n);if(n>5000){set.seed(9201);idx<-sort(sample.int(n,5000))}
  res<-residuals(fit,type=if(inherits(fit,'glm'))'deviance'else'response');pd<-data.frame(Fitted=as.numeric(fitted(fit))[idx],Residual=as.numeric(res)[idx])
  plots[[paste0(prefix,'_Residuals')]]<-ggplot2::ggplot(pd,ggplot2::aes(Fitted,Residual))+ggplot2::geom_point(alpha=.25)+ggplot2::geom_hline(yintercept=0,lty=2)+ggplot2::labs(subtitle=sprintf('殘差圖 n=%d / N=%d；大樣本繪圖抽樣',length(idx),n))+ggplot2::theme_minimal()
  cd<-cooks.distance(fit);add('Influence',data.frame(N=n,Max_Cooks_D=max(cd,na.rm=TRUE),Above_4_over_N=sum(cd>4/n,na.rm=TRUE),Threshold=4/n))
  notes<-c(notes,'VIF 為設計矩陣各欄的診斷，不是多自由度因子的 GVIF；Cook 距離 4/N 僅供探索，不自動刪除個案。')
 }
 if(kind=='linear'){
  add('Residual_normality',shapiro_row(residuals(fit),'模型殘差（相關的估計殘差；p 值為近似診斷）'))
  bp<-lmtest::bptest(fit);add('Breusch_Pagan',data.frame(Statistic=unname(bp$statistic),df=unname(bp$parameter),p_value=bp$p.value))
  plots[[paste0(prefix,'_Residual_QQ')]]<-ggplot2::ggplot(pd,ggplot2::aes(sample=Residual))+ggplot2::stat_qq(alpha=.3)+ggplot2::stat_qq_line(color='#315bea')+ggplot2::theme_minimal()
  notes<-c(notes,'線性模型檢查殘差型態與異質變異；Breusch–Pagan（studentized）虛無假設為同質變異。HC3 可處理部分異質變異，但不能修正非線性或混雜。')
 }else if(kind%in%c('logistic','rr','poisson','negbin')){
  notes<-c(notes,'此廣義線性模型不要求原始變項或殘差常態；需評估連結函數、獨立性、模型設定及具影響力個案。')
  if(kind%in%c('poisson','negbin'))add('Dispersion',data.frame(Pearson_dispersion=sum(residuals(fit,type='pearson')^2)/df.residual(fit),Residual_df=df.residual(fit)))
  if(kind=='rr')notes<-c(notes,'二元 Modified Poisson 使用穩健變異數；不以 Poisson 等平均變異假設作為二元資料適合度檢定。')
  if(kind=='logistic')add('Probability_range',data.frame(Min=min(fitted(fit)),Max=max(fitted(fit))))
 }else if(kind%in%c('cox','ag')){
  ph<-survival::cox.zph(fit);add('PH_test',mtab(ph$table))
  pd<-dplyr::bind_rows(lapply(seq_len(ncol(ph$y)),function(j)data.frame(Time=ph$x,Residual=ph$y[,j],Term=colnames(ph$y)[j])))
  if(nrow(pd)>15000){set.seed(9201);pd<-pd[sample.int(nrow(pd),15000),]}
  plots[[paste0(prefix,'_Schoenfeld')]]<-ggplot2::ggplot(pd,ggplot2::aes(Time,Residual))+ggplot2::geom_point(alpha=.25)+ggplot2::facet_wrap(~Term,scales='free_y')+ggplot2::labs(x='Transformed time (KM)',y='Scaled Schoenfeld residual',subtitle='比例危險假設：查看係數是否隨時間變動；超過 15000 點時繪圖抽樣')+ggplot2::theme_minimal()
  notes<-c(notes,'Schoenfeld 檢定評估比例危險；不是設限獨立性的檢定。獨立設限、時間起點及時間函數需另由研究設計與殘差檢視。',if(kind=='ag')'AG 的 Schoenfeld 檢查屬模型式探索診斷，不是按受試者聚集的穩健 PH 檢定；事件相依結構仍需另評估。')
 }else if(kind=='ordinal')notes<-c(notes,'順序 Logistic：另列比例勝算的聯合 Wald 與各切點係數。原始變项常態不是必要假設。')
 else if(kind=='multinomial')notes<-c(notes,'多項 Logistic：需評估 IIA、充分樣本及模型識別；本版本未實作 IIA 檢定，不將收斂視為 IIA 成立。')
 else if(kind=='aft')notes<-c(notes,'AFT：需要所選存活時間分布與加速因子假設。本版本未提供 AFT 分布的正式適合度檢定；不以收斂代表假設成立。')
 out('模型診斷',tabs,plots,notes)
}
attach_diagnostics<-function(ans,fit,kind,prefix='Model'){
 dg<-tryCatch(diagnostic_fit(fit,kind,prefix),error=function(e)out('診斷',notes=paste('部分診斷無法完成：',conditionMessage(e))))
 ans$tables<-c(ans$tables,dg$tables);ans$plots<-c(ans$plots,dg$plots)
 ans$tables[[paste0(prefix,'_Assumptions')]]<-data.frame(Assessment=c(dg$notes,'獨立性、沒有未測量混雜、選擇機制與測量品質無法單由此資料的 p 值證明。'))
 ans
}
