efa_method<-function(d,items,requested='auto'){
 score<-vapply(d[items],function(v)is.numeric(v)||is.ordered(v)||(is.factor(v)&&all(na.omit(as.character(v))%in%c('0','1'))),logical(1))
 need(all(score),'EFA 題項需為連續數值、已確認計分的順序題或 0／1 題；請先修改未計分的名目型態。')
 if(requested!='auto'){need(requested%in%c('poly','pearson','spearman'),'不支援此相關矩陣。');return(requested)}
 ordinal<-vapply(d[items],function(v)is.ordered(v)||all(na.omit(as.character(v))%in%c('0','1')),logical(1))
 if(all(ordinal))return('poly')
 if(!any(ordinal))return('pearson')
 stop('此題組混合連續與順序題：請先確認計分與模型，再明確選擇 Pearson 或 Spearman；自動模式不將混合型態直接套用純 Polychoric。',call.=FALSE)
}
efa_grouped<-function(d,items,reverse=character(),lower=NULL,upper=NULL,method='auto',B=500,seed=20260910,nfactor=0,progress=function(...)NULL,group_spec='',label_spec=''){
 groups<-reliability_groups(items,group_spec);labels<-reliability_labels(items,label_spec);results<-list();meta<-list()
 for(g in seq_along(groups)){
  name<-names(groups)[g];v<-groups[[g]];need(length(v)>=3,paste(name,'EFA 需至少三題；兩題可使用信度分析。'));m<-efa_method(d,v,method)
  a<-efa_analysis(d,v,intersect(reverse,v),lower,upper,m,B,seed+g-1,nfactor,function(p)progress((g-1+p)/length(groups)))
  if('Loadings'%in%names(a$tables)){
   R<-as.matrix(a$tables$Correlation[,-1,drop=FALSE]);L<-as.matrix(a$tables$Loadings[,-1,drop=FALSE]);Phi<-if('Factor_correlations'%in%names(a$tables))as.matrix(a$tables$Factor_correlations[,-1,drop=FALSE])else diag(ncol(L));res<-R-L%*%Phi%*%t(L)
   rms<-sqrt(mean(res[lower.tri(res)]^2));fit_n<-ncol(L)
  }else{rms<-NA_real_;fit_n<-0L}
  N<-sum(complete.cases(d[,v,drop=FALSE]));meta[[g]]<-data.frame(Scale=name,N=N,Excluded_rows=nrow(d)-N,Correlation=m,Requested_factors=nfactor,PA_suggested=if('PA_status'%in%names(a$tables))a$tables$PA_status$Factors_SMC else NA_integer_,Fitted_factors=fit_n,Off_diagonal_RMS=rms,Seed=seed+g-1,Status=if(fit_n)'Estimated; review fit diagnostics'else'No interpretable factor solution')
  a$items<-v;results[[name]]<-a
 }
 tables<-list();plots<-list();notes<-character()
 for(g in seq_along(results)){a<-results[[g]];name<-names(results)[g]
  for(t in names(a$tables)){z<-a$tables[[t]];if(length(results)>1)z<-cbind(Scale=name,z);tables[[t]]<-dplyr::bind_rows(tables[[t]],z)}
  for(p in names(a$plots))plots[[paste(name,p,sep=' / ')]]<-a$plots[[p]]
  notes<-c(notes,paste0(name,'：',a$notes))
 }
 tables$EFA_summary<-dplyr::bind_rows(meta);ans<-out(if(any(tables$EFA_summary$Fitted_factors>0))'探索性因素分析 EFA'else'EFA 診斷（未產生可解讀的負荷量）',tables,plots,notes=c(notes,'Off-diagonal RMS＝sqrt(mean((R−LΦLᵀ)² 的非對角元素))，不以模型自由度作分母。多因素表保留全部 pattern loadings；不以單一最大負荷代替。','單因素 Spearman 敏感度僅對齊整體正負號；多因素敏感度保留獨立解與原因素順序，不宣稱各欄已對齊主分析。'))
 ans$efa_spec<-list(groups=groups,labels=labels,methods=setNames(tables$EFA_summary$Correlation,tables$EFA_summary$Scale),seed=seed,B=B)
 ans$publication<-efa_publication(results,tables$EFA_summary,labels,reverse);ans
}
efa_publication<-function(results,summary,labels,reverse){
 maxmain<-max(1,vapply(results,function(a)if('Loadings'%in%names(a$tables))ncol(a$tables$Loadings)-1L else 0L,integer(1)))
 maxsens<-max(0,vapply(results,function(a)if('Spearman_loadings'%in%names(a$tables))ncol(a$tables$Spearman_loadings)-1L else 0L,integer(1)))
 same<-unique(summary$Correlation);prefix<-if(length(same)==1)switch(same,poly='Polychoric',pearson='Pearson',spearman='Spearman')else'Primary'
 columns<-c('Question','Item (abbreviated)','n','KMO','Item MSA','PA suggested factors','Correlation',paste(prefix,'loading F',seq_len(maxmain)), 'Communality',if(maxsens)paste('Spearman sensitivity F',seq_len(maxsens)),'Off-diagonal RMS')
 blocks<-list()
 for(g in seq_along(results)){
  a<-results[[g]];s<-summary[g,];v<-a$items;n<-length(v);pd<-as.data.frame(matrix('—',n,length(columns)),stringsAsFactors=FALSE);names(pd)<-columns
  pd[[1]]<-v;pd[[2]]<-paste0(unname(labels[v]),ifelse(v%in%reverse,' (R)',''));pd[[3]]<-as.character(s$N);pd[[6]]<-ifelse(is.na(s$PA_suggested),'—',as.character(s$PA_suggested));pd[[7]]<-switch(s$Correlation,poly='Polychoric ML',pearson='Pearson',spearman='Spearman');pd[[length(columns)]]<-measurement_number(s$Off_diagonal_RMS)
  if('KMO'%in%names(a$tables)){km<-a$tables$KMO;pd[[4]]<-measurement_number(km$MSA[km$Item=='Overall']);pd[[5]]<-measurement_number(km$MSA[match(v,km$Item)])}
  if('Loadings'%in%names(a$tables)){ld<-a$tables$Loadings;L<-as.matrix(ld[match(v,ld$Term),-1,drop=FALSE]);for(j in seq_len(ncol(L)))pd[[7+j]]<-measurement_number(L[,j]);pd[['Communality']]<-measurement_number(a$tables$Communalities$h2[match(v,a$tables$Communalities$Item)])}
  if('Spearman_loadings'%in%names(a$tables)){ld<-a$tables$Spearman_loadings;S<-as.matrix(ld[match(v,ld$Term),-1,drop=FALSE]);if(exists('L',inherits=FALSE)&&ncol(L)==1&&ncol(S)==1&&sum(L*S)<0)S<--S;for(j in seq_len(ncol(S)))pd[[8+maxmain+j]]<-measurement_number(S[,j])}
  blocks[[s$Scale]]<-pd;if(exists('L',inherits=FALSE))rm(L)
 }
 measurement_publication(blocks,columns,c(3,4,6,7,length(columns)),'EFA 建構效度摘要','依實際題型與所選方法計算；PA 為 SMC 平行分析建議數，實際萃取數另見 EFA_summary。主分析為 Spearman 時不重複列敏感度。— 表示不適用或未產生可解讀估計；完整診斷與限制見結果。')
}
