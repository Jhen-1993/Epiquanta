# Unweighted absolute standardized differences; no hypothesis test or sample-size scaling.
smd_pair<-function(a,b){
 numeric<-is.numeric(a)&&is.numeric(b);a<-a[if(numeric)is.finite(a)else!is.na(a)];b<-b[if(numeric)is.finite(b)else!is.na(b)]
 need(length(a)>0&&length(b)>0,'至少一組沒有可用資料。')
 if(numeric){need(length(a)>=2&&length(b)>=2,'連續 SMD 每組至少需兩個有效值。');den<-sqrt((var(a)+var(b))/2);delta<-abs(mean(a)-mean(b));value<-if(den==0)if(delta==0)0 else Inf else delta/den;method<-'Absolute mean difference / sqrt((SD1² + SD2²)/2)'}
 else{
  lev<-sort(unique(c(as.character(a),as.character(b))));need(length(lev)<=100,'類別超過 100 級，未估計廣義 SMD；請檢查是否為識別碼。')
  if(length(lev)==1)return(list(SMD=0,N1=length(a),N2=length(b),Method='Categorical: identical constant level',Status='兩組同一常數層級'))
  p<-as.numeric(prop.table(table(factor(a,levels=lev))));q<-as.numeric(prop.table(table(factor(b,levels=lev))))
  # Use k-1 indicators with negative multinomial off-diagonal covariances.
  p<-p[-1];q<-q[-1];V<-((diag(p,nrow=length(p))-tcrossprod(p))+(diag(q,nrow=length(q))-tcrossprod(q)))/2
  delta<-p-q;e<-eigen(V,symmetric=TRUE);proj<-drop(crossprod(e$vectors,delta));ok<-e$values>max(1,max(e$values))*1e-12
  value<-if(any(abs(proj[!ok])>1e-10))Inf else sqrt(max(0,sum(proj[ok]^2/e$values[ok])))
  method<-if(length(lev)==2)'Absolute proportion difference / sqrt((p1(1-p1)+p2(1-p2))/2)'else'Generalized multinomial SMD: square root Mahalanobis distance'
 }
 list(SMD=value,N1=length(a),N2=length(b),Method=method,Status=if(is.infinite(value))'組間分離且合併變異為零；SMD 無有限值'else'')
}
add_table1_smd<-function(ans,d,vars,group){
 if(!nzchar(group))return(ans)
 g<-droplevels(factor(d[[group]]));lev<-levels(g)
 if(length(lev)<2)return(ans)
 if(length(lev)>20){ans$notes<-c(ans$notes,'分組超過 20 組，未計算所有成對 SMD；請先定義較少比較組。');return(ans)}
 pairs<-combn(lev,2,simplify=FALSE);rows<-list()
 for(v in setdiff(vars,group))for(pair in pairs){
  a<-d[[v]][!is.na(g)&g==pair[1]];b<-d[[v]][!is.na(g)&g==pair[2]]
  r<-tryCatch(smd_pair(a,b),error=function(e)list(SMD=NA_real_,N1=sum(!is.na(a)),N2=sum(!is.na(b)),Method='',Status=conditionMessage(e)))
  rows[[length(rows)+1]]<-data.frame(Variable=v,Group1=pair[1],Group2=pair[2],r,check.names=FALSE)
 }
 if(!length(rows))return(ans)
 tt<-dplyr::bind_rows(rows);ans$tables$SMD_pairs<-tt;column<-if(length(lev)==2)'|SMD|'else'Max |SMD|';ans$publication$data[[column]]<-''
 for(v in unique(tt$Variable)){
  values<-tt$SMD[tt$Variable==v];value<-if(anyNA(values))NA_real_ else max(values)
  j<-which(ans$publication$data$Characteristic==v&ans$publication$kinds%in%c('continuous','heading'))
  ans$publication$data[j,column]<-if(is.na(value))'—'else if(is.infinite(value))'∞'else sprintf('%.3f',value)
 }
 note<-paste('SMD 為未加權絕對差異，不含样本數因子。連續型使用平均差／兩組變異平均的平方根；即使摘要選 median/IQR，SMD 仍衡量平均數差異。二元型使用比例與 Bernoulli 變異；多類別型使用廣義 Mahalanobis SMD。',if(length(lev)>2)'主表為所有成對 |SMD| 最大值，不是平均值；任一配對無法計算時主表留空，完整結果列於 SMD_pairs。','每個變項使用該配對組的非缺失資料；SMD 不經 Bonferroni 校正。0.1 僅是常見參考，不能證明無混雜或分布完全平衡。')
 ans$notes<-c(ans$notes,note);ans$publication$note<-paste(publication_note(ans$publication),note);ans
}
