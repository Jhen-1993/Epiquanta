# Reliability follows the user's prespecified raw-score / Pearson protocol.
reliability_groups<-function(items,spec=''){
 need(length(items)>=2,'請先選擇至少兩個題項。')
 if(!nzchar(trimws(spec)))return(setNames(list(items),'所選題組'))
 lines<-trimws(strsplit(spec,'\n',fixed=TRUE)[[1]]);lines<-lines[nzchar(lines)];groups<-list()
 for(line in lines){a<-strsplit(line,'=',fixed=TRUE)[[1]];need(length(a)==2,'每行題組請使用「構面名稱 = 題項1, 題項2」格式。');name<-trimws(a[1]);v<-trimws(strsplit(a[2],',',fixed=TRUE)[[1]])
  need(nzchar(name)&&!name%in%names(groups),'題組名稱不可空白或重複。');need(length(v)>=2&&!anyDuplicated(v)&&all(v%in%items),paste(name,'需至少兩題，題項不可重複且須先納入所選題項。'));groups[[name]]<-v
 }
 need(setequal(unique(unlist(groups,use.names=FALSE)),items),'所選題項需全部分配到題組；不分析的題項請從選取清單移除。')
 groups
}
reliability_labels<-function(items,spec=''){
 labels<-setNames(items,items);if(!nzchar(trimws(spec)))return(labels)
 lines<-trimws(strsplit(spec,'\n',fixed=TRUE)[[1]]);lines<-lines[nzchar(lines)];seen<-character()
 for(line in lines){pos<-regexpr('=',line,fixed=TRUE)[1];need(pos>1,'題目簡稱請使用「欄位 = 題目簡稱」格式。');key<-trimws(substr(line,1,pos-1));value<-trimws(substring(line,pos+1));need(key%in%items&&!key%in%seen&&nzchar(value),'題目簡稱欄位需已選取、不可重複，文字不可空白。');labels[key]<-value;seen<-c(seen,key)}
 labels
}
reliability_coefficients<-function(x){
 k<-ncol(x);C<-cov(x);den<-sum(C);raw<-if(is.finite(den)&&den>0)k/(k-1)*(1-sum(diag(C))/den)else NA_real_
 # A raw alpha can remain defined even if a bootstrap item is constant.
 r<-if(k==2&&all(diag(C)>0))C[1,2]/sqrt(C[1,1]*C[2,2])else NA_real_
 sb<-if(is.finite(r)&&1+r>0)2*r/(1+r)else NA_real_
 c(Raw_alpha=raw,if(k==2)c(Spearman_Brown=sb))
}
reliability_bootstrap<-function(x,B=5000,seed=20260910,progress=function(...)NULL){
 need(B>=20&&B<=20000&&B==floor(B),'Bootstrap 次數需為 20–20,000 的整數。');need(length(seed)==1&&is.finite(seed)&&seed>=1&&seed<=.Machine$integer.max&&seed==floor(seed),'請指定有效的固定整數種子。')
 RNGkind('Mersenne-Twister','Inversion','Rejection');set.seed(seed);point<-reliability_coefficients(x);replicates<-matrix(NA_real_,B,length(point),dimnames=list(NULL,names(point)))
 for(b in seq_len(B)){ix<-sample.int(nrow(x),nrow(x),replace=TRUE);replicates[b,]<-reliability_coefficients(x[ix,,drop=FALSE]);if(b%%50==0||b==B)progress(b/B)}
 valid<-colSums(is.finite(replicates));audit<-data.frame(Coefficient=names(point),Estimate=unname(point),Requested=B,Valid=valid,Invalid=B-valid,Valid_fraction=valid/B,Seed=seed,Subjects=nrow(x),row.names=NULL)
 if(any(valid/B<.95))stop(paste0('Bootstrap 有效比例低於 95%，停止本次信度分析；',paste(paste0(audit$Coefficient,'：',audit$Valid,'/',B,'（',sprintf('%.1f',100*audit$Valid_fraction),'%），seed=',seed),collapse='；')),call.=FALSE)
 ci<-vapply(seq_along(point),function(j)quantile(replicates[is.finite(replicates[,j]),j],c(.025,.975),names=FALSE,type=7),numeric(2));audit$CI_low<-ci[1,];audit$CI_high<-ci[2,]
 list(audit=audit,replicates=replicates)
}
reliability_analysis<-function(d,items,reverse=character(),lower=NULL,upper=NULL,group_spec='',label_spec='',seed=20260910,subject_id='',progress=function(...)NULL,B=5000){
 groups<-reliability_groups(items,group_spec);labels<-reliability_labels(items,label_spec);need(all(items%in%names(d)),'題項欄位不存在。');need(all(reverse%in%items),'反向題必須包含於所選題項。')
 if(nzchar(subject_id)){need(subject_id%in%names(d)&&!subject_id%in%items,'受試者 ID 需為不同於題項的有效欄位。');need(!anyNA(d[[subject_id]])&&!anyDuplicated(d[[subject_id]]),'受試者 ID 有缺失或重複。此信度入口要求每人一列。')}
 if(length(reverse))need(length(lower)==1&&length(upper)==1&&is.finite(lower)&&is.finite(upper)&&lower<upper,'反向題需指定理論最低與最高分。')
 need(length(seed)==1&&is.finite(seed)&&seed>=1&&seed+length(groups)-1<=.Machine$integer.max&&seed==floor(seed),'固定亂數種子超出可用範圍。')
 summaries<-itemrows<-audits<-list();reps<-list();notes<-character()
 for(g in seq_along(groups)){
  name<-names(groups)[g];v<-groups[[g]];k<-length(v);z<-d[,v,drop=FALSE]
  for(it in v){old<-z[[it]];need(is.numeric(old)||is.ordered(old)||(is.factor(old)&&all(na.omit(as.character(old))%in%c('0','1'))),paste(it,'需為連續數值、已確認計分的順序題或 0／1 題；名目編碼不可直接視為量尺分數。'));z[[it]]<-num(old);need(!any(!is.na(old)&!is.finite(z[[it]])),paste(name,it,'含非數值或非有限作答；請確認編碼。'))}
  z<-z[complete.cases(z),,drop=FALSE];N<-nrow(z);need(N>=3,paste(name,'完整作答受試者不足 3 人。'))
  for(it in intersect(v,reverse)){need(all(z[[it]]>=lower&z[[it]]<=upper),paste(it,'超出指定理論量尺。'));z[[it]]<-lower+upper-z[[it]]}
  x<-as.matrix(z);C<-cov(x);need(all(diag(C)>0),paste(name,'含零變異題項，無法定義標準化 α／CITC；請確認資料。'))
  R<-cor(x,method='pearson');den<-sum(R);std<-if(is.finite(den)&&den>0)k/(k-1)*(1-k/den)else NA_real_;pt<-reliability_coefficients(x)
  need(all(is.finite(pt))&&is.finite(std),paste(name,'原始信度係數不可定義；請檢查總分變異與完全負相關。'))
  bt<-tryCatch(reliability_bootstrap(x,B,seed+g-1,function(p)progress((g-1+p)/length(groups))),error=function(e)stop(paste(name,conditionMessage(e)),call.=FALSE));aa<-bt$audit;aa$Scale<-name;audits[[g]]<-aa;reps[[name]]<-bt$replicates
  rawrow<-aa[aa$Coefficient=='Raw_alpha',];sbrow<-aa[aa$Coefficient=='Spearman_Brown',]
  summaries[[g]]<-data.frame(Scale=name,N=N,Excluded_rows=nrow(d)-N,Items=k,Score_type=if(all(x%in%c(0,1)))'Binary 0/1; raw alpha = KR-20'else if(any(vapply(d[v],is.ordered,logical(1))))'Observed scored items; Pearson (not ordinal alpha)'else'Continuous scored items; Pearson',Alpha=unname(pt['Raw_alpha']),Alpha_low=rawrow$CI_low,Alpha_high=rawrow$CI_high,Standardized_alpha=std,Inter_item_r=if(k==2)R[1,2]else NA_real_,Spearman_Brown=if(k==2)pt['Spearman_Brown']else NA_real_,SB_low=if(k==2)sbrow$CI_low else NA_real_,SB_high=if(k==2)sbrow$CI_high else NA_real_,Seed=seed+g-1,row.names=NULL)
  itemrows[[g]]<-data.frame(Scale=name,Question=v,Item=unname(labels[v]),Reverse=v%in%reverse,N=N,Mean=colMeans(x),SD=sqrt(diag(C)),CITC=if(k>=3)vapply(seq_len(k),function(j){rest<-rowSums(x[,-j,drop=FALSE]);if(var(rest)>0)cor(x[,j],rest)else NA_real_},numeric(1))else rep(NA_real_,k),Alpha_if_deleted=if(k>=3)vapply(seq_len(k),function(j)unname(reliability_coefficients(x[,-j,drop=FALSE])['Raw_alpha']),numeric(1))else rep(NA_real_,k),row.names=NULL)
  if(any(pt<0)||std<0)notes<-c(notes,paste(name,'出現負信度係數；已保留可定義的負值，請確認計分方向與題項一致性。'))
 }
 tabs<-list(Reliability=dplyr::bind_rows(summaries),Items=dplyr::bind_rows(itemrows),Bootstrap=dplyr::bind_rows(audits))
 ans<-out('信度分析：題組與逐題指標',tabs,notes=c('以每題組完整作答受試者為分析樣本，每人一列；題組之間可有不同 N。反向題按理論上下界反向後，使用原量尺分數。','原始 Cronbach α 由共變異數矩陣計算；標準化 α 由 Pearson 相關矩陣計算。3 題以上列 Corrected Item–Total Correlation（該題與其餘題目總分之 Pearson 相關）及刪題後原始 α。','兩題僅列 Pearson 題間 r 與 Spearman–Brown＝2r／(1+r)；原始 α 保留比較，不計算 CITC 或刪單題 α。兩題標準化 α 與 Spearman–Brown 相同。',paste0('原始 α／兩題 Spearman–Brown 使用受試者整組作答有放回 Bootstrap ',B,' 次；每次抽取原題組 N 人。僅排除不可定義值，負值保留；95% CI 為有效估計值 2.5／97.5 百分位（R quantile type=7）。每一係數有效比例需至少 95%，否則停止整次分析。'),'每題組種子＝基礎種子＋題組順序−1，實際值與各係數有效／無效次數見 Bootstrap 表；紀錄保存題組順序與 RNG 設定。','α 衡量內部一致性，不直接證明單一構面；CITC／刪題 α 需結合理論及內容效度，不能只為提高 α 自動刪題。',notes))
 ans$bootstrap_replicates<-reps;ans$reliability_spec<-list(groups=groups,labels=labels,reverse=reverse,lower=lower,upper=upper,missing='Complete cases within each scale',B=B,seed=seed,subject_id=subject_id,RNGkind=RNGkind())
 ans$publication<-reliability_publication(tabs$Reliability,tabs$Items);ans
}
