# Diagram values come only from the fitted lavaan model; no template estimates.
sem_fit_summary<-function(fit){
 fm<-lavaan::fitMeasures(fit)
 first<-function(keys){k<-keys[keys%in%names(fm)&is.finite(fm[keys])];if(length(k))k[1]else NA_character_}
 keys<-c(Chi_square=first(c('chisq.scaled','chisq')),df=first(c('df.scaled','df')),p_value=first(c('pvalue.scaled','pvalue')),CFI=first(c('cfi.robust','cfi.scaled','cfi')),TLI=first(c('tli.robust','tli.scaled','tli')),RMSEA=first(c('rmsea.robust','rmsea.scaled','rmsea')),SRMR=first('srmr'))
 rm<-keys['RMSEA'];suffix<-if(is.na(rm))''else sub('^rmsea','',rm)
 keys<-c(keys,RMSEA_low=first(paste0('rmsea.ci.lower',suffix)),RMSEA_high=first(paste0('rmsea.ci.upper',suffix)))
 values<-vapply(keys,function(k)if(is.na(k))NA_real_ else unname(fm[k]),numeric(1))
 table<-data.frame(Index=names(keys),Value=unname(values),Source=unname(keys),row.names=NULL)
 table<-rbind(table,data.frame(Index='Chi_square_df',Value=if(is.finite(values['df'])&&values['df']>0)unname(values['Chi_square']/values['df'])else NA_real_,Source='Chi_square / df'))
 table
}

sem_publication_diagram<-function(fit,node_labels=NULL){
 pe<-lavaan::parameterEstimates(fit,standardized=TRUE,ci=TRUE)
 need(lavaan::lavInspect(fit,'ngroups')==1,'目前路徑圖只支援單群組；多群組請分群輸出或使用專門繪圖工具。')
 lf<-lavaan::lavNames(fit,'lv');measurement<-pe[pe$op=='=~',,drop=FALSE];reg<-pe[pe$op=='~',,drop=FALSE]
 covs<-pe[pe$op=='~~'&pe$lhs!=pe$rhs,,drop=FALSE]
 main<-unique(c(lf,reg$rhs,reg$lhs));if(!length(main))main<-lavaan::lavNames(fit,'ov')
 todo<-main;order<-character()
 while(length(todo)){roots<-setdiff(todo,reg$lhs[reg$rhs%in%todo & reg$lhs%in%todo]);if(!length(roots))roots<-todo[1];order<-c(order,roots);todo<-setdiff(todo,roots)}
 indicators<-setdiff(unique(measurement$rhs),main);owner<-setNames(measurement$lhs[match(indicators,measurement$rhs)],indicators)
 extra<-setdiff(unique(c(covs$lhs,covs$rhs)),c(main,indicators));order<-c(order,extra)
 # Allocate horizontal space to each construct's indicator row. The structural
 # model is on top; individual straight loading arrows point to boxes below.
 spans<-vapply(order,function(f)max(3.8,sum(owner==f)*2.6+.8),numeric(1))
 centers<-cumsum(spans)-spans/2;names(centers)<-order;base<-3.6
 nodes<-data.frame(Node=order,X=unname(centers),Y=base,Latent=order%in%lf,Role='structural',stringsAsFactors=FALSE)
 for(f in order){v<-names(owner)[owner==f];if(length(v))nodes<-rbind(nodes,data.frame(Node=v,X=centers[f]+(seq_along(v)-(length(v)+1)/2)*2.6,Y=0,Latent=FALSE,Role='indicator'))}
 label<-function(x){a<-if(!is.null(node_labels))unname(node_labels[x])else rep(NA_character_,length(x));a[is.na(a)]<-x[is.na(a)];a}
 wrap<-function(x){if(nchar(x)>35)x<-paste0(substr(x,1,34),'…');paste(substring(x,seq(1,nchar(x),18),pmin(seq(1,nchar(x),18)+17,nchar(x))),collapse='\n')}
 nodes$Label<-vapply(label(nodes$Node),wrap,character(1));nodes$RX<-ifelse(nodes$Latent,1.15,1.05);nodes$RY<-ifelse(nodes$Latent,.4,.38)
 edge_rows<-rbind(data.frame(From=measurement$lhs,To=measurement$rhs,Kind=rep('measurement',nrow(measurement)),Estimate=measurement$est,Std=measurement$std.all,p_value=measurement$pvalue),data.frame(From=reg$rhs,To=reg$lhs,Kind=rep('regression',nrow(reg)),Estimate=reg$est,Std=reg$std.all,p_value=reg$pvalue),data.frame(From=covs$lhs,To=covs$rhs,Kind=rep('covariance',nrow(covs)),Estimate=covs$est,Std=covs$std.all,p_value=covs$pvalue))
 stars<-function(p)ifelse(is.na(p),'',ifelse(p<.001,'***',ifelse(p<.01,'**',ifelse(p<.05,'*',''))))
 edge_rows$Label<-paste0(ifelse(is.finite(edge_rows$Std),sprintf('%.2f',edge_rows$Std),'—'),ifelse(edge_rows$Kind=='measurement','',stars(edge_rows$p_value)))
 # Clip arrows to ellipse/rectangle boundaries instead of drawing through nodes.
 boundary<-function(a,target){delta<-target-c(a$X,a$Y);if(all(delta==0))return(target);scale<-if(a$Latent)1/sqrt((delta[1]/a$RX)^2+(delta[2]/a$RY)^2)else min(a$RX/max(abs(delta[1]),1e-9),a$RY/max(abs(delta[2]),1e-9));c(a$X,a$Y)+scale*delta}
 paths<-list();labels<-list();upper<-base+.6;arc<-0L
 for(i in seq_len(nrow(edge_rows))){e<-edge_rows[i,];a<-nodes[match(e$From,nodes$Node),];b<-nodes[match(e$To,nodes$Node),]
  adjacent<-abs(match(e$From,order)-match(e$To,order))==1
  straight<-e$Kind=='measurement'||(e$Kind=='regression'&&isTRUE(adjacent))
  if(straight){
   aa<-boundary(a,c(b$X,b$Y));bb<-boundary(b,c(a$X,a$Y));xy<-data.frame(X=c(aa[1],bb[1]),Y=c(aa[2],bb[2]));fraction<-if(e$Kind=='measurement').62 else .5
   lx<-aa[1]+fraction*(bb[1]-aa[1]);ly<-aa[2]+fraction*(bb[2]-aa[2]);if(e$Kind=='regression')ly<-ly+.23
  }else{
   arc<-arc+1L;top<-max(a$Y,b$Y)+1.55+.5*arc
   control<-c(mean(c(a$X,b$X)),2*top-mean(c(a$Y,b$Y)));aa<-boundary(a,control);bb<-boundary(b,control)
   t<-seq(0,1,length.out=80);xy<-data.frame(X=(1-t)^2*aa[1]+2*(1-t)*t*control[1]+t^2*bb[1],Y=(1-t)^2*aa[2]+2*(1-t)*t*control[2]+t^2*bb[2]);lx<-mean(c(aa[1],bb[1]));ly<-max(xy$Y)+.2;upper<-max(upper,ly+.3)
  }
  xy$Edge<-i;xy$Kind<-e$Kind;paths[[i]]<-xy;labels[[i]]<-data.frame(X=lx,Y=ly,Label=e$Label)
 }
 pathdata<-dplyr::bind_rows(paths);labeldata<-dplyr::bind_rows(labels)
 # e = measurement error, d = endogenous disturbance. Variance values use
 # actual Std.all residual variances (latent-response scale for categorical Y).
 residual_nodes<-nodes[nodes$Node%in%unique(c(indicators,reg$lhs)),,drop=FALSE]
 residual_nodes$Symbol<-paste0(ifelse(residual_nodes$Role=='indicator','e','d'),ave(seq_len(nrow(residual_nodes)),residual_nodes$Role,FUN=seq_along))
 residual_nodes$EY<-ifelse(residual_nodes$Role=='indicator',-1.6,base+1.25)
 vv<-pe[pe$op=='~~'&pe$lhs==pe$rhs,,drop=FALSE];residual_nodes$Variance<-vv$std.all[match(residual_nodes$Node,vv$lhs)]
 p<-ggplot2::ggplot()
 for(kind in c('measurement','regression','covariance')){
  z<-pathdata[pathdata$Kind==kind,,drop=FALSE];if(nrow(z))p<-p+ggplot2::geom_path(data=z,ggplot2::aes(X,Y,group=Edge),linewidth=if(kind=='regression').65 else .4,color='#303741',arrow=grid::arrow(length=grid::unit(.09,'inches'),type='closed',ends=if(kind=='covariance')'both'else'last'))
 }
 if(nrow(residual_nodes)){
  rr<-residual_nodes;rr$Start<-rr$EY+ifelse(rr$Role=='indicator',.25,-.25);rr$End<-rr$Y+ifelse(rr$Role=='indicator',-rr$RY,rr$RY)
  circles<-dplyr::bind_rows(lapply(seq_len(nrow(rr)),function(i){th<-seq(0,2*pi,length.out=65);data.frame(X=rr$X[i]+.25*cos(th),Y=rr$EY[i]+.25*sin(th),Node=rr$Node[i])}))
  p<-p+ggplot2::geom_segment(data=rr,ggplot2::aes(x=X,xend=X,y=Start,yend=End),linewidth=.35,color='#69717b',arrow=grid::arrow(length=grid::unit(.07,'inches'),type='closed'))+
   ggplot2::geom_polygon(data=circles,ggplot2::aes(X,Y,group=Node),fill='white',color='#69717b',linewidth=.35)+
   ggplot2::geom_text(data=rr,ggplot2::aes(X,EY,label=Symbol),size=2.7)+ggplot2::geom_text(data=rr,ggplot2::aes(x=X+.4,y=EY,label=ifelse(is.finite(Variance),sprintf('%.2f',Variance),'—')),hjust=0,size=2.5,color='#69717b')
  upper<-max(upper,rr$EY+.5)
 }
 ellipse<-dplyr::bind_rows(lapply(which(nodes$Latent),function(i){theta<-seq(0,2*pi,length.out=101);data.frame(X=nodes$X[i]+nodes$RX[i]*cos(theta),Y=nodes$Y[i]+nodes$RY[i]*sin(theta),Node=nodes$Node[i])}))
 if(nrow(ellipse))p<-p+ggplot2::geom_polygon(data=ellipse,ggplot2::aes(X,Y,group=Node),fill='white',color='#303741',linewidth=.6)
 obs<-nodes[!nodes$Latent,,drop=FALSE]
 if(nrow(obs))p<-p+ggplot2::geom_rect(data=obs,ggplot2::aes(xmin=X-RX,xmax=X+RX,ymin=Y-RY,ymax=Y+RY),fill='white',color='#303741',linewidth=.5)
 p<-p+ggplot2::geom_text(data=nodes,ggplot2::aes(X,Y,label=Label),size=3.1,lineheight=.95,color='#20252c')
 if(nrow(labeldata))p<-p+ggplot2::geom_label(data=labeldata,ggplot2::aes(X,Y,label=Label),size=3,fill='white',linewidth=0,label.padding=grid::unit(.08,'lines'))
 fs<-sem_fit_summary(fit);xmin<-min(nodes$X)-1.5;xmax<-max(nodes$X)+1.5;ymin<-if(nrow(residual_nodes))min(-.8,residual_nodes$EY-.5)else min(nodes$Y)-.7
 p<-p+ggplot2::coord_fixed(ratio=1,xlim=c(xmin,xmax),ylim=c(ymin,upper+.2),clip='off')+ggplot2::theme_void(base_family='sans')+
  ggplot2::labs(title='Structural equation model',subtitle=paste0('Standardized estimates (Std.all) | N = ',sum(lavaan::lavInspect(fit,'nobs'))),caption='Ellipses: latent constructs; rectangles: observed variables; e: measurement error; d: disturbance.\nSingle arrows: directional paths; double arrows: covariance. Values by e/d: standardized residual variance.\n* p < .05, ** p < .01, *** p < .001 (unstandardized-parameter Wald p). Categorical variables use a latent-response scale.\nExogenous self-variances are omitted. Model fit is reported separately; paths alone do not establish causality.')+
  ggplot2::theme(plot.title=ggplot2::element_text(size=15,face='bold'),plot.subtitle=ggplot2::element_text(size=9,color='#626973',margin=ggplot2::margin(b=12)),plot.caption=ggplot2::element_text(size=8,color='#626973',hjust=0,margin=ggplot2::margin(t=12)),plot.margin=ggplot2::margin(18,22,18,22))
 attr(p,'export_width')<-min(45,max(11,(xmax-xmin)*.68));attr(p,'export_height')<-max(6,(upper+.2-ymin)*.68+1.6)
 list(plot=p,fit_table=fs,edge_table=edge_rows,node_table=nodes,residual_table=residual_nodes[,c('Node','Symbol','Variance'),drop=FALSE])
}
