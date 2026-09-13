# Plot a coefficient table and forest on a common row coordinate.
# Estimates come from unrounded model results; text rounding never affects positions.
publication_forest<-function(p,rows=seq_len(nrow(p$forest)),axis_range=NULL){
 f<-p$forest[rows,,drop=FALSE];f$Y<-rev(seq_len(nrow(f)));ratio<-p$effect!='Beta';null<-if(ratio)1 else 0
 valid<-is.finite(f$Estimate)&is.finite(f$Lower)&is.finite(f$Upper)&if(ratio)f$Lower>0 else TRUE
 all<-p$forest;vals<-c(all$Lower,all$Upper,null);vals<-vals[is.finite(vals)&(!ratio|vals>0)]
 transform<-if(ratio)log10 else identity
 lim<-range(transform(vals));if(diff(lim)<.01)lim<-lim+c(-.5,.5);lim<-lim+c(-1,1)*diff(lim)*.12
 if(!is.null(axis_range))lim<-axis_range
 pos<-function(a).65+(transform(a)-lim[1])/diff(lim)*.33
 f$X<-pos(f$Estimate);f$L<-pos(f$Lower);f$U<-pos(f$Upper)
 f$State<-ifelse(f$Reference,'Reference',ifelse(is.finite(f$p)&f$p<.05,'p < 0.05','p ≥ 0.05'))
 f$Text<-ifelse(f$Reference,'Ref',ifelse(f$Kind=='heading','',paste0(f2(f$Estimate),' (',f2(f$Lower),', ',f2(f$Upper),')')))
 f$P<-ifelse(f$Kind=='heading'|f$Reference,'',fp(f$p))
 f$Label<-vapply(f$Label,function(s)paste(strwrap(s,width=37),collapse='\n'),character(1))
 headings<-f[f$Kind=='heading',,drop=FALSE];points<-f[valid|f$Reference,,drop=FALSE];intervals<-f[valid&!f$Reference,,drop=FALSE]
 ticks<-pretty(lim,n=4);ticks<-ticks[ticks>=lim[1]&ticks<=lim[2]];tv<-if(ratio)10^ticks else ticks;tickx<-pos(tv)
 n<-nrow(f)
 g<-ggplot2::ggplot()+
  ggplot2::geom_rect(data=headings,ggplot2::aes(xmin=0,xmax=1,ymin=Y-.43,ymax=Y+.43),fill='#e7edf4')+
  ggplot2::annotate('segment',x=pos(null),xend=pos(null),y=.4,yend=n+.55,linetype='dashed',colour='#80858d')+
  ggplot2::geom_segment(data=intervals,ggplot2::aes(x=L,xend=U,y=Y,yend=Y),linewidth=.55)+
  ggplot2::geom_segment(data=intervals,ggplot2::aes(x=L,xend=L,y=Y-.10,yend=Y+.10),linewidth=.5)+
  ggplot2::geom_segment(data=intervals,ggplot2::aes(x=U,xend=U,y=Y-.10,yend=Y+.10),linewidth=.5)+
  ggplot2::geom_point(data=points,ggplot2::aes(X,Y,shape=State,fill=State),size=2.8,colour='#405264')+
  ggplot2::scale_shape_manual(values=c('p < 0.05'=22,'p ≥ 0.05'=22,Reference=23))+
  ggplot2::scale_fill_manual(values=c('p < 0.05'='#176eae','p ≥ 0.05'='white',Reference='#999999'))+
  ggplot2::geom_text(data=f,ggplot2::aes(x=ifelse(Kind=='level',.022,.01),y=Y,label=Label,fontface=ifelse(Kind=='heading','bold','plain')),hjust=0,size=3.1,lineheight=.85)+
  ggplot2::geom_text(data=f,ggplot2::aes(x=.44,y=Y,label=Text),size=3.1)+
  ggplot2::geom_text(data=f,ggplot2::aes(x=.594,y=Y,label=P),size=3.1)+
  ggplot2::annotate('text',x=c(.01,.44,.594,.815),y=n+1,label=c('Variable / Level',names(p$data)[2],'p',p$effect),hjust=c(0,.5,.5,.5),fontface='bold',size=3.5)+
  ggplot2::annotate('segment',x=0,xend=1,y=n+.62,yend=n+.62,linewidth=.5)+
  ggplot2::annotate('segment',x=.65,xend=.98,y=.25,yend=.25,linewidth=.4)+
  ggplot2::annotate('text',x=tickx,y=-.15,label=format(signif(tv,3),trim=TRUE),size=2.8)+
  ggplot2::annotate('text',x=.815,y=-.75,label=paste(p$effect,if(ratio)'(log scale)'else'(linear scale)'),size=3.2)+
  ggplot2::coord_cartesian(xlim=c(0,1),ylim=c(-1,n+1.4),clip='off',expand=FALSE)+
  ggplot2::theme_void(base_family='sans')+ggplot2::theme(legend.position='bottom',legend.title=ggplot2::element_blank(),plot.margin=ggplot2::margin(15,18,10,18),plot.title=ggplot2::element_text(size=13,face='bold'),plot.caption=ggplot2::element_text(size=9,hjust=0))+
  ggplot2::labs(title=paste('Model estimates:',p$outcome),caption='Reference: selected predictor level. Points are model coefficients, not pooled study estimates.\nCI and p-values describe estimation uncertainty; p < 0.05 alone does not establish causality.')
 attr(g,'export_width')<-14;attr(g,'export_height')<-max(4.8,2+n*.34);g
}
add_publication_forest<-function(ans){
 if(is.null(ans$publication$forest))return(ans)
 pages<-split(seq_len(nrow(ans$publication$forest)),ceiling(seq_len(nrow(ans$publication$forest))/28))
 ans$plots$Forest<-NULL
 for(i in seq_along(pages))ans$plots[[paste0('Publication_forest_',i)]]<-publication_forest(ans$publication,pages[[i]])
 ans
}
