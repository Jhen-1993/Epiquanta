source('R/provenance.R',encoding='UTF-8')
source('R/publication.R',encoding='UTF-8')
source('R/measurement_publication.R',encoding='UTF-8')
source('R/forest_publication.R',encoding='UTF-8')
plot_size<-function(p){list(width=attr(p,'export_width') %||% 9,height=attr(p,'export_height') %||% 5)}
# Shared formatting: UI, Word, and formatted Excel sheets use the same function.
display_table <- function(d) {
 d<-as.data.frame(d,check.names=FALSE);original<-d
 for(n in names(d))if(is.numeric(d[[n]])) {
  if(grepl('(^p$|^p_value$|^p_Holm$|^p_Bonferroni$|Pr[(]|pvalue)',n))d[[n]]<-fp(d[[n]])
  else if(n %in% c('N','n','Items','Item_count','Factor','df','df1','df2','Count','Requested','Successful','Empty_cells','Cells','At_risk','Events','Factors','Factors_SMC','Factors_full','Extreme_pairs'))d[[n]]<-ifelse(is.na(d[[n]]),'—',as.character(d[[n]]))
  else d[[n]]<-ifelse(is.na(d[[n]]),'—',ifelse(is.infinite(d[[n]]),'Inf',f2(d[[n]])))
 }
 if(all(c('Index','Value')%in%names(d))){ii<-grepl('pvalue',d$Index);d$Value[ii]<-fp(original$Value[ii])}
 if(all(c('Index','Value','Source')%in%names(d))&&'Chi_square'%in%d$Index){d$Value<-ifelse(is.finite(original$Value),sprintf('%.3f',original$Value),'—');d$Value[d$Index=='df']<-as.character(original$Value[d$Index=='df']);d$Value[d$Index=='p_value']<-fp(original$Value[d$Index=='p_value'])}
 d[is.na(d)]<-'—';d
}
export_xlsx <- function(result,path) {
 sheets<-list(Report=data.frame(Notes=c(result$title,result$notes)))
 if(!is.null(result$provenance)){sheets$Calculation_info<-data.frame(Details=provenance_text(result$provenance));sheets$Packages<-result$provenance$packages;if(!is.null(result$provenance$statistics))sheets$Statistical_packages<-result$provenance$statistics;if(!is.null(result$provenance$plotting))sheets$Plotting_packages<-result$provenance$plotting}
 for(n in names(result$tables))if(nrow(result$tables[[n]]))sheets[[substr(n,1,31)]]<-display_table(result$tables[[n]])
 # Preserve unrounded estimates for further inspection; presentation sheets above
 # intentionally contain formatted strings and match the browser and Word.
 for(n in names(result$tables))if(nrow(result$tables[[n]]))sheets[[substr(paste0('raw_',n),1,31)]]<-as.data.frame(result$tables[[n]])
 if(!is.null(result$publication)){nm<-if(isTRUE(result$publication$custom))'MeasurementTable'else if(isTRUE(result$publication$model))'ModelTable'else'Table1';sheets[[nm]]<-result$publication$data;sheets<-sheets[c(nm,setdiff(names(sheets),nm))]}
 names(sheets)<-make.unique(names(sheets),sep='_');writexl::write_xlsx(sheets,path)
 if(!is.null(result$publication))style_publication_xlsx(path,result$publication)
}
export_docx <- function(result,path) {
 doc<-officer::read_docx();doc<-officer::body_set_default_section(doc,officer::prop_section(page_size=officer::page_size(orient='landscape'),page_margins=officer::page_mar(top=.65,bottom=.65,left=.6,right=.6)))
 doc<-officer::body_add_par(doc,result$title,style='heading 1')
 if(!is.null(result$provenance)){doc<-officer::body_add_par(doc,'計算方法與版本',style='heading 2');for(line in provenance_text(result$provenance))doc<-officer::body_add_par(doc,line)}
 if(!is.null(result$publication)){
  doc<-officer::body_add_xml(doc,word_publication(result$publication))
  doc<-officer::body_add_par(doc,publication_note(result$publication))
 }
 for(n in result$notes)doc<-officer::body_add_par(doc,n)
 for(n in names(result$tables))if(nrow(result$tables[[n]])) {
  if(n=='Table1'&&!is.null(result$publication))next
  tab<-display_table(result$tables[[n]]);doc<-officer::body_add_par(doc,n,style='heading 2')
  # Split wide tables into blocks with the identifier column repeated.
  groups<-split(seq_len(ncol(tab)),ceiling(seq_len(ncol(tab))/7))
  for(g in groups){if(!1 %in% g)g<-c(1,g);doc<-officer::body_add_table(doc,tab[,g,drop=FALSE],style='table_template')}
 }
 for(n in names(result$plots)) {
  tmp<-tempfile(fileext='.png');ps<-plot_size(result$plots[[n]]);ggplot2::ggsave(tmp,result$plots[[n]],width=ps$width,height=ps$height,dpi=180,bg='white')
  w<-min(8.8,6.2*ps$width/ps$height);doc<-officer::body_add_par(doc,n,style='heading 2');doc<-officer::body_add_img(doc,tmp,width=w,height=w*ps$height/ps$width);unlink(tmp)
 }
 print(doc,target=path)
}
