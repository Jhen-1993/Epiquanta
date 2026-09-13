xml_text<-function(x)as.character(htmltools::htmlEscape(as.character(x)))
publication_headers<-function(p){
 if(isTRUE(p$custom))return(list(n=ncol(p$data),ng=0,stat=''))
 n<-ncol(p$data);ng<-sum(startsWith(names(p$data),paste0(p$group,'=')));list(n=n,ng=ng,stat=if(p$summary=='mean')'n (%) / mean ± SD'else'n (%) / median (Q1, Q3)')
}
word_publication<-function(p){
 if(isTRUE(p$custom))return(measurement_word(p))
 h<-publication_headers(p);n<-h$n
 cell<-function(txt,j,bold=FALSE,span=1,merge='',border='',indent=FALSE){paste0('<w:tc><w:tcPr>',if(span>1)paste0('<w:gridSpan w:val="',span,'"/>'),if(nzchar(merge))paste0('<w:vMerge w:val="',merge,'"/>'),if(nzchar(border))paste0('<w:tcBorders><w:',border,' w:val="single" w:sz="6"/></w:tcBorders>'),'<w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:spacing w:after="40" w:before="40"/><w:jc w:val="',if(j==1)'left'else'center','"/>',if(indent)'<w:ind w:left="240"/>','</w:pPr><w:r><w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman" w:eastAsia="新細明體"/><w:sz w:val="21"/>',if(bold)'<w:b/>','</w:rPr><w:t xml:space="preserve">',xml_text(txt),'</w:t></w:r></w:p></w:tc>')}
 row<-function(cells,header=FALSE)paste0('<w:tr><w:trPr>',if(header)'<w:tblHeader/>','<w:cantSplit/></w:trPr>',paste(cells,collapse=''),'</w:tr>')
 names2<-names(p$data);if(h$ng>0)names2[3:(2+h$ng)]<-substring(names2[3:(2+h$ng)],nchar(p$group)+2)
 if(isTRUE(p$model)){head<-paste0(row(c(cell('Characteristic',1,TRUE,merge='restart'),cell(p$outcome,2,TRUE,span=n-1,border='bottom')),TRUE),row(c(cell('',1,merge='continue',border='bottom'),lapply(2:n,function(j)cell(names2[j],j,TRUE,border='bottom'))),TRUE))
 }else if(h$ng>0){
  top<-c(cell('Characteristic',1,TRUE,merge='restart'),cell(paste('Overall',h$stat),2,TRUE,merge='restart'),cell(p$group,3,TRUE,span=h$ng,border='bottom'))
  if(n>2+h$ng)for(j in (3+h$ng):n)top<-c(top,cell(names2[j],j,TRUE,merge='restart'))
  second<-c(cell('',1,merge='continue',border='bottom'),cell('',2,merge='continue',border='bottom'))
  for(j in 3:n)second<-c(second,cell(if(j<=2+h$ng)names2[j]else'',j,TRUE,merge=if(j>2+h$ng)'continue'else'',border='bottom'))
  head<-paste0(row(top,TRUE),row(second,TRUE))
 }else head<-row(lapply(seq_len(n),function(j)cell(if(j==2)paste('Overall',h$stat)else names2[j],j,TRUE,border='bottom')),TRUE)
 body<-vapply(seq_len(nrow(p$data)),function(i)row(lapply(seq_len(n),function(j)cell(p$data[i,j],j,p$kinds[i]%in%c('heading','total'),indent=j==1&&p$kinds[i]=='level'))),character(1))
 paste0('<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:tblPr><w:tblW w:w="0" w:type="auto"/><w:tblBorders><w:top w:val="single" w:sz="8"/><w:bottom w:val="single" w:sz="8"/><w:left w:val="nil"/><w:right w:val="nil"/><w:insideH w:val="nil"/><w:insideV w:val="nil"/></w:tblBorders></w:tblPr><w:tblGrid>',paste0('<w:gridCol w:w="',c(3400,rep(floor(10700/(n-1)),n-1)),'"/>',collapse=''),'</w:tblGrid>',head,paste(body,collapse=''),'</w:tbl>')
}
style_publication_xlsx<-function(path,p){
 path<-normalizePath(path,winslash='/',mustWork=TRUE)
 td<-tempfile();dir.create(td);on.exit(unlink(td,recursive=TRUE));unzip(path,exdir=td)
 stpath<-file.path(td,'xl/styles.xml');st<-xml2::read_xml(stpath);ns<-xml2::xml_ns(st)
 node<-function(name)xml2::xml_find_first(st,paste0('//*[local-name()="',name,'"]'))
 fonts<-node('fonts');nf<-length(xml2::xml_children(fonts));for(b in c(FALSE,TRUE))xml2::xml_add_child(fonts,xml2::read_xml(paste0('<font><sz val="11"/><name val="Times New Roman"/>',if(b)'<b/>','</font>')))
 xml2::xml_set_attr(fonts,'count',as.character(nf+2))
 borders<-node('borders');nb<-length(xml2::xml_children(borders));for(edges in list(c('top','bottom'),'bottom',character()))xml2::xml_add_child(borders,xml2::read_xml(paste0('<border><left/><right/>',paste(vapply(c('top','bottom'),function(e)if(e%in%edges)paste0('<',e,' style="thin"><color rgb="FF000000"/></',e,'>')else paste0('<',e,'/>'),character(1)),collapse=''),'<diagonal/></border>')))
 xml2::xml_set_attr(borders,'count',as.character(nb+3));xf<-node('cellXfs');base<-length(xml2::xml_children(xf))
 for(i in 0:5)xml2::xml_add_child(xf,xml2::read_xml(sprintf('<xf numFmtId="0" fontId="%d" fillId="0" borderId="%d" xfId="0" applyAlignment="1" applyFont="1" applyBorder="1"><alignment horizontal="%s" vertical="center" wrapText="1" indent="%d"/></xf>',nf+as.integer(i%in%c(0,1,4)),nb+if(i==0)0 else if(i==1)1 else 2,if(i%in%c(2,4,5))'left'else'center',as.integer(i==5))))
 xml2::xml_set_attr(xf,'count',as.character(base+6));xml2::write_xml(st,stpath)
 wb<-xml2::read_xml(file.path(td,'xl/workbook.xml'));sheets<-xml2::xml_find_all(wb,'//*[local-name()="sheet"]');idx<-which(xml2::xml_attr(sheets,'name')==if(isTRUE(p$custom))'MeasurementTable'else if(isTRUE(p$model))'ModelTable'else'Table1');stopifnot(length(idx)==1)
 h<-publication_headers(p);n<-h$n;letter<-function(j){s<-'';while(j>0){s<-paste0(LETTERS[(j-1)%%26+1],s);j<-(j-1)%/%26};s}
 cell<-function(r,j,value,style)paste0('<c r="',letter(j),r,'" s="',base+style,'" t="inlineStr"><is><t xml:space="preserve">',xml_text(value),'</t></is></c>')
 names2<-names(p$data);if(h$ng>0)names2[3:(2+h$ng)]<-substring(names2[3:(2+h$ng)],nchar(p$group)+2)
 merges<-character();rr<-list();head1<-names2;head1[2]<-if(isTRUE(p$model)||isTRUE(p$custom))names2[2]else paste('Overall',h$stat)
 if(isTRUE(p$model)){head1<-c('Characteristic',p$outcome,rep('',n-2));merges<-c('A1:A2',paste0('B1:',letter(n),'1'));rr[[2]]<-paste0('<row r="2" ht="32" customHeight="1">',paste(vapply(seq_len(n),function(j)cell(2,j,if(j==1)''else names2[j],1),character(1)),collapse=''),'</row>')}else if(h$ng>0){head1[3:(2+h$ng)]<-c(p$group,rep('',h$ng-1));head2<-rep('',n);head2[3:(2+h$ng)]<-names2[3:(2+h$ng)];merges<-c('A1:A2','B1:B2');if(h$ng>1)merges<-c(merges,paste0('C1:',letter(2+h$ng),'1'));if(n>2+h$ng)merges<-c(merges,vapply((3+h$ng):n,function(j)paste0(letter(j),'1:',letter(j),'2'),character(1)));rr[[2]]<-paste0('<row r="2" ht="32" customHeight="1">',paste(vapply(seq_len(n),function(j)cell(2,j,head2[j],1),character(1)),collapse=''),'</row>')}
 rr[[1]]<-paste0('<row r="1" ht="38" customHeight="1">',paste(vapply(seq_len(n),function(j)cell(1,j,head1[j],0),character(1)),collapse=''),'</row>');start<-length(rr)
 for(i in seq_len(nrow(p$data))){r<-i+start;rr[[r]]<-paste0('<row r="',r,'" ht="20" customHeight="1">',paste(vapply(seq_len(n),function(j)cell(r,j,p$data[i,j],if(j>1)3 else if(p$kinds[i]%in%c('heading','total'))4 else if(p$kinds[i]=='level')5 else 2),character(1)),collapse=''),'</row>')}
 if(isTRUE(p$custom)){
  if(nrow(p$merges))for(i in seq_len(nrow(p$merges))){m<-p$merges[i,];merges<-c(merges,paste0(letter(m$col),m$first+start,':',letter(m$col),m$last+start))}
  for(i in seq_len(nrow(p$data))){r<-i+start;if(p$kinds[i]=='heading'){merges<-c(merges,paste0('A',r,':',letter(n),r));vals<-c(p$data[i,1],rep('',n-1))}else vals<-vapply(seq_len(n),function(j)if(measurement_span(p,i,j)==0)''else as.character(p$data[i,j]),character(1))
   rr[[r]]<-paste0('<row r="',r,'" ht="',max(23,15*ceiling(max(nchar(vals[names(p$data)=='Item (abbreviated)']),0)/32)+8),'" customHeight="1">',paste(vapply(seq_len(n),function(j)cell(r,j,vals[j],if(p$kinds[i]=='heading')4 else if(j%in%c(2,3))2 else 3),character(1)),collapse=''),'</row>')
  }
 }
 last<-start+nrow(p$data)+1;rr[[last]]<-paste0('<row r="',last,'">',paste(vapply(seq_len(n),function(j)cell(last,j,'',1),character(1)),collapse=''),'</row>')
 custom_cols<-if(isTRUE(p$custom))paste(vapply(seq_len(n),function(j){nm<-names(p$data)[j];w<-if(nm=='Item (abbreviated)')36 else if(nm=='n')10 else if(grepl('95%',nm,fixed=TRUE))25 else if(nm=='Question')15 else 18;sprintf('<col min="%d" max="%d" width="%d" customWidth="1"/>',j,j,w)},character(1)),collapse='')else paste0('<col min="1" max="1" width="32" customWidth="1"/><col min="2" max="',n,'" width="23" customWidth="1"/>')
 sheet<-paste0('<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetViews><sheetView showGridLines="0" workbookViewId="0"/></sheetViews><cols>',custom_cols,'</cols><sheetData>',paste(rr,collapse=''),'</sheetData>',if(length(merges))paste0('<mergeCells count="',length(merges),'">',paste0('<mergeCell ref="',merges,'"/>',collapse=''),'</mergeCells>'),'<pageSetup orientation="landscape" paperSize="9" fitToWidth="1" fitToHeight="0"/></worksheet>')
 writeLines(sheet,file.path(td,paste0('xl/worksheets/sheet',idx,'.xml')),useBytes=TRUE)
 zip::zipr(path,list.files(td,full.names=TRUE,all.files=TRUE,no..=TRUE),root=td)
}
publication_html<-function(p){
 if(isTRUE(p$custom))return(measurement_html(p))
 h<-publication_headers(p);nm<-names(p$data);n<-length(nm)
 th<-function(x,attrs='')paste0('<th ',attrs,'>',xml_text(x),'</th>')
 if(isTRUE(p$model)){headers<-paste0('<tr>',th('Characteristic','rowspan="2"'),th(p$outcome,paste0('colspan="',n-1,'"')),'</tr><tr>',paste(vapply(nm[-1],th,character(1)),collapse=''),'</tr>')}else if(h$ng>0){nm[3:(2+h$ng)]<-substring(nm[3:(2+h$ng)],nchar(p$group)+2);top<-c(th('Characteristic','rowspan="2"'),th(paste('Overall',h$stat),'rowspan="2"'),th(p$group,paste0('colspan="',h$ng,'"')));if(n>2+h$ng)for(j in (3+h$ng):n)top<-c(top,th(nm[j],'rowspan="2"'));headers<-paste0('<tr>',paste(top,collapse=''),'</tr><tr>',paste(vapply(nm[3:(2+h$ng)],th,character(1)),collapse=''),'</tr>')
 }else headers<-paste0('<tr>',th('Characteristic'),th(paste('Overall',h$stat)),'</tr>')
 rows<-vapply(seq_len(nrow(p$data)),function(i)paste0('<tr class="',p$kinds[i],'">',paste0('<td>',xml_text(unlist(p$data[i,],use.names=FALSE)),'</td>',collapse=''),'</tr>'),character(1))
 paste0('<div class="table-scroll"><table class="publication"><thead>',headers,'</thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>')
}
model_publication<-function(tab,z,x,label,outcome){
 rows<-list();kinds<-character();used<-integer();raw_rows<-list()
 add<-function(name,estimate='',p='',kind='continuous',value=NA_real_,low=NA_real_,high=NA_real_,pvalue=NA_real_){rows[[length(rows)+1]]<<-data.frame(Characteristic=name,Estimate=estimate,`p-value`=p,check.names=FALSE);kinds<<-c(kinds,kind);raw_rows[[length(raw_rows)+1]]<<-data.frame(Label=name,Kind=kind,Estimate=value,Lower=low,Upper=high,p=pvalue,Reference=estimate=='Ref')}
 put<-function(i,name,kind){used<<-c(used,i);add(name,paste0(f2(tab[[label]][i]),' (',f2(tab$CI_low[i]),', ',f2(tab$CI_high[i]),')'),fp(tab$p_value[i]),kind,tab[[label]][i],tab$CI_low[i],tab$CI_high[i],tab$p_value[i])}
 for(v in x){if(is.factor(z[[v]])){add(v,kind='heading');for(l in levels(z[[v]])){if(l==levels(z[[v]])[1])add(l,'Ref',kind='level')else {i<-which(tab$Term==paste0(v,l));if(length(i)==1)put(i,l,'level')}}}else {i<-which(tab$Term==v);if(length(i)==1)put(i,v,'continuous')}}
 for(i in setdiff(seq_len(nrow(tab)),used))if(tab$Term[i]!='(Intercept)')put(i,tab$Term[i],'continuous')
 pd<-dplyr::bind_rows(rows);names(pd)[2]<-paste0(if(length(x)>1)'Adjusted 'else'',label,' (95% CI)')
 fr<-dplyr::bind_rows(raw_rows);fr$Estimate[fr$Reference]<-if(label=='Beta')0 else 1
 list(data=pd,kinds=kinds,group='',summary='mean',model=TRUE,outcome=outcome,forest=fr,effect=label,adjusted=length(x)>1,covariates=x)
}

publication_note<-function(p){
 if(!is.null(p$note))return(p$note)
 if(isTRUE(p$model))paste0('效果量與 95% CI；Ref 為預測變項參考組。',if(isTRUE(p$adjusted))paste0('同一模型共同納入：',paste(p$covariates,collapse='、'),'。')else'單一預測變項模型。','若含交互作用，主效果是在另一變項參考組／數值 0 下的條件效果。')
 else '類別資料為 n (%)；連續資料依表頭摘要。p 值為整體組間檢定；Holm／Bonferroni（若有）為本表預定檢定的多重檢定校正，與納入干擾因子的模型調整不同。'
}
