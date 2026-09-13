measurement_publication<-function(blocks,columns,merge_cols,title,note){
 rows<-list();kinds<-character();merges<-list();i<-0L
 for(name in names(blocks)){
  head<-as.data.frame(as.list(setNames(rep('',length(columns)),columns)),check.names=FALSE);head[1,1]<-name;rows[[length(rows)+1]]<-head;kinds<-c(kinds,'heading');i<-i+1L
  block<-blocks[[name]];need(ncol(block)==length(columns),'量測表欄位數不一致。');names(block)<-columns;rows[[length(rows)+1]]<-block;kinds<-c(kinds,rep('item',nrow(block)))
  if(nrow(block)>1)for(j in merge_cols)merges[[length(merges)+1]]<-data.frame(first=i+1L,last=i+nrow(block),col=j)
  i<-i+nrow(block)
 }
 list(data=dplyr::bind_rows(rows),kinds=kinds,group='',summary='mean',custom=TRUE,title=title,merges=dplyr::bind_rows(merges),note=note)
}
measurement_number<-function(v)ifelse(is.finite(v),sprintf('%.3f',v),'—')
measurement_ci<-function(e,l,h)paste0(measurement_number(e),' (',measurement_number(l),'–',measurement_number(h),')')
reliability_publication<-function(summary,items){
 blocks<-list()
 for(i in seq_len(nrow(summary))){s<-summary[i,];z<-items[items$Scale==s$Scale,];n<-nrow(z)
  blocks[[s$Scale]]<-data.frame(Items=rep(as.character(s$Items),n),Question=z$Question,Item=paste0(z$Item,ifelse(z$Reverse,' (R)','')),N=rep(as.character(s$N),n),CITC=measurement_number(z$CITC),Deleted=measurement_number(z$Alpha_if_deleted),Raw=rep(measurement_ci(s$Alpha,s$Alpha_low,s$Alpha_high),n),Standardized=rep(measurement_number(s$Standardized_alpha),n),r=rep(measurement_number(s$Inter_item_r),n),SB=rep(if(s$Items==2)measurement_ci(s$Spearman_Brown,s$SB_low,s$SB_high)else'—',n),check.names=FALSE)
 }
 measurement_publication(blocks,c('No. of items','Question','Item (abbreviated)','n','CITC','α if deleted','Raw α (95% CI)','Standardized α','Inter-item r','Spearman–Brown (95% CI)'),c(1,4,7:10),'信度結果表','每題組完整作答樣本；(R) 為指定反向題。CITC＝該題與其餘題目總分的 Pearson 相關；α if deleted 為刪題後原始 α。兩題不列 CITC／刪題 α；95% CI 使用受試者層級百分位 Bootstrap，有效比例低於 95% 停止。')
}
measurement_span<-function(p,i,j){
 m<-p$merges;if(is.null(m)||!nrow(m))return(1L)
 hit<-which(m$col==j&m$first<=i&m$last>=i);if(!length(hit))return(1L)
 if(m$first[hit[1]]==i)m$last[hit[1]]-i+1L else 0L
}
measurement_html<-function(p){
 n<-ncol(p$data);head<-paste0('<tr>',paste0('<th',ifelse(names(p$data)=='Item (abbreviated)',' class="item-label"',''),'>',xml_text(names(p$data)),'</th>',collapse=''),'</tr>')
 rows<-vapply(seq_len(nrow(p$data)),function(i){if(p$kinds[i]=='heading')return(paste0('<tr class="heading"><td colspan="',n,'">',xml_text(p$data[i,1]),'</td></tr>'))
  cells<-vapply(seq_len(n),function(j){span<-measurement_span(p,i,j);if(span==0)return('');paste0('<td',if(names(p$data)[j]=='Item (abbreviated)')' class="item-label"',if(span>1)paste0(' rowspan="',span,'"'),'>',xml_text(p$data[i,j]),'</td>')},character(1));paste0('<tr>',paste(cells,collapse=''),'</tr>')},character(1))
 paste0('<div class="table-scroll"><table class="publication measurement-publication"><thead>',head,'</thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>')
}
measurement_word<-function(p){
 n<-ncol(p$data)
 cell<-function(value,j,bold=FALSE,span=1L,merge=''){paste0('<w:tc><w:tcPr>',if(span>1)paste0('<w:gridSpan w:val="',span,'"/>'),if(nzchar(merge))paste0('<w:vMerge w:val="',merge,'"/>'),'<w:vAlign w:val="center"/></w:tcPr><w:p><w:pPr><w:spacing w:after="35" w:before="35"/><w:jc w:val="',if(j%in%c(2,3))'left'else'center','"/></w:pPr><w:r><w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman" w:eastAsia="新細明體"/><w:sz w:val="18"/>',if(bold)'<w:b/>','</w:rPr><w:t xml:space="preserve">',xml_text(value),'</w:t></w:r></w:p></w:tc>')}
 row<-function(cells,header=FALSE)paste0('<w:tr><w:trPr>',if(header)'<w:tblHeader/>','<w:cantSplit/></w:trPr>',paste(cells,collapse=''),'</w:tr>')
 headers<-row(vapply(seq_len(n),function(j)cell(names(p$data)[j],j,TRUE),character(1)),TRUE)
 body<-vapply(seq_len(nrow(p$data)),function(i){if(p$kinds[i]=='heading')return(row(cell(p$data[i,1],2,TRUE,n)))
  row(vapply(seq_len(n),function(j){span<-measurement_span(p,i,j);cell(if(span==0)''else p$data[i,j],j,merge=if(span==0)'continue'else if(span>1)'restart'else'')},character(1)))},character(1))
 widths<-vapply(names(p$data),function(nm)if(nm=='Item (abbreviated)')2300 else if(nm=='n')550 else if(grepl('95%',nm,fixed=TRUE))1700 else 900,numeric(1));widths<-floor(widths/sum(widths)*14000)
 paste0('<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:tblPr><w:tblW w:w="0" w:type="auto"/><w:tblBorders><w:top w:val="single" w:sz="8"/><w:bottom w:val="single" w:sz="8"/><w:insideH w:val="single" w:sz="2" w:color="DDDDDD"/><w:insideV w:val="single" w:sz="2" w:color="DDDDDD"/></w:tblBorders></w:tblPr><w:tblGrid>',paste0('<w:gridCol w:w="',widths,'"/>',collapse=''),'</w:tblGrid>',headers,paste(body,collapse=''),'</w:tbl>')
}
