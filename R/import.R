# Large files may be read directly from this local machine, bypassing HTTP upload.
local_data_path<-function(path){
 path<-trimws(path);path<-sub('^"(.*)"$','\\1',path)
 need(nzchar(path)&&file.exists(path)&&!dir.exists(path),'找不到檔案。請貼上檔案總管「複製為路徑」取得的完整路徑。')
 normalizePath(path,winslash='/',mustWork=TRUE)
}
read_dataset<-function(path,name=basename(path),encoding='UTF-8',na=c('','NA','N/A'),columns=NULL,header_only=FALSE)withCallingHandlers({
 ext<-tolower(tools::file_ext(name));need(ext%in%c('csv','tsv','xlsx','xls','sav','sas7bdat','dta'),'不支援的檔案格式。')
 if(ext%in%c('csv','tsv')){
  if(encoding=='UTF-8'){
   z<-data.table::fread(file=path,sep=if(ext=='tsv')'\t'else',',encoding='UTF-8',na.strings=na,select=columns,nrows=if(header_only)0 else Inf,check.names=FALSE,showProgress=FALSE,data.table=FALSE,integer64='character')
  }else{
   # fread cannot decode CP950; retain the explicit base-R decoding path.
   z<-read.table(path,header=TRUE,sep=if(ext=='tsv')'\t'else',',quote='"',comment.char='',check.names=FALSE,fileEncoding=encoding,na.strings=na,nrows=if(header_only)0 else -1,colClasses=if(is.null(columns))NA else {nm<-names(read_dataset(path,name,encoding,na,header_only=TRUE));ifelse(nm%in%columns,NA,'NULL')})
  }
 }else{
  if(header_only)z<-switch(ext,xlsx=readxl::read_excel(path,n_max=0,.name_repair='minimal'),xls=readxl::read_excel(path,n_max=0,.name_repair='minimal'),sav=haven::read_sav(path,n_max=0),sas7bdat=haven::read_sas(path,n_max=0),dta=haven::read_dta(path,n_max=0))
  else z<-switch(ext,xlsx=readxl::read_excel(path,.name_repair='minimal'),xls=readxl::read_excel(path,.name_repair='minimal'),sav=haven::read_sav(path,col_select=tidyselect::all_of(columns %||% names(haven::read_sav(path,n_max=0)))),sas7bdat=haven::read_sas(path,col_select=tidyselect::all_of(columns %||% names(haven::read_sas(path,n_max=0)))),dta=haven::read_dta(path,col_select=tidyselect::all_of(columns %||% names(haven::read_dta(path,n_max=0)))))
  if(!is.null(columns))z<-z[,columns,drop=FALSE]
 }
 need(!anyDuplicated(names(z))&&all(nzchar(names(z))),'欄名不可重複或空白；請先修正來源欄名。')
 if(encoding=='UTF-8'){
  strings<-c(list(names(z)),z[vapply(z,is.character,logical(1))])
  need(!any(vapply(strings,function(v)any(!is.na(v)&is.na(iconv(v,from='UTF-8',to='UTF-8',sub=NA))),logical(1))),'檔案包含非 UTF-8 文字；請確認編碼，繁體中文舊檔可試 CP950。')
 }
 as.data.frame(z,check.names=FALSE)
},warning=function(w)stop(paste('讀檔警示，未採用可能不完整的資料：',conditionMessage(w)),call.=FALSE))
