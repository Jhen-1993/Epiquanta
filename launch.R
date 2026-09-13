# Run from RStudio using source('launch.R'), or double-click Start_BioStat.cmd.
if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','English_United States.utf8')
args<-commandArgs(trailingOnly=FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)])
if(length(script))setwd(dirname(normalizePath(script)))
local_lib<-Sys.getenv('BIOSTAT_LIBRARY','library');if(dir.exists(local_lib)).libPaths(c(normalizePath(local_lib),.libPaths()))
need<-c('survey','geepack','lme4','lmerTest','data.table','regmedint','shiny','psych','polycor','lavaan','semTools','sandwich','ggplot2','officer','writexl','pROC','dplyr','tidyr','readxl','haven')
missing<-need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop(paste('Missing packages:',paste(missing,collapse=', '),'Run setup.R first.'))
# Loopback only: do not change to 0.0.0.0 for patient data.
options(shiny.host='127.0.0.1',shiny.port=8765)
shiny::runApp('.',host='127.0.0.1',port=8765,launch.browser=interactive() || Sys.getenv('BIOSTAT_NO_BROWSER')!='1')
