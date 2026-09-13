# Portable launcher: no PowerShell script execution-policy changes required.
args<-commandArgs(FALSE);file<-sub('^--file=','',args[grepl('^--file=',args)])
root<-dirname(normalizePath(file));setwd(root)
if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','English_United States.utf8')
.libPaths(c(file.path(root,'library'),.libPaths()))
dir.create('logs',showWarnings=FALSE)
url<-'http://127.0.0.1:8765/'
ready<-function()tryCatch({old<-options(timeout=2);on.exit(options(old));con<-suppressWarnings(url(url,open='rb'));on.exit(close(con),add=TRUE);any(grepl('Epiquanta|BioStat Studio',suppressWarnings(readLines(con,warn=FALSE))))},error=function(e)FALSE)
tryCatch({
 if(!ready()){
  exe<-normalizePath(file.path(R.home('bin'),'Rscript.exe'),winslash='\\',mustWork=TRUE)
  launch_file<-normalizePath(file.path(root,'launch.R'),winslash='\\',mustWork=TRUE)
  out_file<-normalizePath(file.path(root,'logs/server-output.log'),winslash='\\',mustWork=FALSE)
  err_file<-normalizePath(file.path(root,'logs/server-error.log'),winslash='\\',mustWork=FALSE)
  Sys.setenv(BIOSTAT_NO_BROWSER='1')
  # `system2(wait=FALSE)` can tie the child process lifetime to the launcher
  # console on Windows. Detach through a minimized `start` process so the
  # Shiny service remains alive after this short launcher process exits.
  cmd<-sprintf('start "" /min "%s" --vanilla "%s" > "%s" 2> "%s"',exe,launch_file,out_file,err_file)
  shell(cmd,wait=FALSE)
  cat('Starting Epiquanta...\n');deadline<-Sys.time()+40
  while(!ready()&&Sys.time()<deadline)Sys.sleep(.5)
  if(!ready())stop('Startup did not finish. See logs/server-error.log.')
 }
 cat('Ready: ',url,'\nRunning in background. Close the service from the app when finished.\n',sep='')
 if(!'--no-browser'%in%commandArgs(TRUE))browseURL(url)
},error=function(e){writeLines(conditionMessage(e),'logs/launcher-error.log');message(conditionMessage(e));quit(status=1)})
