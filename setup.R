# First-time online install, only if the bundled project library is unavailable.
dir.create('library',showWarnings=FALSE)
.libPaths(c(normalizePath('library'),.libPaths()))
options(repos=c(CRAN='https://cran.r-project.org'),timeout=600)
packages<-c('survey','geepack','lme4','lmerTest','data.table','regmedint','shiny','psych','polycor','lavaan','semTools','sandwich','lmtest','ggplot2','officer','writexl','pROC','dplyr','tidyr','readxl','haven')
missing<-packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))install.packages(missing,lib='library')
stopifnot(all(vapply(packages,requireNamespace,logical(1),quietly=TRUE)))
writeLines(capture.output(sessionInfo()),'session-info.txt')
message('Setup complete. Run source("launch.R").')
