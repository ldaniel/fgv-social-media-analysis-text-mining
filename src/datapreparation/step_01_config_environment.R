# clearing everything before starting -----------------------------------------
ClearRStudioEnvironment()

# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

info.username  <- Sys.info()[["user"]]
info.sysname   <- Sys.info()[["sysname"]]
info.machine   <- Sys.info()[["machine"]]
info.encoding  <- getOption("encoding")
directoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
directoryPath  <- stringr::str_replace(directoryPath, "/src/datapreparation", "")

setwd(directoryPath)
getwd()

# check required libraries

list.of.packages <- c("rawr", 'reticulate', 'ggwordcloud')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  print('one or more required package for this project is not installed in your environment.')
  print(new.packages)
  print('would you like to install it?')
  user.reply <- readline(prompt="y/n: ")
  if (user.reply == 'y') {
    install.packages(new.packages)
  }
}

