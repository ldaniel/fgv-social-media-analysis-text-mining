## ---- auxiliary_functions.R

# functions -----------------------------------------------------------
ClearRStudioEnvironment <- function() {
  
  # clear environment and memory
  rm(list=ls())
  invisible(gc())
  
  # clear console screen
  cat("\014")
  
  # clear plots
  while (!is.null(dev.list()))  
    dev.off()
}

WriteLog <- function(TaskName, TotalTime, AdditionalInfo) {
  
  log  <- paste('[', Sys.time(), '] ',
                'Task: ', TaskName, ' | ', 
                'Total time : ', TotalTime, ' | ',
                'Memory used: ', pryr::mem_used(), ' bytes | ',
                AdditionalInfo,
                sep = "")
  
  write(log, file = "log.txt", append = TRUE)
}

## ---- end-of-ClearRStudioEnvironment