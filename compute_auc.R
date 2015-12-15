# Fábio Madeira, 2013

# function that prints the AUC
methodAUC <- function(target, label, input, output, 
                      hidden, iterations, rate, momentum){
  suppressPackageStartupMessages(library(ROCR))
  
  data <- read.table(input)
  p <- data[,target]
  l <- data[,label]
  
  # ROCR prediction
  pred <- prediction(p, l)
  perf <- performance(pred,"tpr","fpr")
  
  auc <- performance(pred,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc, digits = 5)
  print(auc)
  
  write.table(cbind(hidden, iterations, rate, momentum, auc), 
              file = output, append = TRUE, sep = "\t",  row.names = FALSE,
              col.names = FALSE, quote = FALSE)
}

args <- commandArgs(TRUE)

input <- args[1]
output <- args[2]
hidden <- args[3]
iterations <-args[4]
rate <- args[5]
momentum <- args[6]

methodAUC(3, 2, input, output, hidden, iterations, rate, momentum)
