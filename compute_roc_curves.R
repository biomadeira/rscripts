# Fábio Madeira, 2013

# function that computes ROC curves for input cross-validation files
computeROC <- function(input, output, method, redundancy, motifsize, iterations){
  suppressPackageStartupMessages(library(ROCR))
  
  # input the data
  data <- read.table(input)
  # to be unspecific in the number of input iterations
  predictions <- data[,7:(7 + iterations - 1)]
  labels <- matrix(rep(data[,6], iterations), length(data[,6]), iterations)
  
  # ROCR prediction
  pred <- prediction(predictions, labels)
  perf <- performance(pred,"tpr","fpr") 
  auc <- performance(pred,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(mean(auc), digits = 6)
  #print(auc)
  
  # output the average ROC curves in pdf for svg compatibility
  title <- paste("10-fold cross-validation:", method, "-", redundancy, "-", motifsize)
  pdf(output)#, width=320, height=320)
  
  plot(perf, col="black", lwd=2, main=title, xlim=c(-0.01, 1.01), ylim=c(-0.01, 1.01), 
       xaxs="i", yaxs="i", avg="vertical", spread.estimate="stddev") #"boxplot" or "stderror"
  #plot(perf, col="blue", lwd=2, main=title, xlim=c(-0.01, 1.01), ylim=c(-0.01, 1.01), 
  #     xaxs="i", yaxs="i", colorize=TRUE) #"boxplot" or "stderror"
  abline(0,1, col="black", lty=2)
  legend(0.60, 0.2, paste("AUC =", round(auc, 4), ""), 
         col="black", lwd=2, bty = "n")
  garbage <- dev.off()
}

args <- commandArgs(TRUE)
input <- args[1]
output <- args[2]
method <- args[3]
motifsize <-args[4]
redundancy <-args[5]
iterations <- as.integer(args[6])

computeROC(input, output, method, motifsize, redundancy, iterations)

# example usage
#input <- "./Test/60/6/SVM_training.txt"
#output <- "./Test/60/6/SVM_training.pdf"
#computeROC(input, output, "SVM", 6, 60, 10)

#input <- "./Test/60/6/SVM_training.txt"
#output <- "./Test/60/6/SVM_training.pdf"
#computeROC(input, output, "SVM", 6, 60, 10)

