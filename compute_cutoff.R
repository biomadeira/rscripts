# Fábio Madeira, 2013

# help determine optimal training threshold by plotting accuracy vs cutoff
computeCOFF <- function(input, output, method, redundancy, motifsize, iterations){
  suppressPackageStartupMessages(library(ROCR))
  
  # input the data
  data <- read.table(input)
  # to be unspecific in the number of input iterations
  predictions <- data[,7:(7 + iterations - 1)]
  labels <- matrix(rep(data[,6], iterations), length(data[,6]), iterations)
  
  # ROCR prediction
  pred <- prediction(predictions, labels)
  perf <- performance(pred,"acc")
  
  # for getting the average maximum and average cut-off position
  # gets the maximum average accuracies and the the index position
  vectx <- c()
  vecty <- c()
  vectpos <- c()
  count <- 0
  for (value in perf@y.values){
    y = max(value)
    p = which.max(value)
    vecty <- c(vecty, y)
    vectpos <- c(vectpos, p)
  }
  # gets the coordinates of the indexed positions
  for (value in vectpos){
    val = as.integer(value)
    count = count + 1
    x <- perf@x.values[[count]][val]
    vectx <- c(vectx, x)
  }
  # average of accuracy and cut-off values
  meanx <- mean(vectx)
  meany <- mean(vecty)
  print(meanx)
  print(meany)
  
  # output the average ROC curves in pdf for svg compatibility
  title <- paste("10-fold cross-validation:", method, "-", redundancy, "-", motifsize)
  pdf(output)#, width=320, height=320)
  
  plot(perf, col="black", lwd=2, main=title, 
       xaxs="i", yaxs="i", avg="vertical", spread.estimate="stddev") #"boxplot" or "stderror"
  abline(v=meanx, col="black", lty=2)
  if (method == "ANN"){
    legend(0.3, 0.57, c(paste("maxQ =", round(meany, 3), ""), paste("cut-off =", round(meanx, 3), "")), 
           col="black", lwd=2, bg="white")#, bty = "n")
  }
  else {
    legend(-0.5, 0.57, c(paste("maxQ =", round(meany, 3), ""), paste("cut-off =", round(meanx, 3), "")), 
           col="black", lwd=2, bg="white")#, bty = "n")
  }
  garbage <- dev.off()
  
}

args <- commandArgs(TRUE)
input <- args[1]
output <- args[2]
method <- args[3]
motifsize <-args[4]
redundancy <-args[5]
iterations <- as.integer(args[6])

computeCOFF(input, output, method, motifsize, redundancy, iterations)

# example usage
#input <- "./Test/60/6/SVM_training.txt"
#output <- "./Test/60/6/SVM_coff.pdf"
#input <- "./Datasets/SCS_1.txt"
#computeCOFF(input, output, "SVM", 6, 60, 10)

