# Fábio Madeira, 2013

# function that computes STATS for input cross-validation files
# Stats covered: AUC, Cut-off, Contigency matrix, MCC
computeSTATS <- function(input, output, method, redundancy, motifsize, 
                         iterations){
  suppressPackageStartupMessages(library(ROCR))
  
  # input the data
  data <- read.table(input)
  # to be unspecific in the number of input iterations
  predictions <- data[,7:(7 + iterations - 1)]
  labels <- matrix(rep(data[,6], iterations), length(data[,6]), iterations)
  
  # ROCR prediction
  pred <- prediction(predictions, labels)
  auc <- performance(pred,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(mean(auc), digits = 6)
  #auc <- round(sd(auc), digits = 6)
  print(auc)
  
  # for getting the average maximum and average cut-off position
  # gets the maximum average accuracies and the the index position
  perf <- performance(pred,"acc")
  vectx <- c()
  vecty <- c()
  vectpos <- c()
  count <- 0
  tpc <- c()
  fpc <- c()
  tnc <- c()
  fnc <- c()
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
    a <- pred@tp[[count]][val]
    b <- pred@fp[[count]][val]
    c <- pred@tn[[count]][val]
    d <- pred@fn[[count]][val]
    vectx <- c(vectx, x)
    tpc <- c(tpc, a)
    fpc <- c(fpc, b)
    tnc <- c(tnc, c)
    fnc <- c(fnc, d)
  }
  # average of acuracy and cut-off values
  cutoff <- round(mean(vectx), 6)
  print(cutoff)
  #cutoff <- round(sd(vectx), 6)
  maxQ <- round(mean(vecty), 6)
  print(maxQ)
  #maxQ <- round(sd(vecty), 6)
  #print(cutoff)
  #print(maxQ)
  
  # add contigency matrix and MCC scores
  tp <- round(mean(tpc), 6)
  fp <- round(mean(fpc), 6)
  tn <- round(mean(tnc), 6)
  fn <- round(mean(fnc), 6)
  p <- round(mean(tp + fn), 1)
  n <- round(mean(tn + fp), 1)
  # sensibility
  sens <- round((tp / (tp + fn)), 6)
  # specificity
  spec <- round((tn / (fp + tn)), 6)
  # positive predictive value
  ppv <- round((tp / (tp + fp)), 6)
  # negative predictive value
  npv <- round((tn / (fn + tn)), 6) 
  # accuracy
  Q <- round((tp + tn) / (length(pred@labels[[1]])),6)
  # mathews correlation coefficient
  top <- ((tp*tn) - (fp*fn))
  bottom <- sqrt((tp+fn)*(tn+fp)*(tp+fp)*(tn+fn))
  mcc <-  round(top / bottom, 6)
  #print(tp)
  #print(fp)
  #print(tn)
  #print(fn)
  #print(Q)
  #print(mcc)
  
  # output the average STATS scores
  write.table(cbind(method, redundancy, motifsize, 
                    auc, maxQ, cutoff, tp, fp, tn, fn, p, n, sens, spec, ppv, npv, Q, mcc),
              file = output, append = TRUE, sep = "\t",  row.names = FALSE,
              col.names = FALSE, quote = FALSE)
}

args <- commandArgs(TRUE)
input <- args[1]
output <- args[2]
method <- args[3]
motifsize <-args[4]
redundancy <-args[5]
iterations <- as.integer(args[6])

computeSTATS(input, output, method, motifsize, redundancy, iterations)

# example usage
#input <- "./Test/60/6/SVM_training.txt"
#output <- "TEST.txt"
#computeSTATS(input, output, "SVM", 6, 60, 10)
