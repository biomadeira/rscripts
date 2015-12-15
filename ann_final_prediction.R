# Fábio Madeira, 2013

# final prediction
predictionANN <- function(input, output, train, hidden, iterations,
                          rate, momentum, uniprot, site, res, net){
  # inserting the necessary libraries
  suppressPackageStartupMessages(library(Rcpp))
  suppressPackageStartupMessages(library(RSNNS))
  
  # input data
  # expects a csv file containing binary encodings for the data
  intrain <- read.csv(train, header=FALSE)
  intest <- read.csv(input, header=FALSE)
  
  # split values and labels: currently for ONE labels  
  max <- ncol(intrain)
  train_V <- intrain[,(1:(max-1))]
  train_L <- intrain[,max]
  max <- ncol(intest)
  test_V <- intest[,(1:(max-1))]
  test_L <- intest[,(max)]
  
  # setting the seed value
  seed <- 2
  set.seed(seed)
  setSnnsRSeedValue(seed)
  
  # ANN training and validation
  # examples of learning functions available:
  # BackpropMomentum, Std_Backpropagation, BackpropWeightDecay, BackpropChunk, SCG
  model <- mlp(train_V, train_L, size=hidden, maxit=iterations,
               learnFunc="BackpropMomentum",learnFuncParams=c(rate, momentum))
  
  predictions <- predict(model,test_V)
  result <- cbind(test_L, predictions[,1])
  
  #print(result)
  write.table(cbind(uniprot, site, res, round(result[,ncol(result)], 6)), file = output, append = TRUE, sep = "\t",  row.names = FALSE,
              col.names = FALSE, quote = FALSE)
}

args <- commandArgs(TRUE)
input <- args[1]
output <- args[2]
train <- args[3]
hidden <- as.integer(args[4])
iterations <- as.integer(args[5])
rate <- as.numeric(args[6])
momentum <- as.numeric(args[7])
uniprot <- as.character(args[8])
site <- as.character(args[9])
res <- as.character(args[10])
net <- args[11]

predictionANN(input, output, train, hidden, iterations, rate, 
              momentum, uniprot, site, res, net)
