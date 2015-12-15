# Fábio Madeira, 2013

# ANN training with RSNNS
# k -> as in k-fold cross validation is the number of partitions to be done to the data
# k = 1 -> is the same as 'loocv' (leave-one-out cross validation)
crossValidationANN <- function(k, input, output, hidden, iterations,
                               rate, momentum, lenPos, lenNeg){
  # inserting the necessary libraries
  suppressPackageStartupMessages(library(Rcpp))
  suppressPackageStartupMessages(library(RSNNS))
  
  # input data
  # expects a csv file containing binary encodings for the data
  D <- read.csv(input, header=FALSE)
  
  # split in positives and negatives: csv positives followed by negatives
  total <- length(1:nrow(D))
  P <- D[(1:lenPos),]
  N <- D[((lenPos+1):total),]
  
  # shuffle the data
  P <- P[sample(1:nrow(P),length(1:nrow(P))),1:ncol(P)]
  N <- N[sample(1:nrow(N),length(1:nrow(N))),1:ncol(N)]
  
  # setting the seed value
  seed <- 2
  set.seed(seed)
  setSnnsRSeedValue(seed)
  
  # to create always a bigger list than the slice * k
  print("slice start end len(test) len(train)")
  # positives
  slicePos <- round(lenPos / k)
  if (slicePos == 0){
    slicePos = 1
  }
  maximum = slicePos * k
  if (maximum < lenPos){
    slicePos = slicePos + 1
  }
  
  # negatives
  sliceNeg <- round(lenNeg / k)
  if (sliceNeg == 0){
    sliceNeg = 1
  }
  maximum = sliceNeg * k
  if (maximum < lenNeg){
    sliceNeg = sliceNeg + 1
  }
  
  # split for train and test: train removed from all and test selected from all
  for (i in 1:k){
    # positives
    startPos <- (slicePos * (i - 1)) + 1
    endPos <- (slicePos * (i - 0))
    if (endPos <= lenPos){
      train_P <- P
      for (j in startPos:endPos){
        train_P <- train_P[-startPos,]
      }
      test_P <- P[startPos:endPos,]
    }
    else {
      endPos <- lenPos 
      train_P <- P
      for (j in startPos:endPos){
        train_P <- train_P[-startPos,]
      }
      test_P <- P[startPos:endPos,]
    }
    print(c(slicePos, startPos, endPos, nrow(test_P), nrow(train_P)))
    
    # negatives
    startNeg <- (sliceNeg * (i - 1)) + 1 
    endNeg <- (sliceNeg * (i - 0))
    if (endNeg <= lenNeg){
      train_N <- N
      for (l in startNeg:endNeg){
        train_N <- train_N[-startNeg,]
      }
      test_N <- N[startNeg:endNeg,]
    }
    else {
      endNeg <- lenNeg 
      train_N <- N
      for (l in startNeg:endNeg){
        train_N <- train_N[-startNeg,]
      }
      test_N <- N[startNeg:endNeg,]
    }
    print(c(sliceNeg, startNeg, endNeg, nrow(test_N), nrow(train_N)))
    
    # join positives and negatives for training and testing
    train <- rbind(train_P, train_N)
    test <- rbind(test_P, test_N)
    
    # split values and labels: currently for ONE labels
    max <- ncol(train)
    train_V <- train[,(1:(max-1))]
    train_L <- train[,max]
    max <- ncol(test)
    test_V <- test[,(1:(max-1))]
    test_L <- test[,(max)]
    
    # ANN training and validation
    # examples of learning functions available:
    # BackpropMomentum, Std_Backpropagation, BackpropWeightDecay, BackpropChunk, SCG
    model <- mlp(train_V, train_L, size=hidden, maxit=iterations,
                 learnFunc="BackpropMomentum",learnFuncParams=c(rate, momentum),
                 inputsTest=test_V, targetsTest=test_L)
    
    predictions <- predict(model,test_V)
    result <- cbind(test_L, predictions[,1])
    #print(result)
    write.table(result, file = output, append = TRUE, sep = "\t",  row.names = TRUE,
                col.names = FALSE, quote = FALSE)
  }
}

args <- commandArgs(TRUE)
k <- as.integer(args[1])
input <- args[2]
output <- args[3]
hidden <- as.integer(args[4])
iterations <- as.integer(args[5])
rate <- as.numeric(args[6])
momentum <- as.numeric(args[7])
lenPos <- as.integer(args[8])
lenNeg <- as.integer(args[9])

crossValidationANN(k, input, output, hidden, iterations, rate, momentum, lenPos, lenNeg)

# example usage
#input <- "./Test/60/6/ANN_POS_NEG.csv"
#output <- "./Test/60/6/ANN_11.txt"
#crossValidationANN(10, input, output, 12, 5, 0.2, 0.1, ?, ?)

