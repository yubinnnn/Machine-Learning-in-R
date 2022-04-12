setwd("D:/πŸ≈¡»≠∏È/∏”Ω≈∑Ø¥◊/R DA-ML/12 ANN")
rm(list=ls())
Tra <- read.csv("Tra1.csv", header=TRUE)
Val <- read.csv("Val1.csv", header=TRUE)
Tes <- read.csv("Tes1.csv", header=TRUE)

D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]
D.tes <- Tes[,-c(1)]

#install.packages("neuralnet")
library(neuralnet)

wSeed <- 13579

D.tra$Class <- ifelse(D.tra$Class==2,0,1)
D.val$Class <- ifelse(D.val$Class==2,0,1)

H42 <- c(4,2); Thres <- 0.1; Step <- 100000; Rate <- 0.01
set.seed(wSeed)
BPnn <- neuralnet(Class~., D.val, hidden = H42, threshold = Thres, stepmax = Step, learningrate = Rate,
                  algorithm = "backprop", err.fct="ce", act.fct = "logistic", linear.output = FALSE)
plot(BPnn)

cutoff <- 0.5
Output.tra <- predict(BPnn, D.tra)
head(Output.tra)
Predict.tra <- ifelse(Output.tra>=cutoff,1,0)
head(Predict.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- round(sum(diag(cTab))/sum(cTab)*100,digits = 2)
Accu.tra #97.79951

Output.val <- predict(BPnn, D.val)
head(Output.val)
Predict.val <- ifelse(Output.val>=cutoff,1,0)
cTab <- table(Actual=D.val$Class, Predict.val)
Accu.val <- round(sum(diag(cTab))/sum(cTab)*100,digits = 2)
Accu.val #97.79951

####################################################################3
install.packages("doBy")
library(doBy)
D.data <- read.csv("bCancerWC.csv", header=T)
text <- D.data
pSeedlist <- c(12345,12301,12302,12303,12304,12305,12306,12307,12308,12309)
for (pSeed in pSeedlist){
  set.seed(pSeed)
  library(doBy)
  
  Training <- doBy::sampleBy(~Z, frac = 0.6, replace = FALSE, data = text)
  rownames(Training) <- Training$Record
  Training <- Training[order(Training$Record),]
  
  vate <- text[-Training$Record,]
  
  Validation <- doBy::sampleBy(~Z,frac = 0.5, replace = FALSE, data = vate)
  Validation <- Validation[order(Validation$Record),]
  rownames(Validation) <- Validation$Record
  
  Test <- text[-c(Validation$Record,Training$Record),]
  
  D.tra <- Training[,-c(1)]
  D.val <- Validation[,-c(1)]
  D.tes <- Test[,-c(1)]
  
  D.tra$Class <- ifelse(D.tra$Class==2,0,1)
  D.val$Class <- ifelse(D.val$Class==2,0,1)
  D.tes$Class <- ifelse(D.tes$Class==2,0,1)
  
  wSeed <- 12345
  set.seed(wSeed)
  
  H42 <- c(4,2); Thres <- 0.1; Step <- 100000; Rate <- 0.01
  
  
  BPnn <- neuralnet(Class~., D.val, hidden = H42, threshold = Thres, stepmax = Step, learningrate = Rate,
                    algorithm = "backprop", err.fct="ce", act.fct = "logistic", linear.output = FALSE)
  
  cutoff <- 0.5
  Output.tra <- predict(BPnn, D.tra)
  Predict.tra <- ifelse(Output.tra>=cutoff,1,0)
  cTab <- table(Actual=D.tra$Class, Predict.tra)
  Accu.tra <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
  
  Output.val <- predict(BPnn, D.val)
  Predict.val <- ifelse(Output.val>=cutoff,1,0)
  cTab <- table(Actual=D.val$Class, Predict.val)
  Accu.val <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
  
  Output.tes <- predict(BPnn, D.tes)
  Predict.tes <- predict(BPnn, D.tes)
  Predict.tes <- ifelse(Output.tes>=cutoff,1,0)
  cTab <- table(Actual=D.tes$Class, Predict.tes)
  Accu.tes <- round(sum(diag(cTab))/sum(cTab)*100, digits = 2)
  
  cat(sprintf("\n Thres=%4.2f Seed=%5d", Thres, pSeed))
  cat("    Tra=", Accu.tra)
  cat("    Val=", Accu.val)
  cat("    Tes=", Accu.tes)
}
