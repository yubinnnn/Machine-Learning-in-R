library(tensorflow)
library(keras)

Tra <- read.csv("Tra1.csv", header=TRUE)
Val <- read.csv("Val1.csv", header=TRUE)
Tes <- read.csv("Tes1.csv", header=TRUE)

Tra$Cancer <- ifelse(Tra$Cancer==2,0,1)
Val$Cancer <- ifelse(Val$Cancer==2,0,1)
Tes$Cancer <- ifelse(Tes$Cancer==2,0,1)

x_Tra <- as.matrix(subset(Tra, select = -c(Record, Cancer)))
y_Tra <- as.matrix(Tra$Cancer)
x_Val <- as.matrix(subset(Val, select = -c(Record, Cancer)))
y_Val <- as.matrix(Val$Cancer)
x_Tes <- as.matrix(subset(Tes, select = -c(Record, Cancer)))
y_Tes <- as.matrix(Tes$Cancer)

pSeed <- 13579
tf$random$set_seed(pSeed)

model <- keras_model_sequential() %>%
  layer_dense(units=20, input_shape = c(9, NULL)) %>%
  layer_dense(units=15, activation = "relu") %>%
  layer_dense(units=7, activation = "relu") %>%
  layer_dense(units=1, activation = "sigmoid")

lRate <- 0.1
model %>% compile(optimizer = optimizer_sgd(lr=lRate),
                  loss="binary_crossentropy",
                  metrics=c("accuracy"))

###########################################
install.packages("doBy")
D.data <- read.csv("bCancerWC.csv", header=T)
text <- D.data
pSeedlist <- c(12345,12301,12302,12303,12304,12305,12306,12307,12308,12309)
for (pSeed in pSeedlist){
  tf$random$set_seed(pSeed)
  library(doBy)
  
  Tra <- doBy::sampleBy(~Z, frac = 0.6, replace = FALSE, data = text)
  rownames(Tra) <- Tra$Record
  Tra <- Tra[order(Tra$Record),]
  
  vate <- text[-Tra$Record,]
  
  Val <- doBy::sampleBy(~Z,frac = 0.5, replace = FALSE, data = vate)
  Val <- Val[order(Val$Record),]
  rownames(Val) <- Val$Record
  
  Tes <- text[-c(Val$Record,Tra$Record),]
  
  Tra$Cancer <- ifelse(Tra$Cancer==2,0,1)
  Val$Cancer <- ifelse(Val$Cancer==2,0,1)
  Tes$Cancer <- ifelse(Tes$Cancer==2,0,1)
  
  x_Tra <- as.matrix(subset(Tra, select = -c(Record, Cancer)))
  y_Tra <- as.matrix(Tra$Cancer)
  x_Val <- as.matrix(subset(Val, select = -c(Record, Cancer)))
  y_Val <- as.matrix(Val$Cancer)
  x_Tes <- as.matrix(subset(Tes, select = -c(Record, Cancer)))
  y_Tes <- as.matrix(Tes$Cancer)
  
  model <- keras_model_sequential() %>%
    layer_dense(units=20, input_shape = c(9, NULL)) %>%
    layer_dense(units=15, activation = "relu") %>%
    layer_dense(units=7, activation = "relu") %>%
    layer_dense(units=1, activation = "sigmoid")
  
  lRate <- 0.1
  model %>% compile(optimizer = optimizer_sgd(lr=lRate),
                    loss="binary_crossentropy",
                    metrics=c("accuracy"))
  
  cat(sprintf("\n Seed=%5d",pSeed))

  for (i in 1:10){
    
  nEpochs <- 50
  model %>% fit(x_Tra, y_Tra, 
                epochs=nEpochs, verbose=0)
  
  accTra <- model %>% evaluate(x_Tra, y_Tra, verbose=0)
  accVal <- model %>% evaluate(x_Val, y_Val, verbose=0)
  
  Accu.tra <- round(accTra$accuracy*100,digits=2)
  Accu.val <- round(accVal$accuracy*100,digits=2)
  
  Upto <- nEpochs*i
  cat(sprintf("\n Epoch : %5d   Train : %6.2f%%   Valid : %6.2f%%",
              Upto, Accu.tra, Accu.val))
  }
}

