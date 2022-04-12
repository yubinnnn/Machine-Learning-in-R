### SVM ?ǽ? ###
- ?????? Split��?? ?ǽ??ϱ? - Holdout Method, repeated random subsampling validation method(????�� ???????ø?)
- Slack ???? ???Կ? ?ʿ??? Regularization Parametar ?? ��?ϱ?(??ǥ?Ӽ??? p??)
- ?? kernel?Լ??? parameter ?? ��?ϱ?(Linear, polonomial, redial basis, sigmoid 4???? r???? ??????)
- ???�?ȯ(p->cost, ????->gamma, c->coef0, d->degree)

- Holdout method?? k?? ?????Ѵ?(seed?? ?ٲٸ�? k???? split�� ????)
- ù??° split?? seed(12345)?? ?��??? ?ǽ?1�� ?????Ѵ?.
- ?? kernel?? ???ؼ? ��???? parameter?? ?????? ????(cost?? ????.. ???? ????..ǥ?? ?ϱ?!!)
- 10?? split?? training ?? validtion data set ???߷?(for loop ????)
- validation data set???? ???? ???? ??Ÿ?? model ??��
- ??�� model?? ???߷? ǥ(test data set)

- ??��5: ��?? Excel Worksheet
- ??�� ???߷? ???ϰ?
- ?̸?: kajSVM.xlsx
- ?????̸?: SVM Model Building (Virus ??��)
- ???? ????: ?й? ?̸?

#######################################################################

setwd("D:/����ȭ��/�ӽŷ���/R DA-ML/10 SVM")
rm(list=ls())
Tra <- read.csv("Tra1.csv", header = T)
Val <- read.csv("Val1.csv", header = T)
Tes <- read.csv("Tes1.csv", header = T)

D.tra <- Tra[,-c(1)]
D.val <- Val[,-c(1)]
D.tes <- Tes[,-c(1)]

#?Է°? ????�� ��?? x?? y?? ?ҷ??Ѵ?
x <- subset(D.tra, select = -c(Class))
y <- D.tra$Class

#install.packages("e1071")
library(e1071)

#tune ?Լ??? ?̿??Ͽ? parameter ?? ��?ϱ?
tSeed <- 12345
set.seed(tSeed) #?????? ?ҷ??? ??

linear_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="linear",
                    ranges = list(cost=c(50,100,150))) #cost??�� ?ڱ? ??��???? ?? ?? ??��

print(linear_tune) #linear?? parameter?? 100??

linearSVM <- svm(Class~., D.tra, type="C-classification", 
                 kernel = "linear", 
                 cost=100, scale=FALSE)

Predict.tra <- predict(linearSVM, D.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  #96.82152

Predict.val <- predict(linearSVM, D.val)
cTab <- table(Actual=D.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val  #98.54015

################polinomial kernel#########
set.seed(tSeed)
poly_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="polynomial",
                    ranges = list(cost=c(50,100,150),
                                  coef0=c(0,1,2),
                                  gamma = seq(0.1,1,by=0.1),
                                  degree=c(2,3)))

#polynomial???? parameter ??�� ?ȳ־�? ??

poly_tune <- svm(Class~., D.tra, type="C-classification", 
                 kernel = "polynomial", 
                 cost=100, scale=FALSE)

############################################################
set.seed(tSeed)
radial_tune <- tune(svm, train.x = x, train.y = y,
                  kernel="radial",
                  ranges = list(cost=c(50,100,150),
                                gamma = seq(0.1,1,by=0.1)))

print(radial_tune)

modelrSVM <- svm(Class~., D.tra, type="C-classification", 
                 kernel = "radial", 
                 cost=50, gamma=0.5,scale=FALSE)

Predict.tra <- predict(modelrSVM, D.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  #100

Predict.val <- predict(modelrSVM, D.val)
cTab <- table(Actual=D.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val  #92.70073

############################################################
set.seed(tSeed)
sigmoid_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="sigmoid",
                    ranges = list(cost=c(50,100,150),
                                  coef0=c(0,1,2),
                                  gamma = seq(0.1,1,by=0.1)))

print(sigmoid_tune)

modelrSVM <- svm(Class~., D.tra, type="C-classification", 
                 kernel = "sigmoid", 
                 cost=50, gamma=0.1, coef0=2,scale=TRUE)

Predict.tra <- predict(modelrSVM, D.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  #93.39853

Predict.val <- predict(modelrSVM, D.val)
cTab <- table(Actual=D.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val  #97.81022

###########################################################33
D.data <- read.csv("bCancerWC.csv", header=T)
install.packages("doBy")
library(doBy)
pSeed <- 12345
set.seed(pSeed)
text <- D.data
Training <- doBy::sampleBy(~Z, frac = 0.6, replace = FALSE, data = text)
rownames(Training) <- Training$Record
Training <- Training[order(Training$Record),]

vate <- text[-Training$Record,]

Validation <- doBy::sampleBy(~Z,frac = 0.5, replace = FALSE, data = vate)
Validation <- Validation[order(Validation$Record),]
rownames(Validation) <- Validation$Record

Test <- text[-c(Validation$Record,Training$Record),]


Training
Validation
Test

D.tra <- Training[,-c(1)]
D.val <- Validation[,-c(1)]
D.tes <- Test[,-c(1)]

x <- subset(D.tra, select = -c(Class))
y <- D.tra$Class

linear_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="linear",
                    ranges = list(cost=c(50,100,150)))

print(linear_tune)  #cost 50

poly_tune <- tune(svm, train.x = x, train.y = y,
                  kernel="polynomial",
                  ranges = list(cost=c(50,100,150),
                                coef0=c(0,1,2),
                                gamma = seq(0.1,1,by=0.1),
                                degree=c(2,3)))


radial_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="radial",
                    ranges = list(cost=c(50,100,150),
                                  gamma = seq(0.1,1,by=0.1)))

print(radial_tune) #cost = 50 / gamma = 0.5

sigmoid_tune <- tune(svm, train.x = x, train.y = y,
                     kernel="sigmoid",
                     ranges = list(cost=c(50,100,150),
                                   coef0=c(0,1,2),
                                   gamma = seq(0.1,1,by=0.1)))

print(sigmoid_tune) #cost = 50 / coef0 = 2 / gamma = 0.1


modelSVM <- svm(Class~., D.tra, type="C-classification", 
                kernel = "linear", 
                cost=50, gamma=0.1, coef0=2,scale=TRUE)

Predict.tra <- predict(modelrSVM, D.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  #96.09756

Predict.val <- predict(modelrSVM, D.val)
cTab <- table(Actual=D.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val  #94.85294

modelSVM <- svm(Class~., D.tra, type="C-classification", 
                kernel = "linear", 
                cost=50, gamma=0.1, coef0=2,scale=TRUE)

pSeed

Predict.tra <- predict(modelrSVM, D.tra)
cTab <- table(Actual=D.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  #96.09756

Predict.val <- predict(modelrSVM, D.val)
cTab <- table(Actual=D.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val  #94.85294



linear_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="linear",
                    ranges = list(cost=c(50,100,150)))

print(linear_tune)


######################10???? ?????? ��?ؼ? for loop ????###########
getwd()
install.packages("doBy")
D.data <- read.csv("bCancerWC.csv", header=T)
text <- D.data

Training <- doBy::sampleBy(~Class, frac = 0.6, replace = FALSE, data = text)
rownames(Training) <- Training$Record
Training <- Training[order(Training$Record),]

vate <- text[-Training$Record,]

Validation <- doBy::sampleBy(~Class,frac = 0.5, replace = FALSE, data = vate)
Validation <- Validation[order(Validation$Record),]
rownames(Validation) <- Validation$Record

Test <- text[-c(Validation$Record,Training$Record),]

D.tra <- Training[,-c(1)]
D.val <- Validation[,-c(1)]
D.tes <- Test[,-c(1)]

x <- subset(D.tra, select = -c(Class))
y <- D.tra$Class

linear_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="linear",
                    ranges = list(cost=c(50,100,150)))

print(linear_tune)  #cost 100

radial_tune <- tune(svm, train.x = x, train.y = y,
                    kernel="radial",
                    ranges = list(cost=c(50,100,150),
                                  gamma = seq(0.1,1,by=0.1)))

print(radial_tune) #cost = 50 / gamma = 0.3

sigmoid_tune <- tune(svm, train.x = x, train.y = y,
                     kernel="sigmoid",
                     ranges = list(cost=c(50,100,150),
                                   coef0=c(0,1,2),
                                   gamma = seq(0.1,1,by=0.1)))

print(sigmoid_tune)



pSeedlist <- c(12345,12301,12302,12303,12304,12305,12306,12307,12308,12309)
for (pSeed in pSeedlist){
  set.seed(pSeed)
  library(doBy)

  Training <- doBy::sampleBy(~Class, frac = 0.6, replace = FALSE, data = text)
  rownames(Training) <- Training$Record
  Training <- Training[order(Training$Record),]
  
  vate <- text[-Training$Record,]
  
  Validation <- doBy::sampleBy(~Class,frac = 0.5, replace = FALSE, data = vate)
  Validation <- Validation[order(Validation$Record),]
  rownames(Validation) <- Validation$Record
  
  Test <- text[-c(Validation$Record,Training$Record),]
  
  D.tra <- Training[,-c(1)]
  D.val <- Validation[,-c(1)]
  D.tes <- Test[,-c(1)]

  modelSVM <- svm(Class~., D.tra, type="C-classification", 
                   kernel = "linear", 
                   cost=100, scale=TRUE)
  
  Predict.tes <- predict(modelSVM, D.tes)
  cTab <- table(Actual=D.tes$Class, Predict.tes)
  Accu.tes <- sum(diag(cTab))/sum(cTab)*100

  
  #Predict.val <- predict(modelSVM, D.val)
  #cTab <- table(Actual=D.val$Class, Predict.val)
  #Accu.val <- sum(diag(cTab))/sum(cTab)*100

  print(pSeed)
  print(Accu.tes)
  #print(Accu.val)
}


???߷?�� ???Ѵ?!!
6???? ???߷??? 10????Ʈ?? ???´?.
pSeed ?? ?????ؼ? ???߷??? ???��? print ?Ѵ?.
