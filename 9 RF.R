setwd("D:바탕화면/머신러닝/R DA-ML/9 RF")
rm(list=ls())

Tra <- read.csv("Tra1.csv", header = TRUE)
Val <- read.csv("Val1.csv", header = TRUE)
Tes <- read.csv("Tes1.csv", header = TRUE)

R.tra <- Tra[,-c(1)]
R.val <- Val[,-c(1)]
R.tes <- Tes[,-c(1)]

#install.packages("rpart")
library(rpart) #가지치기 할때 이중 하나를 선택

set.seed(12345) #똑같은 결과를 내줌
e_DT <- rpart(Class~., R.tra, method="class",   #method=class는 회귀
              parms = list(split="information")) #Class 빼고 모든 데이터 사용
plot(e_DT)
text(e_DT)

e_DT #Thal=bc인 이유는 T6,T7을 의미 T3은 a를 의미.

Predict.tra <- predict(e_DT, R.tra, type="class") #type=class는 분류
Actual.tra <- R.tra$Class
head(Predict.tra)
head(Actual.tra)

#confusionMatrix(분류결과표)
install.packages("caret")
install.packages("lava")
install.packages("e1071")
library(caret)
library(e1071)

#####Tra 적중률######
cM1 <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease")
cM1 #적중률:0.8708 / 민감도: 0.9024 / 특이도: 0.8438

cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cTab

Predict.tra <- predict(e_DT, R.tra, type="class") #type=class는 분류
Actual.tra <- R.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.tra *100

#######Val 적중률#####
Predict.val <- predict(e_DT, R.val, type="class") #type=class는 분류
Actual.val <- R.val$Class
cTab <- table(Actual.val, Predict.val)  #실제와 예측을 바꾸는 것
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.val *100 #적중률: 68.3333


#########overfitting을 없애는 것##########
printcp(e_DT)  #178개 중 82가 에러임 #가지치기에 도움을 줌
cpval <- e_DT$cptable[which.min(e_DT$cptable[,"xerror"]), "CP"]
cpval

pround.e_DT <- prune(e_DT, cp=cpval)
plot(pround.e_DT, margin = 0.2)
text(pround.e_DT, margin = 0.2)

##################pround.e_DT 적용#######
#####Tra 적중률######
Predict.tra <- predict(pround.e_DT, R.tra, type="class") #type=class는 분류
Actual.tra <- R.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.tra *100 #적중률:82.58427

#######Val 적중률#####
Predict.val <- predict(pround.e_DT, R.val, type="class") #type=class는 분류
Actual.val <- R.val$Class
cTab <- table(Actual.val, Predict.val)  #실제와 예측을 바꾸는 것
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.val *100 #적중률: 71.66667


g_DT <- rpart(Class~., R.tra, method="class",   #method=class는 회귀
              parms = list(split="gini"))

##################################################################################

#DT로 모델을 구축했을 때, Training:82.58 / Validation: 71.67 / 편차: 10.91

#Random Forest는 Random Tree의 Ensemble(앙상블)이다.
- Random Tree는 DT를 무작위로 구축하는 것으로서, 다음과 같이 두 종류의 무작위성이 도입된다
  - Bootstrap sampling 방법
  - F개의 속성~~~~~

install.packages("randomForest")
library(randomForest)

x <- subset(R.tra, select = -c(Class))
y <- R.tra$Class
y <- factor(y)

mtrySeed <- 1234
set.seed(mtrySeed)

bestmtry <- tuneRF(x, y, stepFactor = 1.5, improve=0.01, ntreeTry = 49) #ntree는 가능한 홀수로!
print(bestmtry) #4가 에러가 가장 낮음  / stepFactor를 1.5로 하였기 때문에 1*1.5 = 2 -> 3 -> 4...

nTree <- 49
mTry <- 4
rfSeed <- 12345
set.seed(rfSeed)

rfModel <- randomForest(x,y, ntree = nTree, mtry = mTry)
Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual = R.tra$Class, Predict.tra)
cTab
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra  # RF Tra: 100%
R.tra
levels(R.val$CP) <- levels(R.tra$CP)
levels(R.val$Restecg) <- levels(R.tra$Restecg)
levels(R.val$Slope) <- levels(R.tra$Slope)
levels(R.val$Thal) <- levels(R.tra$Thal)

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual = R.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val   #RF Val: 81.67%

Variance <- abs(Accu.tra-Accu.val)
Variance #편차: 18.34

#overfitting을 제거 => random tree의 깊이를 일괄적으로 제한하자! 왜냐하면 개별적으로 가지치기를 할 수 없기 때문.
#Depth의 수치와 Leaf Node의 최대 개수 사이의 관계 => depth가 D면 Leaf Node는 2의 D승

maxLeaf <- 10
rfModel <- randomForest(x,y, ntree = nTree, mtry = mTry, maxnodes = maxLeaf)
Predict.tra <- predict(rfModel, R.tra)
cTab <- table(Actual = R.tra$Class, Predict.tra)
Accu.tra <- sum(diag(cTab))/sum(cTab)*100
Accu.tra

Predict.val <- predict(rfModel, R.val)
cTab <- table(Actual = R.val$Class, Predict.val)
cTab
Accu.val <- sum(diag(cTab))/sum(cTab)*100
Accu.val

nTree; mTry; maxLeaf  #91.57303
Variance <- abs(Accu.tra-Accu.val) #81.66667
Variance  #9.906367
