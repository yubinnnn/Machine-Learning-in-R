#UCI Machine Learning Repository가 데이터 출처

#전처리 후
- record 개수: 297개
- 속성 개수: 15개/ 목표속성: class
- 범주형으로 만들기(Gender, CP, Exang 등등)

############수업#############

setwd("D:/바탕화면/머신러닝/R DA-ML/8 DT")
rm(list=ls())


D.data <- read.csv("HeartD.csv", header = TRUE)

prop.table(D.data$Class)  #데이터 비율 알아내기

Tra <- read.csv("Tra1.csv", header = TRUE)
Val <- read.csv("Val1.csv", header = TRUE)
Tes <- read.csv("Tes1.csv", header = TRUE)

#첫번째 칼럼 빼기
head(Tra)
D.tra <- Tra[,- c(1)]
D.val <- Val[,- c(1)]
D.tes <- Tes[,- c(1)]

install.packages("rpart")
library(rpart) #가지치기 할때 이중 하나를 선택

set.seed(12345) #똑같은 결과를 내줌
e_DT <- rpart(Class~., D.tra, method="class",   #method=class는 회귀
              parms = list(split="information")) #Class 빼고 모든 데이터 사용
plot(e_DT)
text(e_DT)

e_DT #Thal=bc인 이유는 T6,T7을 의미 T3은 a를 의미.

Predict.tra <- predict(e_DT, D.tra, type="class") #type=class는 분류
Actual.tra <- D.tra$Class
head(Predict.tra)
head(Actual.tra)

#confusionMatrix(분류결과표)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

#####Tra 적중률######
cM1 <- confusionMatrix(Predict.tra, Actual.tra, positive = "Disease")
cM1 #적중률:0.8708 / 민감도: 0.9024 / 특이도: 0.8438

cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cTab

Predict.tra <- predict(e_DT, D.tra, type="class") #type=class는 분류
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.tra *100

#######Val 적중률#####
Predict.val <- predict(e_DT, D.val, type="class") #type=class는 분류
Actual.val <- D.val$Class
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
Predict.tra <- predict(pround.e_DT, D.tra, type="class") #type=class는 분류
Actual.tra <- D.tra$Class
cTab <- table(Actual.tra, Predict.tra)  #실제와 예측을 바꾸는 것
cM.tra <- confusionMatrix(t(cTab), positive = "Disease")
Accu.tra <- cM.tra$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.tra *100 #적중률:82.58427

#######Val 적중률#####
Predict.val <- predict(pround.e_DT, D.val, type="class") #type=class는 분류
Actual.val <- D.val$Class
cTab <- table(Actual.val, Predict.val)  #실제와 예측을 바꾸는 것
cM.val <- confusionMatrix(t(cTab), positive = "Disease")
Accu.val <- cM.val$overall["Accuracy"]  #str(cM.tra)를 해보면 overall에 Accuracy가 있음
cTab
Accu.val *100 #적중률: 71.66667


g_DT <- rpart(Class~., D.tra, method="class",   #method=class는 회귀
              parms = list(split="gini"))