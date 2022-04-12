setwd("D:/바탕화면/머신러닝/R DA-ML/7 Assoc")
rm(list=ls())

install.packages("arules")
library(arules)

A.data <- read.transactions("POS2020.csv", sep=",",
                            format = "basket",
                            skip = 1,
                            cols=1)

A.data
summary(A.data)

inspect(A.data)
inspect(A.data[1:5]) #5개만 보이게 하는 것

itemFrequencyPlot(A.data)
itemFrequencyPlot(A.data, support=0.3) #0.3이상인 것만 보이게 하는 것
itemFrequencyPlot(A.data, topN=10) #상위 10개만 출력

apriori(A.data) #support = 0.1 , confidence = 0.8, rule = 75개
apriori(A.data, parameter = list(support=0.2)) #rule이 9개라서 너무 적음
apriori(A.data, parameter = list(confidence = 0.9)) #rule은 41개
apriori(A.data, parameter = list(support=0.2, confidence = 0.5)) #rule = 20개

rule20 <- apriori(A.data, parameter = list(support=0.2, confidence = 0.5))
inspect(rule20) #count = support*50
inspect(sort(rule20, by="lift")) #lift를 기준으로 내림차순
inspect(sort(rule20, by="lift")[1:5]) #상위 5개만 출력
inspect(sort(rule20, decreasing = FALSE, by="lift")[1:5]) #오름차순 후 5개출력

sodaR <- subset(rule20, items %in% "soda") #soda가 있는 rule을 찾는 것 #if then 둘다 포함해서 나옴
inspect(sodaR)

sodaRc9 <- subset(rule20, items %in% "soda" & confidence>0.9)
inspect(sodaRc9)

scR <- subset(rule20, items %in% c("soda","cracker"))
inspect(scR)              

scaR <- subset(rule20, items %ain% c("soda","cracker")) #all in을 의미 #둘다 포함
inspect(scaR)  

thenbeerR <-  subset(rule20, rhs %in% "beer") #beer를 팔기 위해서 무엇 옆에 진열을 해야할까? -> confidence가 높은 것 선택 
inspect(thenbeerR)

wis <-  subset(rule20, lhs %in% "whiskey")
inspect(wis)

erR <- subset(rule20, items %pin% "er") #부분적으로 er이 들어간 것 - pin
inspect(erR)
