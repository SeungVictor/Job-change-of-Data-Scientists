#train데이터와 test데이터csv파일을 엑셀에서 직접가공했습니다
#id, city number 삭제
#gender male, female이외 삭제
#enrolled uni 삭제
# null값이 하나라도 있는행은 삭제하여 트레이닝 데이터 8937개를
#반반씩 testdata와 traindata로 임의로 쪼갰습니다
# 타겟변수 포함 변수는 11개로 임의조정하였습니다


setwd("Z:/home/Drive/Spark/AIBI/Data mining/조모임")
augtrain<-read.csv("aug_train(ps).csv", header = T)
str(augtrain)
head(augtrain,3);tail(augtrain,3)

augtest<-read.csv("aug_test(ps).csv", header = T)
str(augtest)


library(reshape)
augtrain<-as.data.frame(augtrain)

target<-as.factor(augtrain[,11])
city_development_index<-as.numeric(augtrain[,1])
gender<-as.factor(augtrain[,2])
relevent_experience<-as.factor(augtrain[,3])
education_level<-as.factor(augtrain[,4])
major_discipline<-as.factor(augtrain[,5])
experience<-as.factor(augtrain[,6])
company_size<-as.factor(augtrain[,7])
company_type<-as.factor(augtrain[,8])
last_new_job<-as.factor(augtrain[,9])
training_hours<-as.numeric(augtrain[,10])

augtrain<-cbind(target,city_development_index,gender,relevent_experience,education_level,major_discipline,experience,company_size,company_type,last_new_job,training_hours, augtrain[,-1:-11])


str(augtrain)

augtest<-as.data.frame(augtest)
target<-as.factor(augtest[,11])
city_development_index<-as.numeric(augtest[,1])
gender<-as.factor(augtest[,2])
relevent_experience<-as.factor(augtest[,3])
education_level<-as.factor(augtest[,4])
major_discipline<-as.factor(augtest[,5])
experience<-as.factor(augtest[,6])
company_size<-as.factor(augtest[,7])
company_type<-as.factor(augtest[,8])
last_new_job<-as.factor(augtest[,9])
training_hours<-as.numeric(augtest[,10])

augtest<-cbind(target,city_development_index,gender,relevent_experience,education_level,major_discipline,experience,company_size,company_type,last_new_job,training_hours, augtest[,-1:-11])


library(rpart)
moverpart<-rpart(target~., data=augtrain, method="class",
                 control=rpart.control(minsplit =10, maxdepth =5 ))
plot(moverpart);text(moverpart)


print(moverpart)



printcp(moverpart)
plotcp(moverpart)
#cross-validation


#apply cross-validation and after~
moverpart<-prune(moverpart, cp=moverpart$cptable[which.min(moverpart$cptable[,"xerror"]),"CP"])
plot(moverpart);text(moverpart)

summary(moverpart)

movepred<-predict(moverpart, augtest[,-1], type = "class")
summary(movepred)


tree3<-cbind(augtest[,1],movepred)
tree3<-as.data.frame(tree3)
print(tree3)


library(rattle)
library(rpart.plot)
library(RColorBrewer)
rpart.plot(moverpart)
fancyRpartPlot(moverpart)
with(tree3, table(V1, movepred))
# 정확도 429+3402/4470
(429+3402)/4470


 
















