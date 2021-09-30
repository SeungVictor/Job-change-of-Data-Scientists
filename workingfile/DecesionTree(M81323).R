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


str(augtest)


augtest

library(tree)

tree1<-tree(target~., data = augtrain, split = c("deviance"),
            na.action = na.pass,
            control = tree.control(nobs = nrow(augtrain),
                                 minsize = 5, 
                                 mindev = 0.002))


plot(tree1)
text(tree1)

print(tree1)

summary(tree1)
str(augtest)

tree2<-predict(tree1, augtest[, -1], type = "class")

summary(tree2)
print(tree2)
augtest
str(augtest)

tree3<-cbind(augtest[,1],tree2)
#테스트데이터의 실제값과 예측한 데이터(tree2) 묶기
#첫번째해은 실제값의 actural값과 tree2과 합쳐서 tree3로 묶어서 데이터프레임으로 인식해라
tree3
tree3<-as.data.frame(tree3)
print(tree3)



with(tree3, table(V1,tree2))
#missclassification table을 이용해 error 계산하기
 403+3418 = 3821
3821/4470
#정확도 85%?
















