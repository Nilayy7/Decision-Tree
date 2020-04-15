install.packages("C50")
install.packages("party")
library(party)
library(C50)
library(caret)
library(rpart)
company<- Company_Data

Highsales<- ifelse(company$Sales <9,"No","Yes")
Highsales

company1<- data.frame(company[2:11],Highsales)
View(company1)

inTrainingLocal <- createDataPartition(company1$Highsales,p=.75,list = F)
training <- company1[inTrainingLocal,]
testing <- company1[-inTrainingLocal,]

#Model Building
model <- C5.0(training$Highsales~.,data=training) #Trails- Boosting Parameter

#Generate Model Summary
summary(model)

#Predict for test data
pred <- predict.C5.0(model,testing[,-11])
a <- table(testing$Highsales,pred)
sum(diag(a))/sum(a) #0.7272727
plot(model)

#Bagging
acc<-c()
for(i in 1:400)
{
  print(i)
  #Data partion
  inTrainingLocal <-createDataPartition(company1$Highsales,p=.85,list=F)
  training1 <-company1[inTrainingLocal,]
  testing<-company1[-inTrainingLocal,]
  #Model Building
  fittree <-C5.0(training1$Highsales~.,data=training1,trails=20)
  #predicting
  pred<- predict.C5.0(fittree,testing[,-11])
  a<-table(testing$Highsales,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)

#Boosting
#Data Partition for model building and testing
inTrainingLocal <-createDataPartition(company1$Highsales,p=.75,list=F)
training<-company1[inTrainingLocal,]
testing<-company1[-inTrainingLocal,]

#Model
model<-C5.0(training$Highsales~.,data=training,trials=10)

#Generate model summary
summary(model)

#predict test data set
pred<-predict.C5.0(model,testing[,-11])
b <- table(testing$Highsales,pred)
sum(diag(b))/sum(b) #0.8484848
plot(model)


#Model1
model1<-C5.0(training$Highsales~.,data=training,trials=20)

#Generate model summary
summary(model1)

#predict test data set
pred1<-predict.C5.0(model1,testing[,-11])
c <- table(testing$Highsales,pred1)
sum(diag(c))/sum(c) #0.858585
plot(model)

#Model2
model2<-C5.0(training$Highsales~.,data=training,trials=30)

#Generate model summary
summary(model2)

#predict test data set
pred2<-predict.C5.0(model2,testing[,-11])
d <- table(testing$Highsales,pred2)
sum(diag(d))/sum(d) #0.868585

plot(model)

