#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
library(caret)
library(dplyr)
library(C50)
library(tree)
library(party)
hist(Fraud_check$Taxable.Income,main = "Taxable Income")

#If taxable income less than or equal to 30000 then Risky else Good.
RG = ifelse(Fraud_check$Taxable.Income <= 30000,"Risky","Good")
FraudCheck = data.frame(Fraud_check,RG)
View(FraudCheck)
table(FraudCheck$RG)

train <- FraudCheck[1:300,]
test <- FraudCheck[301:600,]

decn_tree <- ctree(RG~Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FraudCheck)
summary(decn_tree)
plot(decn_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients

# using the training Data 
decn_tree1 <- ctree(RG~Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = train)
summary(decn_tree1)
plot(decn_tree1)

#Predictions on test data
pred <- as.data.frame(predict(decn_tree1,newdata=test))
pred["final"] <- NULL
pred_test_df <- predict(decn_tree1,newdata=test)


mean(pred_test_df==test$RG)
#Accuracy = 82%

#Crosstable
CrossTable(test$RG,pred_test_df)

#Confusion Matrix
confusionMatrix(test$RG,pred_test_df)

