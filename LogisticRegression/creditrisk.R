library(Amelia)

library(ROCR)

setwd("C:/Users/Shilpa/Documents/R/usecases")
#train<-read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv")
#View(train)
train<-read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv" , na.strings = c(""," "))
test<-read.csv("R_Module_Day_8.2_Credit_Risk_Validate_data.csv" , na.strings = c(""," "))
predict<-read.csv("R_Module_Day_8.1_Credit_Risk_Test_data.csv" , na.strings = c(""," "))
View(train)
View(test)
View(predict)
summary(train)

train$Credit_History=as.factor(train$Credit_History)
class(train$Credit_History)

train$Credit_History[is.na(train$Credit_History)]="1"
summary(train$Credit_History)


train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)]

table(train$Loan_Status)
missmap(train, main="Missing values vs observed")
summary(train)


train$LoanAmount[is.na(train$LoanAmount)]<-median(train$LoanAmount,na.rm = TRUE)
train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)]<-median(train$Loan_Amount_Term,na.rm = TRUE)
train$Gender[is.na(train$Gender)]<-"Male"

train$Gender[max(table(train$Gender))]
table(train$Gender)
#train$Married[is.na(train$Married)]<-"YES"
train$Married[is.na(train$Married)]<-"Yes"
#train$Self_Employed[is.na(train$Self_Employed)]<-"NO"
train$Self_Employed[is.na(train$Self_Employed)]<-"No"
train$Dependents[is.na(train$Dependents)]<-0
View(train)
summary(train)

colSums(is.na(train))


boxplot(train[7:10])
boxplot(train$ApplicantIncome)
boxplot(train$CoapplicantIncome)

table(train$ApplicantIncome>25000)
table(train$CoapplicantIncome>1200)
train$ApplicantIncome=ifelse((train$ApplicantIncome>25000),25000 , train$ApplicantIncome)
train$CoapplicantIncome=ifelse((train$CoapplicantIncome>12000),12000,train$CoapplicantIncome)

#combine applicant's nd coapplicant's income together into one var called income
train$Income=train$ApplicantIncome+train$CoapplicantIncome
test$Income=test$ApplicantIncome+test$CoapplicantIncome
predict$Income=predict$ApplicantIncome+predict$CoapplicantIncome

View(train)
train[,1]<-NULL
View(train)


##EDA FOR TEST DATA
summary(test)
test[,1]<-NULL
View(test)
colSums(is.na(test))

test$Gender[is.na(test$Gender)]<-"Male"
test$Married[is.na(test$Married)]<-"Yes"
test$Dependents[is.na(test$Dependents)]<-0
test$Self_Employed[is.na(test$Self_Employed)]<-"No"
test$LoanAmount[is.na(test$LoanAmount)]<-median(test$LoanAmount , na.rm=TRUE)
test$Loan_Amount_Term[is.na(test$Loan_Amount_Term)]<-median(test$Loan_Amount_Term , na.rm=TRUE)
test$Credit_History[is.na(test$Credit_History)]<-1

summary(test)

##EDA FOR PREDICT
predict[,1]<-NULL
View(predict)
colSums(is.na(predict))

predict$Gender[is.na(predict$Gender)]<-"Male"
predict$Married[is.na(predict$Married)]<-"Yes"
predict$Dependents[is.na(predict$Dependents)]<-0
predict$Self_Employed[is.na(predict$Self_Employed)]<-"No"
predict$LoanAmount[is.na(predict$LoanAmount)]<-median(predict$LoanAmount , na.rm = TRUE)
predict$Loan_Amount_Term[is.na(predict$Loan_Amount_Term)]<-median(predict$Loan_Amount_Term , na.rm=TRUE)
predict$Credit_History[is.na(predict$Credit_History)]<-1
summary(predict)



##MODEL BUILDING
attach(train)
model1<-glm(Loan_Status~. -ApplicantIncome -CoapplicantIncome , family = binomial(link = "logit"), train)
summary(model1)  #AIC: 588.85

model2<-glm(Loan_Status~Married+Property_Area+Credit_History, family=binomial(link=logit),train)
summary(model2) 

#prediction
pred2=predict(model2,test,type='response')
test$Credit_History=as.factor(test$Credit_History)
summary(pred2)

pred2=ifelse(pred2>0.5,'Y','N')

table(pred2,test$Loan_Status)

#calculate precision
289/(289+19)  #TP/(TP+FP)


