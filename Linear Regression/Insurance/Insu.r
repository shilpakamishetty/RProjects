library(psych)
library(caTools)
library(hydroGOF)


getwd()
Insu<-read.csv("insurance.csv")
View(Insu)

class(Insu)
names(Insu)
dim(Insu)

summary(Insu)
describe(Insu)

colSums(is.na(Insu))  #used to check missing values
boxplot(Insu[,-c(2,5,6)])
boxplot(Insu[,3])

#after boxplot , we identified der is outlers in bmi ,so remove outliers
#outlires are above the max value so use ceiling 
#all the vlues in bmi>45 is iddentified as outliers


table(Insu$bmi>45) #identied no of outliers  
which(Insu$bmi>45) #identified positions of my outliers


BMI1=as.numeric(summary(Insu$bmi)) #here am converting my summary of bmi completely into numeric(this removes the headings of summary)
BMI1

ceiling=BMI1[5]+1.5*(BMI1[5]-BMI1[2]) #performed ceiling q3+1.5(IQR)
ceiling


  
Insu$bmi<-ifelse(Insu$bmi>ceiling,ceiling,Insu$bmi)   #if any value in u r bmi>ceilimg value replace it with ceiling
boxplot(Insu)
boxplot(Insu$bmi)


boxplot(Insu[,-c(2,5,6,7)])
boxplot(Insu[7])

hist(Insu$charges)    #to check whether DV are N.D ,it is not N.D

Insu$lcharges<-log(Insu$charges) 
# transform the data using log
hist(Insu$lcharges)  #now transformed data is N.D

View(Insu)

##BIvariate analysis
cor.test(Insu$age,Insu$lcharges)    #pvalue<0.05 so we reject h0 ,I.V is signi

cor.test(Insu$bmi,Insu$lcharges)    #pvalue <0.05 we reject h0 ,I.V is sign

cor.test(Insu$children,Insu$lcharges)  #pvalue<0.05 we reject h0 , I.V is  signi

t.test(lcharges~sex,Insu)     #pvalue>0.05 so we dont reject h0 ,I.V is not signi
t.test(lcharges~smoker,Insu)  #pvalue<0.05 so we  reject h0 ,I.V is  signi

#ftest
summary(aov(lcharges~region,Insu))   #pvalue>0.05 so we dont reject h0 ,I.V is not signi

#check for multicollinearity

cor.test(Insu$age,Insu$bmi)   #pvalue<0.05 so we reject h0 , there is relation MC is present


cor.test(Insu$age,Insu$children)   #pvalue>0.05 we  dont reject h0,MC is absent
t.test(age~smoker,Insu)           #pvalue>0.05 we  dont reject h0,MC is absent

cor.test(Insu$bmi,Insu$children)      #pvalue>0.05 we  dont reject h0,MC is absent
t.test(bmi~smoker,Insu)                 # #pvalue>0.05 we  dont reject h0,MC is absent


t.test(children~smoker,Insu)               #pvalue>0.05 we  dont reject h0,MC is absent               
 

##MC is present between AGE and BMI we ignored the bmi I.v as r value of age is higher than bmi

split=sample.split(Insu$lcharges,0.7)
train=subset(Insu,split==TRUE)
train
test=subset(Insu,split==FALSE) 
class(train)

attach(train)
model2<-lm(lcharges ~.-sex-region-bmi-charges, train)    
summary(model2)

predcharges<-predict(model2,test) #newdata=test
RMSEtest<-rmse(test$lcharges,predcharges)
RMSEtest
predcharges
