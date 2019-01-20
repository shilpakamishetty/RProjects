

#install.packages("psych")
library(psych)
library(caTools) #used while splitting the data
library(hydroGOF)
library(ROCR)


setwd("C:/Users/Shilpa/Documents/R/usecases")
LGD<-read.csv("LGD_DATA.csv")
View(LGD)

class(LGD)    #to check the class of my data set i.e., DATA FRAME
dim(LGD)      #it displays number of observations and variables.
names(LGD)    #it displays all  the variable names


###EDA PROCESS STARTS
summary(LGD)  #displayes 5 point summary for my dataset
describe(LGD) #calculates all the measures of cental tendency and dispersion.

#Observations made by summary
#1 Account num can be removed as it is unique identifier.
#2 No.of vehicles has to be a factor since it has 4 levels.(but we r nt transforming)
#3 Identified outliers in losses.
#1 remove ACC NUM
#LGD<-LGD[,-1]
LGD[,1]<-NULL
View(LGD)


#2check for Missing values
colSums(is.na(LGD))          #To check if dere r any NA's in LGD , if dere is NA,NA=TRUE(whose value is 1)
                             #if dere is no NA,NA=FALSE(whose value is 0), based on dat my output is 0,so no NA values
#3check for outliers
boxplot(LGD[,-c(4,5)])       #to identify the missing values will use box plot
                             #Box plot works only for quantitative var's , so elminate[4 n 5 col]

#By the boxplot we found there  are outliers in losses
boxplot(LGD$Losses.in.Thousands)

#NEED to check whether my DV is normally distributed , so plot histogram
hist(LGD$Losses.in.Thousands)       #this is not N.D , so use log transform

LGD$logloss<-log(LGD$Losses.in.Thousands)     #applied log transform and saved it in new variable(logloss)
 
hist(log(LGD$Losses.in.Thousands))        #my DV logloss is N.D


###Bivariate analysis(wer we do variable selection)

#1 test relationship between D.V and I.V's

cor.test(LGD$Age,LGD$logloss)       #correlation test is performed as age nd logloss both are quantitative
                                   #p-value < 2.2e-16 ,here pvalue is less than 0.05 ,so I.v AGE is significant

cor.test(LGD$Years.of.Experience,LGD$logloss)     #correlation test is performed as YE nd logloss both are quantitative
                                                  # p-value < 2.2e-16 here pvalue is less than 0.05 ,so I.v YE is significant

cor.test(LGD$Number.of.Vehicles,LGD$logloss)      #p-value = 0.5496 , pvalue>0.05 (dont reject h0). so no.of vehicles is not significant variable.



t.test(logloss~Gender,LGD)       #here we performed t test as logloss(quan) and gende(qual)
                                 #gender is sig ( check pvalue)

t.test(logloss~Married,LGD)        #here we performed t test as logloss(quan) and married(qual)
                                   #married is sig ( check pvalue)



#check for MULTICOLLINEARITY      #here there should be no sig diff between I.V's (h0 is not rejected) ,M.C should be absent

cor.test(LGD$Age,LGD$Years.of.Experience)  # p-value = 2.2e-16 , <0.05 so h0 is rejected , which means there is sig diff
                                          #M.C is present

t.test(Age~Gender,LGD)                    # p-value = 0.5047 ,p>0.05 so accept h0 , which means there is no sig diff
                                          #M.C is absent

t.test(Age~Married,LGD)                    #p-value = 0.5931 , p>0.05 so accept h0 , which means there is no sig diff
                                            #M.C is absent
t.test(LGD$Years.of.Experience~LGD$Gender)  # p-value = 0.3612  p>0.05 so accept h0 , which means there is no sig diff
                                              #M.C is absent

t.test(LGD$Years.of.Experience~LGD$Married)    # p-value = 0.5341 ,p>0.05 so accept h0 , which means there is no sig diff
                                                #M.C is absent

chisq.test(LGD$Gender,LGD$Married)            #p-value = 0.3304 ,,p>0.05 so accept h0 , which means there is no sig diff
                                                #M.C is absent


#M.C is present between AGE & Y.E  , so we can eliminate one
#based on ,when u find the relation between u r I.V(age n Y.E ) with D.V(Loss) u will have r value (in cor test)
# which ever comparision b/w  (DV & IV) have higher r value that will be considered into the model. 

#Splitting the data as train nd testa

split=sample.split(LGD$logloss,0.7)          #sample.split is a func of caTools
train=subset(LGD,split==TRUE)
test=subset(LGD,split==FALSE)



#building the model
#age and Y.E are correlated ,lets tke either one of them

attach(train)
model1<-lm(logloss~.-Losses.in.Thousands-Age-Number.of.Vehicles,data = train)
summary(model1)

#prediction
predloss<-predict(model1,newdata=test) #newdata=test

RMSETest<-rmse(test$logloss,predloss)
RMSETest



