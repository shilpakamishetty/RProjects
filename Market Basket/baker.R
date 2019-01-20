setwd("C:/Users/Shilpa/Documents/R/usecases")
library(arules) 
library(arulesViz) #plotting the rules

bake<-read.csv("BakerySales (1).csv",header = TRUE)
View(bake)
dim(bake)
bake<-read.transactions("BakerySales (1).csv",format='single',sep=',',rm.duplicates = TRUE,cols=c("ReceiptNo","Product"))
class(bake)
# if the data is in the form of pos then,format will be basket and it u dont mention the format it will by default take the format as basket


itemFrequencyPlot(bake,topN=20)  #it
emFrequencyPlot(data, support=0.9)

rules=apriori(data=bake,parameter = list(support=0.04,confidence=0.2,minlen=2))
inspect(rules)

rules=sort(rules,by='lift',decreasing='TRUE')
inspect(rules)

plot(rules,method='grouped')
plot(rules,method='graph')
