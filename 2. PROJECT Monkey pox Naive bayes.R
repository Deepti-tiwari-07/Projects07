install.packages("caret")
library(caret)
library(pROC)
install.packages("klaR")
library(klaR)
library(caret)
install.packages("klaR")
library(klaR)
library(gains)
install.packages("gains")
library(pROC)
install.packages("e1071")
library(e1071)

setwd("D://Deepti//NCI//Machine learning and data mining")##setting the directory

data<- read.csv("monkeypoxcsv.csv", stringsAsFactors = FALSE)##reading the data

str(data)### data structure

table(data$MonkeyPox)## creating a table where you can see the no +ve and -Ve cases

nrow(data)## getting the no of rows

?navie

head(data)
data$MonkeyPox<-as.factor(data$MonkeyPox)##Converting the precdective data into factor
str(data)
set.seed(1)
index<-createDataPartition(data$MonkeyPox,p=0.95,list=FALSE) ## setting the index
##splitting the data into train and test
train<-data[index,]
test<-data[-index,]
ctrl<-trainControl(method="cv",number=10)
set.seed(1)

install.packages("e1071")
library(e1071)
##model building

nbfit<-naiveBayes(MonkeyPox~.,data=train) ##creating the model
nbfit
nbfit1<-naiveBayes(MonkeyPox~.,data=train,laplace=1) ##creating the model
nbfit1
nb_class<-predict(nbfit,newdata=test) ## predecting
table(nb_class)
nb_classfit1<-predict(nbfit1,newdata=test,type="class") ## predecting
table(nb_classfit1)
confusionMatrix(nb_class,test$MonkeyPox)##confusion matrix
table(nb_class,test$MonkeyPox)


###alternative appraoch for predict

default_pred <- predict(nbfit, test, type="class")
default_pred 

table(default_pred, test$MonkeyPox,dnn=c("Prediction","Actual"))
confusionMatrix(default_pred,test$MonkeyPox)

default_raw_pred <- predict(nbfit, test, type="raw")
default_raw_pred

"table(default_raw_pred, test$MonkeyPox,dnn=c("Prediction","Actual"))
confusionMatrix(default_raw_pred,test$MonkeyPox)"""###equal length and how why

##plot
X <- varImp(nbfit)
plot(X)
install.packages("varImp")
library(varImp)
"nbclass_prob<-predict(nb_class,newdata=test,predict.type="prob")
nbclass_prob
head(nbclass_prob)
"""##ASK why prob not working



##CrossTable(nb_class,prop.chisq=FALSE,prop.c=FALSE, dnn=c('pred','act'))
crosstable(nb_class,test$MonkeyPox)




