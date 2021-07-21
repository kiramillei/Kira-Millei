rm(list = ls())
graphics.off()
gc()


setwd("C:\\Users\\kiram\\OneDrive\\?????????????? ????????\\Capstone project")
aps=read.csv("Airline Passenger Satisfaction.csv", stringsAsFactors = TRUE)


#getting to know data
str(aps)

summary(aps)

boxplot(aps$Age)
boxplot(aps$Flight.Distance)
boxplot(aps$Arrival.Delay.in.Minutes)
boxplot(aps$Departure.Delay.in.Minutes)
boxplot(aps$Checkin.service)

library(psych)

describe(aps)

#delete x column
aps=aps[c(-1)]

#missing values 

sapply(aps, function(x) sum(is.na(x)))

median(aps$Arrival.Delay.in.Minutes,na.rm = TRUE)

aps$Arrival.Delay.in.Minute.imp.median =ifelse(is.na(aps$Arrival.Delay.in.Minute), median(aps$Arrival.Delay.in.Minute, na.rm=TRUE), aps$Arrival.Delay.in.Minute)


aps=aps[c(-23)]
sapply(aps, function(x) sum(is.na(x)))

names(aps)[names(aps)=="Arrival.Delay.in.Minute.imp.median"] = "Arrival.Delay.in.Minutes"
#delete id 
aps=aps[c(-1)]

#encoding categorical variables


library("fastDummies")


aps=dummy_cols(aps, select_columns = c("satisfaction"), remove_selected_columns = TRUE)
library(caret)
dummies <- dummyVars(~., data = aps,fullRank=T)
newdata<-(predict(dummies, newdata = aps))

newdata=as.data.frame(newdata)

newdata=newdata[c(-24)]
#varience 
Near_Zero_Var <- nearZeroVar(
  newdata,
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = TRUE)

#create a cook distance

mod=lm(satisfaction_satisfied~ ., data=newdata)
summary(mod)

cooksd=cooks.distance(mod)


plot(cooksd, pch="*", cex=2, main="Observation by cook distance")  # plot cook's distance

abline(h = 4/nrow(newdata), col="red")  # add cutoff line

text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/nrow(newdata),names(cooksd),""), col="red")  # add labels

# influential row numbers

influential=as.numeric(names(cooksd)[(cooksd > (4/nrow(newdata)))])

#removing outliers

#newdata=newdata[-influential, ]

#binning arrival delay in minutes
range(newdata$Arrival.Delay.in.Minutes)

breaks1=c(0,30,90,1584)

tags=c("1","2","3")

group_tags=cut(newdata$Arrival.Delay.in.Minutes,
               breaks = breaks1,
               include.lowest = TRUE,
               right = FALSE,
               labels = tags)

summary(group_tags)

newdata$Arrival.Delay.in.Minutes=cut(newdata$Arrival.Delay.in.Minutes, breaks=breaks1, lebels=tags)

newdata$Arrival.Delay.in.Minutes=as.numeric(newdata$Arrival.Delay.in.Minutes)

newdata$Arrival.Delay.in.Minutes[is.na(newdata$Arrival.Delay.in.Minute)]=0

boxplot(newdata$Arrival.Delay.in.Minutes)
sapply(newdata, function(x) sum(is.na(x)))

#binning departure delay in minutes

range(newdata$Departure.Delay.in.Minutes)

breaks2=c(0,30,90,1592)

tags1=c("1","2","3")

group_tags1=cut(newdata$Departure.Delay.in.Minutes,
                breaks = breaks2,
                include.lowest = TRUE,
                right = FALSE,
                labels = tags1)

summary(group_tags1)

newdata$Departure.Delay.in.Minutes=cut(newdata$Departure.Delay.in.Minutes, breaks=breaks2, lebels=tags1)

newdata$Departure.Delay.in.Minutes=as.numeric(newdata$Departure.Delay.in.Minutes)

newdata$Departure.Delay.in.Minutes[is.na(newdata$Departure.Delay.in.Minutes)]=0

boxplot(newdata$Departure.Delay.in.Minutes)
sapply(newdata, function(x) sum(is.na(x)))

#binning flight distance

range(newdata$Flight.Distance)

breaks3=c(31,414,843,1743,4983)

tags2=c("1","2","3","4")

group_tags2=cut(newdata$Flight.Distance,
                breaks = breaks3,
                include.lowest = TRUE,
                right = FALSE,
                labels = tags2)

summary(group_tags2)

newdata$Flight.Distance=cut(newdata$Flight.Distance, breaks=breaks3, lebels=tags2)

newdata$Flight.Distance=as.numeric(newdata$Flight.Distance)


boxplot(newdata$Flight.Distance)
sapply(newdata, function(x) sum(is.na(x)))

newdata= na.omit(newdata)
sapply(newdata, function(x) sum(is.na(x)))

#create a cook distance again
mod1=lm(satisfaction_satisfied~ ., data=newdata)
summary(mod1)

cooksd1=cooks.distance(mod1)


plot(cooksd1, pch="*", cex=2, main="Observation by cook distance")  # plot cook's distance

abline(h = 12/nrow(newdata), col="red")  # add cutoff line

text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>12/nrow(newdata),names(cooksd),""), col="red")  # add labels

influential1=as.numeric(names(cooksd1)[(cooksd1 > (12/nrow(newdata)))])


#removing outliers

newdata=newdata[-influential1, ]

#want to create boxplot again after removing outliers


boxplot(newdata$Flight.Distance)
boxplot(newdata$Arrival.Delay.in.Minutes)
boxplot(newdata$Departure.Delay.in.Minutes)


#correlation coefficient 
cor(newdata)

#Creating train and test sets:
colnames(newdata)[2] <- "Customer.Type.Loyal.Customer"
colnames(newdata)[4] <- "Type.of.Travel.Personal.Travel" 
colnames(newdata)[6] <- "Class.Eco.Plus" 

trainIndex<- createDataPartition(newdata$satisfaction_satisfied, p=0.75, list=F)

train<- newdata[trainIndex, ]

test<- newdata[-c(trainIndex),]


#Creating balanced training data:



library(imbalance)

newData1<- imbalance::oversample(
  
  train,
  
  ratio =0.8,              # ratio of the minority/majority class
  
  method = "ADASYN", #insert resembling method
  
  classAttr = "satisfaction_satisfied")

library(mlbench)

library(caret)

set.seed(12)

# prepare training scheme

control=trainControl(method="repeatedcv", number=10, repeats=5)

newData1$satisfaction_satisfied<- as.factor(newData1$satisfaction_satisfied)
model=train(satisfaction_satisfied ~., data=newData1, method="glm", preProcess="scale", trControl=control)

str(model)

# estimate variable importance

importance=varImp(model, scale=FALSE)

# summarize importance

print(importance)

# plot importance

plot(importance)
#chosing 80% of the most important features 

#creating a model 
train<-newData1[c(-1,-7,-11,-22)]
test <-test[c(-1,-7,-11,-22)]


aps_train_labels=train[,"satisfaction_satisfied"]
aps_test_labels=test[,"satisfaction_satisfied"]


library(class)
library(gmodels)

aps_test_pred1=knn(train=train, test=test, cl=aps_train_labels, k=1)

CrossTable(x = aps_test_labels, y = aps_test_pred1, prop.chisq=FALSE)

aps_test_pred5=knn(train=train, test=test, cl=aps_train_labels, k=5)

CrossTable(x = aps_test_labels, y = aps_test_pred5, prop.chisq=FALSE)

aps_test_pred11=knn(train=train, test=test, cl=aps_train_labels, k=11)

CrossTable(x = aps_test_labels, y = aps_test_pred11, prop.chisq=FALSE)

aps_test_pred24=knn(train=train, test=test, cl=aps_train_labels24, k=24)

CrossTable(x = aps_test_labels, y = aps_test_pred, prop.chisq=FALSE)

confusionMatrix(as.factor(aps_test_pred1), as.factor(test$satisfaction_satisfied))

#decided to go with logistic regression, decision tree and random forest

#logistic regression

set.seed(12)

apslog <- glm(satisfaction_satisfied~., family=binomial,data=train)

summary(apslog)

aps_probs=predict(apslog,test,type="response")

aps_pred =ifelse(aps_probs>=0.5,1,0)

confusionMatrix(as.factor(aps_pred), as.factor(test$satisfaction_satisfied))
precision <-posPredValue(as.factor(aps_pred), as.factor(test$satisfaction_satisfied), positive = "1")
precision 

table(aps_pred, test$satisfaction_satisfied)

library(pROC)
AUC =auc(as.numeric(aps_pred), as.numeric(test$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8990*0.8449)
Gmean

g <- roc(satisfaction_satisfied~ aps_probs, data = test)
plot(g)   

#decision tree
set.seed(131)

library(rpart)
library(rpart.plot)
fit <- rpart(satisfaction_satisfied~., data = train, method = 'class')
rpart.plot(fit, extra = 106)
predict_aps <-predict(fit,test, type = 'class')
table_mat <- table(test$satisfaction_satisfied, predict_aps)
table_mat


confusionMatrix(as.factor(predict_aps), as.factor(test$satisfaction_satisfied))

precision <-posPredValue(as.factor(predict_aps), as.factor(test$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(predict_aps), as.numeric(test$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8711*0.9077)
Gmean

#random forest 
set.seed(121)

library(randomForest)
require(caTools)


rf=randomForest(satisfaction_satisfied ~ .,data=train,ntree=300,importance=TRUE)


varImpPlot(rf)
print(rf)


pred = predict(rf,test[-20])
cm = table(test[,20], pred)
cm


confusionMatrix(as.factor(pred),as.factor(test$satisfaction_satisfied))


precision=posPredValue(as.factor(pred), as.factor(test$satisfaction_satisfied), positive = "1")
precision 
AUC = auc(as.numeric(pred), as.numeric(test$satisfaction_satisfied))
AUC
Gmean = sqrt(0.9813*0.9413)
Gmean



#model is overfitt, decided to create a lasso regression for feature selection


library(data.table)
library(ggplot2)
library(Matrix)
library(glmnet)

x = model.matrix(satisfaction_satisfied ~.,data=train) 
y = train$satisfaction_satisfied 

set.seed(17)

lasso_model <- cv.glmnet(x, y, alpha=1,family = "binomial") 
best_lambda_lasso <- lasso_model$lambda.1se  
lasso_coef <- lasso_model$glmnet.fit$beta[,  
                                          lasso_model$glmnet.fit$lambda 
                                          == best_lambda_lasso]
coef_l = data.table(lasso = lasso_coef)
coef_l[, feature := names(lasso_coef)]       
to_plot_r = melt(coef_l                     
                 , id.vars='feature'
                 , variable.name = 'model'
                 , value.name = 'coefficient')
ggplot(data=to_plot_r,                       
       aes(x=feature, y=coefficient, fill=model)) +
  coord_flip() +         
  geom_bar(stat='identity', fill='brown4', color='blue') +
  facet_wrap(~ model) + guides(fill=FALSE)

#new set of variables
train1=train[c(-2,-3,-9)]
test1=test[c(-2,-3,-9)]


set.seed(13)


apslog1 <- glm(satisfaction_satisfied~., family=binomial,data=train1)

summary(apslog1)

aps_probs1=predict(apslog1,test1,type="response")

aps_pred1 =ifelse(aps_probs1>=0.5,1,0)


confusionMatrix(as.factor(aps_pred1), as.factor(test1$satisfaction_satisfied))

precision=posPredValue(as.factor(aps_pred1), as.factor(test1$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(aps_pred1), as.numeric(test1$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8771*0.8259)
Gmean


#random forest1
set.seed(15)
rf1=randomForest(satisfaction_satisfied ~ .,data=train1,ntree=100,importance=TRUE)
pred1 = predict(rf1,test1[-17])
cm1 = table(test1[,17], pred1)
cm1


confusionMatrix(as.factor(pred1),as.factor(test1$satisfaction_satisfied))

precision=posPredValue(as.factor(pred1), as.factor(test1$satisfaction_satisfied), positive = "1")
precision 
AUC = auc(as.numeric(pred1), as.numeric(test1$satisfaction_satisfied))
AUC
Gmean = sqrt(0.9725*0.9304)
Gmean

varImpPlot(rf1)
print(rf1)

#desion tree1
set.seed(15)

fit1 <- rpart(satisfaction_satisfied~., data = train1, method = 'class')
rpart.plot(fit1, extra = 106)
predict_aps1 <-predict(fit1,test1, type = 'class')


confusionMatrix(as.factor(predict_aps1), as.factor(test1$satisfaction_satisfied))

precision <-posPredValue(as.factor(predict_aps1), as.factor(test1$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(predict_aps1), as.numeric(test1$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8844*0.8595)
Gmean


#set number 3
#based on fandom forest


train2=train1[c(-2,-3,-5,-6,-9,-16)]
test2=test1[c(-2,-3,-5,-6,-9,-16)]
#random forest

set.seed(17)
rf2=randomForest(satisfaction_satisfied ~ .,data=train2,ntree=100,importance=TRUE)
pred2 = predict(rf2,test2[-11])
cm2 = table(test2[,11], pred2)
cm2


confusionMatrix(as.factor(pred2),as.factor(test2$satisfaction_satisfied))

precision=posPredValue(as.factor(pred2), as.factor(test2$satisfaction_satisfied), positive = "1")
precision 
AUC = auc(as.numeric(pred2), as.numeric(test2$satisfaction_satisfied))
AUC
Gmean = sqrt(0.9401*0.9021)
Gmean


#logistic regression

apslog2 <- glm(satisfaction_satisfied~., family=binomial,data=train2)

summary(apslog2)

aps_probs2=predict(apslog2,test2,type="response")

aps_pred2 =ifelse(aps_probs2>=0.5,1,0)


confusionMatrix(as.factor(aps_pred2), as.factor(test2$satisfaction_satisfied))
precision=posPredValue(as.factor(aps_pred2), as.factor(test2$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(aps_pred2), as.numeric(test2$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8290*0.7817)
Gmean

#decision tree
set.seed(17)

fit2 <- rpart(satisfaction_satisfied~., data = train2, method = 'class')
rpart.plot(fit2, extra = 106)
predict_aps2 <-predict(fit2,test2, type = 'class')


confusionMatrix(as.factor(predict_aps2), as.factor(test2$satisfaction_satisfied))

precision <-posPredValue(as.factor(predict_aps2), as.factor(test2$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(predict_aps2), as.numeric(test2$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8196*0.8923)
Gmean


#desided to combine all tree variables from feature selection


train3=train2[c(-4,-6,-7,-9)]
test3=test2[c(-4,-6,-7,-9)]
#random forest

rf3=randomForest(satisfaction_satisfied ~ .,data=train3,ntree=100,importance=TRUE)
pred3 = predict(rf3,test3[-7])
cm3 = table(test3[,7], pred3)
cm3


confusionMatrix(as.factor(pred3),as.factor(test3$satisfaction_satisfied))

precision=posPredValue(as.factor(pred3), as.factor(test3$satisfaction_satisfied), positive = "1")
precision 
AUC = auc(as.numeric(pred3), as.numeric(test3$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8827*0.8638)
Gmean
#logistic regression 

apslog3 <- glm(satisfaction_satisfied~., family=binomial,data=train3)

summary(apslog3)

aps_probs3=predict(apslog3,test3,type="response")

aps_pred3 =ifelse(aps_probs3>=0.5,1,0)


confusionMatrix(as.factor(aps_pred3), as.factor(test3$satisfaction_satisfied))
precision=posPredValue(as.factor(aps_pred1), as.factor(test3$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(aps_pred3), as.numeric(test3$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8153*0.7860)
Gmean

#decision tree
set.seed(15)

fit3 <- rpart(satisfaction_satisfied~., data = train3, method = 'class')
rpart.plot(fit3, extra = 106)
predict_aps3 <-predict(fit3,test3, type = 'class')


confusionMatrix(as.factor(predict_aps3), as.factor(test3$satisfaction_satisfied))

precision <-posPredValue(as.factor(predict_aps3), as.factor(test3$satisfaction_satisfied), positive = "1")
precision 

AUC =auc(as.numeric(predict_aps3), as.numeric(test3$satisfaction_satisfied))
AUC
Gmean = sqrt(0.8414*0.8597)
Gmean

#compare how long each model running time
#logistic regression
system.time({apslog <- glm(satisfaction_satisfied~., family=binomial,data=train)})
system.time({apslog1 <- glm(satisfaction_satisfied~., family=binomial,data=train1)})
system.time({apslog2 <- glm(satisfaction_satisfied~., family=binomial,data=train2)})
system.time({apslog3 <- glm(satisfaction_satisfied~., family=binomial,data=train3)})


#decision tree

system.time({fit <- rpart(satisfaction_satisfied~., data = train, method = 'class')})
system.time({fit1 <- rpart(satisfaction_satisfied~., data = train1, method = 'class')})
system.time({fit2 <- rpart(satisfaction_satisfied~., data = train2, method = 'class')})
system.time({fit3 <- rpart(satisfaction_satisfied~., data = train3, method = 'class')})

#random forest

system.time({rf=randomForest(satisfaction_satisfied ~ .,data=train,ntree=300,importance=TRUE)})
system.time({rf1=randomForest(satisfaction_satisfied ~ .,data=train1,ntree=100,importance=TRUE)})
system.time({rf2=randomForest(satisfaction_satisfied ~ .,data=train2,ntree=100,importance=TRUE)})
system.time({rf3=randomForest(satisfaction_satisfied ~ .,data=train3,ntree=100,importance=TRUE)})
