#download Data
library(kernlab)
library(caret)
View(forestfires)
class(forestfires)

library(plyr)

summary(forestfires)

#covert in numeric data
forestfires$month <- as.numeric(as.factor(forestfires$month))
forestfires$day <- as.numeric(as.factor(forestfires$day))
-----------------------------------------------------------------------
#histogram
hist(forestfires$area)

#normalize the data
normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

#lapply
forestfires_norm<-as.data.frame(lapply(forestfires[1:30],normalize))
#concrete_norm<-scale(concrete)
View(forestfires)



forestfires1 <- mutate(forestfires, y = log(area + 1))  # default is to the base e, y is lower case
hist(forestfires1$y)
library(e1071)

#ind <- sample(2, nrow(forestfires), replace = TRUE, prob = c(0.7,0.3))
forestfires_train<-forestfires[1:390,]
forestfires_test<-forestfires[391:517,]

###############################A######WITH RBFDOT KERNALS######################
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= forestfires_train,kernel = "rbfdot")


pred_rfdot<-predict(model_rfdot,newdata=forestfires_test)

table(pred_rfdot,forestfires_test$size_category)

agreement <- pred_rfdot == forestfires_test$size_category
table(agreement)
prop.table(table(agreement))##68.50



###############################A######WITH VDOANILLADOT KERNALS######################
model_van<-ksvm(size_category~temp+rain+wind+RH, 
             data= forestfires_train,kernel = "vanilladot")

model_van


Area_pred <- predict(model_van, forestfires_test)

table(Area_pred,forestfires_test$size_category)

agreement <- Area_pred == test$size_category
table(agreement)
prop.table(table(agreement))##68.50

############################### kernal = besseldot###################
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= forestfires_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forestfires_test)
mean(pred_bessel==forestfires_test$size_category) # 70%
####################################### kernel = polydot###############



model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= forestfires_train,kernel = "polydot")
##  Setting default kernel parameters
pred_poly<-predict(model_poly,newdata = forestfires_test)
mean(pred_poly==forestfires_test$size_category) #70.07%
