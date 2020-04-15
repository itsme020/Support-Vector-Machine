library(kernlab)
library(caret)
library(plyr) 
library(ggplot2)
library(psych)
library(e1071)

#import SalaryData_Train data
str(SalaryData_Train)
####### Data.frame
SalaryData_Train$educationno <- as.factor(SalaryData_Train$educationno)
class(SalaryData_Train)
summary(SalaryData_Train)
#upload salary  data
str(SalaryData_Test)
SalaryData_Test$educationno <- as.factor(SalaryData_Test$educationno)
class(SalaryData_Test)

#Visualization 
# Plot and ggplot 
ggplot(data=SalaryData_Train,aes(x=SalaryData_Train$Salary, y = SalaryData_Train$age, fill = SalaryData_Train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#####Building Model##########

#############  kernel = rfdot ########################################################################
model_rfdot<-ksvm(SalaryData_Train$Salary~., 
                  data= SalaryData_Train,kernel = "rbfdot")
model_rfdot
pred_rfdot<-predict(model_rfdot,newdata=SalaryData_Test)
table(pred_rfdot,SalaryData_Test$Salary)
agreement <- pred_rfdot == SalaryData_Test$Salary
table(agreement)
prop.table(table(agreement))
mean(pred_rfdot==SalaryData_Test$Salary)#######85%



############## kernel = vanilladot################################################
model_vanilla<-ksvm(SalaryData_Train$Salary~., 
                    data= SalaryData_Train,kernel = "vanilladot")
model_vanilla

pred_vanilla<-predict(model_vanilla,newdata=SalaryData_Test)

table(pred_vanilla,SalaryData_Test$Salary)

agreement <- pred_vanilla == SalaryData_Test$Salary
table(agreement)
prop.table(table(agreement))

mean(pred_vanilla==SalaryData_Test$Salary) # ####84.64%

#################Vanilladot###############################################################
model1<-ksvm(SalaryData_Train$Salary~., 
             data= SalaryData_Train, kernel = "vanilladot")


model1
Salary_prediction <- predict(model1, SalaryData_Test)

table(Salary_prediction,SalaryData_Test$Salary)

agreement <- Salary_prediction == SalaryData_Test$Salary
table(agreement)
prop.table(table(agreement))#################84.64%
 
######hence rbfdot having more accuracy we can conclude that##########
