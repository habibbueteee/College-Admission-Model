install.packages("readxl")
library(readxl)

data <- read.csv("College_admission.csv")
View(data)
head(data)

str(data)


#Checking missing values. Found none
anyNA(data)
sum(is.na(data))

#Factoring the categorical data

data$rank <- factor(data$rank, order = TRUE)
data$ses <- factor(data$ses)
data$Gender_Male <- factor(data$Gender_Male)
data$Race <- factor(data$Race)
data$admit <- factor(data$admit)

str(data)

#Checking outliers

boxplot(data$gre) # Existence of outliers
boxplot(data$gpa) # Existence of outliers

#Removing outliers for gre
Q_gre <- quantile(data$gre, probs = c(0.25, 0.75)) # 25% is 520 and 75% is 660
iqr_gre <- IQR(data$gre) # Interquantile range is 140
uper_gre <- Q_gre[2]+1.5*iqr_gre # uper limit is 870
lower_gre <- Q_gre[1]-1.5*iqr_gre # lower limit is 310
data_1 <- subset(data, data$gre > lower_gre & data$gre < uper_gre)
boxplot(data_1$gre) # No outliers

#Removing outliers for gpa on already removed dataset
Q_gpa <- quantile(data_1$gpa, probs = c(0.25, 0.75)) # 25% is 3.13 and 75% is 3.67
iqr_gpa <- IQR(data_1$gpa) # Interquantile range is 0.54
uper_gpa <- Q_gpa[2]+1.5*iqr_gpa # uper limit is 4.48
lower_gpa <- Q_gpa[1]-1.5*iqr_gpa # lower limit is 2.32
data_2 <- subset(data_1, data_1$gpa > lower_gpa & data_1$gpa < uper_gpa)
boxplot(data_2$gpa) # No outliers

# Checking if normally distributed

summary(data_2$gre) #Not normally distributed : mean (591.2) > median (580.0), so right skewness
hist(data_2$gre) # Visual inspection

summary(data_2$gpa) #Not normally distributed : median (3.4) > mean (3.398), so left skewness
hist(data_2$gpa) # Visual inspection


#Train-test split

splitIndex <- createDataPartition(data_2$admit, p = .70,list = FALSE, times = 1)
training <- data_2[ splitIndex,]
test <- data_2[-splitIndex,]



#Linear Regression model
LR_model = glm(admit ~ ., data = training,
               family = "binomial")
summary(LR_model)

#gpa, gre and rank are significant- so dropped others
LR_model = glm(admit ~ gpa + rank + gre, data = training,
               family = "binomial")
summary(LR_model)

#Predict on Test throgh MOdel
pred = predict(LR_model,test, type="response")
pred = ifelse(pred>0.5,1,0)
pred = factor(pred)

####Validate the model - Confusion Matrix##

act <- test$admit
# Accuracy
library(caret)
table(pred, act)
a=confusionMatrix(pred,act)
a # Accuracy is 68.38% for logistic regression



#Support Vector machine
install.packages("e1071")
library(e1071)

svmfit =svm(admit ~ gpa+rank+gre, data = training, kernel="linear",
            scale = T)
print(svmfit)
pred_svm = predict(svmfit,test, type="response")
table(pred_svm, act)
a=confusionMatrix(pred_svm,act)
a # Accuracy is 69.23% for SVM

#Decision Tree

library(party)    # FOr decision tree 
library(rpart)    # for Rpart 
library(rpart.plot) #for Rpart plot
library(lattice)  # Used for Data Visualization

fit_dt = rpart(admit ~ gpa+rank+gre, data = training,method = "class", 
            control = rpart.control(minsplit = 30,cp = 0.01))

summary(fit_dt)
pred_dt = predict(fit_dt,test, type="class")
table(pred_dt, act)
a=confusionMatrix(pred_dt,act)
a # Accuracy is 68.38% for Decision Tree


#Random Forest
install.packages("randomForest")
library(randomForest)
require(caret) 
library(pROC)
library(e1071)

fit_rf = randomForest(admit ~ gpa+rank+gre, data = training, do.trace=T)
pred_rf = predict(fit_rf,test)
table(pred_rf, act)
a=confusionMatrix(pred_rf,act)
a # Accuracy is 70.09% for Random FOrest


#Naivee Bayes

install.packages("naivebayes")
library(naivebayes)

fit_nb = naive_bayes(admit ~ gpa+rank+gre, data = training)
pred_nv = predict(fit_rf,test)
table(pred_nv, act)
a=confusionMatrix(pred_nv,act)
a # Accuracy is 70.09% for Naivee Bayes



