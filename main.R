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

library(caret)




