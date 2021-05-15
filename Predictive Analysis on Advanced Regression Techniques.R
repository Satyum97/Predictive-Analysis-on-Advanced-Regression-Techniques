#setwd("C:/Users/Satyum/Desktop/Practical Aspects of DS/Week 2/Assignment 1 (Linear Regression)")
library(tidyverse)
require(corrplot)
library(MASS)

#----------------------------Reading the csv file--------------------------------------------------------------------

dataset <- read_csv('https://personal.utdallas.edu/~sxb180124/house.csv')

#---------------------------Data pre-processing Part-----------------------------------------------------------------

#We find the percentage of null values in each column of the dataset 
#If any numeric column has null percentage greater than 2% then we replace the null values present
#in that column with mean value of the column
vector <- NULL
for(i in 1:(ncol(dataset)))
{
  colname = colnames(dataset[i])
  nullpredict = sum(is.na(dataset[i]))/length(dataset[[i]])*100
  print(paste("Column ", colname, " has ", round(nullpredict), "% of null values"))
  
  if (nullpredict > 2 & is.numeric(dataset[[i]]))
  {
    vector<-c(vector,i)
  }
}

# Function to convert null values to mean value
NAtomean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dataset[vector]=sapply(dataset[vector], NAtomean)



#------------------------Analysis of each of the features against predicted variables------------------------------------------------------

# Storing only numeric column of the dataset to find correlation
numeric_data <- sapply(dataset, function(x) is.numeric(x))

# Creating Correlation Matrix between every two variable and neglecting null values using complete.obs
corMatrix <- cor(dataset[numeric_data],use = "complete.obs")

# plotting correlation matrix for better visualization
corrplot(corMatrix, method = "circle")

# plotting correlation matrix based 
corMat <- as.data.frame(corrplot(corMatrix,method = "number"))

#creating correlation matrix for only those whose values are more than 0.5 with SalePrice
rowName <- row.names(corMat)[abs(corMat$SalePrice) > 0.5]
View(rowName)

# From correlation matrix we get to know that there is a high correlation between OverallQual and SalePrice
cor(dataset$OverallQual,dataset$SalePrice)
ggplot(data = dataset)+
  geom_point(mapping = aes(OverallQual,SalePrice))

#------------------------Regression diagnostics----------------------------------------------------------------------


#fitting the Linear model with all the predictors having correlation greater than 0.5 with SalePrice
model <- lm(SalePrice~OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea 
            + FullBath + TotRmsAbvGrd + GarageCars + GarageArea , data = dataset)
summary(model)


#fitting model with the predictors with high p value
model2 <- lm(SalePrice~OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars, data = dataset)

summary(model2)



#------------------------SELECTION OF PREDICTORS-----------------------------------------------------------------------------

# We use stepAIC to find the optimal model using both direction i.e. step forward and step backward
# StepAIC also remove the variable with maximum multicollinearity
step.model <- stepAIC(model,direction = "both")
summary(step.model)

#------------------------ANOVA TESTING-----------------------------------------------------------------------------

anova(step.model,model)


#------------------------Plotting and Summary For Observations-------------------------------------------------------

ggplot(data=dataset) + geom_line(mapping = aes(x= OverallQual, y = SalePrice))
model3 <- lm(SalePrice ~ OverallQual, data=dataset)
summary(model3)

ggplot(data=dataset) + geom_line(mapping = aes(x= YearBuilt, y = SalePrice))
model4 <- lm(SalePrice ~ YearBuilt, data=dataset)
summary(model4)

ggplot(data=dataset) + geom_line(mapping = aes(x= YearRemodAdd, y = SalePrice))
model5 <- lm(SalePrice ~ YearRemodAdd, data=dataset)
summary(model5)


ggplot(data=dataset) + geom_line(mapping = aes(x= TotalBsmtSF, y = SalePrice))
model6 <- lm(SalePrice ~ TotalBsmtSF, data=dataset)
summary(model6)


ggplot(data=dataset) + geom_line(mapping = aes(x= GrLivArea, y = SalePrice))
model7 <- lm(SalePrice ~ GrLivArea, data=dataset)
summary(model7)


ggplot(data=dataset) + geom_line(mapping = aes(x= GarageCars, y = SalePrice))
model8 <- lm(SalePrice ~ GarageCars, data=dataset)
summary(model8)

ggplot(data=dataset) + geom_line(mapping = aes(x= FullBath, y = SalePrice))
model9 <- lm(SalePrice ~ FullBath, data=dataset)
summary(model9)

ggplot(data=dataset) + geom_line(mapping = aes(x= GarageArea, y = SalePrice))
model10 <- lm(SalePrice ~ GarageArea, data=dataset)
summary(model10)

plot(model)
plot(model2)
plot(step.model)