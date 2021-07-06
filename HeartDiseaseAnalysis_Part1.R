#Get central tendency of data
summary(framingham1)

which(is.na(framingham1))

#Create dataSet with omitted NA values
omitValues<- na.omit(framingham1)
Framingham_NA_omitted <- na.omit(framingham1$totChol)
summary(omitValues)
summary(Framingham_NA_omitted)

#where are missing values
colSums(is.na(framingham1))

#Normalize data using zscore
summary(scaledFramingham)
scaledFramingham<- scale(Framingham_Clean)

#visulaize Data using histograms, boxplots, and scatterplots
summary(scaledFramingham)
hist(framingham1$Age, main = "Histogram for Unscaled Age in Framingham Data")



hist(scaledFramingham [,2], main = "Histogram for Scaled Age in Framingham Data")

boxplot(framingham1$Age, main = "Un-Normalized Age Boxplot for Framingham Data")
boxplot(scaledFramingham [,2], main= "Normalized Data for Age Boxplot for Framingham Data", na.rm =TRUE)

#scatter plot for un-normalized data
plot(framingham1$Age,framingham1$totChol, main=" Scatter Plot of Age Versus Total Cholesterol", 
     xlab="Age", ylab="cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Age","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model1, col= "Black", lwd=3)

#scatterplot for normalized data
plot(scaledFramingham [,2],scaledFramingham [,10], main=" Normalized Scatter Plot of Age Versus Total Cholesterol", 
     xlab="Age", ylab="cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Age","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model2, col= "Black", lwd=3)


#Predict the cholesterol given the age
model1 <- lm(totChol ~ Age, na.action =na.exclude, data = Framingham_Clean)
summary(model1)

framingham1.pred <- predict(model1, framingham1 )
framingham1.pred
framingham1.pred[1]
residuals(model1)
model1$residuals


#Predict the cholesterol given the age (dataset with median values inserted)
model3 <- lm(totChol ~ glucose, na.action =na.exclude, data = framingham_impute_median)
summary(model3)

framingham_impute_median.pred <- predict(model1, framingham1 )
framingham_impute_median.pred
framingham_impute_median.pred[1]
residuals(model3)
model3$residuals

#Predict the cholesterol given the age (unnormalized vs normalized)
model2 <- lm(scaledFramingham [,2]~ scaledFramingham [,10], na.action =na.omit)
summary(model2)

scaledFramingham.pred <- predict(model2, scaledFramingham [2,10])
scaledFramingham.pred
scaledFramingham.pred[,2]
residuals(model2)
model2$residuals


cor(framingham1, use= "complete.obs")
cor(framingham1)

#use a symbolic approach to find max correlation
symnum(cor(framingham1,use = "complete.obs"))

#replace missing values with the median
framingham_impute_median <-data.frame(
  sapply(framingham1,
            function(x)
              ifelse(is.na(x),
                 mean(x, na.rm = TRUE),x)))

library(tidyr)
Framingham_Chol_NA_Omitted<- drop_na(framingham1$totChol)
summary(Framingham_Chol_NA_Omitted)

write.csv(omitValues, "Framingham_missingValuesOmitted")

install.packages("xlsx")
library(xlsx)
write.xlsx(omitValues, "c:/Framingham_missingValuesOmitted.xlsx")
