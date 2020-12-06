
summary(Framingham_Clean)

##look for correlation amoungst features
cor(Framingham_Clean, use= "complete.obs")

install.packages("corrplot")
library(corrplot)
Framingham_Clean.cor <-cor(Framingham_Clean, method = c("pearson"))
corrplot(Framingham_Clean.cor)


#use a symbolic approach to find max correlation
symnum(cor(Framingham_Clean,use = "complete.obs"))

boxplot(Framingham_Clean$cigsPerDay~Framingham_Clean$TenYearCHD,
        main = "Boxplot of Distribution of Smokers diagnosed with CHD", xlab = "Ten Year CHD Diagnosis (0 = NO, 1 = Yes)", ylab= "Number of cigsPerDay",
        col= rainbow(2), border = "Black", horizontal = FALSE)


boxplot(Framingham_Clean$Age~Framingham_Clean$TenYearCHD,
        main = "Boxplot of Age distribution of Patients Diagnosed with CHD", xlab = "Ten Year CHD Diagnosis (0 = NO, 1 = Yes)", ylab= "Patient Age (Years)",
        col= rainbow(2) , border = "Black", horizontal = FALSE)

boxplot(Framingham_Clean$Education~Framingham_Clean$TenYearCHD,
        main = "Boxplot of Education and CHD Diagnosis", xlab = "Ten Year CHD Diagnosis (0 = NO, 1 = Yes)", ylab= "Patient Education (Years)",
        col= rainbow(2) , border = "Black", horizontal = FALSE, yaxp = c(0,4,4))

boxplot(Framingham_Clean$totChol~Framingham_Clean$TenYearCHD,
        main = "Boxplot of Total Cholesterol and CHD Diagnosis", xlab = "Patient Total Cholesterol", ylab= "Ten Year CHD Diagnosis (0 = NO, 1 = Yes)",
        col= rainbow(3) , border = "Black", horizontal = TRUE, xaxp= c(100,600,10))



#scatter plot for normalized data
plot(Framingham_Clean$sysBP~Framingham_Clean$diaBP, main=" Scatter Plot of Diastolic vs Systolic Blood Pressure", 
     xlab="Diastolic Blood Pressure", ylab="Systolic Blood Pressure", col= rainbow(2), pch = c(15, 17))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("sysBP","DiaBP"), col= rainbow(2) , pch = c(15, 17) )
abline(model6, col= "Black", lwd=3)
model6 <- lm(sysBP ~ diaBP, data = Framingham_Clean)
summary(model1)


plot(Framingham_Clean$Age~Framingham_Clean$prevalentHyp, main=" Scatter Plot of Age vs Prevalent Hypertension", 
     xlab="Prevalent Hypertension", ylab="Age(Years)", col= rainbow(2), pch = c(15, 17))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Age","sysBP"), col= rainbow(2) , pch = c(15, 17) )
abline(model6, col= "Black", lwd=3)
model6 <- lm(Age ~ sysBP, data = Framingham_Clean)
summary(model1)



#CREATE SCATTERPLOT LEGEND
legend("topright", c("Age","sysBP"), col= rainbow(2) , pch = c(15, 17) )
abline(model6, col= "Black", lwd=3)
model6 <- lm(Age ~ sysBP, data = Framingham_Clean)
summary(model1)


#scatter plot For cigperday and cholesterol data
plot(Framingham_Clean$cigsPerDay,Framingham_Clean$totChol, main=" Scatter Plot of CigsPerDay and Total Cholesterol", 
     xlab="Cigarettes Per Day", ylab="Total Cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("cigsPerDay","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model1, col= "Black", lwd=3)
model1 <- lm(totChol ~ cigsPerDay, data = Framingham_Clean)
summary(model1)



#scatter plot For Systolic BP and cholesterol data
plot(Framingham_Clean$sysBP,Framingham_Clean$totChol, main=" Scatter Plot of Systolic BP vs Total Cholesterol", 
     xlab="Systolic BP", ylab="Total Cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Systolic BP","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model2, col= "Black", lwd=3)

model2 <- lm(totChol ~ sysBP, data = Framingham_Clean)
summary(model2)



#scatter plot For Systolic BP and cholesterol data
plot(Framingham_Clean$diaBP,Framingham_Clean$totChol, main=" Scatter Plot of Diastolic Blood Pressure vs Total Cholesterol", 
     xlab="Diastolic BP", ylab="Total Cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Diastolic BP","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model3, col= "Black", lwd=3)

model3 <- lm(totChol ~ diaBP, data = Framingham_Clean)
summary(model3)



#scatter plot For Systolic BP and cholesterol data
plot(Framingham_Clean$glucose,Framingham_Clean$totChol, main=" Scatter Plot of Fasting Glucose vs Total Cholesterol", 
     xlab="Fasting Glucose", ylab="Total Cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Fasting Glucose","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model4, col= "Black", lwd=3)

model4 <- lm(totChol ~ glucose, data = Framingham_Clean)
summary(model4)



#scatter plot For Systolic BP and cholesterol data
plot(Framingham_Clean$BMI,Framingham_Clean$totChol, main=" Scatter Plot of Body Mass Index vs Total Cholesterol", 
     xlab="Body Mass Index", ylab="Total Cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Body Mass Index","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model5, col= "Black", lwd=3)

model5 <- lm(totChol ~ BMI, data = Framingham_Clean)
summary(model5)


#scatter plot For Systolic BP and cholesterol data
plot(Framingham_Clean$heartRate,Framingham_Clean$totChol, main=" Scatter Plot of Heart Rate vs Total Cholesterol", 
     xlab="Body Mass Index", ylab="Total Cholesterol", col=rainbow(2), pch = c(15,16))
#CREATE SCATTERPLOT LEGEND
legend("topright", c("Heart Rate","Cholesterol"), col= rainbow(2) , pch = c(15, 16) )
abline(model6, col= "Black", lwd=3)

model6 <- lm(totChol ~ heartRate, data = Framingham_Clean)
summary(model6)

#create pivot tables
library(tidyr)
library(dplyr)
install.packages("reshape2")
library(reshape2)

Framingham_Clean %>% group_by(Age) %>%
        summarize(median(totChol))

#Create Histogram plots
hist(Framingham_Clean$Age, freq= TRUE, main = "Age Frequency in Framingham Data", 
     ylim = c(0,800), Xlab = "Patient Age (Years)", col = rainbow(8))

hist(Framingham_Clean$Age, freq= FALSE, main = "Age Frequency in Framingham Data", 
      Xlab = "Patient Age (Years)", col = rainbow(8), ylim = c(0.00, 0.05))
x<-seq(30,70, length.out=100)
y<- with(Framingham_Clean, dnorm(x, mean(Age), sd(Age)))
lines(x,y, col="Black", lwd=3)

hist(Framingham_Clean$totChol, freq= FALSE, main = "Cholesterol Frequency in Framingham Data", 
     Xlab = "Patient Age (Years)", col = rainbow(8))
x<-seq(100,400, length.out=650)
y<- with(Framingham_Clean, dnorm(x, mean(totChol), sd(totChol)))
lines(x,y, col="Black", lwd=3)

hist(Framingham_Clean$cigsPerDay, freq= FALSE, main = "Cigarettes Per Day Frequency in Framingham Data", 
      col = rainbow(8))
x<-seq(0,70, length.out=200)
y<- with(Framingham_Clean, dnorm(x, mean(cigsPerDay), sd(cigsPerDay)))
lines(x,y, col="Black", lwd=3)



#create a line plot
library(ggplot2)
ggplot(Framingham_Clean, aes(x = Age, y = totChol)) +
  geom_line(size = 1.5, color = "lightgrey") +
  geom_point(size = 1.5, color = "steelblue") +
  labs(y = "Total Cholesterol", x = "Age in years",title = "Total Cholesterol Changes with Age")
