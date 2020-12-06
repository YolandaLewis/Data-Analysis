#Create a dataset removing the variables: Sex, Education, and currentSmoker

#MyDataset<- (Framingham_Clean [c(2,5:16)])


MyDataset <- Framingham_Clean

# Plot the dataset excluding Sex, Education, and currentsmoker

plot(totChol_outliers_removed, main= "Dataset Scatterplots with variables to be considered in the model", col= "Purple")

boxplot(MyDataset, main= "Boxplots of Variables to be used in Analysis", col = rainbow(13))

cor(totChol_outliers_removed)

#Check five number summary to observe the data distribution
summary(MyDataset [c(7:12)])

#Remove outliers from continous variables
Q <- quantile(MyDataset$totChol, probs=c(.25, .75), na.rm = FALSE)
totChol_IQR <- IQR(MyDataset$totChol)
totChol_upper <-  Q[2]+1.5*totChol_IQR # Upper Range  
totChol_lower<- Q[1]-1.5*totChol_IQR # Lower Range

totChol_outliers_removed1<- subset(MyDataset, MyDataset$totChol > (Q[1] - 1.5*totChol_IQR) & MyDataset$totChol < (Q[2]+1.5*totChol_IQR))

#observe the new boxplot
boxplot(totChol_outliers_removed, main= "Boxplot with Outliers in Total Cholesterol Removed", col= rainbow(13))       
summary(totChol_outliers_removed[c(7:12)])

## build first regression model
MyDataset_model1 <- lm(totChol ~ ., data = totChol_outliers_removed)
summary(MyDataset_model1)

library("car")
vif(MyDataset_model1)

# Model 2 glucose removed
MyDataset_model2 <- lm(totChol ~ Age + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp +
                          diabetes + sysBP + diaBP + BMI + heartRate + TenYearCHD,  data = totChol_outliers_removed)
summary(MyDataset_model2)
vif(MyDataset_model2)

# Model 3 prevalent Stroke removed
MyDataset_model3 <- lm(totChol ~ Age + cigsPerDay + BPMeds + prevalentHyp +
                         diabetes + sysBP + diaBP + BMI + heartRate + TenYearCHD,  data = totChol_outliers_removed)
summary(MyDataset_model3)
vif(MyDataset_model3)

# model 4 diabetes removed
MyDataset_model4 <- lm(totChol ~ Age + cigsPerDay + BPMeds + prevalentHyp +
                          sysBP + diaBP + BMI + heartRate + TenYearCHD,  data = totChol_outliers_removed)
summary(MyDataset_model4)
vif(MyDataset_model4)

#model 5 TenYearCHD removed
MyDataset_model5 <- lm(totChol ~ Age + cigsPerDay + BPMeds + prevalentHyp +
                         sysBP + diaBP + BMI + heartRate,  data = totChol_outliers_removed)
summary(MyDataset_model5)
vif(MyDataset_model5)

#model 6 prevalenthyp removed
MyDataset_model6 <- lm(totChol ~ Age + cigsPerDay + BPMeds +
                         sysBP + diaBP + BMI + heartRate,  data = totChol_outliers_removed)
summary(MyDataset_model6)
vif(MyDataset_model)

#model 7 sysBP removed
MyDataset_model7 <- lm(totChol ~ Age + cigsPerDay + BPMeds + diaBP + BMI + heartRate,  data = totChol_outliers_removed)
summary(MyDataset_model7)
vif(MyDataset_model7)

#model 8 cigsPerday and BPMEDs removed
MyDataset_model8 <- lm(totChol ~ Age + diaBP + BMI + prevalentHyp + heartRate,  data = totChol_outliers_removed)
summary(MyDataset_model8)
vif(MyDataset_model8)

##use forward selction to confirm model 5 is the best model
MyDataset_model9 <- lm(totChol ~ 1, data=totChol_outliers_removed)
forward_Selection_MyData <- stepAIC(MyDataset_model9, direction="forward", scope=list(upper=MyDataset_model1,lower=MyDataset_model9))

#use backward selection
back_sel_MyData <- stepAIC(MyDataset_model1, direction="backward")

#view final models using forward and backward selection
forward_Selection_MyData$anova
back_sel_MyData$anova

##Perform cross validation on the model
TenFold_cv<- cv.lm(data= totChol_outliers_removed[c(1,2,5,7,8,9,10,11)], form.lm = formula(totChol ~ .), m=10)


## Create seperate datasets
data_hyp_notPresent<- subset(totChol_outliers_removed$prevalentHyp == 0)
data_hyp_Present <- subset(totChol_outliers_removed$prevalentHyp == 1)

#create model to use for feature selection
MyDataset_hyp_notPresent_model1 <- lm(totChol ~ ., data = data_hyp_notPresent)
summary(MyDataset_hyp_notPresent_model1)

MyDataset_hyp_present_model1 <- lm(totChol ~ ., data = data_hyp_Present)
summary(MyDataset_hyp_present_model1)

#perform backward feature selection 
hyp_Present <- stepAIC(MyDataset_hyp_notPresent_model1, direction="backward")
hyp_notPresent <- stepAIC(MyDataset_hyp_present_model1, direction="backward")
