#create model with all variables
MyDataset_model1 <- lm(totChol ~ ., data = totChol_outliers_removed)
summary(MyDataset_model1)

plot( totChol_outliers_removed,MyDataset_model1$residuals, main = "Residuals
      (Response is Total Cholesterol)")

#look at residuals of a histogram 
hist(MyDataset_model5$residuals, main = "Histogram of Model residuals", xlab= "Residuals")

#create QQplots and PPlots
qqnorm(MyDataset_model5$residuals)
qqline(MyDataset_model5$residuals)

# check the sum of the residuals
sum(MyDataset_model5$residuals)

#____________________________________________________________
# trying to figure out zscore normalization for residuals
mean1 = mean(Resid_model1.MydataSet$residuals)
sd1 = sd(Resid_model1.MydataSet$residuals)
resid_zscore_age =(Resid_model1.MydataSet - mean1)/sd1
#_______________________________________________________________


#Plot residuals of the first order model
plot(MyDataset_model5$residuals, main = "Residuals Versus Fitted values
      (Response is Total Cholesterol)", xlab= "Fitted Value", ylab= "Residual")
durbinWatsonTest(MyDataset_model5)

#plot residuals against the independent variables----------------------------

#Plot for Age
Resid_model1.MydataSet <- lm( totChol~ Age, data = totChol_outliers_removed)
summary(Resid_model1.MydataSet)

Age_Std_Residuals <- rstandard(Resid_model1.MydataSet)
plot( Resid_model1.MydataSet$fitted.values,Resid_model1.MydataSet$residuals, main = "Residuals Versus Age
      (Response is Total Cholesterol)", xlab= "Age")
durbinWatsonTest(Resid_model1.MydataSet)

#---try to transform age variable---------------------------------------------
sqrt(totChol_outliers_removed$Age)
asin(sqrt(totChol_outliers_removed$Age))
#------------------------------------------------------------------------

#plot for cigsPerDay
Resid_model2.MydataSet <- lm( totChol~ cigsPerDay, data = totChol_outliers_removed)
summary(Resid_model2.MydataSet)

cigsPerDay_Std_Residuals <- rstandard(Resid_model2.MydataSet)
plot( totChol_outliers_removed$cigsPerDay,cigsPerDay_Std_Residuals, main = "Residuals Versus cigsPerDay
      (Response is Total Cholesterol)", xlab= "Cigarettes Per Day")


#plot for BPMeds
Resid_model3.MydataSet <- lm( totChol~ BPMeds, data = totChol_outliers_removed)
summary(Resid_model3.MydataSet)

BPMeds_Std_Residuals <- rstandard(Resid_model3.MydataSet)
plot( totChol_outliers_removed$BPMeds, BPMeds_Std_Residuals, main = "Residuals Versus BPMeds
      (Response is Total Cholesterol)", xlab= "BPMeds (0= not present, 1= Present)")


#plot for prevalentStroke
Resid_model4.MydataSet <- lm( totChol~ prevalentStroke, data = totChol_outliers_removed)
summary(Resid_model4.MydataSet)

prevalentStroke_Std_Residuals <- rstandard(Resid_model4.MydataSet)
plot( totChol_outliers_removed$prevalentStroke,prevalentStroke_Std_Residuals, main = "Residuals Versus Prevalent Stroke
      (Response is Total Cholesterol)", xlab= "Prevalent Stroke (0= not present, 1= Present)")



#plot for prevalentHyp
Resid_model5.MydataSet <- lm( totChol~ prevalentHyp, data = totChol_outliers_removed)
summary(Resid_model5.MydataSet)

prevalentHyp_Std_Residuals <- rstandard(Resid_model6.MydataSet)
plot( totChol_outliers_removed$prevalentHyp,prevalentHyp_Std_Residuals, main = "Residuals Versus Prevalent Hypertension
      (Response is Total Cholesterol)", xlab= "Prevalent Hypertension (0= not present, 1= Present)")


#plot for diabetes
Resid_model6.MydataSet <- lm( totChol~ diabetes, data = totChol_outliers_removed)
summary(Resid_model6.MydataSet)

diabetes_Std_Residuals <- rstandard(Resid_model6.MydataSet)
plot( totChol_outliers_removed$diabetes,diabetes_Std_Residuals, main = "Residuals Versus Diabetes
      (Response is Total Cholesterol)", xlab= "Diabetes")


#plot for sysBP
Resid_model7.MydataSet <- lm( totChol~ sysBP, data = totChol_outliers_removed)
summary(Resid_model7.MydataSet)

sysBP_Std_Residuals <- rstandard(Resid_model7.MydataSet)
plot( totChol_outliers_removed$sysBP, sysBP_Std_Residuals, main = "Residuals Versus Systolic Blood Pressure
      (Response is Total Cholesterol)", xlab= "Systolic Pressure")

#plot for diaBP
Resid_model8.MydataSet <- lm( totChol~ diaBP, data = totChol_outliers_removed)
summary(Resid_model8.MydataSet)

plot( totChol_outliers_removed$diaBP,diaBP_Std_Residuals, main = "Residuals Versus Diastolic Blood Pressure
      (Response is Total Cholesterol)", xlab= "Diastolic Pressure")
diaBP_Std_Residuals <- rstandard(Resid_model8.MydataSet)


#plot for BMI
Resid_model9.MydataSet <- lm( totChol~ BMI, data = totChol_outliers_removed)
summary(Resid_model9.MydataSet)

BMI_Std_Residuals <- rstandard(Resid_model9.MydataSet)
plot( totChol_outliers_removed$BMI,BMI_Std_Residuals, main = "Residuals Versus BMI
      (Response is Total Cholesterol)", xlab= "BMI")

#plot for heartRate
Resid_model10.MydataSet <- lm( totChol~ heartRate, data = totChol_outliers_removed)
summary(Resid_model10.MydataSet)

heartRate_Std_Residuals <- rstandard(Resid_model10.MydataSet)
plot( totChol_outliers_removed$heartRate, heartRate_Std_Residuals, main = "Residuals Versus Heart Rate
      (Response is Total Cholesterol)", xlab= "Heart Rate (Beats Per Minute)")

#plot for glucose
Resid_model11.MydataSet <- lm( totChol~ glucose, data = totChol_outliers_removed)
summary(Resid_model11.MydataSet)

glucose_Std_Residuals <- rstandard(Resid_model11.MydataSet)
plot( totChol_outliers_removed$glucose, glucose_Std_Residuals, main = "Residuals Versus Glucose
      (Response is Total Cholesterol)", xlab= "Glucose")

#plot for TenYearCHD
Resid_model12.MydataSet <- lm( totChol~ TenYearCHD, data = totChol_outliers_removed)
summary(Resid_model12.MydataSet)

TenYearCHD_Std_Residuals <- rstandard(Resid_model12.MydataSet)
plot( totChol_outliers_removed$TenYearCHD, TenYearCHD_Std_Residuals, main = "Residuals Versus Diagnosis of CHD Within 10 years
      (Response is Total Cholesterol)", xlab= "TenYearCHD")


#variable did not work, remove it from the dataset
totChol_outliers_removed<- select(totChol_outliers_removed,-18)

#make plots against the model----------------------------------------------------------------------

#standardize residuals in the model
Std_Residuals <- rstandard(MyDataset_model9 )

#Plot Age versus residuals
plot( totChol_outliers_removed$Age,Std_Residuals, main = "Residuals Versus Age 
(Response is Total Cholesterol)", xlab= "Age", ylab= "Standardized Residuals")


#plot for cigPerDay versus residuals
plot( totChol_outliers_removed$cigsPerDay,Std_Residuals, main = "Residuals Versus cigsPerDay
  (Response is Total Cholesterol)", xlab= "Cigarettes Per Day", ylab= "Standardized Residuals")

#plot for BPMeds
plot( totChol_outliers_removed$BPMeds, Std_Residuals, main = "Residuals Versus BPMeds
  (Response is Total Cholesterol)", xlab= "BPMeds (0= not present, 1= Present)", ylab= "Standardized Residuals")
 
 #plot for prevalentHyp
plot( totChol_outliers_removed$prevalentHyp,Std_Residuals, main = "Residuals Versus Prevalent Hypertension
     (Response is Total Cholesterol)", xlab= "Prevalent Hypertension (0= not present, 1= Present)", ylab= "Standardized Residuals")

#plot for sysBP
plot( totChol_outliers_removed$sysBP, Std_Residuals, main = "Residuals Versus Systolic Blood Pressure
(Response is Total Cholesterol)", xlab= "Systolic Pressure", ylab= "Standardized Residuals")

#plot for diaBP
plot( totChol_outliers_removed$diaBP,Std_Residuals, main = "Residuals Versus Diastolic Blood Pressure
(Response is Total Cholesterol)", xlab= "Diastolic Pressure", ylab= "Standardized Residuals")

#plot for diaBP_BMI
plot( totChol_outliers_removed$diaBP_BMI,Std_Residuals, main = "Residuals Versus diaBP_BMI
             (Response is Total Cholesterol)", xlab= "diaBP_BMI", ylab= "Standardized Residuals")

#plot for BMI_age
plot( totChol_outliers_removed$BMI_Age,Std_Residuals, main = "Residuals Versus BMI_age
             (Response is Total Cholesterol)", xlab= "BMI_age", ylab= "Standardized Residuals")

#plot for diaBP_age
plot( totChol_outliers_removed$diaBP_Age,Std_Residuals, main = "Residuals Versus diaBP_age
             (Response is Total Cholesterol)", xlab= "diaBP_age", ylab= "Standardized Residuals")

#plot for hearRate
plot( totChol_outliers_removed$heartRate,Std_Residuals, main = "Residuals Versus HeartRate
             (Response is Total Cholesterol)", xlab= "HeartRate", ylab= "Standardized Residuals")

#plot the dependent variable residuals
plot(totChol_outliers_removed$totChol, MyDataset_model9$residuals, main="Residuals Versus Dependent Variable
            (Response is Total Cholesterol)", xlab= "Total Cholesterol", ylab= "Standardized Residuals")

#create create second order and interaction terms------------------------------------------------------
totChol_outliers_removed$diaBP_Age <- totChol_outliers_removed$diaBP * totChol_outliers_removed$Age
totChol_outliers_removed$BMI_Age <- totChol_outliers_removed$BMI * totChol_outliers_removed$Age
totChol_outliers_removed$diaBP_BMI <- totChol_outliers_removed$diaBP* totChol_outliers_removed$BMI
totChol_outliers_removed$heartRate_BMI <- totChol_outliers_removed$heartRate * totChol_outliers_removed$BMI

#variable did not work, remove it from the dataset
totChol_outliers_removed<- select(totChol_outliers_removed,-17)

#Final Model________________________________________________________________________
MyDataset_model9 <- lm(totChol ~ Age + BPMeds + prevalentHyp +
                          + sysBP + diaBP + BMI + heartRate
                       + diaBP_Age + BMI_Age + diaBP_BMI, data = totChol_outliers_removed)
summary(MyDataset_model9)

#__________________________________________________________________________--
plot(MyDataset_model9$fitted.values,Std_Residuals, main= "Residuals Versus Fitted values",
     xlab= "Fitted Value", ylab= "Residual")

durbinWatsonTest(MyDataset_model9)
vif(MyDataSet_model9)

#Perform backward selection
#use backward selection


back_sel_finalModel <- stepAIC(MyDataset_model1, direction="backward")

MyDataset_model9_data <- totChol_outliers_removed[c(1,5,7:11,14:16)]
TenFold_cv<- cv.lm(data= MyDataset_model9_data, form.lm = formula(totChol ~ .), m=10)
