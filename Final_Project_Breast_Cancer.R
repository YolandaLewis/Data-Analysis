# Assignemnt  : Final Project (Breast Cancer Dataset Group)
# Class       : Advanced Data Analysis and Regression
# Authors     : Yolanda Lewis, Daniel O'Brien, Jose Guzman, Will Ranick, Ross Gibson
# Date        : August 10, 2020

#########################################################
##################### Data Import #######################
#########################################################

#DANIEL path: "/Users/danielobrien/desktop"
#ROSS path: "D:\School\DSC 424 - Advanced Data Analysis\Final Project"

#replace w/ correct path for your computer
setwd("D:/School/DSC 424 - Advanced Data Analysis/Final Project")
#import data to 'dataset' variable
dataset <- read.csv("breast_cancer_dataset.csv", sep=",", header=T)

#prints first 6 rows to be sure data is correct
head(dataset)
#prints last 6 rows to be sure data is correct
tail(dataset)
#confirm 'dataset' row and column count (569; 32)
dim(dataset)
#data structures
str(dataset)
#variable names
names(dataset)

#########################################################
##################### Data Cleansing #####################
#########################################################

#check for NAs across ALL variables (none found)
sum(is.na(dataset))

#convert 'diagnosis' variable into a new numerical binary variable ('num_diagnosis')
#   where a malignant tumor (one that can spread to the surronding area and the rest
#   of the body) is '1' and a benign tumor (one that cannot spread) is a '0'
dataset$num_diagnosis <- ifelse(dataset$diagnosis == 'M', 1, 0)

#confirm new response variable field is created
head(dataset)
dim(dataset)

#remove 'id' field
dataset <- dataset[,2:33]

#confirm only 'id' field is removed
names(dataset)

#split dataset by field type in prep for exploratory analysis
dataset_mean <- data.frame(dataset$num_diagnosis, dataset$radius_mean, dataset$texture_mean,
                           dataset$perimeter_mean, dataset$area_mean, dataset$smoothness_mean,
                           dataset$compactness_mean, dataset$concavity_mean, dataset$concave.points_mean,
                           dataset$symmetry_mean, dataset$fractal_dimension_mean)

dataset_se <- data.frame(dataset$num_diagnosis, dataset$radius_se, dataset$texture_se,
                           dataset$perimeter_se, dataset$area_se, dataset$smoothness_se,
                           dataset$compactness_se, dataset$concavity_se, dataset$concave.points_se,
                           dataset$symmetry_se, dataset$fractal_dimension_se)

dataset_worst <- data.frame(dataset$num_diagnosis, dataset$radius_worst, dataset$texture_worst,
                           dataset$perimeter_worst, dataset$area_worst, dataset$smoothness_worst,
                           dataset$compactness_worst, dataset$concavity_worst, dataset$concave.points_worst,
                           dataset$symmetry_worst, dataset$fractal_dimension_worst)

#########################################################
################# Exploratory Analysis ##################
#########################################################

#import 'psych' or 'Hmisc' and print descriptive statistics
#describe() function is used in both packages
#library(Hmisc)
library(psych)
describe(dataset[2:32]) #removes categorical 'diagnosis' variable

#run and output correlation plot for split datasets (for easier visualization)
library(corrplot)
corrplot(cor(dataset_mean,method="spearman"), method = "number")
corrplot(cor(dataset_se,method="spearman"), method = "number")
corrplot(cor(dataset_worst,method="spearman"), method = "number")

#check graphs for split datasets (for easier visualization)
ggpairs(dataset_mean)
ggpairs(dataset_se)
ggpairs(dataset_worst)

###############################################
################### PCA #######################
###############################################
PCAdataset = subset(dataset, select = -c(diagnosis, num_diagnosis))

#PCA Code
PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

#Test KMO Sampling Adequacy

library(psych)
KMO(PCAdataset)
#Overall MSA =  0.83

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(PCAdataset)
#p-value < 2.22e-16


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(PCAdataset,check.keys=TRUE)
#raw_alpha = 0.58

library(psych)
comp <- fa.parallel(PCAdataset)
comp

p = prcomp(PCAdataset, center=T, scale=T)
p

#Scree Plot
plot(p)
abline(1, 0)

#Check PCA Summary Information
summary(p)
print(p)

#Eigenvalues Greater than 1
p2$values
table(p2$values> 1)

#PCA Loadings 6 Factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=6, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#PCA Loadings 5 Factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#PCA loadings 4 factors
p2 = psych::principal(PCAdataset, rotate="varimax", nfactors=4, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#PCA Loadings 5 Factors
p2 = psych::principal(PCAdataset, rotate="varimax" ,nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.6, sort=T)

#Calculating scores

scores <- p2$scores
scores_1 <- scores[,1]
scores_2 <- scores[,2]
scores_3 <- scores[,3]
scores_4 <- scores[,4]
scores_5 <- scores[,5]

summary(scores_1)

summary(scores_2)

summary(scores_3)

summary(scores_4)

summary(scores_5)

###############################################
#################### FA #######################
###############################################


#GGplot Correlation
ggcorr(dataset_mean, method = c("pairwise","spearman"), label=TRUE)
ggcorr(dataset_se, method = c("pairwise","spearman"), label=TRUE)
ggcorr(dataset_worst, method = c("pairwise","spearman"), label=TRUE)


#Remove area, radius, fractal, concave.points and concavity for multicollinearity
dataset_mean2 <- dataset[,c(4,5,7,8,11)]
dataset_se2 <- dataset[,c(14,15,17,18,21)]
dataset_worst2 <- dataset[,c(24,25,27,28,31)]


#GGplot Correlation
ggcorr(dataset_mean2, method = c("pairwise","spearman"), label=TRUE)
ggcorr(dataset_se2, method = c("pairwise","spearman"), label=TRUE)
ggcorr(dataset_worst2, method = c("pairwise","spearman"), label=TRUE)

#Remove diagnosis and bring together
bcancer4 <- dataset[,c(4,5,7,8,11,14,15,17,18,21,24,25,27,28,31)]
bcancer4

ggcorr(bcancer4, method = c("pairwise","spearman"), label=TRUE)



# Run a correlation test to see how correlated the variables are.  Which correlations are significant
options("scipen"=100, "digits"=5)
round(cor(bcancer4), 2)
MCorrTest = corr.test(bcancer4, adjust="none")
MCorrTest

M = MCorrTest$p
M

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)
#Test KMO Sampling Adequacy


library(psych)
KMO(bcancer4)
#Overall MSA =  0.63, Ok sample size

dim(bcancer4)

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(bcancer4)
#p-value < 2.22e-16 (Very Small Number), We have enough variance to run the model


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(bcancer4,check.keys=TRUE)
#raw_alpha = 0.60

#Conducting Factor Analysis

fit = factanal(bcancer4, 3)
print(fit$loadings, cutoff=.45, sort=T)
summary(fit)

###############################################
############# Logistic Regression #############
###############################################
#install necessary packages
library(cluster)
library(factoextra)
library(MASS)
library(corrplot)
library(DAAG)

dataset <- breast_cancer_dataset[,c(1:32)]
WS_BreastCancer_data2 <-dataset[,c(2:32)] 

#convert diagnosis column into a binary variable (dummy variable)
WS_BreastCancer_data2$diagnosis <- ifelse(WS_BreastCancer_data2$diagnosis == "M", 1,0)
str(WS_BreastCancer_data2)
names(WS_BreastCancer_data2)

#Scale the data
WS_BreastCancer_data2[c(2:31)]<- lapply(WS_BreastCancer_data2[c(2:31)], function(x) c(scale(x)))


#Check for multicollinearity
BC_cor<-cor(WS_BreastCancer_data2)
corrplot(BC_cor, method='circle')


#Try logistic regression with all variables dispite multicollinearity
set.seed(123)
breastCancerData_model1<- glm(diagnosis~., family = binomial(), data = WS_BreastCancer_data2)
summary(breastCancerData_model1)
breastCancerData_model1$rank
car::vif(breastCancerData_model1)

#The model created presented overfitting due to near perfect multicollinearity with many of the variables.
#So we should remove these variables

# Remove correlated variables from the dataset
names(WS_BreastCancer_data2)
clean_breastCancerData<- WS_BreastCancer_data2 [,c(1,6,10,13,16,20,21,30)]
names(clean_breastCancerData)

#separate data into training and testing
train<- clean_breastCancerData[1:445,]
test<- clean_breastCancerData[446:556,]

#Try logistic regression on clean data
set.seed(123)
BreastCancerData_model2 <- glm(diagnosis~., family = binomial(), data = train)
summary(BreastCancerData_model2)
car::vif(BreastCancerData_model2)

#access model performance
predicted <- predict(BreastCancerData_model2, test, type="response")  # predicted scores
pr <- prediction(predicted, test$diagnosis)
model_perform <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(model_perform)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Use automated variable selection

breastCancer_var_selection <- step(BreastCancerData_model2, direction = "backward")

breastCancer_var_selection$coefficients
breastCancer_var_selection$anova
breastCancer_var_selection$aic

#use model from backward selection to perform logistic regression
BreastCancerData_model3 <- glm(diagnosis ~ smoothness_mean + texture_se + symmetry_se + symmetry_worst, family = binomial(), data = clean_breastCancerData)
summary(BreastCancerData_model3)
BreastCancerData_model3$formula
BreastCancerData_model3$coefficients

#get odds ratios and confidence intervals
exp(cbind(OR = coef(BreastCancerData_model3), confint(BreastCancerData_model3)))

exp(coef(BreastCancerData_model3))
exp(coef(BreastCancerData_model3))-1
confint((BreastCancerData_model3))
plot(BreastCancerData_model3)


#use cross validation to check final model.
cv.binary(BreastCancerData_model3,nfolds =10)

#Code Source used to assess logisic regression: https://datascienceplus.com/perform-logistic-regression-in-r/

#analyze table of deviance
anova(BreastCancerData_model3, test = "Chisq")

#assess the fit of the model
install.packages("pscl")
library(pscl)
pR2(BreastCancerData_model3)

#assess the predictability of the final model

library(ROCR)
predicted <- predict(BreastCancerData_model3, test, type="response")
pr <- prediction(predicted, test$diagnosis)
model_perform <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(model_perform)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# ROC (Receiver Operating Characteristics Curve, another view)
plotROC(test$diagnosis, predicted)


library(InformationValue)
optCutOff <- optimalCutoff(test$diagnosis, predicted)
optCutOff

# misclassification error 
misClassError(test$diagnosis, predicted, threshold = optCutOff)

confusionMatrix(test$diagnosis, predicted, threshold = optCutOff)

#Calculate Specificity and Sensitivity
sensitivity(test$diagnosis, predicted, threshold = optCutOff) 
specificity(test$diagnosis, predicted, threshold = optCutOff)

model_outliers <- cooks.distance(BreastCancerData_model3)
model_outliers

###############################################
############# Multiple Regression #############
#Exploratory Only, Not Utilized for Prediction#
###############################################
#Open base dataframe
View(breast_cancer_dataset)

#Install package to create dummy variables
install.packages('fastDummies')
library('fastDummies')

#Make copy of original dataframe
bcd <- breast_cancer_dataset

#Use dummy_cols function to create dummy variables from 'diagnosis'
bcdDum <- dummy_cols(bcd, select_columns = c('diagnosis'), remove_selected_columns = TRUE)
View(bcdDum)

#Amalgamate new variables into single binary variable (0 = Benign, 1 = Malignant)
bcdDum$dM2 <- bcdDum$diagnosis_M * 2
bcdDum$diagnosis_binary <- bcdDum$diagnosis_B + bcdDum$dM2 - 1

#Replace working dataframe with updated one
bcd <- bcdDum
View(bcd)

#Remove unnecessary and incidental variables
bcd$id <- NULL
bcd$diagnosis_B <- NULL
bcd$diagnosis_M <- NULL
bcd$dM2 <- NULL
View(bcd)

#Create correlation matrix to search for multicollinearity
bcdCor <- cor(bcd)
bcdCor

#Create full model (all variables)
bcdFull <- lm(diagnosis_binary ~ ., bcd)

#Perform backwards elimination on full model
bcd1 <- step(bcdFull, direction = "backward")
summary(bcd1)

#Create new (final) model by removing variables that cause multicollinaearity and those which are not significant
bcd2 <- lm(formula = diagnosis_binary ~ compactness_mean + concave.points_mean + smoothness_se + concavity_se + concave.points_se + radius_worst + texture_worst + concavity_worst + symmetry_worst + fractal_dimension_worst, data = bcd)
summary(bcd2)

###############################################
#################### LDA ######################
###############################################

#set correct starting dataset for LDA analysis
LDADataset = dataset[2:32]
dim(LDADataset)

#check correlation between all variables in the dataset
library(corrplot)
corrplot(cor(LDADataset,method="spearman"), method = "number")

#run linear regression model for the sole purpose of checking VIF scores
m1 <- lm(num_diagnosis ~ ., data=LDADataset)
#Check VIF scores
library(DescTools)
VIF(m1)

#remove numerical diagnosis variable and ind. variables that have VIF greater than 10
LDADataset = subset(dataset, select = -c(num_diagnosis, radius_mean, radius_worst,
                                         perimeter_mean, perimeter_worst, concavity_mean,
                                         radius_se, compactness_worst, concave.points_worst,
                                         area_mean, compactness_mean, texture_worst,
                                         concavity_worst, area_se))

names(LDADataset)

#check model assumptions: normality, independence, and equal variances
library(ggplot2)
#GGpairs
ggpairs(LDADataset)

#split data into train and test sets (80/20 split)
library(caTools)
set.seed(10)
sample = sample.split(LDADataset[,1], SplitRatio = .80)
LDADataset_train = subset(LDADataset, sample == TRUE)
LDADataset_test  = subset(LDADataset, sample == FALSE)

#confirm split worked properly
dim(LDADataset)
dim(LDADataset_train)
dim(LDADataset_test)

#import MASS package used for LDA analysis
library(MASS)

#create LDA model using 'lda' function from MASS package
LDA <- lda(diagnosis ~ ., data=LDADataset_train)
LDA

#plot the two different dignosis types
plot(LDA)

#make diagnosis predictions using test set
p = predict(LDA, newdata=LDADataset_test)$class
p

#compare the results of the prediction (Confusion Matrix)
table(p, LDADataset_test$diagnosis)

#calculates prediction accuracy of the LDA model
mean(p == LDADataset_test$diagnosis)
#94.69%

#returns the coefficients for decision boundry line
coef(LDA)