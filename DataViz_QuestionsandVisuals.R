#DSC 465 Homework #3

#load libraries
library(dplyr)
library(ggplot2)
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(ggforce)
library(scales)
library(magrittr)
library(scales)

#explore data
summary(PerceptionExperiment)

#Probelm 1

#Find number of encodings
PerceptionExperiment_I <- PerceptionExperiment %>% 
  group_by(Test, Display, TestNumber) %>%
  summarise()%>%
  arrange( desc(TestNumber))

#Create error column in Perception dataset
PerceptionExperiment_II<-  PerceptionExperiment%>%
  mutate(Error = Response - TrueValue)%>%
  mutate(AbsoluteError = abs(Error))


#Part 1A (Bar graph to show distribution of people for each encoding)
#Also completed using tableau

Perception_plotI<- PerceptionExperiment_II %>% 
  group_by(Test)%>%
  summarise(Avg_error= mean(Error))

ggplot(Perception_boxplot, aes(x= Test, y= Avg_error))+
  geom_col()+
  

#Part 1B (Plot response )
#Completed using tableau

hist(PerceptionExperiment_II$AbsoluteError, main =  "Mapping Perception Distribution of Error", 
     xlab = "Absolute Error", border="Black", col= "light blue")


#Part 1C

#filter data for display and subjects
Perception_boxplot <- PerceptionExperiment_II %>%
  filter(Subject >= 56 & Subject <= 73)
  
ggplot(PerceptionExperiment_II, aes(x= factor(Display), y= Error))+
  geom_boxplot()+
  ggtitle("Distribution of Mapping Perception Error for Subjects 56-73")+
  labs(x= "Display", y= "Subject Perception Error")


#Part 1D

PerceptionExperiment_II%>%
  ggplot(aes(Trial, Response, color= Subject))+
  geom_violin()+
  geom_sina()+
  ggtitle("Distribution of Subject Responses for Trials B, C, and D")

#----------------------------------------------------------------------------------------------------------

# Problem 2 
summary(MessierData)
sum(is.na(MessierData))
MessierData<- na.omit(MessierData)

#Part 2A. 
#Done using tableau
Messier_SP <- ggplot(MessierData, aes(x=year, y= Messier..))

Messier_SP+
  geom_point(aes(color= Kind))+
  ylim(0, max(MessierData$Messier..))+
  labs(x= "Year", y= "Messier Number")+
  ggtitle("Meesier Number Vs Year")

#Part 2B

ggplot(MessierData, aes(x= reorder(Kind,Distance..LY.,na.rm =TRUE) , y= Distance..LY.))+
  geom_boxplot()+
  scale_y_log10(breaks = 10^(3:8),labels= trans_format("log10", math_format(.x)))+
  ggtitle("Distance Vs Kind of Object")+
  labs(x= "Kind", y= "Log of Object Distance")
  
#Part 2C (Create a scatter plot)

ggplot(MessierData, aes(x=Apparent.Magnitude, y=Distance..LY.))+
  geom_point(alpha= 0.5,size= 1.5, aes(color= Kind))+
  scale_y_log10(breaks = 10^(3:8),labels= trans_format("log10", math_format(.x)))+
  labs(x= "Apparent Magnitude", y= "Log of Object Distance")+
  ggtitle("Object Distance Vs Apparent Magnitude")

#Part 2D

ggplot(MessierData, aes(x=Apparent.Magnitude, y=Distance..LY.))+
  geom_point(alpha= 0.5, aes(color= Kind, size= Size.....))+
  scale_y_log10(breaks = 10^(3:8),labels= trans_format("log10", math_format(.x)))+
  labs(x= "Apparent Magnitude", y= "Log of Object Distance")+
  ggtitle("Object Distance Vs Apparent Magnitude")
#--------------------------------------------------------------------------------------------------
#Probelm 3 completed using Tableau

#----------------------------------------------------------------------------------------------

#Probelm 4

summary(AirQuality)
sum(is.na(AirQuality))
AirQuality<- na.omit(AirQuality)

#Part A Create a scatter plot with a fitted line
AirQuality_SP <- ggplot(AirQuality, aes(x=Wind, y= Solar.R))

AirQuality_SP+
  geom_point()+
  stat_smooth(method=loess)+
  ggtitle("Relationship Between Solar Radiation and Wind")

#Part B

AirQuality_compare <- AirQuality %$%
  data.frame(wind=sort(Wind),
             ozone=sort(Ozone),
             solar=sort(Solar.R),
             temp=rescale(Temp, to=c(0,1)) %>% sort)

AirQuality_compare%>%
  mutate(wind=(wind-min(wind))/(max(wind)-min(wind))) %>%
  mutate(solar=rescale(solar, to=c(0,1))) %>%
  ggplot(aes(wind,solar)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0)+
  ggtitle("Wind Vs Solar Radiation Distribution Comparision")

#Part C  
library(reshape)

Combined_AirQuality <- melt(AirQuality, id=c("Day","Month"))
head(Combined_AirQuality)

Combined_AirQuality %>%
  ggplot(aes(variable, value))+
  geom_beeswarm()+
  labs(x="Measurement")+
  ggtitle("Measurement Distribution Comparison")

#Part D (Make QQ plot for extra credit)
#n/a


