#Question 1

#Basic Statistical Data Description.This summary includes the Minimum values,
#1st and 3rd Quartiles, median, mean, and the max values
summary(bank_data)

#Create a function to calculate the modes of the numeric and categorical valaues

modeValue<- function(x){
  modeval<- unique(x)
  modeval[which.max(table(match(x, modeval)))]
  
}

modeValue(bank_data$age)
modeValue(bank_data$income)
modeValue(bank_data$children)
modeValue(bank_data$gender)
modeValue(bank_data$region)
modeValue(bank_data$married)
modeValue(bank_data$car)
modeValue(bank_data$savings_acct)
modeValue(bank_data$current_acct)
modeValue(bank_data$mortgage)
modeValue(bank_data$pep)

table(bank_data$age)

#histogram to show frequency of categorical and numeric attributes
hist(bank_data$age, main= "Histogram for Age", xlab ='Age (years)', border = "black",
     col = rainbow(5),ylim=c(0,80), xlim = c(10,70))
abline(h=seq(0,80,10), col= "grey", lty = "dotted")

#histogram for income
hist(bank_data$income, main= "Histogram for Income", xlab ='Income (dollars)', border = "black",
     col = rainbow(5),ylim=c(0,120), xlim = c(1000,70000))
abline(h=seq(0,120,10), col= "grey", lty = "dotted")

#histogram for children
hist(bank_data$children, main= "Histogram for Children", xlab ='Number of Children', border = "black",
     col = rainbow(5),ylim=c(0,300), xlim = c(0,3))

#boxplot to show the five number summary for numeric attributes
boxplot(bank_data$age,main= "Age BoxPlot",ylab="Customer Age (years)", 
        ylim=c(10,70),col= "purple", border= "Black", horizontal = FALSE)

boxplot(bank_data$income,main= "Income BoxPlot",ylab="Income (Dollars)", ylim=c(5000, 70000),
        col= "purple", border= "Black", horizontal = FALSE)

boxplot(bank_data$children,main= "BoxPlot for Children",ylab="Number of Children", ylim=c(0,3),
        col= "purple", border= "Black", horizontal = FALSE)

#standard deviation of numeric variables
sd(bank_data$age)
sd(bank_data$income)
sd(bank_data$children)

#variance of numeric variables
var(bank_data$age)
var(bank_data$income)
var(bank_data$children)


#PLOT DATA andd perform summaries on data based on customers who purchase PEP.
plot(x= bank_data$pep, y = bank_data$age,frame = FALSE, 
     xlab = "PEP", ylab= "Age (Years)", main = "Age versus Purchase of PEP", ylim=c(10,70),col = colors , pch = shapes)

plot(x= bank_data$pep, y = bank_data$income,frame = FALSE,xlab = "PEP", ylab= "Income (dollars)", 
     ylim=c(5000,70000), main = "Income versus Purchase of PEP",col = colors , pch = shapes)

summary(bank_data$age,bank_data$pep)
summary(bank_data$income,bank_data$pep)
summary(bank_data$children,bank_data$pep)



bank.data<-data.frame(bank_data$pep,bank_data$income)
bank.data = table(bank_data$pep, bank_data$income)
print(bank.data)
print(chisq.test(bank.data))
