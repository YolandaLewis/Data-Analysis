#DSC 465 HOMEWORK 1

#load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(cowplot)
library(scales)

#create a new data frame for dataset
Intel.98 <- Intel.1998 [c(1:8)]

#Change Date data type
Intel.98$Date<- as.Date(Intel.98$Date, format("%m/%d/%y"))

#Problem 1

#Part A.  Create a line graph
Intel_line<- ggplot(data =Intel.98 , aes(x=Date, y= Close))+geom_line(color= "steelblue", size=1)+ 
  ggtitle("Intel Stock Closing Price Vs Closing Date Per Quarter")+ scale_x_date(date_breaks="1 month", date_labels = "%b%Y")
Intel_line

#Part B. Create a bar graph
ggplot(data =Intel.98 , aes(x=Date, y= Volume))+geom_bar(stat="identity", color="steelblue") + ggtitle("Intel Stock Volume Vs Closing Date Per Quarter")

#Part C. Crate a Scatter plot

#create a range variable
Range<- Intel.98$High - Intel.98$Low

#Add variable to Data frame
Intel.98$Range<-Range

#Create scatterplot
ggplot(Intel.98, aes(x=Volume, y=Range)) + geom_point()+ggtitle("Intel Stock Volume vs Daily Stock Range")

#Problem 4 Create visual of cell phone carriers

cellPlans = data.frame(
  c("ATT", "Sprint", "Verizon", "ATT", "Sprint",
    "Verizon", "ATT", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "ATT", "ATT", "Sprint"),
  c(1, 1, 2, 3, 3, 4, 6, 6, 8, 10, 12, 12, 16, 16,
    24, 24, 25, 30, 40),
  c(30, 20, 35, 40, 30, 50, 60, 45, 70, 80, 80, 60,
    90, 90, 110, 80, 110, 135, 100))
names(cellPlans) = c("Company", "DataGB", "Price")


ggplot(data= cellPlans, aes(x=DataGB, y= Price, color= Company))+geom_line() + 
  ggtitle("Cell Phone Data Price (GB) Per Cell Phone Carrier")

cell_Bar_Plot<-ggplot(data= cellPlans, aes(x=Company, y= DataGB, fill= Company)) +geom_bar(stat = 'identity', width = 0.5) + 
  ggtitle("Cell Phone Data Plans Per Cell Phone Carrier")

cell_Bar_Plot

# make the plot horizontal
plot_horizontal <- cell_Bar_Plot + coord_flip()
plot_horizontal


