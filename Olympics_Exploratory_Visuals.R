# DSC 465 Group Project Visualizations

#install libraries
install.packages("ggplot")
install.packages("ggmap")
library(ggplot2)
library(tidyselect)
library(tidyr)
library(dplyr)
library(plotly)
library(tidyverse)
library(data.table)
library(gridExtra)
library(forcats)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library("maps")

# Data Cleaning

# Check Out Data
sum(is.na(olympics))
head(olympics)
summary(olympics)

# Change city variable name
rename(olympics, "City" = ï..City) %>% head()

#check female participation in sports over time

#Create data frame for athletes and Gender
Athlete_GenderCount <- olympics %>% group_by(Year, Gender)%>%
  summarize(Athletes = length(unique(Athlete)))


# Plot female vs male athletes
ggplot(Athlete_GenderCount, aes(x=Year, y=Athletes, group=Gender, color=Gender)) +
  geom_point(size=2) +
  geom_line() +
  geom_text(x=1996, y=500, label= "IOC Mandates 
            Women Competitors", color="red")+
  geom_text(x=1994, y= 950, label= "Games Are 
            Boycott Free", color = "Black")+
  scale_color_manual(values=c("Blue","Red")) +
  #scale_x_continuous(breaks=c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))+
  labs(y= "Number of Athletes", title = "Total Number of Male and Female Olympians in the Summer Olympics From 1976-2008") +
  theme(plot.title = element_text(hjust = 0.5))



#Check medals for top female Olympians

#Data frame with countries and count of medals for female participates
female_medal_count <- olympics %>% filter(Gender=='Women', !is.na(Medal)) %>%
  group_by(Country_Code,Athlete, Medal) %>%
  summarize(Count=length(Medal))%>%
  summarize(Total = sum(Count)) %>%
  arrange(desc(Total))

#dataframe with top countries for female total medals
Top_Countries_Female_medals <- olympics %>% filter(Gender=='Women', !is.na(Medal)) %>%
  group_by(Country_Code, Athlete, Medal)%>%
  summarize(Total_Medals = sum(Bronze) + sum(Silver) +sum(Gold))%>%
  summarize(Count=length(Medal))%>%
  summarize(Total = sum(Count)) %>%
  arrange(desc(Total))

# Order Athletes by total medal count
athlete_medal_count <- female_medal_count %>%
  group_by(Athlete, Count) %>%
  arrange(desc(Count)) %>%
  select(Athlete)

# Plot Top 5 Women Olympians
install.packages("ggflags")
library(ggflags)

Top5_female_medals <- olympics %>% filter(Gender=='Women', !is.na(Medal)) %>%
  group_by(Country_Code, Athlete, Medal)%>%
  summarize(Total_Medals = sum(Bronze) + sum(Silver) +sum(Gold))%>%
  summarize(Total = sum(Total_Medals))%>%
  arrange(desc(Total_Medals))

#Make Top 5 female bar plot "Code Help from Jonathan"----------------------------------------------------------
Torres <- filter(olympics, Athlete == "TORRES, Dara")
thompson <- filter(olympics, Athlete == "THOMPSON, Jenny")
fischer <- filter(olympics, Athlete == "FISCHER, Birgit")
coughlin <- filter(olympics, Athlete == "COUGHLIN, Natalie")
Van_Almsick <- filter(olympics, Athlete == "VAN ALMSICK, Franziska")

five <- rbind(Torres, thompson, fischer, coughlin, Van_Almsick)

Female_Athlete <- group_by(five, Athlete, Country_Code, Medal) %>% summarize(All_Medals = sum(Bronze)+
                                                                       sum(Silver) + sum(Gold))

Female_Athlete$Country_Code[which(Female_Athlete$Country_Code == "USA")] = "us"
Female_Athlete$Country_Code[which(Female_Athlete$Country_Code == "RUS")] = "ru"
Female_Athlete$Country_Code[which(Female_Athlete$Country_Code == "GER")] = "de"

Female_Athlete$Medal <- factor(Female_Athlete$Medal, levels = c("Bronze", "Silver", "Gold"))


Top5_Female_Winners <- ggplot(Female_Athlete, aes(x = reorder(Athlete, All_Medals), y = All_Medals, fill=Medal)) +
  geom_bar(stat="identity", colour= "black", size= .3, width= .4)+
  coord_flip() +
  scale_fill_manual(values = c("#E69F00", "grey", "gold"), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16))+
  labs(y="Number of Medals", x=" Olympian")+
  ggtitle("Top 5 Female Olympians from 1976-2008")+
  theme_bw()+
  theme(panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.background = element_rect(colour="black", size=0.5),
        axis.ticks.y=element_blank(),
        panel.grid.minor = element_line(size = 0.1, linetype = 'dashed',
                                        colour = "grey"),
        panel.grid.major = element_line(size = 0.35, linetype = 'dashed',
                                        colour = "grey"))
Top5_Female_Winners

# Top 5 Female barplot-------------------------------------------------------------------------------------
Top5_female_barplot<- ggplot(Top5_female_medals[1:5,], aes(x= fct_reorder(Athlete,Total), y=Total, fill=Country_Code)) +
  geom_col(width = 0.25) +
  coord_flip()+
  labs(y= "Total Medals", x= "Athlete", fill= "Country") +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14))+
  ggtitle("Top Female Olympic Medal Winners from 1976-2008") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_grey()
Top5_female_barplot


Top5_female_barplot<- ggplot(Top5_female_medals[1:5,], aes(x= fct_reorder(Athlete,Total_Medals), y=Total_Medals, fill=Country_Code)) +
  geom_col(width = 0.25) +
  coord_flip()+
  labs(y= "Total Medals", x= "Athlete", fill= "Country") +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14))+
  ggtitle("Top Female Olympic Medal Winners from 1976-2008") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_grey()
Top5_female_barplot



#Explore Data---------------------------------------------------------------------------------------------------------------
Top_Countries_medals <- olympics %>% filter(!is.na(Medal)) %>%
  group_by(Country_Code, Athlete, Medal)%>%
  summarize(Total_Medals = sum(Bronze) + sum(Silver) +sum(Gold))%>%
  summarize(Count=length(Medal))%>%
  summarize(Total = sum(Count)) %>%
  arrange(desc(Total))
  
  
#Animate gender line plot-----------------------------------------------------------------------
install.packages("gifski")
install.packages("gapminder")
library(gganimate) # for animation layer
library(gifski) # for making the animation; restart R after installing
library(gapminder) # for gapminder data about countries

Olympics_Gender_TimePlot <- ggplot(Athlete_GenderCount, aes(x=Year, y=Athletes, color=Gender)) +
  geom_point(aes(group = seq_along(Year))) +
  geom_point(size=2)+
  geom_line()  +
  geom_text(x=1996, y=500, label= "IOC Mandates 
            Women Competitors", color="red")+
  geom_text(x=1994, y= 950, label= "Games Are 
            Boycott Free", color = "Black")+
  transition_reveal(Year)+
  scale_color_manual(values=c("darkblue","red")) +
  scale_x_continuous(breaks=c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))+
  labs(y= "Number of Athletes", title = "Total Number of Male and Female Olympians From 1976-2008") +
  theme(plot.title = element_text(hjust = 0.5))
Olympics_Gender_TimePlot

# Save as gif:
anim_save("Olympics_Gender_LinePlot2.gif")

#-----------------------------------------------------------------------------------------------

#Number of althletes particpating overtime------------------------------------------------
Athlete_time_plot<-olympics %>%
  group_by(Year) %>%
  summarize(Athletes = length(unique(Athlete)))%>%
  ggplot(aes(x = Year, y = Athletes)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("red")) +
  scale_x_continuous(breaks=c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))+
  labs(x = "Year", y = "Number of Athletes", title = "Number of Athletes Participating in the Olympics Overtime (1976-2008)")+
  theme_grey()  

Athlete_time_plot
  
  
  
#Try a word cloud---------------------------------------------------
install.packages("wordcloud")
library(wordcloud)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("wordcloud2")
library(wordcloud2) 

install.packages("tm")
library(tm)

#Vector containing only text
Sport_cloud<- olympics$Sport

##create a corpus
olympicsSportCloud<-Corpus(VectorSource(Sport_cloud))

#create document term matrix
dtm <- TermDocumentMatrix(olympicsSportCloud) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
Sportcloud_df <- data.frame(word = names(words),freq=words)

#Generate word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = Sportcloud_df$word, freq = Sportcloud_df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale = c(3.5, 0.60),
          colors=brewer.pal(8, "Dark2"))

set.seed(1234)
wordcloud2(data=Sportcloud_df, size=1.6, color= "random-dark", shape ='circle' )

#Create a mosaic plot--------------------------------------------------------------
olympics_table<-ftable(olympics)



#Create a highest Medal map----------------------------------------------------------------------
olympic_Gold <- olympics %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total))


olympic_Silver <- olympics %>%
  group_by(Country) %>%
  filter(Medal == "Silver") %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total)) %>%
  filter(!Country %in% olympic_Gold$Country) %>%
  mutate(Medal_Total = "Silver")

olympic_Bronze <- olympics %>%
  group_by(Country) %>%
  filter(Medal == "Bronze") %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total)) %>%
  filter(!(region %in% olympic_Gold$region & region %in% olympic_Silver$region) )%>%
  mutate(Medal_Total = "Bronze")

olympic_Gold$Medal_Total <- "Gold"


Medal_Total <- rbind(olympic_Gold, olympic_Silver)
Medal_Total <- rbind(Medal_Total, olympic_Bronze)

Medal_Total$region[which(Medal_Total$region == "United States")] = "USA"


world_map <- map_data("world")
olympic_world_map <- left_join(world_map, Medal_Total, by ='region')
olympic_world_map$Medal_Total[is.na(olympic_world_map$Medal_Total)] <- "No Medal"

olympic_world_map$Medal_Total<-factor(olympic_world_map$Medal_Total, levels = c("Gold","Silver", "Bronze", "No Medal" ))

#Create a map for overlay
#winning_Countries<-olympic_world_map%>%dplyr::filter(!Medal_Total=="No Medal")

ggplot(data = olympic_world_map, aes(x=long, y=lat, group = group, fill=Medal_Total)) +
  geom_polygon(colour='black') +
  scale_fill_manual(name= "Medal Won", values = c("gold", "darkgrey", "#E69F00", "white"))+
  labs(x= "longtude", y= "latitude",title = 'Highest Medal Won Per Country in Olympic Games (1976-2008)') +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")+
  theme(legend.position="right"))


#Create Womens medal map-----------------------------------------------------------------------

Womens_olympic_Gold <- olympics %>%
  filter(Medal == "Gold", Gender=="Women") %>%
  group_by(Country) %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total))


Womens_olympic_Silver <- olympics %>%
  group_by(Country) %>%
  filter(Medal == "Silver", Gender=="Women") %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total)) %>%
  filter(!Country %in% olympic_Gold$Country) %>%
  mutate(Medal_Total = "Silver")

Womens_olympic_Bronze <- olympics %>%
  group_by(Country) %>%
  filter(Medal == "Bronze", Gender=="Women") %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total)) %>%
  filter(!(region %in% olympic_Gold$region & region %in% olympic_Silver$region) )%>%
  mutate(Medal_Total = "Bronze")

Womens_olympic_Gold$Medal_Total <- "Gold"


Medal_Total <- rbind(Womens_olympic_Gold, Womens_olympic_Silver)
Medal_Total <- rbind(Medal_Total, Womens_olympic_Bronze)

Medal_Total$region[which(Medal_Total$region == "United States")] = "USA"


world_map <- map_data("world")
olympic_world_map <- left_join(world_map, Medal_Total, by ='region')
olympic_world_map$Medal_Total[is.na(olympic_world_map$Medal_Total)] <- "No Medal"

olympic_world_map$Medal_Total<-factor(olympic_world_map$Medal_Total, levels = c("Gold","Silver", "Bronze", "No Medal" ))

#Create a map for overlay
#winning_Countries<-olympic_world_map%>%dplyr::filter(!Medal_Total=="No Medal")

ggplot(data = olympic_world_map, aes(x=long, y=lat, group = group, fill=Medal_Total)) +
  geom_polygon(colour='black') +
  scale_fill_manual(name= "Medal Won", values = c("gold", "darkgrey", "#E69F00", "white"))+
  labs(x= "longtude", y= "latitude",title = 'Highest Medal Won for Women Per Country in Olympic Games (1976-2008)') +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")+
      theme(legend.position="right"))

#----------------------------------------------------------------------------------------------
 #Create a Mens Map----------------------------------------------------------------------------------
Mens_olympic_Gold <- olympics %>%
  filter(Medal == "Gold", Gender=="Men") %>%
  group_by(Country) %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total))


Mens_olympic_Silver <- olympics %>%
  group_by(Country) %>%
  filter(Medal == "Silver", Gender=="Men") %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total)) %>%
  filter(!Country %in% olympic_Gold$Country) %>%
  mutate(Medal_Total = "Silver")

Mens_olympic_Bronze <- olympics %>%
  group_by(Country) %>%
  filter(Medal == "Bronze", Gender=="Men") %>%
  summarize(Medal_Total = length(Medal)) %>%
  arrange(desc(Medal_Total)) %>% 
  mutate(region = str_trim(Country),Medal_Total = str_trim(Medal_Total)) %>%
  filter(!(region %in% olympic_Gold$region & region %in% olympic_Silver$region) )%>%
  mutate(Medal_Total = "Bronze")

Mens_olympic_Gold$Medal_Total <- "Gold"


Medal_Total <- rbind(Mens_olympic_Gold, Mens_olympic_Silver)
Medal_Total <- rbind(Medal_Total, Mens_olympic_Bronze)

Medal_Total$region[which(Medal_Total$region == "United States")] = "USA"


world_map <- map_data("world")
olympic_world_map <- left_join(world_map, Medal_Total, by ='region')
olympic_world_map$Medal_Total[is.na(olympic_world_map$Medal_Total)] <- "No Medal"

olympic_world_map$Medal_Total<-factor(olympic_world_map$Medal_Total, levels = c("Gold","Silver", "Bronze", "No Medal" ))

#Create a map for overlay
#winning_Countries<-olympic_world_map%>%dplyr::filter(!Medal_Total=="No Medal")

ggplot(data = olympic_world_map, aes(x=long, y=lat, group = group, fill=Medal_Total)) +
  geom_polygon(colour='black') +
  scale_fill_manual(name= "Medal Won", values = c("gold", "darkgrey", "#E69F00", "white"))+
  labs(x= "longtude", y= "latitude",title = 'Highest Medal Won for Men Per Country in Olympic Games (1976-2008)') +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")+
      theme(legend.position="right"))