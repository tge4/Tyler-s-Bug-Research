setwd("Data")

getwd()
list.files()


library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("scales")



###Data import
allBug <- read.csv("SheetsAndGit1015.csv" )
colnames(allBug)

#Total number of bugs each week for all watersheds
week_sum <- allBug %>% group_by(date) %>% summarize(count_sum = sum(count, na.rm = TRUE))


#Total number of bugs each month for each watershed
month_Sum <- allBug %>% group_by(month,year, Watershed) %>%
  summarize(count_sum = sum(count, na.rm = TRUE))



### Visualization- Bugs a month

#All bugs in all years in all watersheds: 

#Organize by month-
allBugMonth <- allBug %>% group_by(month) %>% summarize(month_count = sum(count))

m = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

#plot- 
ggplot()+geom_bar(allBugMonth, mapping = aes(x=month, y = month_count), fill = "darkblue", stat = "identity")+
  labs(title = "All Bugs, All Watersheds per Month", x = "Month", y  = "Total Number of Bugs")+
  scale_x_continuous(breaks = 1:12,  labels = m )

#All bugs in each watershed in each month of 2018, grouped bar plot
wsBugSum2018 <- month_Sum %>% subset(month_Sum$year==2018) %>% 
  group_by(month, Watershed)%>% summarize(month_count = sum(count_sum))


ggplot(wsBugSum2018, aes(fill = Watershed, x = month, y = month_count))+
  geom_bar(position = "dodge", stat = "identity")+scale_x_continuous(breaks = c(5,6,7,8,9,10,11))+
  scale_fill_brewer(palette = "Paired")+
  labs(title = "All Bugs, All Watersheds per Month (2018)", x = "Month", y  = "Total Number of Bugs")


#All bugs in each watershed in for each month of 2019, grouped bar plot
wsBugSum2019 <- month_Sum %>% subset(month_Sum$year==2019) %>% 
  group_by(month, Watershed)%>% summarize(month_count = sum(count_sum))



ggplot(wsBugSum2019, aes(fill = Watershed, x = month, y = month_count))+
  geom_bar(position = "dodge", stat = "identity")+scale_x_continuous(breaks = c(3,4,5,6,7,8,9,10,11))+
  scale_fill_brewer(palette = "Accent")+
  labs(title = "All Bugs, All Watersheds per Month (2019)", x = "Month", y  = "Total Number of Bugs")


###Visualizing Bugs a Year: 

#How many total bugs in each watershed for all of 2018?
#Collapse months into year
wsBugTotal2018 <- wsBugSum2018%>% group_by(Watershed)%>% summarize(year_count = sum(month_count))

#plot-
ggplot(wsBugTotal2018, aes(fill =Watershed, x = Watershed, y = year_count))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette = "Set3")+
  labs(title = "All Bugs, All Watersheds (2018)", x = "Watershed", y  = "Total Number of Bugs")
#Collapse months into year
wsBugTotal2019 <- wsBugSum2019%>% group_by(Watershed)%>% summarize(year_count = sum(month_count))

#plot-
ggplot(wsBugTotal2019, aes(fill =Watershed, x = Watershed, y = year_count))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_brewer(palette = "Set3")+
  labs(title = "All Bugs, All Watersheds (2019)", x = "Watershed", y  = "Total Number of Bugs")     



