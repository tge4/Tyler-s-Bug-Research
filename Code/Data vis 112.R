
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("tidyverse")
library("scales")
library("ggpubr")
library("rstatix")


setwd("C:/Users/tyler/Documents/Data")
list.files()

master <- read.csv( "DistinctData112.csv" )

x <- master %>% 
  gather("aDipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","aDipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count)  %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, Date, invert, size, count_zero)%>% mutate(month = month(date), year = year(date), week = week(date))

write.csv(x, "TidyData112.csv")

y <- x %>% dplyr::select(Watershed, date, year)

bugsCount <- x %>% group_by(Watershed, year, invert) %>% summarize(bugCount = sum(count_zero))

#stacked bar chart of all bugs collected, colored by type of bug, faceted by year
ggplot(bugsCount, aes(x = Watershed, y=bugCount, fill = invert)) + geom_bar( stat = "identity")+ 
  xlab("Watershed")+ ylab("Total Bugs")+ 
  facet_wrap(year ~ .)+
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Total Bugs from each watershed")

#stacked bar with x = week, y = mean bugs by order, facet = watershed and year

bugSummary <- x %>%filter(invert %in% c("Caddisfly", "aDipteran", "Mayfly","Stonefly"))%>% group_by(Watershed, date, week, invert, year)%>% 
  summarize(mean = mean(count_zero), stdev = sd(count_zero), bugCount = sum(count_zero))

l <- ggplot(bugSummary, aes(x = week, y=mean, fill = invert)) + geom_bar( stat = "identity")+ 
  xlab("Week of Year")+ ylab("Mean Bugs")+ 
  facet_grid(year ~ Watershed)+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Mean Aquatic Bugs from each watershed")

pdf("Mean Aquatic Bugs Faceted by Year, Ws 11 2.pdf", l, width = 8, height = 7) #w,h are in inches 
l #ggplot object, just print it 
dev.off() #takes it out of Rstudio





