setwd("C:/Users/tyler/Documents/Data")

getwd()
list.files()

data <- read.csv("HB_WaTER_Stickytrap_mastersheet - round.csv")

library("tidyverse")
library("dplyr")
library("lubridate")
library("ggplot2")
library("scales")
str(data)
#counts up total bugs on each individual sheet 

x <- data %>% mutate(date = as.Date(Date, format = '%m/%d/%y'), year = year(date), month = month(date)) %>%  
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small",  
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% 
  dplyr::group_by(date, Watershed, month, year, Sample) %>%
  summarize(count_sum = sum(count))

xa <- x %>% dplyr::group_by(date, Watershed, year, month) %>%summarize(mean = mean(count_sum), stdev = sd(count_sum))%>%
  mutate (year = as.character(year))

write.csv(x,"TidyData1028.csv")

x18 <- xa %>% subset(year == 2018)
x19 <- xa %>% subset(year == 2019)
#graph set 1- comparing all the watersheds in the same year

#scatterplot of mean and stdev bug count from all sheets from same date/ watershed, faceted by watershed for 2018 
ggplot(x18, aes(x = date, y = mean, color = Watershed))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
  facet_grid(Watershed ~ .)+scale_x_date(labels = date_format("%W"))+
  scale_color_brewer(palette = "Dark2")+ theme(legend.position = "none")+ 
  ggtitle("Mean Bugs from each watershed each week 2018")

#same as above, unfaceted

ggplot(x18, aes(x = date, y = mean, color = Watershed))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
 scale_x_date(labels = date_format("%W"))+
  scale_color_brewer(palette = "Dark2")+  ggtitle("Mean Bugs from each watershed each week 2018")



#same as first two for 2019 
ggplot(x19, aes(x = date, y = mean, color = Watershed))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
  facet_grid(Watershed ~ .)+scale_x_date(labels = date_format("%W"))+
  scale_color_brewer(palette = "Dark2")+ theme(legend.position = "none")+
  ggtitle("Mean Bugs from each watershed each week 2019")

#= unfaceted

ggplot(x19, aes(x = date, y = mean, color = Watershed))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
  scale_x_date(labels = date_format("%W"))+
  scale_color_brewer(palette = "Dark2")+  ggtitle("Mean Bugs from each watershed each week 2019")+ 
  jitter(rep(0, 5))


#graph set 2- comparing watersheds to themselves across years
#most complete sets in both years for ws 1, 6, hbk. next most in 2 & 4
x16h <- xa %>% subset(Watershed %in% c("1","6","HBK")) %>% mutate (year = as.character(year))

#scatterplot comparing mean stdev bugs in 1 6 hbk 
ggplot(x16h, aes(x = date, y = mean, color = year))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
  facet_grid(Watershed ~ .)+scale_x_date(labels = date_format("%W"))+
  scale_color_manual(breaks = c("2019", "2018"), values = c("#009999", "#0000FF"))+
  ggtitle("Mean Bugs from each watershed each week")

#scatterplot comparing mean stdev bugs in all watersheds
ggplot(xa, aes(x = date, y = mean, color = year))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
  facet_grid(Watershed ~ .)+scale_x_date(labels = date_format("%W"))+
  scale_color_manual(breaks = c("2019", "2018"), values = c("#ff3333", "#0000FF"))+
  ggtitle("Mean Bugs from each watershed each week")

