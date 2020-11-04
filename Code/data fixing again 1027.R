
library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("tidyverse")


setwd("C:/Users/tyler/Documents/Data")
list.files()
gross <- read.csv("HB_WaTER_Stickytrap_mastersheet - round.csv")

master <- read.csv("2019 samples - STICKY TRAPS.csv" )

str(x)

y <- unique(gross) %>%  mutate(date = lubridate::mdy(Date), year = year(date)) %>% filter(year == 2019)%>% 
  dplyr::select(Watershed, date, Sample) 


x <- master %>%   mutate(date = lubridate::mdy(DATE), year = year(date), Sample = Sample.ID) %>% filter(year == 2019) %>%
  dplyr::select(Watershed, date, Sample)

x <- x %>% mutate_all(funs(str_replace(., "hbk", "HBK")))

#x and y are the same length which is good?


#ones that are in the master sheet, but not in the counted sheet. This one includes ones that haven't been counted yet
 diff <- dplyr::setdiff(x,y)
 
 #ones that are in the counted sheet but not in the master sheet. This one would be zero in an ideal world
 diff2 <- dplyr::setdiff(y,x)
 
 #the 17 differences here are all ones that have values in the counted set but not in the master set, have been verified
 
 

