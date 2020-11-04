setwd("Data")

getwd()
list.files()

data <- read.csv("SheetsAndGit1015.csv")

library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("scales")

#counts up total bugs on each individual sheet 
x <- data %>% mutate(date = as.Date(date)) %>%  dplyr::group_by(date, Watershed, year, month, Sample) %>%
  summarize(count_sum = sum(count))

xa <- x %>% dplyr::group_by(date, Watershed, year, month) %>%summarize(mean = mean(count_sum), stdev = sd(count_sum))

x18 <- xa %>% subset(year == 2018)
#graphs woo
ggplot(x18, aes(x = date, y = mean))+geom_point()+ 
  xlab("Week")+ ylab("Mean Bugs")+ geom_errorbar(aes(ymin = mean-stdev, ymax = mean+stdev))+ 
  facet_grid(~Watershed)+scale_x_date(labels = date_format("%W"))

str(x)
