install.packages("vegan")
library(tidyverse)
library(vegan)
data(varespec)

setwd("C:/Users/tyler/Documents/Data")
list.files()

master <- read.csv( "TidyData112.csv")

#trying to do the shannon 

#step 1- group by watershed, step 2- sum by invertebrate, 
#step 3- divide the total count for the watershed, divide by invertebrate sum, label that the p
#step 4- H = -sum(p*log(p))
#do this for all the data, then for year, then for month

names(master)

f <- master %>% group_by(Watershed,invert) %>% summarize(bugSum = sum(count_zero)) %>% 
  spread(key = invert, value = bugSum) %>% select(-Other, -Terrestrial) %>%
  mutate(Watershed = as.character(Watershed))

l <- as.data.frame(t(f[-1]))
colnames(l) = c("1","2", "3", "4", "6", "9", "HBK" )

 
D <- as.table(diversity(l[1], "simpson"))


K <- as.table(diversity(f, "simpson"))


vegan::evenness(f)

#make a dataframe with columns = inverts, rows = watersheds