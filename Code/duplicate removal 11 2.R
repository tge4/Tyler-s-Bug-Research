#getting rid of duplicate rows

setwd("C:/Users/tyler/Documents/Data")

x <- read.csv( "HB_WaTER_Stickytrap_mastersheet - Bugs to upload 10 27.csv" )

y <- distinct(x) #makes a dataset with no duplicate rows, means that it'll only keep one of a set of two rows that are completely empty,
#but I don't think that matters for statistics, it does for recording

write.csv(y, "DistinctData112.csv")
