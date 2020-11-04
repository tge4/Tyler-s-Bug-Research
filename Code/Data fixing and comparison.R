
setwd("Data")
library(RCurl)
library("tidyr")
library("dplyr")
library("lubridate")


#Loading in the data from my computer and github

sheetsData <- read.csv("TidyData923.csv")%>% transform(Watershed = as.character(Watershed))%>% transform(date = as.Date(date))

gitURL <- getURL("https://raw.githubusercontent.com/tge4/HBK-bugs-summer-2019/20636097b46749f1dcf82f09d9362d064b8ab1d0/All%20watersheds.csv")

gitData <- read.csv(text = gitURL)

#Putting both sheets into the same format

sheet2018 <- sheetsData %>% subset(sheetsData$year==2018) %>% 
  dplyr::select("Watershed", "date") %>% transform(date = as.Date(date))%>% transform(Watershed = as.character(Watershed))

git2018 <- gitData%>% dplyr::select("Watershed", "Date")%>%  mutate(date = lubridate::mdy(Date))%>%
  dplyr::select("Watershed", "date") %>% transform(Watershed = as.character(Watershed))

str(git2018)

str(sheet2018)
#Checking what things in google sheets match what is in github

#taking out duplicate date/watershed pairs
unique <- distinct(sheet2018)
unique2 <- distinct(git2018)

#pairs that are in both sets: 
common <- dplyr::intersect(unique, unique2)

#pairs that are in the google sheet but not on github
diff <- dplyr::setdiff(unique, unique2)

#pairs that are in github but not google sheets
diff2 <- dplyr::setdiff(git2018, sheet2018)

#now trying to reupload the missing data from sheets into github

gitNeeds <- dplyr::left_join(diff, sheet2018, by = c("Watershed", "date"))
#write.csv(gitNeeds, "2018 Data Upload 10 12.csv", row.names = F)


#transform unique git data to append to new data set

allBug_column <- gitData %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small",  
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") 

head(allBug_column)

#remove NAs and replace with zeroes
allBug_Zero <- allBug_column %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count)  
head(allBug_Zero)

#Change format of dates
allBug_tidy <-  allBug_Zero %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, size, count_zero)%>% mutate(month = month(date), year = year(date))%>% 
  transform(Watershed = as.character(Watershed))

str(sheetsData)

allData <- dplyr::bind_rows(sheetsData, allBug_tidy)

#write.csv(allData, "2018 Data Upload 10 12.csv", row.names = F)


x <- unique(allData)
y <- distinct(allData)
z <- distinct(sheetsData)
a <- distinct(allBug_tidy)





