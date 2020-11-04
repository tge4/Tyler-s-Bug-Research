setwd("Data")
library(RCurl)
library("tidyr")
library("dplyr")
library("lubridate")

list.files()
w1 <- read.csv("w1.csv") 
w2 <- read.csv("w2.csv")
w3 <- read.csv("w3.csv")
w4 <- read.csv("w4.csv")
w5 <- read.csv("w5.csv")
w6 <- read.csv("w6.csv")
w9 <- read.csv("w9.csv")
hbk <- read.csv("hbk.csv")

names <- c("Sample",  "Watershed"   , "Date"  , 
           "Side" ,"Ws"  , "Date.1"   ,   "Dipteran_Large"  , 
           "Terrestrial_Large",  "Caddisfly_Large" ,  "Mayfly_Large"    ,  
           "Stonefly_Large"  ,  "Other_Large"   , "Dipteran_Small" , "Terrestrial_Small"    ,  
           "Caddisfly_Small" ,"Other_Small")

names2 <- c("Sample",  "Date"   , "Side"  , 
            "Watershed" ,"Date.1"  ,   "Dipteran_Large"  , 
            "Terrestrial_Large",  "Caddisfly_Large" ,  "Mayfly_Large"    ,  
            "Stonefly_Large"  ,  "Other_Large"   , "Dipteran_Small" , "Terrestrial_Small"    ,  
            "Caddisfly_Small" ,"Other_Small")

names3 <- c("Sample",  "Side"   , "Date"  , 
            "Watershed" ,"Date.1"  ,   "Dipteran_Large"  , 
            "Terrestrial_Large",  "Caddisfly_Large" ,  "Mayfly_Large"    ,  
            "Stonefly_Large"  ,  "Other_Large"   , "Dipteran_Small" , "Terrestrial_Small"    ,  
            "yeet", "Other_Small")


colnames(w1) = names
colnames(w2) = c("Sample",  "Date"   , "Side"  , 
                 "Watershed" ,"Date.1"  ,   "Dipteran_Large"  , 
                 "Terrestrial_Large",  "Caddisfly_Large" ,  "Mayfly_Large"    ,  
                 "Stonefly_Large"  ,  "Other_Large"   , "Dipteran_Small" , "Terrestrial_Small"    ,  
                 "Caddisfly_Small" ,"Other_Small", "yeet")

colnames(w3) = names3
colnames(w4) =  names2
colnames(w5) = names2
colnames(w6) = names2
colnames(w9) = names3
colnames(hbk) =  c("Sample",  "Side"   , "Date"  , 
                   "Watershed" ,"Date.1"  ,   "Dipteran_Large"  , 
                   "Terrestrial_Large",  "Caddisfly_Large" ,  "Mayfly_Large"    ,  
                   "Stonefly_Large"  ,  "Other_Large"   , "Dipteran_Small" , "Terrestrial_Small"    ,  
                    "Other_Small", "yeet")

w1_tidy <- w1 %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", "Caddisfly_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))

w2_tidy <- w2 %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", "Caddisfly_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))
  
w3_tidy <- w3 %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))     
#this doesn't create a perfext 10x bigger df

w4_tidy <- w4 %>% mutate(Other_Large=as.integer(Other_Large)) %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))
#this doesn't create a perfext 10x bigger df

    str(hbk)
w5_tidy <- w5 %>%
      gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
             "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
             key ="invert_size", value = "count")%>%
      separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
      dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
      dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
      mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))   
#this doesn't create a perfext 10x bigger df
 
w6_tidy <- w6 %>%mutate(Caddisfly_Large=as.integer(Caddisfly_Large)) %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))   
#this doesn't create a perfext 10x bigger df    
       
w9_tidy <- w9  %>% mutate(Mayfly_Large=as.integer(Mayfly_Large)) %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed))          
 
hbk_tidy <- hbk  %>% mutate(Mayfly_Large=as.integer(Mayfly_Large)) %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, Sample, size, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed)) 

y = colnames(w9_tidy)

#finally put them all together

allWS10_14 <-do.call("rbind", list(hbk_tidy, w9_tidy, w6_tidy, w5_tidy, w4_tidy, w3_tidy, w2_tidy, w1_tidy)) 

  allWS10_14 <- allWS10_14 %>%  mutate(Sample = as.character(Sample)) %>% 
.  
  

str(allWS10_14)
 
#make a new file with all these changes
#write.csv(allWS10_14, "TidyData1015.csv", row.names = F)

#compare to github data 

gitURL <- getURL("https://raw.githubusercontent.com/tge4/HBK-bugs-summer-2019/20636097b46749f1dcf82f09d9362d064b8ab1d0/All%20watersheds.csv")

gitData <- read.csv(text = gitURL)

git_tidy <- gitData  %>%
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count) %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert,size, Trap, count_zero)%>% 
  mutate(month = month(date), year = year(date), Watershed = as.character(Watershed), Trap = as.character(Trap)) 

colnames(git_tidy) = c("Watershed",  "date"    ,   "invert"  ,   "size"   ,    "Sample"   ,    "count_zero", "month"  ,    "year" )

#I think this is the right one, union means there shouldn't be duplicates

allData1015 <- dplyr::union(git_tidy, allWS10_14)%>%
  mutate(count = ifelse(is.na(count_zero)==TRUE, 0, count_zero)) %>% 
  dplyr::select(-count_zero) %>% mutate(Watershed = ifelse(is.na(Watershed)==TRUE, 1, Watershed)) 

#this data set is 1672 rows smaller than allWS10_14

h <- unique(allData1015) #154 of the rows in this dataframe aren't unique

#I've counted a lot of bugs 
sum(h$count)
sum(allData1015$count)
#these two things are equal in the number of bugs they have, 
#which means there are duplicate rows because 154 of them are empty/full of zeroes
#! good news!


write.csv(h, "SheetsAndGit1015.csv", row.names = F)


#10/24 add- need to create a data set that has all of the data and the site information
#and to ad new data to. 

w1Add <- w1 %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
w2Add <- w2 %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
w3Add <- w3 %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
w4Add <- w4 %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
w6Add <- w6 %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
w9Add <- w9 %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
hbkAdd <- hbk %>% dplyr::select("Sample", "Watershed", "Date", "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                              "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")
gitAdd <- gitData %>% mutate(Sample = Trap) %>% dplyr::select(-Trap)%>% dplyr::select("Sample", "Watershed", "Date", 
                             "Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
                             "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small")

newDataSheet1024 <- do.call("rbind", list(w1Add, w2Add, w3Add, w4Add, w6Add, w9Add, hbkAdd, gitAdd))

newDataSheet1024 <-newDataSheet1024 %>% mutate(Caddisfly_Large=as.numeric(Caddisfly_Large), Stonefly_Large = as.numeric(Stonefly_Large), Other_Large = as.numeric(Other_Large)) 

newDataSheet1024[is.na(newDataSheet1024)] <- 0

str(newDataSheet1024)

write.csv(newDataSheet1024, "WorkingSheet1024.csv", row.names = F)

