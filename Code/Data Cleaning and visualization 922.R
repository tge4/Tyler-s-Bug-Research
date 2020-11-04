setwd("Data")

getwd()
list.files()


library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")



###Data cleaning: 
allBug <- read.csv("AllBugs923.csv" )
colnames(allBug)

#changing column names
allBug_column <- allBug %>% 
  gather("Dipteran_Large","Terrestrial_Large","Caddisfly_Large","Mayfly_Large","Stonefly_Large",
         "Other_Large","Dipteran_Small","Terrestrial_Small", "Other_Small", "Caddisfly_Small", 
         key ="invert_size", value = "count")%>%
  separate(invert_size, into=c("invert", "size"), sep = "_") 
 
head(allBug_column)

#remove NAs and replace with zeroes
allBug_Zero <- allBug_column %>% mutate(count_zero = ifelse(is.na(count)==TRUE, 0, count)) %>% 
  dplyr::select(-count)  
head(allBug_Zero)

#Change format of dates
allBug_tidy <-  allBug_Zero %>%  mutate(date = lubridate::mdy(Date)) %>% 
  dplyr::select(Watershed, date, invert, size, count_zero)%>% mutate(month = month(date), year = year(date))

head(allBug_tidy)



#make a data frame withthe count as numbers
str(allBug_tidy)
allBug_tidy$count_zero <- as.numeric(as.character(allBug_tidy$count_zero))

#make a new file with all these changes
write.csv(allBug_tidy, "TidyData923.csv", row.names = F)

#Total number of bugs each week for all watersheds
allBug_sum <- allBug_tidy %>% group_by(date) %>% summarize(count_sum = sum(count_zero, na.rm = TRUE))

head(allBug_sum)

#Total number of bugs each month for each watershed
wsBugSum <- allBug_tidy %>% group_by(month,year, Watershed) %>%
  summarize(count_sum = sum(count_zero, na.rm = TRUE))



### Visualization- Bugs a month

#All bugs in all years in all watersheds: 

      #Organize by month-
      allBugMonth <- allBug_sum %>% group_by(month) %>% summarize(month_count = sum(count_sum))
      
      #plot- 
      ggplot()+geom_bar(allBugMonth, mapping = aes(x=month, y = month_count), fill = "darkblue", stat = "identity")+
        labs(title = "All Bugs, All Watersheds per Month", x = "Month", y  = "Total Number of Bugs")

#All bugs in each watershed in each month of 2018, grouped bar plot
      wsBugSum2018 <- wsBugSum %>% subset(wsBugSum$year==2018) %>% 
        group_by(month, Watershed)%>% summarize(month_count = sum(count_sum))
      
      ggplot(wsBugSum2018, aes(fill = Watershed, x = month, y = month_count))+
        geom_bar(position = "dodge", stat = "identity")+scale_x_continuous(breaks = c(5,6,7,8,9,10,11))+
        scale_fill_brewer(palette = "Paired")+
        labs(title = "All Bugs, All Watersheds per Month (2018)", x = "Month", y  = "Total Number of Bugs")


#All bugs in each watershed in for each month of 2019, grouped bar plot
        wsBugSum2019 <- subset(wsBugSum, wsBugSum$year==2019)%>% 
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
        
        
        
