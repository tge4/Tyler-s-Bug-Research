library(tidyverse) #we're using dplyr, ggplot2, and tidyr
library(lubridate)
library(magrittr)
setwd("Data")
getwd()
list.files()

ws = read.csv("TidyData923.csv")
chem = read.csv("HBEFdata_Current_2020-09-30.csv")
names(ws) <- c("site", "date","invert" ,"size"  ,     "count_zero", "month"  ,    "year"  )
#create vector of aquatic taxa
aq <- c("Caddisfly", "Dipteran", "Mayfly", "Stonefly")

#create a dataframe with aquatic inverts summed by site and date
ws_sum <- ws %>% filter(invert %in% aq) %>% group_by(site, date) %>% summarize(sum_inverts = sum(count_zero))



#combine invert and chemical data
ws_chem <- ws_sum %>% left_join(chem, by=c("site", "date"))

ws_chem_w6 <- 

head(ws_chem_w6)

base_temp = 10
ws_chem_w6$dd <- NA
running_count = 0

for(i in 1:nrow(ws_chem_w6)){
  if(ws_chem_w6$temp[i] - base_temp > 0){
    running_count<-  running_count + ws_chem_w6$temp[i] - base_temp
    ws_chem_w6$dd[i] <- running_count
  }
  else{
    ws_chem_w6$dd[i] <- running_count
  }
}

head(ws_chem_w6 %>% select(site, date, dd))


ws_chem_w9 <- ws_chem %>% filter(site == 'W9')

base_temp = 10
ws_chem_w9$dd <- NA
running_count = 0

for(i in 1:nrow(ws_chem_w9)){
  if(ws_chem_w9$temp[i] - base_temp > 0){
    running_count<-  running_count + ws_chem_w9$temp[i] - base_temp
    ws_chem_w9$dd[i] <- running_count
  }
  else{
    ws_chem_w9$dd[i] <- running_count
  }
}

head(ws_chem_w9 %>% select(site, date, dd))

#push the tables together 

ws_dd_9 <- ws_chem_w9 %>% select(site, date, dd)
ws_dd_6 <- ws_chem_w6 %>% select(site, date, dd)

ws_dd <- rbind(ws_dd_9, ws_dd_6)
head(ws_dd)
tail(ws_dd)

#dd plot

ggplot(ws_dd)+geom_point(aes(x = date, y = dd, color = site), stat = 'identity')+ggtitle("Cumulative Degree Days Above 10 Celsius")+ 
  labs(y = "Cumulative Degree Days", x = "Date") + 
  theme(axis.title = element_text(size=16, color='black', face="bold"), 
        axis.text = element_text(size=12, color='black'), legend.position = 'right',legend.text = element_text(size = 12))


#w9 degree day chart 
ggplot(ws_chem_w9)+geom_point(aes(x = date, y = dd ), stat = 'identity')+ggtitle("Cumulative Degree Days Above 10 Celsius")+ 
  labs(y = "Cumulative Degree Days (0)", x = "Date") + 
  theme(axis.title = element_text(size=16, color='black', face="bold"), 
        axis.text = element_text(size=12, color='black'), legend.position = 'none')



#w6 degree day chart
ggplot(ws_chem_w6)+geom_point(aes(x = date, y = dd ), stat = 'identity')+ggtitle("Cumulative Degree Days Above 10 Celsius")+ 
  labs(y = "Cumulative Degree Days (0)", x = "Date") + 
  theme(axis.title = element_text(size=16, color='black', face="bold"), 
        axis.text = element_text(size=12, color='black'), legend.position = 'none')
#legend.text = element_text(size = 12))
