library(tidyverse)


bom_data <- read_csv("data/BOM_data.csv")
#Temp_min_max need separating 
bom_data_sep <- separate(bom_data, Temp_min_max, into = c("min_temp", "max_temp"), sep = "/")

bom_meta <- read_csv("data/Bom_stations.csv")
#this is wide, would be better as a long data set 
bom_meta_long <- gather(bom_meta, key = "Station_number", value = "amount", -info)
meta <- spread(bom_meta_long, key = "info", value = "amount")


#Q1 
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?
#filter out things that aren't numbers
#group_by station number 
#use n()

ans_Q1 <-filter(bom_data_sep, min_temp != "-", max_temp != "-", Rainfall != "-" ) %>% 
  group_by(Station_number) %>% summarise(n_days =  n()) 


#Q2
#Which month saw the lowest average daily temperature difference?
#group by month 
#calculate diff between max_temp and min_temp and save that in new column
#calc monthly mean 
#
ans_Q2 <- group_by(bom_data_sep, Month) %>% 
  mutate(daily_temp_diff = as.numeric(max_temp) - as.numeric(min_temp)) %>% 
  summarise(mean_daily_temp_diff = mean(daily_temp_diff, na.rm = TRUE)) %>% 
  filter(mean_daily_temp_diff == min(mean_daily_temp_diff))

#Q3 
#Which state saw the lowest average daily temperature difference?

#need to combine/join data sets to answer this Q
bom_data_sep <- mutate(bom_data_sep, Station_number = as.character(Station_number))
joined_data <- full_join(bom_data_sep, meta) 

ans_Q3 <- group_by(joined_data, state) %>% 
  mutate(daily_temp_diff = as.numeric(max_temp) - as.numeric(min_temp)) %>% 
  summarise(mean_daily_temp_diff = mean(daily_temp_diff, na.rm = T)) %>% #can also use drop.na() instead
  filter(mean_daily_temp_diff == min(mean_daily_temp_diff))
# a lot of this code is reused from Q3 - this could be streamlined


#Q4
#Does the westmost (lowest longitude) or eastmost (highest longitude) 
#weather station in our dataset have a higher average solar exposure?

#using joined_data again 

tib_Q4 <- group_by(joined_data, lon) %>% 
  summarise(mean_solar_exp = mean(as.numeric(Solar_exposure), na.rm = T))  

west <- filter(tib_Q4,lon == min(as.numeric(lon))) 
east <-  filter(tib_Q4, lon == max(as.numeric(lon))) 
ansQ4 <- west < east
#messy - but the westmost station gets less solar exposure

