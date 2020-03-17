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
ans_Q2 <- group_by(bom_data_sep, Month) %>% 
mutate(daily_temp_diff = as.numeric(max_temp) - as.numeric(min_temp)) %>% 
  summarise(mean_daily_temp_diff = mean(daily_temp_diff, na.rm = T)) %>% 
  filter(mean_daily_temp_diff == min(mean_daily_temp_diff))

#Q3 
#Which state saw the lowest average daily temperature difference?

