#Use the data files provided to answer the Qs in "Pulling it all together" 
#part of the Reproducibility challenge (week2)

#use the tools we have learnt in the course so far: 
library(tidyverse)

#read in provided data files 
#tidy them up 

bom_data <- read_csv("data/BOM_data.csv")
#Temp_min_max need separating 
bom_data_sep <- separate(bom_data, Temp_min_max, into = c("min_temp", "max_temp"), sep = "/")

bom_meta <- read_csv("data/Bom_stations.csv")
#this is wide, would be better as a long data set 
meta <- gather(bom_meta, key = "Station_number", value = "amount", -info) %>% spread(key = "info", value = "amount")

#bom_data_sep and meta are the 'tidy' versions 


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
#filter to only show row that contains the lowest mean daily temperature difference

ans_Q2 <- group_by(bom_data_sep, Month) %>% 
  mutate(daily_temp_diff = as.numeric(max_temp) - as.numeric(min_temp)) %>% 
  summarise(mean_daily_temp_diff = mean(daily_temp_diff, na.rm = TRUE)) %>% 
  filter(mean_daily_temp_diff == min(mean_daily_temp_diff))

#Q3 
#Which state saw the lowest average daily temperature difference?

#need to combine/join data sets to answer next two Qs so saving as new tibble:
bom_data_sep <- mutate(bom_data_sep, Station_number = as.character(Station_number))
joined_data <- full_join(bom_data_sep, meta) 

#group by state 
#calculate temp differences 
#filter to show row that contains lowest mean daily temperature difference

ans_Q3 <- group_by(joined_data, state) %>% 
  mutate(daily_temp_diff = as.numeric(max_temp) - as.numeric(min_temp)) %>% 
  summarise(mean_daily_temp_diff = mean(daily_temp_diff, na.rm = T)) %>% #can also use drop.na() instead
  filter(mean_daily_temp_diff == min(mean_daily_temp_diff))
# two lines of this code is reused from Q3 - this could be streamlined
# save bom_data_w_temp_diff and use this instead of starting from bom_data_sep in both Q2 and Q3

#alternate version of Q3 - average first, then subtract, to see if we get the same answer 
group_by(joined_data, state) %>% 
  filter(min_temp != "-", max_temp != "-", Rainfall != "-" ) %>% 
  summarise(mean_max_temp = mean(as.numeric(max_temp, na.rm = T)), mean_min_temp = mean(as.numeric(min_temp,na.rm = T))) %>% 
  mutate(daily_temp_diff = as.numeric(mean_max_temp) - as.numeric(mean_min_temp)) 

#Q4
#Does the westmost (lowest longitude) or eastmost (highest longitude) 
#weather station in our dataset have a higher average solar exposure?

#using joined_data again 
#group by longitude
#calculate mean solar exposure 

tib_Q4 <- group_by(joined_data, lon) %>% 
  summarise(mean_solar_exp = mean(as.numeric(Solar_exposure), na.rm = T))  %>% 
  arrange(lon) %>% 
  slice(-2:- (n()-1))

#alternate solution to keep more info in table 
group_by(joined_data, lon, Station_number) %>% 
  summarise(mean_solar_exp = mean(as.numeric(Solar_exposure), na.rm = T))  %>% 
  arrange(lon) %>% 
  ungroup() %>% 
  slice(-2:- (n()-1))

#ANOTHER, BETTER WAY
tib_Q4 <- group_by(joined_data, lon) %>% 
  summarise(mean_solar_exp = mean(as.numeric(Solar_exposure), na.rm = T))  %>% 
  filter(lon %in% range(as.numeric(lon)))

ansQ4 <-filter(tib_Q4, mean_solar_exp == max(mean_solar_exp))


