library(tidyverse)
library(cowplot)

#read in provided data files 
#tidy them up 

bom_data <- read_csv("data/BOM_data.csv")
#Temp_min_max need separating 
bom_data_sep <- separate(bom_data, Temp_min_max, into = c("min_temp", "max_temp"), sep = "/")

bom_meta <- read_csv("data/Bom_stations.csv")
#this is wide, would be better as a long data set 
meta <- gather(bom_meta, key = "Station_number", value = "amount", -info) %>% spread(key = "info", value = "amount")

#bom_data_sep and meta are the 'tidy' versions 

#the above is taken from code in 'bom_analysis.R", written in Week2 

#Q1
#For the Perth dtatopm (ID 9225) produce three scatter plots shhowing the relationship between the maximum temperature 
#and each other measurement recorded (minimum temperature, rainfall, solar exposure)

perth <- filter(bom_data_sep, Station_number == 9225) %>% mutate(min_temp = as.numeric(min_temp), max_temp = as.numeric(max_temp),
                                                                 Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure))

plot_min_temp <- perth %>% 
  ggplot(aes(x = min_temp, y = max_temp)) + geom_point(alpha = 0.5, colour = "grey50") +
  labs(x = "Minimum daily temperature",
       y = "Maximum daily temperature" )

plot_rainfall <- perth %>% 
  ggplot(aes(x = Rainfall, y = max_temp)) + geom_point(alpha = 0.5, colour = "grey50")+ 
  labs(x = "Daily rainfall",
       y = "Maximum daily temperature" )

plot_solar_exp <- perth %>% 
  ggplot(aes(x = Solar_exposure, y = max_temp)) + geom_point(alpha = 0.5, colour = "grey50") +
  labs(x = "Daily solar exposure",
       y = "Maximum daily temperature" )

#Q2
#Display the four measurements on 1 plot using aesthetics 
plot_all <- drop_na(perth) %>% 
  ggplot(aes(x = min_temp, y = max_temp, colour = Solar_exposure, size = Rainfall)) +
  geom_point(alpha = 0.5) + 
  labs(x = "Minimum daily temperature",
       y = "Maximum daily temperature" )# +
 # scale_color_gradient(low = "blue", high = "red") #this needs work


#Q3
#Combine the 4 plots on one panel using the cowplot package
v1 <- plot_grid(plot_min_temp, plot_rainfall, plot_solar_exp, plot_all, ncol = 1)
v2 <- plot_grid(plot_min_temp, plot_rainfall, plot_solar_exp, plot_all)

ggsave(filename = "perth_1c.png", plot = v1, width = 20, height = 35, units = "cm", dpi = 400)
ggsave(filename = "perth_2c.png", plot = v2, width = 25, height = 25, units = "cm", dpi = 400)


#Q4 
#Average monthly rainfall for each station - produce a lineplot that shows the state each station is in 

bom_data_sep <- mutate(bom_data_sep, min_temp = as.numeric(min_temp), max_temp = as.numeric(max_temp),
                       Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure)) 
bom_data_sep <- mutate(bom_data_sep, Station_number = as.character(Station_number))
joined_data <- full_join(bom_data_sep, meta) 

monthly_rain <- joined_data %>% 
  group_by(state, Station_number, Month) %>% summarise(mean_rain = mean(Rainfall, na.rm = T)) %>% mutate(Month = as.factor(Month))

#first option
monthly_rain %>% 
  ggplot(aes(x = Month, y = mean_rain, colour = state, group = Station_number)) + 
  geom_line() + 
  labs(y = "Mean monthly rainfall", 
       x = "Month", 
       colour = "State")

#second option
monthly_rain %>% 
  ggplot(aes(x = Month, y = mean_rain, group = Station_number)) + 
  geom_line() + 
  labs(y = "Mean monthly rainfall", 
       x = "Month", 
       colour = "State") +
  facet_wrap(~state)
