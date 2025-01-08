### CRESCENT LAKE TEMPERATURE DATA ANALYSIS ####

#Written by Peter Frank#

#Script for analysis of temperature data from Crescent Lake on the Kenai Peninsula in Alaska.#
#Data was from a single series of 17 Hobo temperature sensors spaced every two meters #
#in the water column deployed between March to October of 2024#

##PACKAGES##
install.packages("remotes")
require(remotes)
remotes::install_github("robertladwig/LakeModelR")

install.packages("LakeModelR")

install.packages("rLakeAnalyzer")

install.packages("tidyverse")

install.packages("dplyr")

install.packages("ggplot2")

install.packages("ggthemes")

install.packages("gganimate")

install.packages("lubridate")

install.packages("zoo")

library(LakeModelR)
require(tidyverse)
require(plyr)
library(rLakeAnalyzer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(maps)
library(lubridate)
library(zoo)

##Transform raw data from wide format to long format for analysis##
TempData <- TempData_Raw %>% pivot_longer(cols = c('33','31','29','27','25','23','21',
                                                   '19','17','13','11','9','7','5','1'),
                                          names_to = 'Depth',
                                          values_to = 'Temp')

TempData$Date <- mdy(TempData$Date)

TempData <- transform(TempData, Depth = as.numeric(Depth))

write.csv(TempData, file = "CrescentLake_TempData.csv")


##Manipulate Temp & Flow Data for Analysis##

#Creek Flow#
CrescentCreek_Flow <- CrescentCreek_OutFlow %>% pivot_longer(cols = c('1957','1958','1959','1960'),
                                                             names_to = 'Year',
                                                             values_to = 'CFS')

CrescentCreek_Flow <- transform(CrescentCreek_Flow, Year = as.numeric(Year, format = "%Y"))

CrescentCreek_Flow$Date2 <- as.Date(with(CrescentCreek_Flow,paste(date,"2024",sep="/")),"%m/%d/%Y")

CrescentCreek_Flow$date<-as.Date(with(CrescentCreek_Flow,paste(date,Year,sep="/")),"%m/%d/%Y")

#Lake Flow#
CrescentLake_Flow <- CrescentLake_OutFlow %>% pivot_longer(cols = c('1957','1958','1959','1960'),
                                                             names_to = 'Year',
                                                             values_to = 'CFS')

CrescentLake_Flow <- transform(CrescentLake_Flow, Year = as.numeric(Year, format = "%Y"))

CrescentLake_Flow$Date2 <- as.Date(with(CrescentLake_Flow,paste(date,"2024",sep="/")),"%m/%d/%Y")

CrescentLake_Flow$date<-as.Date(with(CrescentLake_Flow,paste(date,Year,sep="/")),"%m/%d/%Y")

#Aggregate hourly temperature data by day/month and depth#
daily_avg_temp <- aggregate(Temp ~ Date + Depth, data = TempData, FUN = mean, na.rm = TRUE)

daily_avg_temp <- transform(daily_avg_temp, Depth = as.numeric(Depth))

daily_avg_temp$Date <- mdy(daily_avg_temp$Date)


daily_avg_temp$Month <-format(daily_avg_temp$Date, "%m")

monthly_avg_temp <- aggregate(Temp ~ Month + Depth, data = daily_avg_temp, FUN = mean, na.rm = TRUE)

#Plot Temperature data
ggplot(TempData, aes(Date, Depth, fill = Temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
  midpoint = 8, 
  high = scales::muted("red"), 
  low = scales::muted("blue")) +
  coord_cartesian(expand = FALSE)

# Monthly Temperature Profiles Symbolized by Temperature
MonthLabel = c("February","March","April","May","June","July","August","September","October")
names(MonthLabel) = c("02","03","04","05","06","07","08","09","10")

daily_avg_temp %>% ggplot(aes(x = Temp, y = Depth, colour = Temp)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint = 8,
    high = "red", mid = "purple", low = "blue", space = "Lab")+
  facet_wrap( ~ Month, labeller = labeller(Month = MonthLabel)) +
  labs(title = "Monthly Temperature Profiles - Crescent Lake, Alaska",
       subtitle = "February to October 2024",
       y = "Depth (meters)",
       x = "Temperature (C)") + theme_bw(base_size = 15)

# Monthly Temperature Profiles Symbolized by Date
daily_avg_temp %>% ggplot(aes(x = Temp, y = Depth, colour = as.factor(Date))) +
  geom_point() +
  scale_y_reverse() +
  facet_wrap( ~ Month, labeller = labeller(Month = MonthLabel)) +
  labs(title = "Monthly Temperature Profiles - Crescent Lake, Alaska",
       subtitle = "February to October 2024",
       y = "Depth (meters)",
       x = "Temperature (C)") + 
       theme_bw(base_size = 15) +
       theme(legend.position = "none")

#Animated Time Series of daily temperature profiles#
daily_avg_temp %>% ggplot(aes(x = Temp, y = Depth, colour = as.factor(Temp))) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient2(
    midpoint = 8,
    high = "red", mid = "purple", low = "blue", space = "Lab")+
  labs(title = "Monthly Temperature Profiles - Crescent Lake, Alaska",
       subtitle = "February to October 2024",
       y = "Depth (meters)",
       x = "Temperature (C)") + 
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  #transition_time(Date)
  #ease_aes("linear")

#Plot Crescent Lake Outflow#
ggplot(CrescentLake_Flow, aes(x = Date2, y = CFS, group = Year, colour = as.factor(Year))) +
  geom_line() +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%B") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(colour = "Year") +
  ggtitle("Stream Discharge (CFS) from Crescent Lake, Alaska") +
  xlab("Date") + ylab("Discharge (Cubic Feet per Second)")
