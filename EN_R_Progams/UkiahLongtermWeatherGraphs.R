#Data gathered from local weather station (NOAA) since 1893 is analyzed and compared to 2021 weather conditions

sessionInfo()
library(ggplot2)
library(xts)
library(tidyverse)
library(lubridate)
library(knitr)

getwd()
setwd("C:/SET/YOUR/OWN")
prep<- read.csv (file ="UkiahWeatherData.csv",
               header = TRUE, 
               sep = ",")
prep<- read.csv (file ="UkiahWeatherData.csv",
               header = TRUE, 
               sep = ",")
names(prep)
#Get R to recognize date
class(prep$DATE)

#Convert factor or character string into date, and supply format using lubridate package
prep$DATE <- mdy(prep$DATE)
glimpse(prep$DATE)

#Add Day of year (DOY) column
as_tibble(prep)
 
prep <- mutate(prep, DOY = as.numeric(strftime(prep$DATE, format = "%j")))
prep <- mutate(prep, date = as.numeric(strftime(prep$DATE, format = "%d")))
prep <- mutate(prep, Month = as.character(strftime(prep$DATE, format = "%B")))
prep <- mutate(prep, month_number = as.numeric(strftime(prep$DATE, format = "%m")))
prep <- mutate(prep, month_year = as.yearmon(strftime(prep$DATE, format = "%m-%Y")))
prep <- mutate(prep, year = as.numeric(strftime(prep$DATE, format = "%Y")))


#Convert atmp from celcius to Farenheit and add to data frame

#F_atmp <- (prep$atmp * 9/5) + 32

#prep <- data.frame(prep,F_atmp)

#Preserve changes made to prep, and create a new table for modifications
prepDates <- prep

as_tibble(prepDates)
prepDailyAverages <- prepDates %>%
    
    group_by(year) %>%  #First group the data by Month
                    dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                     #sum = sum(TAVG),
                     #N    = length(TAVG),
                     Mean = mean((TMAX+TMIN)/2),
                     Max = mean(TMAX),
                     Min = mean(TMIN)) %>%
                     #median = median(TAVG),
                     #sd   = sd(TAVG),
                     #se   = sd / sqrt(N) )%>%
    
#Create a Table of descriptive stats      
    arrange(year) %>%
    as.data.frame(prepDailyAverages)
    glimpse(prepDailyAverages)
    
    print(prepDailyAverages)

kable(prepDailyAverages[,c(1,2,3,4,5)], digits=0, caption="Average Daily Air Temperature")

write.csv(prepDailyAverages, "prepyear.csv")

Year <- ggplot(data = prepDailyAverages, aes(year,Mean,))+
        geom_ribbon(aes(ymin=Min, ymax=Max), alpha=0.1)+
        geom_smooth(method = "lm", se = FALSE, colour = "grey35", linetype = "dashed")+
        geom_line( size = 1.0)+
  scale_x_continuous(breaks = seq (1900,2020,20))+
       

  ggtitle("Yearly Average Air Temperature with Average Max/Min (Farenheit)")+
  labs(x="Year", y="Air Temperature (F)")

Year + theme_bw()


prepDates1 <- prep

as_tibble(prepDates1)
prepDaily1Averages <- prepDates1 %>%
  
  group_by(DOY) %>%  #First group the data by Month
  dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                   #sum = sum(TAVG),
                   #N    = length(TAVG),
                   Mean = mean((TMAX+TMIN)/2),
                   Max = mean(TMAX),
                   Min = mean(TMIN)) %>%
  #median = median(TAVG),
  #sd   = sd(TAVG),
  #se   = sd / sqrt(N) )%>%
  
  #Create a Table of descriptive stats      
  arrange(DOY) %>%
  as.data.frame(prepDaily1Averages)
glimpse(prepDaily1Averages)

kable(prepDaily1Averages[,c(1,2,3,4,5)], digits=0, caption="Average Daily Air Temperature")

#Create Boxplot to show monthly air temperatures

prepDates2 <- prepDates %>%
  
  
  group_by(month_number) %>%  #First group the data by Month
  dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                   mean = mean(TAVG),
                   TMAX = mean(TMAX),
                   TMIN = mean(TMIN))%>%
  
  
  #Create a Table of descriptive stats      
  arrange(month_number) %>%
  as.data.frame(prepDates2)
glimpse(prepDates2)

kable(prepDates2[,c(1,3,4,5)], digits=0, caption="Average Daily Air Temperature")

Monthly <- ggplot(prepDates, aes(x=as.factor(month_number), y= TAVG))+
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21)+
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                   labels=c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  ggtitle("Monthly Air Temp by Month Across 127 Years")+
  labs(x="Month", y="Air Temperature (F)")

Monthly + theme_bw()

#Create a comparison between 127 year of monthly average temps and 2021

prep2021<- read.csv (file ="Weather2021.csv",
                     header = TRUE, 
                     sep = ",")
names(prep2021)
#Get R to recognize date
class(prep2021$DATE)

#Convert factor or character string into date, and supply format using lubridate package
prep2021$DATE <- mdy(prep2021$DATE)
glimpse(prep2021$DATE)

#Add Day of year (DOY) column

#prep2021 <- mutate(prep, DOY = as.numeric(strftime(prep2021$DATE, format = "%j")))
prep2021 <- mutate(prep2021, date = as.numeric(strftime(prep2021$DATE, format = "%d")))
prep2021 <- mutate(prep2021, Month = as.character(strftime(prep2021$DATE, format = "%B")))
prep2021 <- mutate(prep2021, month_number = as.numeric(strftime(prep2021$DATE, format = "%m")))
prep2021 <- mutate(prep2021, month_year = as.yearmon (strftime(prep2021$DATE, format = "%Y/%m")))
prep2021 <- mutate(prep2021, year = as.numeric(strftime(prep2021$DATE, format = "%Y")))

Average <- ((prep2021$TMAX+prep2021$TMIN)/2)

prep2021 <- data.frame(prep2021,Average)

astibble(prep2021)

prep2021 <- prep2021 %>% 
  select(DOY, Average)


combined_data <- left_join(prepDaily1Averages, prep2021, by = "DOY") %>%
  rename("Avg Temp - 2021" = "Average",
         "Mean Daily: 1893-2020" = "Mean") %>%
  
  pivot_longer(cols = c("Avg Temp - 2021", "Mean Daily: 1893-2020"),
               names_to = "Parameter",
               values_to = "Value")

#Create a Continuous Line Graph with Ribbon

LongComp <- ggplot(data = combined_data, aes(DOY, Value))+
  #geom_line(aes(linetype = Parameter))+ # if you want to show the variation
  geom_smooth(se = FALSE, colour = "black", aes(linetype = Parameter))+ #if you want a smoothed look
  geom_line(aes(y=Min),color= "black", linetype = "dotted")+
  geom_line(aes(y=Max),color= "black", linetype = "dotted")+
  #geom_ribbon(aes(ymin=Min, ymax=Max), alpha=0.1)+
  #geom_smooth(se = FALSE)+
  scale_linetype_manual(values = c("twodash", "solid"))+
  scale_x_continuous(breaks=c(32,92,152,213,274,335), 
                     labels=c("Feb","Apr","June", "Aug","Oct","Dec"))+
  ggtitle("The Last Century compared to 2021")+
  labs(x="Month", y="Air Temperature (F)")

LongComp + theme_bw()


