#Ukiah Longterm Precipitation Trends
#Comparison between historical averages (1900 - 2021) and 2021
#Goal, Tables showing longterm trend and a graph with 2021 overlaying historical averages


sessionInfo()
library(ggplot2)
library(tidyverse)
library(lubridate)
library(knitr)
#library(psych)

getwd()
setwd("H:/My Drive/02EnvirExchNetwkGrant/ENGrantDueProducts/EN_R_DataSources")
prep<- read.csv (file ="UkiahWeatherData.csv",
               header = TRUE, 
               sep = ",")
names(prep)
#Get R to recognize date
class(prep$DATE)

#Convert factor or character string into date, and supply format using lubridate package
prep$DATE <- mdy(prep$DATE)
#prep$DATE <- ymd(prep$DATE)

glimpse(prep$DATE)

#Transform Date. Create DOY, date, Month, month_number,month_year, and year colums to use in analysis



prep <- mutate(prep, order_DOY = as.numeric(strftime(prep$DATE, format = "%j")))
prep <- mutate(prep, date = as.numeric(strftime(prep$DATE, format = "%d")))
prep <- mutate(prep, month = as.character(strftime(prep$DATE, format = "%B")))
prep <- mutate(prep, order_month_number = as.numeric(strftime(prep$DATE, format = "%m")))
prep <- mutate(prep, month_year = as.character(strftime(prep$DATE, format = "%m-%Y")))
prep <- mutate(prep, order_year = as.numeric(strftime(prep$DATE, format = "%Y")))

#SPECIAL NOTE - In California Water Budgets begin on October 1 and end on September 30.
#Transform months to match Water Budget Calendar

DOY <- ifelse(prep$order_DOY <= 273, prep$order_DOY + 92, prep$order_DOY - 273)
month_number <- ifelse(prep$order_month_number <= 9, prep$order_month_number + 3, prep$order_month_number - 9)
year <- ifelse(prep$order_month <= 9, prep$order_year + 1, prep$order_year - 0)

precip_mm <- (prep$PRCP * 25.4)

prep <- data.frame(prep, precip_mm)
prep <- data.frame(prep, DOY)
prep <- data.frame(prep, month_number)
prep <- data.frame(prep, year)




#Preliminary Transformations Complete, 
#Save for use later...

prepsave <- prep

#Sub-select date range

prep <- prep[prep$DATE >= "1899-10-01" & prep$DATE <= "2012-09-30", ]

prep <- as_tibble(prep)

#as_tibble(prep)

#Preserve changes made to prep, and create a new table for modifications

prep <- prep %>%
    
    group_by(month_number,order_year) %>%  #First group the data by Month
                    dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                     Precip = sum(precip_mm))%>%
                    
    
#Create a Table of descriptive stats showing the sum of precipitation by month for all years 1900-2021
  
    arrange(month_number) %>%
    as.data.frame(prep)
    glimpse(prep)

    kable(prep[,c(1,2,3,4)], digits=2, caption="Monthly Precipitation 1900 - 2020")
    
#Save output to directory
    
write.csv(prep, "prepSumPrecipMonthYear.csv")

#Use above table to create a monthly average of precip, October - September, for all years.

prep2 <- prep %>%
    
  group_by(month_number) %>%  #Group the data by Month
                    dplyr::summarise(N = n() , #specifies that dplyr package should be used, otherwise computer gets confused.
                    Precip_mm = mean(Precip),
                    #median = median(Precip_sum),
                    Max = max(Precip),
                    Min = min(Precip)) %>%
                    #sd   = sd(Precip_sum),
                    #se   = sd / sqrt(N),
                    #Range = quantile(Precip_sum),
                    #IQR = IQR(Precip_sum),
                    
#Create a Table
  
arrange(month_number) %>%
as.data.frame(prep2)
glimpse(prep2)

#Recode Months to Hydro Calendar to avoid confusion
Month <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept")

prep2 <- data.frame(prep2, Month)


kable(prep2[,c(6,3)], digits=2, caption="Average Monthly Precipitation Since 1900")

write.csv(prep, "prepSumPrecipMonth.csv")

prep2 %>%
  
  dplyr::select(month_number, Precip_mm) %>%
  head


#write.csv(prep, "prepAveragePrecipByMonth.csv")

prep2021 <- as_tibble(prepsave)

prep2021 <- prep2021[prep2021$DATE >= "2012-10-01" & prep2021$DATE <= "2021-09-30", ]

prep2021 <- prep2021 %>%

  group_by(month_number,year) %>%  #Group the data by Month
  dplyr::summarise(N = n() , #specifies that dplyr package should be used, otherwise computer gets confused.
                   Precip = sum(precip_mm))%>%

arrange(month_number) %>%
  as.data.frame(prep2021)
glimpse(prep2021)

  combined_data <- left_join(prep2, prep2021, by = "month_number") %>%
    rename("Oct 2020 - Sept 2021" = "Precip",
           "Mean Precip: 1903-2020" = "Precip_mm") %>%
    pivot_longer(cols = c("Oct 2020 - Sept 2021", "Mean Precip: 1903-2020"),
                 names_to = "Parameter",
                 values_to = "Value")
  
  head(combined_data)
  combined_data <- as.data.frame(combined_data)
  str(combined_data)
  write.csv(combined_data, "PrecipCombined_cleaned.csv")
  
 

monthly = combined_data %>%
  group_by(Parameter, month_number) %>%
  summarise(mean = mean(Value)) %>%
  pivot_wider(names_from = Parameter, values_from = mean) %>%
  arrange(month_number)
as.data.frame(monthly)

monthly <- data.frame(monthly, Month)

kable(monthly[,c(4,2,3)], digits=1, caption="Means by month")


### Add Theme for Visualizations
mytheme <- theme_bw(base_size = 18, base_family = "")+theme(
  #Panel
  panel.grid.major = element_line(colour = "#E5E5E5"),
  panel.grid.minor = element_line(colour = "#E5E5E5"),
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(colour = "#ADADAD", fill = NA, size=1),
  #Text and Axis
  axis.title.x = element_text(colour="black", face="bold", size=16),
  axis.text.x = element_text(colour="black", size=12),
  axis.title.y = element_text(colour="black", size=16, face="bold",
                              lineheight = 1.2, angle = 90),
  axis.text.y = element_text(colour="black", size=12),
  plot.title = element_text(colour="black", size=18, face="bold"),
  axis.line = element_line(colour="#A1A1A1", size=1),
  #Legend
  legend.title = element_text(colour="black", size=16, face="bold"))

#Create a Continuous Line Graph with Ribbon

PrecipLine <- ggplot(data = combined_data, aes(month_number, Value, color = Parameter)) +
  #geom_line()+ 
  geom_smooth(size =0.7, span = 0.75, se = FALSE)+ #smoothed for presentation purposes
  scale_x_continuous(breaks=c(1,4,8,12), 
               labels=c("October","January","May","September"))+
               xlab("Month")+
               ylab("Precipitation (mm)")
  
PrecipLine + aes(group=rev(Parameter)) + mytheme



PrecipBar <-ggplot(combined_data,aes(x=month_number, y=Value, fill=Parameter))+
  geom_bar(position="dodge", stat="identity", colour="black")+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                #width=.2, position=position_dodge(0.9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), 
                   labels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept"))+
                   xlab("Month")+
                   ylab("Precipitation (mm)")
  
  
  #xlab("Interval (Weeks)")+
 
# ylab("Ammonium - N" ~ ("mg N L"^-1)~"\n")+
  #scale_fill_grey(start=.5, end=1,
                 # labels=c("Fall","Spring"),
                  #guide_legend(title="Treatment"))

PrecipBar + mytheme

