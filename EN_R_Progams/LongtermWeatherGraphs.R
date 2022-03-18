sessionInfo()
library(ggplot2)
library(grid)
library(Hmisc)
library(zoo)
library(xts)
library(tidyverse)
library(lubridate)

getwd()
#setwd() Set the working directory to find your file or go to File ---> Import Dataset
setwd("C:/enter/the/path/here")

prep<- read.csv (file ="bvlweather.csv",
               header = TRUE, 
               sep = ",")
names(prep)
#Get R to recognize date
class(prep$date)

#Convert factor or character string into date, and supply format using lubridate package
prep$date <- mdy(prep$date)
glimpse(prep$date)

#Add Day of year (DOY) column
prep <- mutate(prep, DOY = as.numeric(strftime(prep$date, format = "%j")))


#Convert atmp from celcius to Farenheit and add to data frame

F_atmp <- (prep$atmp * 9/5) + 32

prep <- data.frame(prep,F_atmp)

#Preserve changes made to prep, and create a new table for modifications
prepDates <- prep

as_tibble(prepDates)
prepDailyAverages <- prepDates %>%
    
    group_by(date) %>%  #First group the data by Month
                    dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                     #sum = sum(F_atmp),
                     #N    = length(F_atmp),
                     mean = mean(F_atmp),
                     median = median(F_atmp),
                     sd   = sd(F_atmp),
                     se   = sd / sqrt(N) )%>%
    
#Create a Table of descriptive stats      
    arrange(date) %>%
    as.data.frame(Summary1)
glimpse(prepDailyAverages)

kable(prepDailyAverages[,c(1,3,5)], digits=0, caption="Average Daily Air Temperature")


#Create a Continuous Line Graph with Ribbon

wthr <- ggplot(prepDailyAverages, aes(x=date, y=mean))+
     geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd), alpha=0.2)+
     geom_line()+    
     scale_x_date(breaks= seq(min(prepDailyAverages$date), max(prepDailyAverages$date), by = "2 month"), date_labels = "%b")+
     ggtitle("Continuous Average Monthly Air Temperature (Farenheit)")+
     labs(x="Month", y="Air Temperature (F)")
        #+
     #+ xlab("Month")
     #+ ylab("Mean")


#wthr + facet_grid(ntype ~., scales="free_y")

    # + mytheme 



wthr + geom_smooth() + mytheme

#Create Boxplot to show monthly airtemperatures

prepDailyAverages <- prepDates %>%


group_by(date,Month) %>%  #First group the data by Month
    dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                     #sum = sum(F_atmp),
                     #N    = length(F_atmp),
                     mean = mean(F_atmp),
                     median = median(F_atmp),
                     sd   = sd(F_atmp),
                     se   = sd / sqrt(N) )
    
#Create a Table of descriptive stats      
    arrange(Month) %>%
    as.data.frame(Summary1)
glimpse(prepDailyAverages)

kable(prepDailyAverages[,c(1,3,5)], digits=0, caption="Average Daily Air Temperature")
  
Monthly <- ggplot(prepDates, aes(x=as.factor(Month), y= F_atmp))+
    geom_boxplot(outlier.size = 1.5, outlier.shape = 21)+
    scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
    
    ggtitle("Monthly Pump Flow")+
    labs(x="January", y="Average Gallons per Day")+
    ggtitle("Continuous Average Monthly Air Temperature (Farenheit)")+
    labs(x="Month", y="Air Temperature (F)")

Monthly + mytheme

Monthly <- ggplot(prepDates, aes(x=as.factor(Month), y= F_atmp))+
geom_line(aes(y=mean-sd), colour="grey50", linetype="dotted")+
     geom_line(aes(y=mean+sd), colour="grey50", linetype="dotted")+
     geom_line()

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
airm <-apply.monthly(airx, mean)

ggplot(soton.df, aes(Month, Max.Temp)) + geom_boxplot() +
    ylab("Maximum Temperature") +
    opts(title = "Temperature at Southampton Weather Station (1950-1999)")


#Get monthly means with xts

lg <- ggplot(prepdata2,aes(x=time,y=mean, fill=ttt, group=ttt))+
     geom_line(linetype="dashed", colour="black", size=0.5,
               position=position_dodge(0.3))+
     geom_point(shape=21, size=3.5, position=position_dodge(0.3))+     
     
     scale_x_discrete(breaks=c("a", "b", "c", "d"), 
                      labels=c("PL", "V6", "TS","HS"))+
     
     scale_fill_manual(values=c("black","white"), 
                       limits=c("a", "b"), 
                       labels=c("Fall", "Spring"),
                       guide_legend(title="Treatment"))+
     xlab("Sampling Time")+
     labs(y=expression(paste(mg~N~"*"~m^2~"*"~d^-1)))


lg + facet_grid(ntype ~., scales="free_y")+mytheme 

