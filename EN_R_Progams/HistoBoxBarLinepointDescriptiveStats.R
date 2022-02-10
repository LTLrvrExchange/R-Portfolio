#These will aid in developing various plot.


#Data exploration
#Histograms
#Box plot
#Bar plot
#Linepoint plot
#Descriptive Stats

library(tidyverse)
library(psych)
library(ggplot2)
library(plyr)
library(knitr)
library(lubridate)

getwd()
setwd("H:/My Drive/02EnvirExchNetwkGrant/ENGrantDueProducts/EN_R_DataSources")


prep<-read.csv (file ="SoilN.csv",
                header = TRUE, 
                sep = ",")
glimpse(prep)
prep$date <- mdy(prep$date)
class(prep$date)

prep2 <- prep 


# Some quick plots to visualize data

#Histogram

histogram1 <- hist(prep2$nh4, col = "lightgray") #Histogram showing distrobution of all data

histogram2 <- ggplot(prep2, aes(nh4, fill=ttt))+
                geom_histogram()

histogram2

#BoxPlots

box1 <- boxplot(nh4 ~ ttt, data = prep2) #Basic boxplot. Grouped by ttt which is treatment

box2 <- ggplot(prep2,aes(interval, nh4, fill=ttt))+ #ggplot2 Boxplot. Grouped by both depth and ttt
        geom_boxplot(width=.75, outlier.size=3, outlier.shape=21)+
        
        xlab("Depth (cm)")+
        ylab("Nitrate - N" ~ ("mg N L"^-1)~"\n")+
        scale_fill_grey(start=.5, end=1,
                        labels=c("Fall","Spring"),
                        guide_legend(title="Treatment"))
box2

#Some descriptive statistics to assist with upcoming Barplots

prepbar1 <- prep2 %>%

        group_by(ttt) %>%  #First group the data by Month
        dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                         #sum = sum(F_atmp),
                         #N    = length(F_atmp),
                         mean = mean(nh4),
                         median = median(nh4),
                         sd   = sd(nh4),
                         se   = sd / sqrt(N) )

prepbar1

prepbar2 <- prep2 %>%
        
        group_by(ttt,interval) %>%  #First group the data by Month
        dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                         #sum = sum(F_atmp),
                         #N    = length(F_atmp),
                         mean = mean(nh4),
                         median = median(nh4),
                         sd   = sd(nh4),
                         se   = sd / sqrt(N) )

prepbar2

kable(prepbar2[,c(1:7)], digits=0, caption="Descriptive Stats by Treatment and Depth")

# Independent 2-group t-test (with defaults)
#t.test(log(mgl) ~ ttt, data = prep)

#Barplots with errorbars taken from descriptive statistics

barplot(mean ~ ttt, data=prepbar1)

bar <-ggplot(prepbar2,aes(x=interval, y=mean, fill=ttt))+
        geom_bar(position="dodge", stat="identity", colour="black")+
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                      width=.2, position=position_dodge(0.9))+
        xlab("Interval (Weeks)")+
        ylab("Ammonium - N" ~ ("mg N L"^-1)~"\n")+
        scale_fill_grey(start=.5, end=1,
                        labels=c("Fall","Spring"),
                        guide_legend(title="Treatment"))
bar


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

bar+mytheme

library(plyr)



lg <- ggplot(prepbar2,aes(x=interval,y=mean, fill=ttt, group=ttt))+
     geom_hline(yintercept=0, colour="#A1A1A1")+
     geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                   width=0.2, size=0.3, colour="black", position=position_dodge(0.3))+     
     geom_line(linetype="dashed", colour="black", size=0.5,
               position=position_dodge(0.3))+
     geom_point(shape=21, size=3.5, position=position_dodge(0.3))+
        xlab("Interval (Weeks)")+
        ylab("Ammonium - N" ~ ("mg N L"^-1)~"\n")+
        scale_fill_grey(start=.5, end=1,
                        labels=c("Fall","Spring"),
                        guide_legend(title="Treatment"))
lg
lg+mytheme
rm(list = ls())  # Clean up