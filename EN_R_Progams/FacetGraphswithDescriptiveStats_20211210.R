#Monthly Pump Flows Facet Graph with Descriptive Statistics
library(tidyverse)
library(psych)
library(ggplot2)
#library(plyr)
library(knitr)


getwd()
#setwd() Set the working directory to find your file or go to File ---> Import Dataset
setwd("C:/enter/the/path/here")
prep <- read_csv("Pump5_January.csv",
                col_names = TRUE, #Describe each columns data type
                cols(
                Week = col_character(),
                Pump = col_character(),
                Day = col_integer(),
                Date = col_date(format = "%m/%d/%Y"),
                gpd = col_double(),
                amp = col_double()
         ))

prep$Pump <- str_replace(prep$Pump, "p", "Pump ") #Renaming the "p" to Pump to help label the graph better


glimpse(prep)

#Group the data and then perform descriptive stats, i.e. number of samples (N), sum, mean, median, standard deviation, standard error

prepdata <- prep %>%
        
        group_by(Week,Pump) %>% #First group the data by week of year and then by PumpID, you dont need to group by two variables
                dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                sum = sum(gpd),
                mean = mean(gpd),
                median=median(gpd),
                sd   = sd(gpd),
                se   = sd / sqrt(N) ) %>%

#Create a Table of descriptive stats      
                arrange(Week,Pump) %>%
                as.data.frame(Summary1)

kable(prepdata[,c(1,2,4)], digits=0, caption="Dosing Pump Flows for January 2017")


prepdata2 <- prepdata

#Create a graph with ggplot2

lg <- ggplot(prepdata2,aes(x=Week,y=mean))+
                geom_hline(yintercept=0, colour="#A1A1A1")+
                geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                   width=0.2, size=0.3, colour="black", position=position_dodge(0.3))+     
                geom_line(linetype="dashed", colour="black", size=0.5,
                   position=position_dodge(0.3))+
                geom_point(shape=21, size=3.5, position=position_dodge(0.3))+
        
                scale_x_discrete(breaks=c("1", "2", "3", "4", "5"),
                         labels=c("Jan.1", "Jan.8", "Jan.15","Jan22", "Jan31"))+
        
                ggtitle("Monthly Pump Flow")+
                labs(x="January", y="Average Gallons per Day")
lg

#Load "mytheme" for defined graph preferences

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

lg + facet_grid(Pump ~., scales="free_y")+mytheme 
     
