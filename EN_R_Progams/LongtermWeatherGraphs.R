sessionInfo()
library(ggplot2)
library(grid)
library(Hmisc)
library(zoo)
library(xts)
library(tidyverse)
library(lubridate)

getwd()
#setwd()
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


#Create zoo Timeseries of same data
#airz <- zoo(prep$atmp, Rdate)
#precipz <- zoo(prep$precip, Rdate)
#stmpz <- zoo(prep$stmp4, Rdate)
#solrz <- zoo(prep$solr, Rdate)


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

names(airm)

plot(airm)
p <- ggplot(airx, aes(x=Rdate, y=V1))
p + geom_line()


plot1<- ts(airm, rep(NA, 365)),
start=2013-01-01, end=2013-12-31)
                 
    
drunkenness <- ts(c(3875, 4846, 5128, 5773, 7327,
                    6688, 5582, 3473, 3186,
                    rep(NA, 51)),
                  start=1912, end=1971)

plot1 <- xyplot(spots ~ monthNames, xlab="", type="l",
                  main="Average Yearly Sunspots",
                  scales=list(x=list(alternating=2)))
> plot2 <- xyplot(spots ~ 1749:1983, xlab="Year", type="l")
> print(plot1, position=c(0, 0.2, 1, 1), more=TRUE)
> print(plot2, position=c(0, 0, 1, 0.33))

plot1 <-ggplot(airz
     + layer(stat = "summary",
                   geom = "line",
                   fill = "black",
                   linetype = 3,
                   size = 0.25,
                   colour = "antiquewhite",
                   alpha = 0.2, 
                   fun.data = mean)
     
     +layer(stat = "summary",
                  geom = "ribbon",
                  fill = "black",
                  linetype = 3,
                  size = 0.25,
                  colour = "antiquewhite",
                  alpha = 0.2, 
                  fun.data = sd
          )) 
)

plot(airz, type = "b", xaxt = "n", xlab = "")
monthNames <- months(ISOdate(2013,  1:12, 1))
axis(1, at = 1:12, labels = monthNames, las = 2)





#Make sure new data format is there. Boom!




names(air)


Rdate2 <- strptime(as.character(rain$dat), "%m/%d/%Y")
class(Rdate2)
rain <- data.frame(rain, Rdate2)
rain


#Plot
airt2 <- ggplot(data = airm,
     mapping = aes(x = Rdate, y = atmean))
     iqr <- function(x, ...) {
     qs <- quantile(as.numeric(x), probs = c(0.25, 0.75), na.rm = TRUE)
     names(qs) <- c("ymin","ymax")
     qs
     }
     airt2 + layer(stat = "summary",
     geom = "ribbon",
     fill = "black",
     linetype = 3,
     size = 0.25,
     colour = "antiquewhite",
     alpha = 0.2, 
     fun.data = iqr
     ) +
     
     layer(stat = "summary",
     geom = "line",
     col = "black",
     size = 0.5, 
     fun.y = mean
     )+

bars<- ggplot(data = rain, aes(Rdate2=X, rain=Y, fill=group, width=0.8) ) +
     geom_errorbar(aes(ymin=Y, ymax=Y+error, width = 0.2), position=position_dodge(width=0.8)) +
     geom_bar(stat="identity", position=position_dodge(width=0.8)) +
     geom_bar(stat="identity", position=position_dodge(width=0.8), colour="black", show_guide=FALSE)
+
     scale_fill_manual(values=c("grey70", "white")) +
     scale_x_discrete("X", limits=c(1:12)) +
     scale_y_continuous("Y (units)", expand=c(0,0), limits = c(0, 40), breaks=seq(0, 40, by=5)) + ggtitle ("My nice plot") +
     theme_bw() +
     theme( plot.title = element_text(face="bold", size=14),
            axis.title.x = element_text(face="bold", size=12),
            axis.title.y = element_text(face="bold", size=12, angle=90),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y=element_text(angle=90, hjust=0.5),
            legend.title = element_blank(),
            legend.position = c(0.85,0.85),
            legend.key.size = unit(1.5, "lines"),
            legend.key = element_rect()
     )
    

data = rain,
                mapping = aes(x = Rdate, fill=rain$precip))
    
colour = factor(Rdate), fill = factor(Rdate)), 
linetype = 2, alpha= 0.1)

               

plot <- qplot(Rdate, mean(atmean), data = air, geom = "line") +
     ylab("Air Temperature") +
     geom_line(xintercept=0, colour="gray50")
plot
plot + scale_x_date(major = "weeks")



)

# lesion becomes a classifying factor

f3 <- y +  scale_x_continuous("Date)") +
     scale_y_continuous("Precipitation(mm)") +
     scale_shape_manual(values=c(79,79)) +
     scale_fill_manual(values=c("white","black")) +
     stat_abline(intercept=0, slope=0, linetype="dotted") +
     annotate("text", x=11, y=10, label="X") +
     theme_bw()

optns <- theme (
     plot.title = element_text(face="bold", size=14),
     axis.title.x = element_text(face="bold", size=12),
     axis.title.y = element_text(face="bold", size=12, angle=90),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     legend.position = c(0.2,0.8),
     legend.title = element_blank(),
     legend.text = element_text(size=12),
     legend.key.size = unit(1.5, "lines"),
     legend.key = element_blank()
)
f3 +  ggtitle ( "MY awsome plot for publication") + optns


#####################################
#junk?
myd <- data.frame (X = c(1:12,1:12),
                   Y = c(8, 12, 13, 18,  22, 16, 24, 29,  34, 15, 8, 6,
                         9, 10, 12, 18, 26, 28, 28, 30, 20, 10, 9, 9),
                   group = rep (c("A-group", "B-group"), each = 12),
                   error = rep (c(2.5, 3.0), each = 12))

#Convert factor or character string into date, and supply format
Rdate <- strptime(as.character(air$date), "%m/%d/%Y")
#Check conversion, should be POSIXt
class(Rdate)
#Add formated date back to dataframe.  Boom!
air <- data.frame(air, Rdate)
#Make sure new data format is there. Boom!
names(air)
