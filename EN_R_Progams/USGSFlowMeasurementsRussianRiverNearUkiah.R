library(tidyverse)
# # specifically, we'll use these packages from tidyverse:
# library(dplyr)
# library(tidyr)
# library(ggplot2)
library(dataRetrieval)
library(knitr)
library(lubridate)


# Russian River gauge station near Ukiah CA
siteNo <- "11461000"

# Daily mean stream flow discharge in cubic feet per second
pCode <- "00060"

# Pull data for the full calendar year of 2018
start.date <- "2021-01-01"
end.date <- "2021-12-31"

# Pull the data
RussianRnearUkiahRcfs <- readNWISuv(siteNumbers = siteNo,
                          parameterCd = pCode,
                          startDate = start.date,
                          endDate = end.date)
# str(RussianRnearUkiahRcfs) - what are X_00060_00000 and X_00060_00000_cd? the next step
#  renames them more intuitively

# rename the columns
#RussianRnearUkiahRcfs <- renameNWISColumns(RussianRnearUkiahRcfs)
# names(RussianRnearUkiahRcfs)
# unique(RussianRnearUkiahRcfs$agency_cd)            #USGS
# unique(RussianRnearUkiahRcfs$site_no)              #01034500 11461500
# length(unique(RussianRnearUkiahRcfs$dateTime))     #many
# length(unique(RussianRnearUkiahRcfs$GH_Inst))    #many
# unique(RussianRnearUkiahRcfs$GH_Inst_cd)         #"A e", "A"
# unique(RussianRnearUkiahRcfs$tz_cd)                #UTC
# length(unique(RussianRnearUkiahRcfs$DOY))          #365


RussianRnearUkiahRcfs <- mutate(RussianRnearUkiahRcfs, DOY = as.numeric(strftime(RussianRnearUkiahRcfs$dateTime, format = "%j")))
RussianRnearUkiahRcfs <- RussianRnearUkiahRcfs %>%
  # add day of year (DOY) to the data frame
  #mutate(DOY = strftime(RussianRnearUkiahRcfs$Date, format = "%j")) %>%
  
  mutate(date = strftime(RussianRnearUkiahRcfs$date, format = "%D")) %>%
  mutate(Month = strftime(RussianRnearUkiahRcfs$date, format = "%B")) %>%
  mutate(month_number = strftime(RussianRnearUkiahRcfs$date, format = "%m")) %>%
  # keep just two of the columns
  select(date, month_number, Month, DOY, X_00060_00000)

# pull surface-water annual statistics
# https://nwis.waterdata.usgs.gov/usa/nwis/pmcodes/help?output_formats_help#Daily-value_streamflow_data_options
RussianRnearUkiahRcfsStat <- readNWISstat(siteNumbers = 11461000,
                                parameterCd = "00060",
                                statReportType="daily")

RussianRnearUkiahRcfsStat$Date <- lubridate::mdy(paste0(RussianRnearUkiahRcfsStat$month_nu,
                                              "-", RussianRnearUkiahRcfsStat$day_nu,
                                              "-", RussianRnearUkiahRcfsStat$begin_yr))

#RussianRnearUkiahRcfsStat %>%
  #mutate(date = make_date(month_nu, day_nu, end_yr))

#RussianRnearUkiahRcfsStat$Date <- as.Date(with(df, paste(month_nu, day_nu, begin_yr,sep="-")), "%m-%d-%y")
#df$date


#RussianRnearUkiahRcfsStat <- mutate(RussianRnearUkiahRcfsStat, DOY = as.numeric(strftime(RussianRnearUkiahRcfsStat$Date, format = "%j")))

RussianRnearUkiahRcfsStat$DOY <- as.numeric(strftime(RussianRnearUkiahRcfsStat$Date, format = "%j"))

RussianRnearUkiahRcfsStat <- RussianRnearUkiahRcfsStat %>% 
  select(DOY, mean_va) # mean_va = annual mean of daily mean values for that DOY

RussianRnearUkiahR <- left_join(RussianRnearUkiahRcfs, RussianRnearUkiahRcfsStat, by = "DOY") %>%
  rename("Instantaneous: 2018" = "X_00060_00000",
         "Mean Daily: 1903-2020" = "mean_va") %>%
  pivot_longer(cols = c("Instantaneous: 2018", "Mean Daily: 1903-2020"),
               names_to = "Parameter",
               values_to = "Value")

last_date = RussianRnearUkiahR$date[dim(RussianRnearUkiahR)[1]]
last_date_inst = filter(RussianRnearUkiahR, date == last_date & Parameter == "Instantaneous: 2018")$Value
last_date_mean = filter(RussianRnearUkiahR, date == last_date & Parameter == "Daily mean: 1903-2020")$Value

Qplots <- ggplot(data = RussianRnearUkiahR,
                 aes(as.numeric(DOY), Value, color = Parameter)) +
  geom_line() +
  scale_x_continuous(breaks=c(32,92,152,213,274,335), 
                     labels=c("Feb","Apr","June", "Aug","Oct","Dec")) +
  labs(title = paste0("USGS Flow Measurements on the ",
                      attr(RussianRnearUkiahRcfs, "siteInfo")$station_nm), 
       x = "Date", 
       y = attr(RussianRnearUkiahRcfs, "variableInfo")$variableDescription) +
  theme(legend.position = "bottom", legend.box = "vertical",
        plot.title = element_text(size=12))

Qplots

monthly = RussianRnearUkiahR %>%
  group_by(Parameter, month_number, Month) %>%
  summarise(mean=mean(Value)) %>%
  pivot_wider(names_from = Parameter, values_from = mean) %>%
  arrange(month_number) %>%
  mutate("Which is greater?" = ifelse(`Instantaneous: 2018` > `Mean Daily: 1903-2020`,
                                      "Instantaneous: 2018", "Mean Daily: 1903-2020"))

kable(monthly[,2:5], digits=0, caption="Means by month")
```

