---
title: "Economic and Public Health Consequences of Severe Weather Events in the U.S."
author: "Anton Kasenkov"
date: "25/11/2019"
output:
        html_document:
                keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE,
                      fig.width = 12,
                      fig.height = 8,
                      warning = FALSE,
                      message = FALSE)
Sys.setenv(TZ = "UTC")
Sys.setlocale("LC_ALL","English")
options(stringsAsFactors = FALSE)
rm(list = ls()); gc()
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(knitr)
setwd("C:/r/coursera/repres2/reproducible_research_course_project_2/")
```

## Synopsis

In this descriptive analysis the severe weather events have been studied with the purpose of finding out the top-10 most harmful ones in terms of:
a) population health;
b) economic consequences.
The analysis mainly consists of selecting relevant cases, recoding, aggregating and plotting the data from National Oceanic and Atmospheric Administration (NOAA) database.


```r
# we begin by downloading the dataset:
url <- paste0("https://d396qusza40orc.",
              "cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")
download.file(url, "data.csv.bz2")
```

## Data Processing

The following steps of data processing are aimed at the creation of 2 summary tables necessary for the final plots. First, the data must be loaded into R. Then only relevant to the purpose of the study variables are left. Those are:  
* BGN_DATE - date when the event took place,   
* EVTYPE - type of the event,   
* FATALITIES - the estimated number of fatalities,  
* INJURIES - the estimated number of injuries,  
* PROPDMG - property damage value,   
* PROPDMGEXP - factor by which PROPDMG should be multiplied in order to get the final value of property damage,   
* CROPDMG - crops damage value,   
* CROPDMGEXP - factor for CROPDMG.  

The next step of data processing would be the recoding of factor variables for crops and property damage in accordance with the official documentation. The recoding is necessary since the original values are not numeric.  

After this, the corresponding estimates of damage were multiplied by their factors, thus resulting in the correct numbers for damage evaluation.   

Due to the big size of the data, the decision was made to trim down some of the parts of it. Some variables were removed because they were not needed anymore. But most importantly, the number of observations was significantly reduced. The reason behind this is the desire to have more accurate comparison between different weather events. According to <a href="https://www.ncdc.noaa.gov/stormevents/details.jsp">this</a> documentation, prior to 1996 only tornado, thunderstorm wind and hail types of weather events were recorded.   

Finally, the data were aggregated by type of event and sums were calculated. Property damage and crops damage were summed since those variables are measured in usd. Fatalities and injuries were summed because they are measured in the number of people affected by particular type of event.


```r
# reading and preparing the data:
df <- read.csv("data.csv.bz2") %>%
        select(2, 8, 23:28) %>% # selecting only relevant columns
        mutate(BGN_DATE = as_date(mdy_hms(BGN_DATE)),
               year = year(BGN_DATE), # creating year variable
               CROPDMGEXP = tolower(CROPDMGEXP), # recoding crops damage exp
               cropdmgexp = case_when(
                       CROPDMGEXP == "" ~ 10^0,
                       CROPDMGEXP == "?" ~ 10^0,
                       CROPDMGEXP == "0" ~ 10^0,
                       CROPDMGEXP == "2" ~ 10^2,
                       CROPDMGEXP == "k" ~ 10^3,
                       CROPDMGEXP == "m" ~ 10^6,
                       CROPDMGEXP == "b" ~ 10^9
               ),
               cropdmg = CROPDMG * cropdmgexp, # final value of crops damage
               PROPDMGEXP = tolower(PROPDMGEXP), # recoding prop damage exp
                propdmgexp = case_when(
                       PROPDMGEXP == "" ~ 10^0,
                       PROPDMGEXP == "+" ~ 10^0,
                       PROPDMGEXP == "-" ~ 10^0,
                       PROPDMGEXP == "?" ~ 10^0,
                       PROPDMGEXP == "0" ~ 10^0,
                       PROPDMGEXP == "1" ~ 10^1,
                       PROPDMGEXP == "2" ~ 10^2,
                       PROPDMGEXP == "3" ~ 10^3,
                       PROPDMGEXP == "4" ~ 10^4,
                       PROPDMGEXP == "5" ~ 10^5,
                       PROPDMGEXP == "6" ~ 10^6,
                       PROPDMGEXP == "7" ~ 10^7,
                       PROPDMGEXP == "8" ~ 10^8,
                       PROPDMGEXP == "k" ~ 10^3,
                       PROPDMGEXP == "m" ~ 10^6,
                       PROPDMGEXP == "b" ~ 10^9
               ),
               propdmg = PROPDMG * propdmgexp, # final value of prop damage
               evtype = str_replace_all(tolower(EVTYPE), "[^[:alpha:]]", "")) %>% 
        select(-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP,
                  BGN_DATE, propdmgexp, cropdmgexp)) %>% 
        filter(year >= 1996) # filtering only relevant rows

# estimating damage to population health by type of event:
pop_health <- df %>%
        group_by(evtype) %>%
        summarise_at(vars(FATALITIES, INJURIES), ~sum(.)) %>%
        mutate(totalhdmg = FATALITIES + INJURIES) %>%
        filter(totalhdmg != 0) %>%
        arrange(-totalhdmg) %>%
        mutate(evtype = factor(evtype, levels = evtype))

# estimating economic consequences of weather events: 
econ_cons <- df %>% 
        group_by(evtype) %>%
        summarise_at(vars(propdmg, cropdmg), ~sum(.)) %>%
        mutate(totalecdmg = propdmg + cropdmg) %>%
        filter(totalecdmg != 0) %>%
        arrange(-totalecdmg) %>%
        mutate(evtype = factor(evtype, levels = evtype),
               totalecdmg = round(totalecdmg / 10^9))
```

## Results

The following plots represent the most harmful (top-10) severe weather events in terms of population health and economic consequences. It should be mentioned that not much effort was made to clean the EVTYPE variable, thus it contains some duplicates. The researcher can only hope that this has a negligble effect on the rating of events.


```r
ggplot(pop_health %>% slice(1:10), aes(x = evtype, y = totalhdmg, fill = evtype))+
        geom_col()+
        theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
        labs(title = "Most harmful weather events (fatalities & injuries)",
             x = "",
             y = "people")
```

![](repr_research_project_2_files/figure-html/pophealth-1.png)<!-- -->
As it can be seen from the previous graph, Tornados, Excessive heat and Floods cause a lot of public health problems.

```r
ggplot(econ_cons %>% slice(1:10), aes(x = evtype, y = totalecdmg, fill = evtype))+
        geom_col()+
        theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
        labs(title = "Most harmful weather events (property & crops damage)",
             x = "",
             y = "billions USD")
```

![](repr_research_project_2_files/figure-html/econcons-1.png)<!-- -->
In terms of damage to the property and crops floods, hurricanes and storm surges are most dangerous events.
