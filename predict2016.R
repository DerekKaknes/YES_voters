#' Load external libraries that may be useful for us
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(scales)

#' Create a class to convert date strings to Dates
setClass('sboeDate')
setAs('character', 'sboeDate', function(from) as.Date(from, format='%Y%m%d'))

#' Load voter file data from Assembly District 70
#' from csv file into R data.frame using read.csv 
a70_voters <- read.csv('assembly70.csv',
                   stringsAsFactors = F,
                   colClasses = c(lastname='character', firstname='character', 
                                  sboeid='factor', rstreetname='character', 
                                  rzip='factor', rzipplus='factor', dob='sboeDate',
                                  gender='factor', enrollment='factor', status='factor',
                                  regdate='sboeDate', voterhistory='character'))

#' Load historical election results from Assembly District 70
a70_results <- read.csv('nyc_pres_results.csv',
                        stringsAsFactors = F,
                        colClasses = c(ElectionYear='character', Democrat='numeric',
                                       Republican='numeric', Other='numeric',
                                       Total='numeric'))
#' Make bar graph showing election results from 2000 to 2012
a70_results %>% 
  select(-Total) %>%
  gather(Party, Votes, Democrat:Other) %>%
  ggplot(.) +
  geom_bar(aes(x=ElectionYear, y=Votes, fill=Party), stat='identity') + 
  ggtitle("Assembly District 70 Presidential Election Results") + 
  scale_y_continuous(labels=comma)

a70_voters %>%
  filter(status=='ACTIVE') %>%
  select(gender, enrollment) %>%
  group_by(enrollment) %>%
  ggplot(.) +
  geom_bar(aes(x=enrollment, fill=gender)) +
  ggtitle("Party Enrollment in Assembly District 70") +
  xlab("Party Enrollment") +
  ylab("Voter Count") +
  scale_y_continuous(labels=comma)
          
