library(fixest)
library(ggplot2)
library(tidyverse)
library(vtable)
library(Ecdat)
library(ggstance)
library(multcomp)
library(NHANES)
library(purrr)
library(lubridate)
library(correlationfunnel)
library(corrplot)
library(inspectdf) 
library(dplyr)
library(haven)
library(ipumsr)



#IPUMS TYPE 1:
ddi <- read_ipums_ddi("Rawdata/cps_00001.xml")
og_df <- read_ipums_micro(ddi) # WORKS!


#Raw Data 2
ddi2 <- read_ipums_ddi("Rawdata/cps_00002.xml")
og_df2 <- read_ipums_micro(ddi2) 

#initial industry names Code Read In
indnames <- read_csv('RawData/indnames.csv')  # how do I read it in with Haven()?
names(indnames) <- toupper(names(indnames)) # change the case to match

#join the tables to industry names:
join_df2 <- left_join(og_df2, indnames, by='IND')


#clean Data:

# Outliers:
outliers <- (colMeans(is.na(join_df2)))*100
outliers

#YEAR     SERIAL      MONTH    HWTFINL      CPSID   ASECFLAG     REGION   STATEFIP    METAREA     CBSASZ     FAMINC     PERNUM 
#0.00000    0.00000    0.00000    0.00000    0.00000   90.88771    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000 
#WTFINL     CPSIDP        AGE        SEX       RACE      MARST     NCHILD    CITIZEN   NATIVITY     HISPAN    EMPSTAT        IND 
#0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000 
#CLASSWKR   WHYUNEMP     WKSTAT       EDUC   SCHLCOLL    DIFFANY COVIDTELEW  COVIDUNAW  COVIDPAID  COVIDLOOK    INDNAME 
#0.00000    0.00000    0.00000    0.00000    0.00000    0.00000   55.32061   55.32061   55.32061   55.32061   30.34504 

#So the wors outliers appear to be ASECFLAG, COVIDTELEW, COVIDUNAW, COVIDPAID, COVIDLOOK and INDNAME(where it is 0)
#----FIRST QUESTION, SHOULD WE DROP MISSING ROWS OF INDNAME, and FULL COLUMNS OF THE REST?
  
join_drop <- join_df2[join_df2$ IND!= 0, ]

#Categorical:
#FAMINC: HIstogram:

join_drop %>% ggplot(mapping = aes(FAMINC)) + 
  geom_histogram(bins = 100, fill="blue") # seems like we have mostly 800 and above?

#WKSTAT
join_drop %>% ggplot(mapping = aes(WKSTAT)) + 
  geom_histogram(bins = 100, fill="blue") # mainly lower values?

#EDUC:
join_drop %>% ggplot(mapping = aes(EDUC)) + 
  geom_histogram(bins = 100, fill="blue") # some spread! worth investigating:)

#SCHLCOLL
join_drop %>% ggplot(mapping = aes(SCHLCOLL)) + 
  geom_histogram(bins = 100, fill="blue") # mainly 0s and 5s, meaning NIU or doesn't attend any school:)


#numerical:



#correlation



#professors step by step guidance:





#change floor base
#This regression seems to want one observation per month, so we’d need to create a year-month variable, use 
final_df %>% group_by(year, month, yearmo) %>% summarize(RetailEmployment = sum(indname == 'Retail Trade'))
















