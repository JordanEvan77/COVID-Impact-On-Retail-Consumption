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



#IPUMS:
ddi <- read_ipums_ddi("Rawdata/cps_00001.xml")
og_df <- read_ipums_micro(ddi) # WORKS!

#initial Code Read In
og_df <- read_csv('RawData/asec_csv_repwgt_2021.csv')  # how do I read it in with Haven()?
read_dta()

#change floor base

#This regression seems to want one observation per month, so we’d need to create a year-month variable, use 
final_df %>% group_by(year, month, yearmo) %>% summarize(RetailEmployment = sum(indname == 'Retail Trade'))

#clean Data: