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
library(corrplot)
library(dplyr)
library(haven)
library(ipumsr)
library(zoo)



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

# Null Values
nulls <- (colMeans(is.na(join_df2)))*100
nulls

#YEAR     SERIAL      MONTH    HWTFINL      CPSID   ASECFLAG     REGION   STATEFIP    METAREA     CBSASZ     FAMINC     PERNUM 
#0.00000    0.00000    0.00000    0.00000    0.00000   90.88771    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000 
#WTFINL     CPSIDP        AGE        SEX       RACE      MARST     NCHILD    CITIZEN   NATIVITY     HISPAN    EMPSTAT        IND 
#0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000    0.00000 
#CLASSWKR   WHYUNEMP     WKSTAT       EDUC   SCHLCOLL    DIFFANY COVIDTELEW  COVIDUNAW  COVIDPAID  COVIDLOOK    INDNAME 
#0.00000    0.00000    0.00000    0.00000    0.00000    0.00000   55.32061   55.32061   55.32061   55.32061   30.34504 

#So the wors outliers appear to be ASECFLAG, COVIDTELEW, COVIDUNAW, COVIDPAID, COVIDLOOK and INDNAME(where it is 0)
#----FIRST QUESTION, SHOULD WE DROP MISSING ROWS OF INDNAME, and FULL COLUMNS OF THE REST?

#COVIDUNNAW means unable to work due to covid, this may be our outcome variable that we are looking for !
#May be valueable to review with covidlook, which means covid kept them from being able to job search. 
  
#join_drop <- join_df2[join_df2$ IND!= 0, ] #turn this off so I can recode
join_drop <- join_df2

join_drop <- join_drop %>% dplyr::select(-c(ASECFLAG, COVIDTELEW, COVIDPAID)) #removes 3 cols 

#create dummy variable for monthyear:
join_drop <- join_drop %>% mutate(monthyear = ym(paste(YEAR, MONTH)))
class(join_drop$monthyear)

# EDUC Conversion factors:
join_drop <- join_drop %>% mutate(EDUC = sjlabelled::as_label(EDUC))
join_drop <- join_drop %>% mutate(STATEFIP = sjlabelled::as_label(STATEFIP))
join_drop <- join_drop %>% mutate(REGION = sjlabelled::as_label(REGION))
join_drop <- join_drop %>% mutate(METAREA = sjlabelled::as_label(METAREA))
join_drop <- join_drop %>% mutate(CBSASZ = sjlabelled::as_label(CBSASZ))
join_drop <- join_drop %>% mutate(SEX = sjlabelled::as_label(SEX))
join_drop <- join_drop %>% mutate(RACE = sjlabelled::as_label(RACE))
join_drop <- join_drop %>% mutate(MARST = sjlabelled::as_label(MARST))
join_drop <- join_drop %>% mutate(CITIZEN = sjlabelled::as_label(CITIZEN))
join_drop <- join_drop %>% mutate(NATIVITY = sjlabelled::as_label(NATIVITY))
join_drop <- join_drop %>% mutate(HISPAN = sjlabelled::as_label(HISPAN))
join_drop <- join_drop %>% mutate(CLASSWKR = sjlabelled::as_label(CLASSWKR))
join_drop <- join_drop %>% mutate(WHYUNEMP = sjlabelled::as_label(WHYUNEMP))
join_drop <- join_drop %>% mutate(SCHLCOLL = sjlabelled::as_label(SCHLCOLL))
join_drop <- join_drop %>% mutate(DIFFANY = sjlabelled::as_label(DIFFANY))
join_drop <- join_drop %>% mutate(COVIDUNAW = sjlabelled::as_label(COVIDUNAW))
join_drop <- join_drop %>% mutate(COVIDLOOK = sjlabelled::as_label(COVIDLOOK))

join_drop <- join_drop %>% mutate(WKSTAT = sjlabelled::as_label(WKSTAT))



#DUMMY VARIABLES:
join_drop <- join_drop %>% mutate(IncNumber = case_when(FAMINC ==112 ~ 700, FAMINC == 120 ~ 1500, FAMINC == 121 ~ 1250, 
                                                        FAMINC == 122 ~ 1750, FAMINC == 130 ~ 2500, FAMINC == 131 ~ 2250,
                                                        FAMINC == 132 ~ 2750, FAMINC ==140 ~ 3500, FAMINC ==141 ~ 3250, 
                                                        FAMINC == 150 ~ 4500, FAMINC == 200 ~ 6000, FAMINC == 210 ~ 6500,
                                                        FAMINC == 220 ~ 5500, FAMINC == 230 ~ 7000, FAMINC == 231 ~ 6750,
                                                        FAMINC == 232 ~ 6500, FAMINC == 233 ~ 7250, FAMINC == 234 ~ 7500,
                                                        FAMINC == 300 ~ 8500, FAMINC == 310 ~ 7750, FAMINC == 320 ~ 8250,
                                                        FAMINC == 330 ~ 8750, FAMINC == 340 ~ 8500, FAMINC == 350 ~ 9500,
                                                        FAMINC == 400 ~ 12500, FAMINC == 410 ~ 10500, FAMINC ==420 ~ 11500,
                                                        FAMINC == 430 ~ 11500, FAMINC == 440 ~ 11000, FAMINC == 450 ~ 12500,
                                                        FAMINC == 460 ~ 13500, FAMINC == 470 ~ 13750, FAMINC == 480 ~ 13500,
                                                        FAMINC == 490 ~ 14500, FAMINC == 500 ~ 17500, FAMINC == 510 ~ 15500,
                                                        FAMINC == 520 ~ 16500, FAMINC == 530 ~ 17500, FAMINC == 540 ~ 16750,
                                                        FAMINC == 550 ~ 18750, FAMINC == 560 ~ 19000, FAMINC == 600 ~ 22500,
                                                        FAMINC == 700 ~ 37500, FAMINC == 710 ~ 27500, FAMINC == 720 ~ 32500,
                                                        FAMINC == 730 ~ 37500, FAMINC == 740 ~ 45000, FAMINC == 800 ~ 65000,
                                                        FAMINC == 810 ~ 67500, FAMINC == 820 ~ 55000, FAMINC == 830 ~ 67500,
                                                        FAMINC == 840 ~ 87000, FAMINC == 841 ~ 87500, FAMINC == 842 ~ 125000,
                                                        FAMINC == 843 ~ 150000, FAMINC == 995 ~0, FAMINC == 996 ~0, 
                                                        FAMINC == 997 ~ 0, FAMINC == 999 ~ 0)) #works and runs!

 #write.csv(join_drop, 'Rawdata/firstdata.csv')


#change floor base, at industry level: 
#This regression seems to want one observation per month, so we’d need to create a year-month variable, use 
join_drop %>% group_by(YEAR, MONTH, monthyear) %>% summarize(RetailEmployment = sum(INDNAME == 'Retail Trade'))


# the above is just a starting point for how we could do this. 
#His notes say this from the spec:
"If you want an analysis to be at the industry-month level, you should make your data be at that level too! 
Use mutate(yearmo = year*100 + month) to create a year-month variable, and then use group_by(yearmo, indname) %>% summarize() 
to collapse data to the yearmo/indname level"

#selecting Attributes
final_df <- join_drop %>% dplyr::select(-c(CBSASZ, HISPAN, STATEFIP, COVIDLOOK, METAREA, NATIVITY, IND, EMPSTAT, CITIZEN))


#DUMMY:
#Covid Time Period:

final_df$covid1_dummy <- ifelse(final_df$monthyear == '2020-03-01' | final_df$monthyear == '2020-04-01' , 1, 0)

final_df$covid2_dummy <- ifelse(final_df$monthyear == '2020-05-01' | final_df$monthyear == '2020-06-01' |
                                  final_df$monthyear == '2020-07-01' | final_df$monthyear == '2020-08-01' |
                                  final_df$monthyear == '2020-09-01' | final_df$monthyear == '2020-10-01', 1, 0)

final_df$covid3_dummy <- ifelse(final_df$monthyear == '2020-11-01' | final_df$monthyear == '2020-12-01' |
                                  final_df$monthyear == '2021-01-01' | final_df$monthyear == '2021-02-01' |
                                  final_df$monthyear == '2021-03-01' | final_df$monthyear == '2021-04-01' |
                                  final_df$monthyear == '2021-05-01' | final_df$monthyear == '2021-06-01' |
                                  final_df$monthyear == '2021-07-01', 1, 0)


#Race:

final_df$race_dummy <- ifelse(final_df$RACE =='White', 1, 0)

#SCHLOL:
final_df$student_dummy <- ifelse(final_df$SCHLCOLL =='Does not attend school, college or university' | 
                                   final_df$SCHLCOLL == 'NIU', 0, 1)

final_df %>% ggplot(mapping = aes(INDNAME)) + 
  geom_histogram(stat = 'count',  fill="blue") +
  coord_flip()

#INDUSTRY: Non retail
final_df$INDNAME <- ifelse(is.na(final_df$INDNAME), 'Not retail', final_df$INDNAME)




write.csv(final_df, 'Rawdata/final_clean.csv')









