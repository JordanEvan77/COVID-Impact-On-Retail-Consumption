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
  
join_drop <- join_df2[join_df2$ IND!= 0, ]

join_drop <- join_drop %>% dplyr::select(-c(ASECFLAG, COVIDTELEW, COVIDPAID)) #removes 3 cols 

#create dummy variable for monthyear:
join_drop <- join_drop %>% mutate(monthyear = ym(paste(YEAR, MONTH)))
class(join_drop$monthyear)

# EDUC Conversion factors:
join_drop <- join_drop %>% mutate(EDUC = sjlabelled::as_label(EDUC))



#DUMMY VARIABLES:
join_drop <- join_drop %>% mutate(IncNumber = case_when(FAMINC ==112 ~ 700, FAMINC == 120 ~ 1500, FAMINC == 121 ~ 1250, 
                                                        FAMINC == 122 ~ 1750, FAMINC == 130 ~ 2500, FAMINC == 131 ~ 2250,
                                                        
                                                        FAMINC == 842 ~ 125000))
      



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

#CBSASZ
join_drop %>% ggplot(mapping = aes(CBSASZ)) + 
  geom_histogram(bins = 100, fill="blue") # reasonably distributed

#METAREA:
join_drop %>% ggplot(mapping = aes(METAREA)) + 
  geom_histogram(bins = 100, fill="blue")  #mostly 9999, does that mean the same as 99, unidentified?

#SEX:
join_drop %>% ggplot(mapping = aes(SEX)) + 
  geom_histogram(bins = 100, fill="blue")

#RACE:
join_drop %>% ggplot(mapping = aes(RACE)) + 
  geom_histogram(bins = 100, fill="blue") # a mainly white demographic, which is why the weights are crucial.

#MARST
join_drop %>% ggplot(mapping = aes(MARST)) + 
  geom_histogram(bins = 100, fill="blue") # mainly married or never married.

#NCHILD:
join_drop %>% ggplot(mapping = aes(NCHILD)) + 
  geom_histogram(bins = 100, fill="blue") #mainly 0 children in household, which is interesting

#CITIZEN:
join_drop %>% ggplot(mapping = aes(CITIZEN)) + 
  geom_histogram(bins = 100, fill="blue") # mainly folsk born in the US

#NATIVITY: Foreign Birth place:
join_drop %>% ggplot(mapping = aes(NATIVITY)) + 
  geom_histogram(bins = 100, fill="blue") # mainly either both parents born in US or individual is born outside US

#HISPAN:
join_drop %>% ggplot(mapping = aes(HISPAN)) + 
  geom_histogram(bins = 100, fill="blue") # we only have about 250k Mexican individuals, other latinx is limited

#EMPSTAT: Crucial:
join_drop %>% ggplot(mapping = aes(EMPSTAT)) + 
  geom_histogram(bins = 100, fill="blue") # majority are employed

#IND: Industry type:
join_drop %>% ggplot(mapping = aes(IND)) + 
  geom_histogram(bins = 100, fill="blue") # couldn't plot with names, but interesting to see lumps

#CLASS WKR:
join_drop %>% ggplot(mapping = aes(CLASSWKR)) + 
  geom_histogram(bins = 100, fill="blue") # mainly private for profit workers

#WHYUNEMP:
join_drop %>% ggplot(mapping = aes(WHYUNEMP)) + 
  geom_histogram(bins = 100, fill="blue")  #mainly NIU, which is disapointing, may be useless

#WKSTAT:
join_drop %>% ggplot(mapping = aes(WKSTAT)) + 
  geom_histogram(bins = 100, fill="blue") # mainly full time workers


#----SEcond question, should all of the above be turned into factor() variables?

#PLOT OVER TIME: EMPSTAT AND FAMINC by IND:

#FAMINC:
top_earners <- join_drop %>% 
  group_by(INDNAME) %>% 
  Summarize(avg_inc = mean(IncNumber)) %>%  
  arrange(desc(avg_inc)) %>% 
  head(5)


top_earner_data <- join_drop %>% group_by(INDNAME, montyear) %>% summarize(avg_inc = mean(IncNumber)) %>% 
  filter(INDNAME %in% top_earners$INDNAME)

#this will take a long while to actually show up after being visualized
top_earner_data %>% 
  ggplot(mapping = aes(x = monthyear, y = IncNumber, color = as.factor(INDNAME))) + 
  geom_line() # a bit messy, but interesting! might be able to clean up by removing bottom values



#EMPSTAT: This isn't working yet
top_stats_data <- join_drop %>% filter(EMPSTAT < 25) %>% group_by(EMPSTAT) %>% summarize(n= n(EMPSTAT)) # this isn't working yet


top_earner_data %>% 
  ggplot(mapping = aes(x = monthyear, y = n, color = as.factor(EMPSTAT))) + 
  geom_line()


#Empstat: Actually works!
Unemp_data <- join_drop %>% filter(EMPSTAT == 21) %>%  group_by(monthyear, EMPSTAT)  %>% summarize(counted = n())

Unemp_data%>% 
  ggplot(mapping = aes(x = monthyear, y = counted, color = as.factor(EMPSTAT))) + 
  geom_line()

#EMPSTAT: Now without a filter: Works!
Unemp_data2 <- join_drop %>%  group_by(monthyear, EMPSTAT)  %>% summarize(counted = n())

Unemp_data2 %>% 
  ggplot(mapping = aes(x = monthyear, y = counted, color = as.factor(EMPSTAT))) + 
  geom_line() # major drop in people at work (10) and slight bump in unemployment (21)

#Empstat: RETAIL ONLY
Unemp_data3 <- join_drop %>% filter(EMPSTAT == 21, INDNAME == 'Retail Trade') %>%  group_by(monthyear, EMPSTAT) %>% 
  summarize(counted = n())

Unemp_data3%>% 
  ggplot(mapping = aes(x = monthyear, y = counted, color = as.factor(EMPSTAT))) + 
  geom_line() # no data for 2020 forward?


#numerical:

#WTFINL: Weights in the data set
join_drop %>% ggplot(mapping = aes(WTFINL)) + 
  geom_histogram(bins = 100, fill="blue") # fascinating, closest to smooth so far, probably not relevant except for control

#AGE:
join_drop %>% ggplot(mapping = aes(AGE)) + 
  geom_histogram(bins = 100, fill="blue") # ah, even more normalized, good to see.


#correlation

cor_ind <- join_drop %>% 
  dplyr::select(-c(monthyear, INDNAME)) %>% 
  dplyr::select(where(is.numeric))

#filter relevant items


corrplot(cor(cor_ind),
         order = "original",
         diag = FALSE,
         method = "number",
         addCoef.col = 'black',
         type = "upper",
         tl.srt = 45,
         tl.col = "black")  # Some interesting correlations! would be worth investigating further.


#BOXPLOTS:
join_drop%>% 
  ggplot(aes(x = factor(YEAR), y = FAMINC)) +
  geom_boxplot() +
  theme_bw()

join_drop%>% 
  ggplot(aes(x = factor(YEAR), y = EMPSTAT)) +
  geom_boxplot() +
  theme_bw()

join_drop%>% 
  ggplot(aes(x = factor(YEAR), y = WKSTAT)) +
  geom_boxplot() +
  theme_bw()
#professors step by step guidance: none so far





#change floor base, at industry level: 
#This regression seems to want one observation per month, so we’d need to create a year-month variable, use 
join_drop %>% group_by(YEAR, MONTH, monthyear) %>% summarize(RetailEmployment = sum(INDNAME == 'Retail Trade'))
# the above is just a starting point for how we could do this. 
#His notes say this from the spec:
"If you want an analysis to be at the industry-month level, you should make your data be at that level too! 
Use mutate(yearmo = year*100 + month) to create a year-month variable, and then use group_by(yearmo, indname) %>% summarize() 
to collapse data to the yearmo/indname level"
















