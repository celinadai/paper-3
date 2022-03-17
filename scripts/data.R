#### Preamble ####
# Purpose: Prepare and clean the data from U.S. 2021 GSS
# Author: Shengyi Dai, Suofeiya Guo 
# Data: 15 March 2022
# Contact: celina.dai@mail.utoronto.ca,sofia.guo@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2021 U.S. GSS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- haven::read_dta("1STA304/paper3/inputs/data/2021_stata/gss2021.dta")

####Prepare Data####
# Just keep some variables that may be of interest
names(raw_data)

reduced_data <- 
  raw_data %>% 
  select(sex, 
         age,
         degree,
         income,
         finrela,
         satfin,
         happy,
         health,
         )
  
  
rm(raw_data)

         
#### Recode Data ####

#Questions for sex:
# What is your sex when you born?
#Options are from the codebook "GSS 2021 Codebook R1b.pdf"
reduced_data <- 
  reduced_data %>% 
 
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
  ))%>%
  #Questions for degree:
  # What is your degree?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(degree = case_when(
    degree == 0 ~ "Less than high school",
    degree == 1 ~ "Hight school",
    degree == 2 ~ "associate/junior college",
    degree == 3 ~ "Bachelors",
    degree == 4 ~ "Graduate",
  ))%>%
  #Questions for income:
  # Which group is your family income in?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(income = case_when(
    income == 1 ~ "under $1000",
    income == 2 ~ "$1000-$2999",
    income == 3 ~ "$3000-$3999",
    income == 4 ~ "$4000-$4999",
    income == 5 ~ "$5000-$5999",
    income == 6 ~ "$6000-$6999",
    income == 7 ~ "$7000-$7999",
    income == 8 ~ "$9000-$9999",
    income == 9 ~ "$10000-$14999",
    income == 10 ~ "$15000-$19999",
    income == 11 ~ "$20000-$24999",
    income == 12 ~ "More than $25000",
  ))%>%
  #Questions for happy:
  #Are you happy everydat?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(happy = case_when(
    happy == 1 ~ "Very happy",
    happy == 2 ~ "Pretty happy",
    happy == 3 ~ "Not too happy",
  ))%>%
  #Questions for health:
  #What would you say your own health?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(health = case_when(
    health == 1 ~ "Excellent",
    health == 2 ~ "Good",
    health == 3 ~ "Fair",
    health == 4 ~ "Poor",
  ))%>%
  #Questions for finrela:
  #Compare with American families in general, are you family' income above average, average, or below average?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(finrela = case_when(
    finrela == 1 ~ "Far below average",
    finrela == 2 ~ "Below average",
    finrela == 3 ~ "Average",
    finrela == 4 ~ "Above average",
    finrela == 5 ~ "Far above average"
  ))%>%
  #Questions for satfin:
  #Are you satisfied with your income?
  #Options are from the codebook "GSS 2021 Codebook R1b.pdf"
  mutate(satfin = case_when(
    satfin == 1 ~ "Pretty well satisfied",
    satfin == 2 ~ "More of less satisfied",
    satfin == 3 ~ "Not satisfied at all",
  ))%>%
  filter(!is.na(sex), !is.na(degree), !is.na(age), !is.na(income), !is.na(happy),
         !is.na(health), !is.na(finrela), !is.na(satfin))%>%
  filter(age >= 18)%>%
  rename(income_vs_average = finrela,
         satisfication = satfin)
#### Some relationship between variables ####
#count of income
reduced_data%>%
  ggplot(aes(y=income))+geom_bar()
#count of degree
reduced_data%>%
  ggplot(aes(x=degree))+geom_bar()
#income vs. degree
reduced_data%>%
  ggplot(aes(y=income, fill=degree)) + geom_bar()+ theme_classic()
#finrela vs. satfin
reduced_data%>%
  ggplot(aes(x=income_vs_average, fill=satisfication)) + geom_bar(position="dodge")+ theme_classic()+
  labs(x = "xxx", y = "YYY", title = "XXX")

#### Save ####
write.csv(reduced_data, "1STA304/paper3/outputs/data/cleaned_gss.csv")
