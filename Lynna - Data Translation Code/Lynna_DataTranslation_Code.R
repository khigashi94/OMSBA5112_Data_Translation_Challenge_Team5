#Lynna Tran

# Lynna: Education in Data.pdf. 
# Page 1. Household size and education attainment per person, 
# per household. How much education that household has?

library(tidyverse)
library(haven)
library(dplyr)


#SEC 1 - Household roster
#education of father, education of mother

household <- read_dta('sec1.dta') 
household$new_PID <- paste(household$clust, household$nh, household$pid, sep = "_" )
household$new_HHID <- paste(household$clust, household$nh, sep = "_" )

father_ed <- household %>%          #levels of education of individuals father
  select(clust, nh, pid,  s1q15, new_PID,new_HHID )  %>%
  filter(!is.na(s1q15)) %>%
  filter(s1q15 != 98, s1q15 != 1)  #filtering out NA and None values 


mother_ed <- household %>%
  select(clust, nh, pid,  s1q19, new_PID, new_HHID )  %>%
  filter(!is.na(s1q19)) %>%
  filter(s1q19 != 98, s1q19 != 1) #filtering out NA and None values 
  

##gathering father and mother education into one dataframe
parents_ed <- full_join(father_ed, mother_ed, by = c("new_PID", 'new_HHID') ) %>%
  select(new_PID, new_HHID, s1q15, s1q19)
  
  

##SEC2A - General Education
ed_general <- read_dta('sec2a.dta')
ed_general$new_PID <- paste(ed_general$clust, ed_general$nh, ed_general$pid, sep = "_" )
ed_general$new_HHID <- paste(ed_general$clust, ed_general$nh, sep = "_" )



##level_study -
##getting the average of the highest level of study completed and highest qualification
##attained per household

#highest level of study completed and highest qualification attained per individual - data file extraction
level_study <- ed_general %>%        
  select(clust, nh, pid,  s2aq2, s2aq3, new_PID, new_HHID )  %>%
  filter(!is.na(s2aq3) | !is.na(s2aq2))  %>%
  filter(s2aq3!= 1, s2aq3 != 1) %>%
  rename(highest_level = s2aq2,
         highest_qualification = s2aq3)
 
##count of how many individuals per household attained levels of study and qualification 
level_study_hh_count <- count(level_study, vars = new_HHID) %>% 
  rename(new_HHID = vars,
         hh_count = n)

##total of levels of study and qualification per household 
level_study_household <- level_study%>%  
  select(new_PID, new_HHID, highest_level, highest_qualification ) %>%
  group_by(new_HHID) %>%
  summarise(highest_level_totals = sum(highest_level),
            highest_qualification_totals = sum(highest_qualification))

##averages of levels of study and qualifications by dividing totals by household count 
level_study_household_avgs <- full_join(level_study_household, level_study_hh_count, by = 'new_HHID') %>% 
  mutate(highest_level_avgs =  highest_level_totals/hh_count) %>%
  mutate(highest_qualification_avgs =  highest_qualification_totals/hh_count)

##time going to school #####
##getting average time a household's individual spends going to school 

##time values extraction from data files 
time_spent_going_school <- ed_general%>%  
  select(clust, nh, pid,s2aq5a , s2aq5b,new_PID, new_HHID) %>%
  filter(s2aq5a != 99 | s2aq5b != 99) %>%
  mutate(ptimetotals = (s2aq5a * 60) + s2aq5b)## %>%

##household count of how many individuals in the house has records for time spent going to school 
time_spent_going_school_hh_count <- count(time_spent_going_school, vars = new_HHID) %>%
  rename(new_HHID = vars,
         hh_count = n)

##totals that a household spends going to school 
time_spent_going_school_household <- time_spent_going_school%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(new_PID, new_HHID, ptimetotals) %>%
  group_by(new_HHID) %>%
  summarise(household_time_totals = sum(ptimetotals))

##averages of time spent going to school by diving totals by household count 
time_spent_going_school_averages <- full_join(time_spent_going_school_household , time_spent_going_school_hh_count, by = 'new_HHID') %>% 
  mutate(hh_time_spent_going_to_school_avg =  household_time_totals/hh_count)



##Total educational expenses #####
##average education expense for individual per household 

##selecting all education expenses columns 
educ_expenses <- ed_general%>%
  select(clust, nh, pid,  s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

##changing null values to zero so not to mess up sums 
educ_expenses[is.na(educ_expenses)] <- 0

##sums of all education expenses into one total for each individual 
educ_expenses_totals <- educ_expenses %>%
  mutate(rowSums(educ_expenses))  %>%
  mutate(totals = rowSums(educ_expenses) - (nh + pid + clust)) %>%
  select(clust, nh, pid,  totals) %>%
  filter(totals > 0)                      ##filtering out individuals that dont have an educ expense as not to mess up household count later

educ_expenses_totals$new_HHID <- paste(educ_expenses_totals$clust, educ_expenses_totals$nh, sep = "_" )

##household count of how many individual with education expenses
educ_expenses_hh_count <- count(educ_expenses_totals, vars = new_HHID) %>%
  rename(new_HHID = vars,
         hh_count = n)

##totals that a household spends on education expenses for everyone who has education expenses
educ_expenses_totals_household <- educ_expenses_totals%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(new_HHID, totals ) %>%
  group_by(new_HHID) %>%
  summarise(household_expenses_totals = sum(totals))

##averages of education expenses by diving totals by household count 
educ_expenses_household_avgs <- full_join(educ_expenses_totals_household  , educ_expenses_hh_count, by = 'new_HHID') %>% 
  mutate(hh_educ_expenses_avg =  household_expenses_totals /hh_count)


##############################################
##DUMMY AGRICULTURE PROFIT VARIABLE ##########
##############################################
AGG1 <- read_dta("AGG1.dta")
AGG1$new_HHID <- paste(AGG1$clust, AGG1$nh, sep = "_" )

AGG2 <- read_dta("AGG2.dta")
AGG2$new_HHID <- paste(AGG2$clust, AGG2$nh, sep = "_" )


##merge 

educ_variables <- left_join(time_spent_going_school_averages, level_study_household_avgs, by = "new_HHID") %>%
    left_join(educ_expenses_household_avgs, by = "new_HHID") %>%
    select("new_HHID",  "hh_time_spent_going_to_school_avg",  "highest_level_avgs", "highest_qualification_avgs", "hh_educ_expenses_avg")


profit_educ_df <- full_join(AGG2, educ_variables, by = "new_HHID")
profit_educ_lm <- lm(profit_educ_df, formula = agri2c ~ hh_time_spent_going_to_school_avg + highest_level_avgs +highest_qualification_avgs + hh_educ_expenses_avg)
summary(profit_educ_lm)


