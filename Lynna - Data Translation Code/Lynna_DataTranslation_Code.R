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



##level_study -- #highest level of study completed and highest qualification attained
level_study <- ed_general %>%         #highest level of study completed and highest qualification attained
  select(clust, nh, pid,  s2aq2, s2aq3, new_PID, new_HHID )  %>%
  filter(!is.na(s2aq3) | !is.na(s2aq2))  %>%
  filter(s2aq3!= 1, s2aq3 != 1)


##time going to school 


time_spent_going_school <- ed_general%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(clust, nh, pid,s2aq5a , s2aq5b,new_PID, new_HHID ) %>%
  filter(s2aq5a != 99 | s2aq5b != 99) %>%
  mutate(timetotals = (s2aq5a * 60) + s2aq5b)


##Total educational expenses
educ_expenses <- ed_general%>%
  select(clust, nh, pid, s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

educ_expenses[is.na(educ_expenses)] <- 0

educ_expenses_totals <- educ_expenses %>%
  mutate(rowSums(educ_expenses))  %>%
  mutate(totals = rowSums(educ_expenses) - (nh + pid + clust)) %>%
  select(clust, nh, pid,  totals)

educ_expenses_totals$new_PID <- paste(educ_expenses_totals$clust, educ_expenses_totals$nh, educ_expenses_totals$pid, sep = "_" )
educ_expenses_totals$new_HHID <- paste(educ_expenses_totals$clust, educ_expenses_totals$nh, sep = "_" )

educ_expenses_totals_by_household <- educ_expenses_totals%>%
  group_by(new_HHID) %>%
  summarise(household_totals = sum(totals))

##############################################
##DUMMY AGRICULTURE PROFIT VARIABLE ##########
##############################################
AGG1 <- read_dta("AGG1.dta")
AGG1$new_HHID <- paste(AGG1$clust, AGG1$nh, sep = "_" )

##comparing profit to parents_ed
profit_parents_df<- left_join(AGG1, parents_ed, by = "new_HHID" )
profit_parents_lm <- lm(profit_parents_df, formula =  totemp ~ s1q15 + s1q19)
summary(profit_parents_lm)

##comparing profit to level of study
profit_levelstudy_df<- left_join(AGG1, level_study, by = "new_HHID" )
profit_levelstudy_lm <- lm(profit_levelstudy_df, formula =  totemp ~ s2aq2 + s2aq3)
summary(profit_levelstudy_lm)


##comparing profit to time spent going to school 
profit_timetoschool_df<- left_join(AGG1, time_spent_going_school, by = "new_HHID" )
profit_timetoschool_lm <- lm(profit_timetoschool_df, formula =  totemp ~ timetotals)
summary(profit_timetoschool_lm)



##comparing profit to education expenses 
profit_eduexpenses_df<- left_join(AGG1, educ_expenses_totals_by_household, by = "new_HHID" )
profit_eduexpenses__lm <- lm(profit_eduexpenses_df, formula =  totemp ~ household_totals)
summary(profit_eduexpenses__lm)



