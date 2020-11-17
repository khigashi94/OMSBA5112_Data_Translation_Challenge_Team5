#Lynna Tran

# Lynna: Education in Data.pdf. 
# Page 1. Household size and education attainment per person, 
# per household. How much education that household has?

library(tidyverse)
library(haven)
library(dplyr)

household <- read_dta('sec1.dta')
#education of father, education of mother

father_ed <- household %>%          #levels of education of individuals father
  select(clust, nh, pid,  s1q15)  %>%
  filter(!is.na(s1q15))

father_ed$new_PID <- paste(father_ed$clust, father_ed$nh, father_ed$pid, sep = "_" )


mother_ed <- household %>%
  select(clust, nh, pid,  s1q19)  %>%
  filter(!is.na(s1q19))

mother_ed$new_PID <- paste(mother_ed$clust, mother_ed$nh, mother_ed$pid, sep = "_" )


ed_general <- read_dta('sec2a.dta')


##level_study -- #highest level of study completed and highest qualification attained
level_study <- ed_general %>%         #highest level of study completed and highest qualification attained
  select(clust,nh, pid,  s2aq2, s2aq3)  %>%
  filter(!is.na(s2aq3))

level_study$new_PID <- paste(level_study$clust, level_study$nh, level_study$pid, sep = "_" )


##time going to school 
time_spent_going_school <- ed_general%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(clust, nh, pid,  s2aq5a, s2aq5b)

time_spent_going_school_combined <- ed_general%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(clust, nh, pid,  s2aq5a , s2aq5b) %>%
  filter(s2aq5a != 99 | s2aq5b != 99) %>%
  mutate(timetotals = (s2aq5a * 60) + s2aq5b)


time_spent_going_school_combined$new_PID <- paste(time_spent_going_school_combined$clust, time_spent_going_school_combined$nh, time_spent_going_school_combined$pid, sep = "_" )

##Total educational expenses
educ_expenses <- ed_general%>%
  select(clust, nh, pid,  s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

educ_expenses[is.na(educ_expenses)] <- 0

educ_expenses_totals <- educ_expenses %>%
  mutate(rowSums(educ_expenses))  %>%
  mutate(totals = rowSums(educ_expenses) - (nh + pid + clust)) %>%
  select(clust, nh, pid, totals)

educ_expenses_totals$new_PID <- paste(educ_expenses_totals$clust, educ_expenses_totals$nh, educ_expenses_totals$pid, sep = "_" )


scholarship <- ed_general%>%
  filter(s2aq15 == 1) %>%
  select(clust, nh, pid,  s2aq15, s2aq16)

scholarship$new_PID <- paste(scholarship $clust, scholarship$nh, scholarship$pid, sep = "_" )


##Education: Literacy/Apprenticeship
ed_literacy <- read_dta('sec2c.dta')

#can read in either Ghanaian or English language
reading_literacy <- ed_literacy %>%
  filter(s2cq2 != 1 | s2cq1 == 1) %>%
  select(clust, nh, pid, s2cq1, s2cq2 )

reading_literacy$new_PID <- paste(reading_literacy$clust, reading_literacy$nh, reading_literacy$pid, sep = "_" )


#can read in English
read_english <- ed_literacy %>%
  filter(s2cq1 == 1) %>%
  select(clust, nh, pid, s2cq1 )

read_english$new_PID <- paste(read_english$clust, read_english$nh, read_english$pid, sep = "_" )


#can read in Ghana
read_ghana <- ed_literacy %>%
  filter(s2cq2 != 1 ) %>%
  select(clust, nh, pid, s2cq2 )

read_ghana$new_PID <- paste(read_ghana$clust, read_ghana$nh, read_ghana$pid, sep = "_" )


#can write in either Ghanaian or English language 
writing_literacy <- ed_literacy %>%
  filter(s2cq3 == 1 | s2cq4 != 1) %>%
  select(clust, nh, pid, s2cq3, s2cq4)

writing_literacy$new_PID <- paste(writing_literacy$clust, writing_literacy$nh, writing_literacy$pid, sep = "_" )

#can write in English language 
write_english<- ed_literacy %>%
  filter(s2cq3 == 1) %>%
  select(clust, nh, pid, s2cq3 )

write_english$new_PID <- paste(write_english$clust, write_english$nh, write_english$pid, sep = "_" )


##can write in Ghana language 
write_ghana <- ed_literacy %>%
  filter(s2cq4 != 1) %>%
  select(clust, nh, pid, s2cq4 )


write_ghana$new_PID <- paste(write_ghana$clust, write_ghana$nh, write_ghana$pid, sep = "_" )


