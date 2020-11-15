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
  select(nh, pid,  s1q15)  %>%
  filter(!is.na(s1q15))

mother_ed <- household %>%
  select(nh, pid,  s1q19)  %>%
  filter(!is.na(s1q19))


ed_general <- read_dta('sec2a.dta')


##level_study -- #highest level of study completed and highest qualification attained
level_study <- ed_general %>%         #highest level of study completed and highest qualification attained
  select(nh, pid,  s2aq2, s2aq3)  %>%
  filter(!is.na(s2aq3))

##time going to school 
time_spent_going_school <- ed_general%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(nh, pid,  s2aq5a, s2aq5b)

time_spent_going_school_combined <- ed_general%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(nh, pid,  s2aq5a , s2aq5b) %>%
  mutate(s2aq5a * 60)

##Total educational expenses
educ_expenses <- ed_general%>%
  select(nh, pid,  s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

educ_expenses[is.na(educ_expenses)] <- 0

educ_expenses_totals <- educ_expenses %>%
  mutate(rowSums(educ_expenses))  %>%
  mutate(totals = rowSums(educ_expenses) - (nh + pid)) %>%
  select(nh, pid, totals)

##trying to get the sum of school expenses 
sum_education_expenses2 <- educ_expenses  %>% 
  filter(!is.na( s2aq6)) %>% 
  mutate(cumsum(educ_expenses))## na.rm = TRUE)#, dims = 1))
                

scholarship <- ed_general%>%
  select(nh, pid,  s2aq15, s2aq16)

#variable might not be needed 
ed_career <- read_dta('sec2b.dta')
#higher levels of education 

##Education: Literacy/Apprenticeship
ed_literacy <- read_dta('sec2c.dta')

#can read in either Ghanaian or English language
reading_literacy <- ed_literacy %>%
  select(nh, pid, s2cq1, s2cq2 )

#can read in English
read_english <- ed_literacy %>%
  select(nh, pid, s2cq1 )

#can read in Ghana
read_ghana <- ed_literacy %>%
  select(nh, pid, s2cq2 )

#can write in either Ghanaian or English language 
writing_literacy <- ed_literacy %>%
  select(nh, pid, s2cq3, s2cq4)

#can write in English language 
write_ghana <- ed_literacy %>%
  select(nh, pid, s2cq3 )

##can write in Ghana language 
write_ghana <- ed_literacy %>%
  select(nh, pid, s2cq4 )

