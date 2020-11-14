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
#levelofstudy, left school
#education expenses
#time spent getting to school ~~ road conditions 

##level_study -- #highest level of study completed and highest qualification attained
level_study <- ed_general %>%         #highest level of study completed and highest qualification attained
  select(nh, pid,  s2aq2, s2aq3)  %>%
  filter(!is.na(s2aq3))

time_spent_going_school <- ed_general%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(nh, pid,  s2aq5a, s2aq5b)  

educ_expenses <- ed_general%>%
  select(nh, pid,  s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

##trying to get the sum of school expenses 
sum_education_expenses2 <- educ_expenses  %>% 
  filter(!is.na( s2aq6)) %>% 
  mutate(cumsum(educ_expenses))## na.rm = TRUE)#, dims = 1))
                

scholarship <- ed_general%>%
  select(nh, pid,  s2aq15, s2aq16)

#variable might not be needed 
ed_career <- read_dta('sec2b.dta')
#higher levels of education 


ed_literacy <- read_dta('sec2c.dta')
#can write
