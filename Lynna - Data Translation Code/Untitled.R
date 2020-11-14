#Lynna Tran

# Lynna: Education in Data.pdf. 
# Page 1. Household size and education attainment per person, 
# per household. How much education that household has?

library(tidyverse)
library(haven)

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

level_study <- ed_general %>%
  select(nh, pid,  s2aq2, s2aq3)  %>%
  filter(!is.na(s2aq3))

ed_career <- read_dta('sec2b.dta')
#higher levels of education 

ed_literacy <- read_dta('sec2c.dta')
#can write
