
library(tidyverse)
library(here)
library(haven)





###### DATA GATHER ######
#########################
SEC1 <- read_dta("sec1.dta")
  SEC1$new_PID <- paste(SEC1$clust, SEC1$nh, SEC1$pid, sep = "_" )
  SEC1$new_HHID <- paste(SEC1$clust, SEC1$nh, sep = "_" )
  select(SEC1, clust, nh, pid, sex, rel, agem, mar, s1q7, s1q8, s1q10, s1q11, s1q13, s1q14, s1q16, s1q17, s1q18, s1q20, s1q21, s1q22, s1q23)

## Employment ##
    # screening questions and list of occupations (12 months)
    SEC4A <- read_dta("sec4a.dta") 
        SEC4A$new_PID <- paste(SEC4A$clust, SEC4A$nh, SEC4A$pid, sep = "_")
        SEC4A$new_HHID <- paste(SEC4A$clust, SEC4A$nh, sep = "_" )
        #select(s4aq4, s4aq5, s4aq6, s4aq7)
    emp_screening <- select(SEC4A, clust, nh, pid, s1q23, s4aq0_pr, s4aq1, s4aq2, s4aq3, s4aq5, s4aq6, s4aq7, s4aq9, s4aq10a, s4aq10b, s4aq10c, s4aq11)  
      
      
    # characteristics of main occupation
    SEC4B <- read_dta("sec4b.dta")%>%
        SEC4B$new_PID <- paste(SEC4B$clust, SEC4B$nh, SEC4B$pid, sep = "_" )
        SEC4B$new_HHID <- paste(SEC4B$clust, SEC4B$nh, sep = "_" )
    main_occup <- select(SEC4B, 
                          s4bq1, s4bq4,
                          s4bq6a ,s4bq6b, s4bq7a, s4bq7b, s4bq8,
                        s4bq17a, s4bq17b, s4bq18, s4bq19a, s4bq19b,
                        s4bq22, s4bq23, s4bq24a, s4bq24b, s4bq25)
    # emp history
    SEC4H <- read_dta("sec4h.dta")
        SEC4H$new_PID <- paste(SEC4H$clust, SEC4H$nh, SEC4H$pid, sep = "_" )
    emp_history <- select(SEC4H, s4hq1, s4hq2, s4hq3, s4hq4, s4hq5, s4hq6, s4hq7, s4hq8, s4hq9)
## credit/loan
  SEC12A1 <- read_dta("sec12a1.dta")
      SEC12A1$new_HHID <- paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )
  loan_credit <- select(SEC12A1, s12aq1, s12aq2)
  SEC12A2 <- read_dta("sec12a2.dta")
      SEC12A2$new_HHID <- paste(SEC12A2$clust, SEC12A2$nh, sep = "_" )
  credit <- select(SEC12A2, loancd, s12aq4, s12aq5, s12aq6, s12aq7, s12aq8, s12aq9s12aq10, s12aq11a, s12aq11b, s12aq12, )
  
  SEC12B <- read_dta("sec12b.dta")
      SEC12B$new_HHID <- paste(SEC12B$clust, SEC12B$nh, sep = "_" )
  assets <- select(SEC12B, hassetcd, s12bq1, s12bq2a, s12bq2b, s12bq2c, s12bq3a, s12bq3b, s12bq3cs12bq4a, s12bq4b, s12bq4c)
  SEC12C <- read_dta("sec12c.dta")
      SEC12C$new_HHID <- paste(SEC12C$clust, SEC12C$nh, sep = "_" )
  savings <- select(SEC12C, savingcd, s12cq3, s12cq4, s12cq5, s12cq6)
## Housing ##
  SEC7 <- read_dta("sec7.dta")
    SEC7$new_HHID <-paste(SEC7$clust, SEC7$nh, sep = "_" )
    SEC12A1$new_HHID <-  paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )
      

    
    
    
    
###### JOINS ######
###################
occupation <- left_join(SEC4B, SEC4H, by = "new_PID")

    
  credit_assets <- left_join(SEC7, 
                           left_join(SEC12A1
                                     , left_join(SEC12A2
                                                 , left_join(SEC12B, SEC12C
                                                             , by = "new_HHID")
                                                 , by = "new_HHID")
                                     , by = "new_HHID")
                           , by = "new_HHID")










## Respondent IDs ##
SEC6 <- read_dta("sec6.dta")







# Expenditures ##
# SEC9A11 <- read_dta("sec9a11.dta")
# SEC9A12 <- read_dta("sec9a12.dta")
# SEC9A2 <- read_dta("sec9a2.dta")
# SEC9B0 <- read_dta("sec9b0.dta")
# SEC9B <- read_dta("sec9b.dta")
# SEC9C <- read_dta("sec9c.dta")
# 
# SEC10A <- read_dta("sec10a.dta")
# SEC10B <- read_dta("sec10b.dta")
# SEC10C <- read_dta("sec10c.dta")
# SEC10D <- read_dta("sec10d.dta")
# SEC10E <- read_dta("sec10e.dta")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # alt jobs
# SEC4C <- read_dta("sec4c.dta")
# SEC4D <- read_dta("sec4d.dta") 
# SEC4E <- read_dta("sec4e.dta")
