
library(tidyverse)
library(haven)




###### DATA GATHER ######
#########################
    SEC1 <- read_dta("sec1.dta")
      SEC1$new_PID <- paste(SEC1$clust, SEC1$nh, SEC1$pid, sep = "_" )
      SEC1$new_HHID <- paste(SEC1$clust, SEC1$nh, sep = "_" )
  
## Employment ##
    # screening questions and list of occupations (12 months)
    SEC4A <- read_dta("sec4a.dta") 
        SEC4A$new_PID <- paste(SEC4A$clust, SEC4A$nh, SEC4A$pid, sep = "_")
        SEC4A$new_HHID <- paste(SEC4A$clust, SEC4A$nh, sep = "_" )
        
    # characteristics of main occupation
    SEC4B <- read_dta("sec4b.dta")
        SEC4B$new_PID <- paste(SEC4B$clust, SEC4B$nh, SEC4B$pid, sep = "_" )
        SEC4B$new_HHID <- paste(SEC4B$clust, SEC4B$nh, sep = "_" )
    
    # emp history
    SEC4H <- read_dta("sec4h.dta")
        SEC4H$new_PID <- paste(SEC4H$clust, SEC4H$nh, SEC4H$pid, sep = "_" )

## credit/loan
    SEC12A1 <- read_dta("sec12a1.dta")
        SEC12A1$new_HHID <- paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )
    
    
    SEC12A2 <- read_dta("sec12a2.dta")
        SEC12A2$new_HHID <- paste(SEC12A2$clust, SEC12A2$nh, sep = "_" )
    
    SEC12B <- read_dta("sec12b.dta")
        SEC12B$new_HHID <- paste(SEC12B$clust, SEC12B$nh, sep = "_" )
    
    SEC12C <- read_dta("sec12c.dta")
        SEC12C$new_HHID <- paste(SEC12C$clust, SEC12C$nh, sep = "_" )
        SEC12C$new_acctID <- paste(SEC12C$clust, SEC12C$nh, SEC12C$s12cq3, sep = "_" )

## Housing ##
    SEC7 <- read_dta("sec7.dta")
        SEC7$new_HHID <-paste(SEC7$clust, SEC7$nh, sep = "_" )
        SEC12A1$new_HHID <-  paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )
      
## Aggregates ##
      AGG2 <- read_dta("AGG2.dta")
        AGG2$new_HHID <-paste(AGG2$clust, AGG2$nh, sep = "_" )
      AGG3 <- read_dta("AGG3.dta")
        AGG3$new_HHID <-paste(AGG3$clust, AGG3$nh, sep = "_" )
        
      #remittances, from family
      AGG5<- read_dta("AGG5.dta")
        AGG5$new_HHID <-paste(AGG5$clust, AGG5$nh, sep = "_" )
      AGG8 <- read_dta("AGG8.dta")
        AGG8$new_HHID <-paste(AGG8$clust, AGG8$nh, sep = "_" )
      #remittances, to family
      AGG12<- read_dta("AGG12.dta")
        AGG12$new_HHID <-paste(AGG12$clust, AGG12$nh, sep = "_" )
        
        
    
###### JOINS ######
###################

  credit_assets <- left_join(SEC7, 
                           left_join(SEC12A1
                                     , left_join(SEC12A2
                                                 , left_join(SEC12B, SEC12C
                                                             , by = "new_HHID")
                                                 , by = "new_HHID")
                                     , by = "new_HHID")
                           , by = "new_HHID") 
          

  #credit <- transmute(SEC12A2, loancd, s12aq4, s12aq5, s12aq6, s12aq7, s12aq8, s12aq9s12aq10, s12aq11a, s12aq11b, s12aq12, )
  
  emp_screening <- transmute(SEC4A, clust, nh, pid, s1q23, s4aq0_pr, s4aq1, s4aq2, s4aq3, s4aq5, s4aq6, s4aq7, s4aq9, s4aq10a, s4aq10b, s4aq10c, s4aq11)  
  
  loan_credit <- transmute(SEC12A1, new_HHID, owe_money = s12aq1, loan_paid = s12aq2)
  loans <- transmute(SEC12A2, new_HHID, loan_source = s12aq5, load_amt = s12aq6, load_purpose =  s12aq7, loan_guarantee = s12aq8,
                     loan_repaid = s12aq9, load_refused = s12aq10, loan_why_refuse = s12aq12)
  savings <- transmute(SEC12C, new_HHID, new_acctID, savingcd, savings_val = s12cq4, savings_added = s12cq5, savings_withdrawn = s12cq6)
  
  assets <- transmute(SEC12B, hassetcd, own_item_bool = s12bq1, s12bq3a, s12bq3b, s12bq3c, s12bq4a, s12bq4b, s12bq4c)
  

  
  #credit <- transmute(SEC12A2, loancd, s12aq4, s12aq5, s12aq6, s12aq7, s12aq8, s12aq9s12aq10, s12aq11a, s12aq11b, s12aq12, )
  main_occup <- transmute(SEC4B, new_PID, new_HHID, same_work = s4bq1, main_work =  s4bq4, 
                          main_pay = s4bq6a, pay_time_unit = s4bq6b, main_timework = s4bq7a, main_time_unit = s4bq7b, 
                          emp_status = s4bq8,rent_nosub = s4bq17a, rent_nosub_unit = s4bq17b, transp_subsidy = s4bq18, 
                          val_trans_subs = s4bq19a, val_trans_sub_unit = s4bq19b)
  
  emp_history <- transmute(SEC4H, new_PID, main_activ = s4hq1, years_ago = s4hq2, main_occ_prev = s4hq5, 
                           prev_industry = s4hq6, working_for = s4hq8, yrs_main_occ_prev = s4hq9)
  commute <- SEC4B %>%
    transmute(SEC4B,new_PID, new_HHID, work_place = s4bq22, work_distance = s4bq23, work_often = s4bq24a, commute_time = s4bq24b)
  
  #select(SEC1, clust, nh, pid, sex, rel, agem, mar, s1q7, s1q8, s1q10, s1q11, s1q13, s1q14, s1q16, s1q17, s1q18, s1q20, s1q21, s1q22, s1q23)
  
  agg_inc <- mutate(AGG2, new_HHID = paste(AGG2$clust, AGG2$nh))
  tot_inc <- 
  
    
    
    
    
    
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
