
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
      AGG1 <- read_dta("AGG1.dta")
        AGG1$new_HHID <- paste(AGG1$clust, AGG1$nh, sep = "_" )
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
      
      # farmland rent  
      SUBAGG22 <- read_dta("SUBAGG22.dta")%>%
        transmute(new_HHID = paste(SUBAGG22$clust, SUBAGG22$nh, sep = "_" ), expland)
      #crop exp
      SUBAGG23 <- read_dta("SUBAGG23.dta") %>%
      transmute(new_HHID = paste(SUBAGG23$clust, SUBAGG23$nh, sep = "_" ), expcrop)

      #livestock inputs
      SUBAGG24 <- read_dta("SUBAGG24.dta") %>%
        transmute(new_HHID = paste(SUBAGG24$clust, SUBAGG24$nh, sep = "_" ), expliv)
        
      #######      #######      #######      #######      #######      #######      #######
      
        #credit <- transmute(SEC12A2, loancd, s12aq4, s12aq5, s12aq6, s12aq7, s12aq8, s12aq9s12aq10, s12aq11a, s12aq11b, s12aq12, )
        
        loan_credit <- transmute(SEC12A1, new_HHID, owe_money = s12aq1, loan_paid = s12aq2)
          # loan_owes <- loan_credit %>%
          #   filter(owe_money==1)
          # loan_pays <- loan_credit %>%
          #   filter(owe_money!=1)
        
      
        HH_tot_inc <- transmute(AGG1, new_HHID, totemp)
        
        agg_inc <- AGG2 %>%
          rowwise() %>%
          transmute(new_HHID, sum_aggr_inc = sum(c(agri1c, agri2c)))
        
        non_farm_inc <- AGG3 %>%
          rowwise() %>%
          transmute(new_HHID, sum_nonagg_inc = sum(c(nfsey1, nfsey2,nfsey3)))
        
        loans <- SEC12A2 %>%
          transmute(new_HHID,  loan_amt = s12aq6)
        
        savings <- SEC12C %>%
          transmute(new_HHID,savings_val = s12cq4, savings_added = s12cq5, savings_withdrawn = s12cq6)
                    #new_acctID, savingcd, savings_val = s12cq4, savings_added = s12cq5, savings_withdrawn = s12cq6)
        
        assets <- SEC12B %>%
          rowwise() %>%
          transmute(new_HHID, assets_paid = sum(c(s12bq3a, s12bq3b, s12bq3c)),
                    assets_curr_val = sum(c(s12bq4a, s12bq4b, s12bq4c)))
        
      
      
        agg_expense <- left_join(SUBAGG22, 
                                left_join(SUBAGG23, SUBAGG24, by = "new_HHID")
                                , by = "new_HHID")
                    
        
        
        
#### JOIN   HH_tot_inc, agg_inc, non_farm_inc, savings, assets FOR ACTUAL REGRESSION
income_assets_ <- left_join(HH_tot_inc
                              , left_join(agg_inc
                                          , left_join(non_farm_inc
                                                      , left_join(loans
                                                                  , left_join(savings, assets, by = "new_HHID")
                                                                  , by = "new_HHID")
                                                      , by = "new_HHID")
                                          , by = "new_HHID")
                              , by = "new_HHID")
                
            
  


#### COMPARE INDIVIDUAL WORK WITH INDIVIDUAL INCOME - Can break into hourly income
main_occup <- transmute(SEC4B, new_PID, new_HHID, same_work = s4bq1, main_work =  s4bq4, 
                          main_pay = s4bq6a, pay_time_unit = s4bq6b, main_timework = s4bq7a, main_time_unit = s4bq7b, 
                          emp_status = s4bq8,rent_nosub = s4bq17a, rent_nosub_unit = s4bq17b, transp_subsidy = s4bq18, 
                          val_trans_subs = s4bq19a, val_trans_sub_unit = s4bq19b) %>%
    filter(main_work == 111 | main_work == 112 | main_work == "6-0" | main_work =="6-1" | main_work == "6-2")
  
  
  

