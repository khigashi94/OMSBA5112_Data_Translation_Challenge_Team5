
library(tidyverse)
library(here)
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
        select(s4aq4, s4aq5, s4aq6, s4aq7)
      
      
      
    # characteristics of main occupation
    SEC4B <- read_dta("sec4b.dta")
        SEC4B$new_PID <- paste(SEC4B$clust, SEC4B$nh, SEC4B$pid, sep = "_" )
        SEC4B$new_HHID <- paste(SEC4B$clust, SEC4B$nh, sep = "_" )
        
        select(S4BQ1, S4BQ4,
               S4BQ6A,S4BQ6B, S4BQ7A, S4BQ7B, S4BQ8,
               S4BQ17A, S4BQ17B, S4BQ18, S4BQ19A, S4BQ19B,
               S4BQ22, S4BQ23, S4BQ24A, S4BQ24B, S4BQ25)
    # emp history
    SEC4H <- read_dta("sec4h.dta")
        SEC4H$new_PID <- paste(SEC4H$clust, SEC4H$nh, SEC4H$pid, sep = "_" )
        select(S4AQ2, S4AQ3, S4AQ5, S4AQ6, S4AQ7)

## credit/loan
  SEC12A1 <- read_dta("sec12a1.dta")
      SEC12A1$new_HHID <- paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )
  SEC12A2 <- read_dta("sec12a2.dta")
      SEC12A2$new_HHID <- paste(SEC12A2$clust, SEC12A2$nh, sep = "_" )
  SEC12B <- read_dta("sec12b.dta")
      SEC12B$new_HHID <- paste(SEC12B$clust, SEC12B$nh, sep = "_" )
  SEC12C <- read_dta("sec12c.dta")
      SEC12C$new_HHID <- paste(SEC12C$clust, SEC12C$nh, sep = "_" )
  
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
