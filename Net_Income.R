
library(tidyverse)
library(haven)
library(dplyr)

### Load Data


#################### General ########################
#####################################################

SEC1 <- read_dta('SEC1.dta')%>%
  distinct(clust, nh, pid, .keep_all=TRUE)
SEC1$new_HHID <- paste(SEC1$clust, SEC1$nh, sep = "_" )
SEC1$new_PID <- paste(SEC1$clust, SEC1$nh, SEC1$pid, sep = "_" )
new_HHIDs_list <- distinct(SEC1, new_HHID, .keep_all=FALSE)








#################### Profit ########################
####################################################

AGG1 <- read_dta("AGG1.dta")%>%
  transmute(new_HHID = paste(AGG1$clust, AGG1$nh, sep = "_" ), totemp)
  

AGG2 <- read_dta("AGG2.dta")%>%
  transmute(new_HHID = paste(AGG2$clust, AGG2$nh, sep = "_" ), sum_aggr_inc = rowSums(AGG2[,c('agri1c', 'agri2c')]))


# SUBAGG7 SEFARM = Farm self employment income
SUBAGG7  <- read_dta("SUBAGG7.dta") %>%
  transmute(new_HHID = paste(SUBAGG7$clust, SUBAGG7$nh, sep = "_" ), self_emp_inc = sefarm)

# SUBAGG9 WATINC = Income from water sold
SUBAGG9  <- read_dta("SUBAGG9.dta")%>%
  transmute(new_HHID = paste(SUBAGG9$clust, SUBAGG9$nh, sep = "_" ), inc_water_sold = watinc)


# LNDINC1 = Income from renting out land
# LNDINC2 = Income from sharecropping
SUBAGG10  <- read_dta("SUBAGG10.dta") %>%
  transmute(new_HHID = paste(SUBAGG10$clust, SUBAGG10$nh, sep = "_" ), sum_inc_land = rowSums(SUBAGG10[,c('lndinc1', 'lndinc2')]))

 
# LIVINC = Income from renting out livestock
SUBAGG11  <- read_dta("SUBAGG11.dta") %>%
  transmute(new_HHID = paste(SUBAGG11$clust, SUBAGG11$nh, sep = "_" ), inc_rent_livestock = livinc)

# EQINC = Income from renting out agricultural equipment
SUBAGG12 <- read_dta('SUBAGG12.dta')%>%
transmute(new_HHID = paste(SUBAGG12$clust, SUBAGG12$nh, sep = "_" ), inc_rent_equip = eqinc)

#CRPINC1 = Revenue from sale of cash crops – main outlet
#CRPINC2 = Revenue from sale of cash crops – other outlet
SUBAGG13 <- read_dta('SUBAGG13.dta')%>%
transmute(new_HHID = paste(SUBAGG13$clust, SUBAGG13$nh, sep = "_" ), sum_cash_crops = rowSums(SUBAGG13[,c('crpinc1', 'crpinc2')]))

#SUBAGG14 ROOTINC = Revenue from sale of roots/fruit/vegetables
SUBAGG14 <- read_dta('SUBAGG14.dta')%>%
transmute(new_HHID = paste(SUBAGG14$clust, SUBAGG14$nh, sep = "_" ), inc_food_sales = rootinc)

#SUBAGG15 INCOTHAG = Revenue from other agricultural source
SUBAGG15 <- read_dta('SUBAGG15.dta')%>%
transmute(new_HHID = paste(SUBAGG15$clust, SUBAGG15$nh, sep = "_" ), inc_other_agr = incothag)

#SUBAGG16 TRCRPINC = Revenue from the sale of transformed crop products
SUBAGG16 <- read_dta('SUBAGG16.dta')%>%
transmute(new_HHID = paste(SUBAGG16$clust, SUBAGG16$nh, sep = "_" ), inc_transformed_crop = trcrpinc)


income <- new_HHIDs_list%>%
  full_join(AGG1, by = 'new_HHID')%>%
  full_join(AGG2, by = 'new_HHID')%>%
  full_join(SUBAGG7, by = 'new_HHID')%>%
  full_join(SUBAGG9, by = 'new_HHID')%>% 
  full_join(SUBAGG10, by = 'new_HHID')%>%
  full_join(SUBAGG11, by = 'new_HHID')%>%
  full_join(SUBAGG12, by = 'new_HHID')%>%
  full_join(SUBAGG13, by = 'new_HHID')%>%
  full_join(SUBAGG14, by = 'new_HHID')%>%
  full_join(SUBAGG15, by = 'new_HHID')%>%
  full_join(SUBAGG16, by = 'new_HHID') 
  income[is.na(income)]=0






  














############## Expenditures #####################
#################################################

# farmland rent  
SUBAGG22 <- read_dta("SUBAGG22.dta")%>%
  transmute(new_HHID = paste(SUBAGG22$clust, SUBAGG22$nh, sep = "_" ), exp_land_rent= -1*expland)


#crop exp
SUBAGG23 <- read_dta("SUBAGG23.dta")%>%
  transmute(new_HHID = paste(SUBAGG23$clust, SUBAGG23$nh, sep = "_" ), exp_crops = -1*expcrop)
# %>%
  # SUBAGG23[is.na(expcrop)] <- 0

#livestock inputs
SUBAGG24 <- read_dta("SUBAGG24.dta") %>%
  transmute(new_HHID = paste(SUBAGG24$clust, SUBAGG24$nh, sep = "_" ), exp_livestock = -1* expliv)


#EXPFDPR1 = Labour costs on food processing
#EXPFDPR2 = Other costs on food processing
SUBAGG25<- read_dta("SUBAGG25.dta")%>%
  transmute(new_HHID = paste(SUBAGG25$clust, SUBAGG25$nh, sep = "_" ), process_cost = -1* rowSums(SUBAGG25[,c('expfdpr1', 'expfdpr2')]))

#SUBAGG31 DEPNEQ = Depreciation of farming equipment 
SUBAGG31 <- read_dta("SUBAGG31.dta")%>%
  transmute(new_HHID = paste(SUBAGG31$clust, SUBAGG31$nh, sep = "_" ), equip_depreciation = -1*depneq)




expenses <- new_HHIDs_list%>%
  full_join(SUBAGG22, by = 'new_HHID')%>%
  full_join(SUBAGG23, by = 'new_HHID')%>%
  full_join(SUBAGG24, by = 'new_HHID')%>%
  full_join(SUBAGG25, by = 'new_HHID')%>% 
  full_join(SUBAGG31, by = 'new_HHID')
  expenses[is.na(expenses)]=0


  
  
  
  
############## NET INCOME FINAL JOIN AND TABLE #####################
#####################################################

net_join <- income%>%
  left_join(expenses, by = 'new_HHID')%>%
  mutate(sum_aggr_inc  = rowSums(net_join[,3:16]))

net_inc <- net_join %>%
  transmute(new_HHID, totemp, sum_aggr_inc)

  

  



