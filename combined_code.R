# feijiang_dtc_code.R
# Feifei Jiang
# Agriculture Land and Plot variables in data.pdf
# How much land that household has in acres? What crops are growing?

# install.packages("tidyverse")
# install.packages("haven")


library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(magrittr)



### ------ agriculture land ------ ###
agri_land <- read_dta('data/glss4_new/sec8a1.dta')
agri_land$new_hhid <- paste(agri_land$clust, agri_land$nh, sep="_") 
land <- select(agri_land, nh, clust, s8aq2, s8aq3, s8aq4, s8aq5)  
land$land_ropes_2_acres = ifelse(land$s8aq3==3, land$s8aq4 * 0.1, land$s8aq4 * 1)

# variables
hh_own_any_lands_12_months <- land$s8aq2 
plot_areas <- land$s8aq3        # unit of measure in acres, poles, ropes, other
land_owned <- land$s8aq4        # convert land to acres
was_land_bought_or_rented <- land$s8aq5


# visualize by plot area
ggplot(data = agri_land, mapping = aes (x = s8aq4, y = nh)) + 
  geom_point(na.rm = TRUE)

### ------ agriculture plot details ------ ###
agri_plot <- read_dta('data/glss4_new/sec8b.dta') 
agri_plot$new_hhid <- paste(agri_plot$clust, agri_plot$nh, sep="_") 
plot <- select(agri_plot, nh, clust, s8bq4a, s8bq4b, s8bq5, s8bq12a, s8bq12b)
plot$plot_ropes_2_acres = ifelse(plot$s8bq4b %in% c("3") , plot$s8bq4a * 0.1, plot$s8bq4a * 1)

# variables
land_size <- plot$s8bq4a          # convert land to acres
unit_of_measure <- plot$s8bq4b    # unit of measure in acres, poles, ropes, other
farm_owned <- plot$s8bq5          
crop1_growing <- plot$s8bq12a
crop2_growing <- plot$s8bq12b


### ----- merge agri land and plot by nh and clust. filter N/A ----- ###
agri_merge <- left_join(land, plot, by = "nh", "clust", all.y = TRUE) %>% 
  select(nh, clust.x, s8aq2, s8aq3, s8aq4, land_ropes_2_acres, s8aq5, s8aq3, s8aq4, s8bq4a, plot_ropes_2_acres,s8bq4b, s8bq5, s8bq12a, s8bq12b)
agri_merge$new_hhid <- paste(agri_merge$clust.x, agri_merge$nh, sep="_") 

# visualize land owned by household, legend in land size
ggplot(agri_merge, aes(x = s8aq4, y = nh)) + 
  geom_point(aes(colour = s8bq4a)) 


# AGRI1C = HH agricultural income (1) - corrected
agg2_agri_income <- read_dta('data/glss4_new/aggregates/agg2.dta')
# variables
agri_income_agri1c <- agg2_agri_income$agri1c
sd(agri_income)

##Lynna's code

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



##level_study -
##getting the average of the highest level of study completed and highest qualification
##attained per household

#highest level of study completed and highest qualification attained per individual - data file extraction
level_study <- ed_general %>%        
  select(clust, nh, pid,  s2aq2, s2aq3, new_PID, new_HHID )  %>%
  filter(!is.na(s2aq3) | !is.na(s2aq2))  %>%
  filter(s2aq3!= 1, s2aq3 != 1) %>%
  rename(highest_level = s2aq2,
         highest_qualification = s2aq3)

##count of how many individuals per household attained levels of study and qualification 
level_study_hh_count <- count(level_study, vars = new_HHID) %>% 
  rename(new_HHID = vars,
         hh_count = n)

##total of levels of study and qualification per household 
level_study_household <- level_study%>%  
  select(new_PID, new_HHID, highest_level, highest_qualification ) %>%
  group_by(new_HHID) %>%
  summarise(highest_level_totals = sum(highest_level),
            highest_qualification_totals = sum(highest_qualification))

##averages of levels of study and qualifications by dividing totals by household count 
level_study_household_avgs <- full_join(level_study_household, level_study_hh_count, by = 'new_HHID') %>% 
  mutate(highest_level_avgs =  highest_level_totals/hh_count) %>%
  mutate(highest_qualification_avgs =  highest_qualification_totals/hh_count)

##time going to school #####
##getting average time a household's individual spends going to school 

##time values extraction from data files 
time_spent_going_school <- ed_general%>%  
  select(clust, nh, pid,s2aq5a , s2aq5b,new_PID, new_HHID) %>%
  filter(s2aq5a != 99 | s2aq5b != 99) %>%
  mutate(ptimetotals = (s2aq5a * 60) + s2aq5b)## %>%

##household count of how many individuals in the house has records for time spent going to school 
time_spent_going_school_hh_count <- count(time_spent_going_school, vars = new_HHID) %>%
  rename(new_HHID = vars,
         hh_count = n)

##totals that a household spends going to school 
time_spent_going_school_household <- time_spent_going_school%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(new_PID, new_HHID, ptimetotals) %>%
  group_by(new_HHID) %>%
  summarise(household_time_totals = sum(ptimetotals))

##averages of time spent going to school by diving totals by household count 
time_spent_going_school_averages <- full_join(time_spent_going_school_household , time_spent_going_school_hh_count, by = 'new_HHID') %>% 
  mutate(hh_time_spent_going_to_school_avg =  household_time_totals/hh_count)



##Total educational expenses #####
##average education expense for individual per household 

##selecting all education expenses columns 
educ_expenses <- ed_general%>%
  select(clust, nh, pid,  s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

##changing null values to zero so not to mess up sums 
educ_expenses[is.na(educ_expenses)] <- 0

##sums of all education expenses into one total for each individual 
educ_expenses_totals <- educ_expenses %>%
  mutate(rowSums(educ_expenses))  %>%
  mutate(totals = rowSums(educ_expenses) - (nh + pid + clust)) %>%
  select(clust, nh, pid,  totals) %>%
  filter(totals > 0)                      ##filtering out individuals that dont have an educ expense as not to mess up household count later

educ_expenses_totals$new_HHID <- paste(educ_expenses_totals$clust, educ_expenses_totals$nh, sep = "_" )

##household count of how many individual with education expenses
educ_expenses_hh_count <- count(educ_expenses_totals, vars = new_HHID) %>%
  rename(new_HHID = vars,
         hh_count = n)

##totals that a household spends on education expenses for everyone who has education expenses
educ_expenses_totals_household <- educ_expenses_totals%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select(new_HHID, totals ) %>%
  group_by(new_HHID) %>%
  summarise(household_expenses_totals = sum(totals))

##averages of education expenses by diving totals by household count 
educ_expenses_household_avgs <- full_join(educ_expenses_totals_household  , educ_expenses_hh_count, by = 'new_HHID') %>% 
  mutate(hh_educ_expenses_avg =  household_expenses_totals /hh_count)


##############################################
##DUMMY AGRICULTURE PROFIT VARIABLE ##########
##############################################
AGG1 <- read_dta("AGG1.dta")
AGG1$new_HHID <- paste(AGG1$clust, AGG1$nh, sep = "_" )

AGG2 <- read_dta("AGG2.dta")
AGG2$new_HHID <- paste(AGG2$clust, AGG2$nh, sep = "_" )


##merge 

educ_variables <- left_join(time_spent_going_school_averages, level_study_household_avgs, by = "new_HHID") %>%
  left_join(educ_expenses_household_avgs, by = "new_HHID") %>%
  select("new_HHID",  "hh_time_spent_going_to_school_avg",  "highest_level_avgs", "highest_qualification_avgs", "hh_educ_expenses_avg")


profit_educ_df <- full_join(AGG2, educ_variables, by = "new_HHID")
profit_educ_lm <- lm(profit_educ_df, formula = agri2c ~ hh_time_spent_going_to_school_avg + highest_level_avgs +highest_qualification_avgs + hh_educ_expenses_avg)
summary(profit_educ_lm)


###Kristen code


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
                                                                , left_join (loan_credit
                                                                             , left_join(savings, assets, by = "new_HHID")
                                                                             , by = "new_HHID")
                                                                , by = "new_HHID")
                                                    , by = "new_HHID")
                                        , by = "new_HHID")
                            , by = "new_HHID")




inc_exp <- left_join(income_assets_, agg_expense, by = "new_HHID")



#### COMPARE INDIVIDUAL WORK WITH INDIVIDUAL INCOME - Can break into hourly income
main_occup <- transmute(SEC4B, new_PID, new_HHID, same_work = s4bq1, main_work =  s4bq4, 
                        main_pay = s4bq6a, pay_time_unit = s4bq6b, main_timework = s4bq7a, main_time_unit = s4bq7b, 
                        emp_status = s4bq8,rent_nosub = s4bq17a, rent_nosub_unit = s4bq17b, transp_subsidy = s4bq18, 
                        val_trans_subs = s4bq19a, val_trans_sub_unit = s4bq19b) %>%
  
  filter(main_work == 111 | main_work == 112 | main_work == "6-0" | main_work =="6-1" | main_work == "6-2")





##### reg_totpay

reg_totpay <- lm(totemp ~ savings_val + sum_aggr_inc + assets_curr_val +loan_amt +loan_paid, data = income_assets_)

test_reg <- lm(totemp ~ savings_val, data = income_assets_)

hist(rstandard(reg_totpay), # normal distribution of errors?
     xlab = "Standardized residuals")

plot(fitted(reg_totpay), resid(reg_totpay),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))





##### reg_main_occ
reg_exp_totemp <- lm(totemp ~ expland + expcrop + expliv, data = inc_exp)

hist(rstandard(reg_exp_totemp), # normal distribution of errors?
     xlab = "Standardized residuals")

plot(fitted(reg_exp_totemp), resid(reg_exp_totemp),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))





##### reg_agginc
reg_agginc <- lm(sum_aggr_inc ~expland + expcrop + expliv, data = inc_exp) 

hist(rstandard(reg_agginc), # normal distribution of errors?
     xlab = "Standardized residuals")

plot(fitted(reg_agginc), resid(reg_agginc),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))





##### reg_agginc_loan
reg_agginc_loan <- lm(sum_aggr_inc ~ loan_amt + savings_val + assets_curr_val + expcrop + expliv, data = inc_exp) 

hist(rstandard(reg_agginc_loan), # normal distribution of errors?
     xlab = "Standardized residuals")

plot(fitted(reg_agginc_loan), resid(reg_agginc_loan),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))






##### reg_nonagginc_loan
reg_nonagginc_loan <- lm(sum_nonagg_inc ~ loan_amt + savings_val + assets_curr_val, data = inc_exp) 

hist(rstandard(reg_nonagginc_loan), # normal distribution of errors?
     xlab = "Standardized residuals")

plot(fitted(reg_nonagginc_loan), resid(reg_nonagginc_loan),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))





