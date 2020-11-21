library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(magrittr)

### ------ agriculture land ------ ###
### ------------------------------ ###

agri_land <- read_dta('data/glss4_new/sec8a1.dta') 
agri_land$new_HHID <- paste(agri_land$clust, agri_land$nh, sep="_") 
land <- select(agri_land, new_HHID, s8aq1, s8aq3, s8aq4, s8aq5) 
land$land_size_ropes_2_acres = ifelse(land$s8aq3==3, land$s8aq4 * 0.1, land$s8aq4 * 1)

# variables for conversion
plot_areas <- land$s8aq3        # unit of measure in acres, poles, ropes, other
land_owned <- land$s8aq4        # convert land to acres


### -------------------------------------- ###
### ------ agriculture plot details ------ ###
### -------------------------------------- ###
agri_plot <- read_dta('data/glss4_new/sec8b.dta') 
agri_plot$new_HHID <- paste(agri_plot$clust, agri_plot$nh, sep="_")
plot <- select(agri_plot, new_HHID, s8bq4a, s8bq4b, s8bq5) 
plot$plot_size_ropes_2_acres = ifelse(plot$s8bq4b %in% c("3") , plot$s8bq4a * 0.1, plot$s8bq4a * 1)

# variables for conversion
land_size <- plot$s8bq4a          # convert land to acres
unit_of_measure <- plot$s8bq4b    # unit of measure in acres, poles, ropes, other

# figure out what crops got planted the most
crop1 <- plot %>% 
  select(new_HHID, s8bq12a) %>% 
  group_by(s8bq12a)
crop1_count <-  count(crop1, vars = s8bq12a)

Cassava_crops_growing_one <- plot %>% 
  filter(s8bq12a == 18) %>% 
  select(new_HHID, s8bq12a) %>%
  rename(cassava = s8bq12a )

cassava_distinct <- distinct(Cassava_crops_growing_one, new_HHID,cassava  )

Maize_crops_growing_three <- plot %>% 
  filter(s8bq12a == 22) %>% 
  select(new_HHID, s8bq12a)  %>%
  rename(maize = s8bq12a )


crop1_combined <- full_join(cassava_distinct, Maize_crops_growing_three, by = 'new_HHID')


# figure out what crops2 got planted the most
crop2 <- plot %>%
  select(new_HHID, s8bq12b) %>%
  group_by(s8bq12b)
crop2_count <-  count(crop2, s8bq12b)


Unknown_crops_growing_0 <- plot %>%
  filter(s8bq12b == 0) %>%
  select(new_HHID, s8bq12b) %>%
  rename(unknown = s8bq12b )


Pawpaw_crops_growing_three <- plot %>%
  filter(s8bq12b == 36) %>%
  select(new_HHID, s8bq12b) %>%
  rename(pawpaw = s8bq12b )

crop2_combined <- full_join(Unknown_crops_growing_0 , Pawpaw_crops_growing_three, by = 'new_HHID')


crop_combined <- full_join(crop1_combined , crop2_combined, by = 'new_HHID')


crop_combined_distinct <- distinct(crop_combined,cassava, maize, unknown, pawpaw)

### ----------------------------------------------------------------- ###
### ------ merged agriculture land + plot + agriculture income ------ ###
### ----------------------------------------------------------------- ###
agri_merge <- left_join(land, plot, by = "new_HHID", all.y = TRUE) %>% 
  group_by(new_HHID) %>%
  rename(HH_own_any_land = s8aq1, 
         was_land_bought_or_rented = s8aq5,
         farm_owned_by_HH_member = s8bq5) %>% 
  summarize(HH_own_any_land, 
            was_land_bought_or_rented, 
            farm_owned_by_HH_member, 
            total_land_size_ropes_2_acres = sum(land_size_ropes_2_acres), 
            total_plot_size_ropes_2_acres = sum(plot_size_ropes_2_acres))

agri_merge_distinct <- distinct(agri_merge,HH_own_any_land , was_land_bought_or_rented, total_land_size_ropes_2_acres, total_plot_size_ropes_2_acres )

agri_vars <- left_join(agri_merge_distinct, crop_combined_distinct, by =  "new_HHID" )

colnames(agri_vars )

agri_vars_distinct <- distinct(agri_vars, HH_own_any_land , was_land_bought_or_rented, total_land_size_ropes_2_acres, total_plot_size_ropes_2_acres,
                               cassava, maize, unknown, pawpaw)




#Lynna Tran

# Lynna: Education in Data.pdf. 
# Page 1. Household size and education attainment per person, 
# per household. How much education that household has?


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
  mutate(ptimetotals = (s2aq5a * 60) + s2aq5b)

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
SUBAGG23 <- read_dta("SUBAGG23.dta")%>%
  transmute(new_HHID = paste(SUBAGG23$clust, SUBAGG23$nh, sep = "_" ), expcrop)

#livestock inputs
SUBAGG24 <- read_dta("SUBAGG24.dta")%>%
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

colnames(income_assets_)

income_assets_distinct <- distinct(income_assets_, new_HHID, sum_aggr_inc , sum_nonagg_inc,  totemp, loan_amt , owe_money,
                                   loan_paid , savings_val , savings_added , savings_withdrawn , assets_paid , assets_curr_val )

inc_exp <- left_join(income_assets_, agg_expense, by = "new_HHID")


Final_df <- left_join(educ_variables, income_assets_distinct, by = "new_HHID")%>%
  left_join(agri_vars_distinct , by = "new_HHID")

colnames(Final_df)

final_lm <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
               + highest_qualification_avgs + hh_educ_expenses_avg + totemp + loan_amt + owe_money
               + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid + assets_curr_val +
                 total_land_size_ropes_2_acres)


##+  HH_own_any_land + was_land_bought_or_rented 
##+ cassava + maize) ##+ unknown 
## + pawpaw
##   + total_land_size_ropes_2_acres + total_plot_size_ropes_2_acres)

summary(final_lm)

##these variables have the highest significant values 
#3 stars 

##highest_qualification_avgs 
highest_qualification_profit_lm <- lm(Final_df, formula = sum_aggr_inc ~ highest_qualification_avgs)

summary(highest_qualification_profit_lm)

hist(rstandard(highest_qualification_profit_lm ), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of Average
     Highest Qualification' )

plot(fitted(highest_qualification_profit_lm ), resid(highest_qualification_profit_lm ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Average Highest 
     Qualifications in Household ',
     abline(h = 0, col = "blue"))

##dummies /specs 

highest_qualification_profit_poly <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                        + I(highest_qualification_avgs^2) + highest_qualification_avgs + hh_educ_expenses_avg + totemp + loan_amt + owe_money
                                        + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid + assets_curr_val +
                                          total_land_size_ropes_2_acres)


ggplot(data = Final_df, aes(x = I(highest_qualification_avgs ^2), y = sum_aggr_inc)) +
  geom_smooth() +
  xlab("Average of Qualifications Squared") +
  ylab(" Profits by household") +
  ggtitle("Average Qualifications by 
          Profits per Household -- Polynomial")


##assets_paid  ## feifei

assets_paid_lm <- lm(Final_df, formula = sum_aggr_inc ~ assets_paid)


summary(assets_paid_lm)


hist(rstandard(assets_paid_lm ), # normal distribution of errors?
     xlab = "Standardized residuals",  main = 'Standardized Residuals of Assets Paid '
)


plot(fitted(assets_paid_lm ), resid(assets_paid_lm ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Assets Paid in Household ',
     abline(h = 0, col = "blue"))


# polynomial
assets_paid_poly <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs + hh_time_spent_going_to_school_avg
                       + highest_qualification_avgs + hh_educ_expenses_avg + totemp + loan_amt + owe_money
                       + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid  + assets_curr_val +
                         total_land_size_ropes_2_acres)


summary(assets_paid_poly )

# polynomial with dummy

assets_paid_poly_dummy <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs + hh_time_spent_going_to_school_avg
                             + highest_qualification_avgs + hh_educ_expenses_avg + totemp + loan_amt + owe_money
                             + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid + I(assets_paid ^2) + assets_curr_val +
                               total_land_size_ropes_2_acres)
summary(assets_paid_poly_dummy)


##2 stars 
##highest_level_avgs
##savings_withdrawn 

##hh_educ_expenses_avg
##owe_money
owe_money_lm <- lm(Final_df, formula = sum_aggr_inc ~ owe_money)
summary(owe_money_lm)
hist(rstandard(owe_money_lm ), # normal distribution of errors?
     xlab = "Standardized residuals", main = 'Standardized Residuals of Owed Money Paid ')


plot(fitted(owe_money_lm ), resid(owe_money_lm ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Owed Money in Household ',
     abline(h = 0, col = "blue"))
# polynomial
owe_money_poly <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs + hh_time_spent_going_to_school_avg
                     + highest_qualification_avgs + hh_educ_expenses_avg + totemp + loan_amt + owe_money
                     + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid  + assets_curr_val +
                       total_land_size_ropes_2_acres)
summary(owe_money_poly)
# polynomial with dummy
owe_money_poly_dummy <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs + hh_time_spent_going_to_school_avg
                           + highest_qualification_avgs + hh_educ_expenses_avg + totemp + loan_amt + owe_money + I(owe_money ^2)
                           + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid + assets_curr_val +
                             total_land_size_ropes_2_acres)
summary(owe_money_poly_dummy)

##1 stars 
##hh_educ_expenses_avg 

avg_educexpenses_profit_lm <- lm(Final_df, formula = sum_aggr_inc ~  hh_educ_expenses_avg)

summary(avg_educexpenses_profit_lm)

hist(rstandard( highest_educexpenses_profit_lm), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of Average
     Education Expenses' )

plot(fitted(avg_educexpenses_profit_lm), resid(avg_educexpenses_profit_lm),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Average Education Expenses
     in Household ',
     abline(h = 0, col = "blue"))

##dummies /specs 

avg_educexpenses_profit_poly <- lm(Final_df, formula = sum_aggr_inc ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                   + highest_qualification_avgs + hh_educ_expenses_avg + I(hh_educ_expenses_avg^2) +
                                     totemp + loan_amt + owe_money
                                   + loan_paid + savings_val + savings_added + savings_withdrawn + assets_paid + assets_curr_val +
                                     total_land_size_ropes_2_acres)


ggplot(data = Final_df, aes(x = I(hh_educ_expenses_avg^2), y = sum_aggr_inc)) +
  geom_smooth() +
  xlab("Average of Qualifications Squared") +
  ylab(" Profits by household") +
  ggtitle("Average Education Expenses per household
           -- Polynomial")