# DATA TRANSLATION GROUP 5 
# Feifei Jiang, Kristen Higashi, Lynna Tran, Vish Diwan 



library(tidyverse)
library(haven)
library(dplyr)
library(magrittr)

### ------ load agriculture data and add primary key  ------ ###
### ------------------------------ ###

agri_land <- read_dta('data/glss4_new/sec8a1.dta') 
agri_land$new_HHID <- paste(agri_land$clust, agri_land$nh, sep="_") 


agri_plot <- read_dta('data/glss4_new/sec8b.dta') 
agri_plot$new_HHID <- paste(agri_plot$clust, agri_plot$nh, sep="_")


### ------ agriculture land ------ ###
### ------------------------------ ###

##land dataset, converting ropes to acres 
##land <- select(agri_land, new_HHID, s8aq1, s8aq3, s8aq4, s8aq5) 
##land$land_size_ropes_2_acres = ifelse(land$s8aq3==3, land$s8aq4 * 0.1, land$s8aq4 * 1)

land <- select(agri_land, new_HHID, s8aq1, s8aq3, s8aq4, s8aq5) %>%
  mutate(total_land_area = ifelse(agri_land$s8aq3==3, agri_land$s8aq4 * 0.1, agri_land$s8aq4 * 1)) %>%
  filter(!is.na(total_land_area))

###might not need this anymore 
# variables for conversion
#plot_areas <- land$s8aq3        # unit of measure in acres, poles, ropes, other
#land_owned <- land$s8aq4        # convert land to acres


### -------------------------------------- ###
### ------ agriculture plot details ------ ###
### -------------------------------------- ###

# plot <- select(agri_plot, new_HHID, s8bq4a, s8bq4b, s8bq5) 
# plot$plot_size_ropes_2_acres = ifelse(plot$s8bq4b %in% c("3") , plot$s8bq4a * 0.1, plot$s8bq4a * 1)


plot <- select(agri_plot, new_HHID, s8bq4a, s8bq4b, s8bq5)  %>%
  mutate(plot_area = ifelse(agri_plot$s8bq4b %in% c("3") , agri_plot$s8bq4a * 0.1, agri_plot$s8bq4a * 1)) %>%
  group_by(new_HHID) %>%
  mutate(hh_plot_area = sum(plot_area)) %>%
  distinct(hh_plot_area, new_HHID)

# # variables for conversion
# land_size <- plot$s8bq4a          # convert land to acres
# unit_of_measure <- plot$s8bq4b    # unit of measure in acres, poles, ropes, other

# figure out what crops got planted the most

crop1 <- agri_plot %>% 
  select(new_HHID, s8bq12a) %>%
  count(name = 'count_of_crop', vars = s8bq12a) %>%
  arrange(desc(count_of_crop))
  

#top primary crop
##crop 18 cassava and 22 maize, are most popular (0 was top 2 but it was an unknown variable so it doesnt matter), respectively 


Cassava_crops <- agri_plot %>% 
  filter(s8bq12a == 18) %>% 
  select(new_HHID, s8bq12a) %>%
  mutate(cassava = (s8bq12a == 18))  %>% 
  filter(cassava == TRUE) %>%
  distinct(cassava,new_HHID)


Maize_crops <- agri_plot %>% 
  filter(s8bq12a == 22) %>% 
  select(new_HHID, s8bq12a)  %>%
  mutate(maize = (s8bq12a == 22))  %>% 
  filter(maize == TRUE) %>%
  distinct(maize,new_HHID)



# figure out what crops2 got planted the most
crop2 <- agri_plot %>%
  select(new_HHID, s8bq12b) %>%
  count(name = 'count_of_crop', vars = s8bq12b) %>%
  arrange(desc(count_of_crop))


 Cassava_minor_crop<- agri_plot %>%
   filter(s8bq12b == 18) %>%
   select(new_HHID, s8bq12b) %>%
   mutate(minor_cassava = (s8bq12b == 18))  %>% 
   filter(minor_cassava == TRUE) %>%
   distinct(minor_cassava,new_HHID)
   

 Plantain_minor_crop<- agri_plot %>%
   filter(s8bq12b == 06) %>%
   select(new_HHID, s8bq12b) %>%
   mutate(minor_Plantain = (s8bq12b == 06))  %>% 
   filter(minor_Plantain == TRUE) %>%
   distinct(minor_Plantain,new_HHID)




crop_combined <- full_join(Cassava_crops , Maize_crops, by = 'new_HHID') %>%
  full_join(Cassava_minor_crop ,by = 'new_HHID') %>%
  full_join(Plantain_minor_crop ,by = 'new_HHID')




### ----------------------------------------------------------------- ###
### ------ merged agriculture land + plot + agriculture income ------ ###
### ----------------------------------------------------------------- ###
# agri_merge <- left_join(land, plot, by = "new_HHID", all.y = TRUE) %>% 
#   rename(HH_own_any_land = s8aq1, 
#          was_land_bought_or_rented = s8aq5,
#          farm_owned_by_HH_member = s8bq5) %>% 
#   summarize(HH_own_any_land, 
#             was_land_bought_or_rented, 
#             farm_owned_by_HH_member, 
#             total_land_size_ropes_2_acres = sum(land_size_ropes_2_acres), 
#             total_plot_size_ropes_2_acres = sum(plot_size_ropes_2_acres))

agri_merge <- full_join(land, plot, by = "new_HHID", all.y = TRUE) %>%
  full_join(crop_combined, by = "new_HHID", all.y = TRUE) %>%
  select(new_HHID,"total_land_area", "hh_plot_area", 
         "cassava", "maize", "minor_cassava","minor_Plantain"  )

agri_merge[is.na(agri_merge)]=0

### ------ load household roster data and add primary key  ------ ###
### ------------------------------ ###

#SEC 1 - Household roster
#education of father, education of mother


setwd("OMSBA5112_Data_Translation_Challenge_Team5/Data/glss4_new")
# household <- read_dta('sec1.dta') 
# household$new_HHID <- paste(household$clust, household$nh, sep = "_" )
# ##household$new_PID <- paste(household$clust, household$nh, household$pid, sep = "_" )
# 
# 
# father_ed <- household %>%          #levels of education of individuals father
#   select(clust, nh, pid,  s1q15, new_HHID )  %>%
#   filter(!is.na(s1q15)) %>%
#   filter(s1q15 != 98, s1q15 != 1)  #filtering out NA and None values 
# 
# 
# mother_ed <- household %>%
#   select(clust, nh, pid,  s1q19, new_HHID )  %>%
#   filter(!is.na(s1q19)) %>%
#   filter(s1q19 != 98, s1q19 != 1) #filtering out NA and None values 
# 
# 
# ##gathering father and mother education into one dataframe
# parents_ed <- full_join(father_ed, mother_ed, by = c( 'new_HHID') ) %>%
#   select( new_HHID, s1q15, s1q19)



##SEC2A - General Education
ed_general <- read_dta('sec2a.dta')
ed_general$new_HHID <- paste(ed_general$clust, ed_general$nh, sep = "_" )
##ed_general$new_PID <- paste(ed_general$clust, ed_general$nh, ed_general$pid, sep = "_" )


##level_study -
##getting the average of the highest level of study completed and highest qualification
##attained per household

#highest level of study completed and highest qualification attained per individual - data file extraction
level_study <- ed_general %>%        
  select( s2aq2, s2aq3, new_HHID )  %>%
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
  select( new_HHID, highest_level, highest_qualification ) %>%
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
  select(s2aq5a , s2aq5b, new_HHID) %>%
  filter(s2aq5a != 99 | s2aq5b != 99) %>%
  mutate(ptimetotals = (s2aq5a * 60) + s2aq5b)

##household count of how many individuals in the house has records for time spent going to school 
time_spent_going_school_hh_count <- count(time_spent_going_school, vars = new_HHID) %>%
  rename(new_HHID = vars,
         hh_count = n)

##totals that a household spends going to school 
time_spent_going_school_household <- time_spent_going_school%>%  #time spend going to school *will have to combine hours and minutes into one variable*
  select( new_HHID, ptimetotals) %>%
  group_by(new_HHID) %>%
  summarise(household_time_totals = sum(ptimetotals))

##averages of time spent going to school by diving totals by household count 
time_spent_going_school_averages <- full_join(time_spent_going_school_household , time_spent_going_school_hh_count, by = 'new_HHID') %>% 
  mutate(hh_time_spent_going_to_school_avg =  household_time_totals/hh_count)



##Total educational expenses #####
##average education expense for individual per household 

##selecting all education expenses columns 
educ_expenses <- ed_general%>%
  select(clust, nh, s2aq6, s2aq7, s2aq8, s2aq9, s2aq10, s2aq11, s2aq12, s2aq13, s2aq13a)

##changing null values to zero so not to mess up sums 
educ_expenses[is.na(educ_expenses)] <- 0

##sums of all education expenses into one total for each individual 
educ_expenses_totals <- educ_expenses %>%
  mutate(rowSums(educ_expenses))  %>%
  mutate(totals = rowSums(educ_expenses) - (nh + clust)) %>%
 ### select()  totals) %>%
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


##merge 

educ_variables <- left_join(time_spent_going_school_averages, level_study_household_avgs, by = "new_HHID") %>%
  left_join(educ_expenses_household_avgs, by = "new_HHID") %>%
  select("new_HHID",  "hh_time_spent_going_to_school_avg",  "highest_level_avgs", "highest_qualification_avgs", "hh_educ_expenses_avg")


educ_variables[is.na(educ_variables)]=0
#######################################################
###ended here -
#######################################################

# SEC1 <- read_dta('SEC1.dta')%>%
#   distinct(clust, nh, pid, .keep_all=TRUE)
# SEC1$new_HHID <- paste(SEC1$clust, SEC1$nh, sep = "_" )
# SEC1$new_PID <- paste(SEC1$clust, SEC1$nh, SEC1$pid, sep = "_" )
# new_HHIDs_list <- distinct(SEC1, new_HHID, .keep_all=FALSE)

#################### Profit Load Data########################
####################################################

AGG1 <- read_dta("aggregates/AGG1.dta")%>%
  transmute(new_HHID = paste(AGG1$clust, AGG1$nh, sep = "_" ), totemp, nh, clust)

AGG2 <- read_dta("aggregates/AGG2.dta")%>%
  transmute(new_HHID = paste(AGG2$clust, AGG2$nh, sep = "_" ), sum_aggr_inc = rowSums(AGG2[,c('agri1c', 'agri2c')]))

# SUBAGG7 SEFARM = Farm self employment income
SUBAGG7  <- read_dta("aggregates/subagg7.dta") %>%
  transmute(new_HHID = paste(SUBAGG7$clust, SUBAGG7$nh, sep = "_" ), self_emp_inc = sefarm)

# SUBAGG9 WATINC = Income from water sold
SUBAGG9  <- read_dta("aggregates/subagg9.dta")%>%
  transmute(new_HHID = paste(SUBAGG9$clust, SUBAGG9$nh, sep = "_" ), inc_water_sold = watinc)

# LNDINC1 = Income from renting out land
# LNDINC2 = Income from sharecropping
SUBAGG10  <- read_dta("aggregates/SUBAGG10.dta") %>%
  transmute(new_HHID = paste(SUBAGG10$clust, SUBAGG10$nh, sep = "_" ), sum_inc_land = rowSums(SUBAGG10[,c('lndinc1', 'lndinc2')]))


# LIVINC = Income from renting out livestock
SUBAGG11  <- read_dta("aggregates/SUBAGG11.dta") %>%
  transmute(new_HHID = paste(SUBAGG11$clust, SUBAGG11$nh, sep = "_" ), inc_rent_livestock = livinc)

# EQINC = Income from renting out agricultural equipment
SUBAGG12 <- read_dta('aggregates/SUBAGG12.dta')%>%
  transmute(new_HHID = paste(SUBAGG12$clust, SUBAGG12$nh, sep = "_" ), inc_rent_equip = eqinc)

#CRPINC1 = Revenue from sale of cash crops – main outlet
#CRPINC2 = Revenue from sale of cash crops – other outlet
SUBAGG13 <- read_dta('aggregates/SUBAGG13.dta')%>%
  transmute(new_HHID = paste(SUBAGG13$clust, SUBAGG13$nh, sep = "_" ), sum_cash_crops = rowSums(SUBAGG13[,c('crpinc1', 'crpinc2')]))

#SUBAGG14 ROOTINC = Revenue from sale of roots/fruit/vegetables
SUBAGG14 <- read_dta('aggregates/SUBAGG14.dta')%>%
  transmute(new_HHID = paste(SUBAGG14$clust, SUBAGG14$nh, sep = "_" ), inc_food_sales = rootinc)

#SUBAGG15 INCOTHAG = Revenue from other agricultural source
SUBAGG15 <- read_dta('aggregates/SUBAGG15.dta')%>%
  transmute(new_HHID = paste(SUBAGG15$clust, SUBAGG15$nh, sep = "_" ), inc_other_agr = incothag)

#SUBAGG16 TRCRPINC = Revenue from the sale of transformed crop products
SUBAGG16 <- read_dta('aggregates/SUBAGG16.dta')%>%
  transmute(new_HHID = paste(SUBAGG16$clust, SUBAGG16$nh, sep = "_" ), inc_transformed_crop = trcrpinc)

income <- full_join(AGG1, AGG2, by = 'new_HHID')%>%
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


sum_income <- income %>%
  select('nh', 'clust', 'totemp','sum_aggr_inc',  'inc_water_sold' , 'sum_inc_land' ,
         'inc_rent_livestock' ,  'inc_rent_equip' , 'sum_cash_crops' , 'inc_food_sales' , 
         'inc_other_agr', 'inc_transformed_crop') %>%
  mutate(sum_income, total = rowSums(sum_income))  %>%
  mutate(sum_income, sum_income_value = rowSums(sum_income) - (nh + clust))

sum_income$new_HHID <- paste(sum_income$clust, sum_income$nh, sep = "_" )



############## Expenditures #####################
#################################################

# farmland rent  
SUBAGG22 <- read_dta("aggregates/SUBAGG22.dta")
SUBAGG22$new_HHID <- paste(SUBAGG22$clust, SUBAGG22$nh, sep = "_" )
  
SUBAGG22 <- SUBAGG22 %>%
  select(nh, clust, new_HHID, expland) %>%
  mutate( exp_land_rent= -1*expland) %>%
  filter(!is.na(exp_land_rent))

#crop exp
SUBAGG23 <- read_dta("aggregates/SUBAGG23.dta") %>%
  transmute(new_HHID = paste(SUBAGG23$clust, SUBAGG23$nh, sep = "_" ), exp_crops = -1*expcrop)


# %>%
# SUBAGG23[is.na(expcrop)] <- 0

#livestock inputs
SUBAGG24 <- read_dta("aggregates/SUBAGG24.dta") %>%
  transmute(new_HHID = paste(SUBAGG24$clust, SUBAGG24$nh, sep = "_" ), exp_livestock = -1* expliv)

#EXPFDPR1 = Labour costs on food processing
#EXPFDPR2 = Other costs on food processing
SUBAGG25<- read_dta("aggregates/SUBAGG25.dta")%>%
  transmute(new_HHID = paste(SUBAGG25$clust, SUBAGG25$nh, sep = "_" ), process_cost = -1* rowSums(SUBAGG25[,c('expfdpr1', 'expfdpr2')]))


#SUBAGG31 DEPNEQ = Depreciation of farming equipment 
SUBAGG31 <- read_dta("aggregates/SUBAGG31.dta")%>%
  transmute(new_HHID = paste(SUBAGG31$clust, SUBAGG31$nh, sep = "_" ), equip_depreciation = -1*depneq)

expenses <- full_join(SUBAGG22, SUBAGG23, by = 'new_HHID')%>%
  full_join(SUBAGG24, by = 'new_HHID')%>%
  full_join(SUBAGG25, by = 'new_HHID')%>% 
  full_join(SUBAGG31, by = 'new_HHID')
expenses[is.na(expenses)]=0


sum_expenses <- expenses%>%
  select('nh', 'clust', 'exp_land_rent' , 'exp_crops' , 'exp_livestock' ,
         'process_cost' , 'equip_depreciation') %>%
  mutate(sum_expenses, total = rowSums(sum_expenses))  %>%
  mutate(sum_expenses, sum_expenses_value = rowSums(sum_expenses) - (nh + clust))

sum_expenses$new_HHID <- paste(sum_expenses$clust, sum_expenses$nh, sep = "_" )

############## NET INCOME FINAL JOIN AND TABLE #####################
#####################################################


net_join <- left_join(sum_income, sum_expenses, by = 'new_HHID')
  
net_join[is.na(net_join)]=0 
  
profit_join <- net_join %>%
  mutate(sum_profit  = (sum_income_value + sum_expenses_value)) %>%
  select(new_HHID, sum_income_value, sum_expenses_value, sum_profit)



Final_df <- left_join(profit_join, educ_variables, by = "new_HHID")%>%
  left_join( agri_merge, by = "new_HHID")

colnames(Final_df)

final_lm <- lm(Final_df, formula = sum_profit ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
               + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
              cassava + maize + minor_cassava + minor_Plantain)
   
summary(final_lm)

##these variables have the highest significant values 
#3 stars 

##highest_qualification_avgs 
highest_qualification_profit_lm <- lm(Final_df, formula = sum_profit ~ highest_qualification_avgs)

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