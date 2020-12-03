# DATA TRANSLATION GROUP 5 
# Feifei Jiang, Kristen Higashi, Lynna Tran, Vish Diwan 


library(tidyverse)
library(haven)
library(dplyr)
library(magrittr)

############## REGION INFORMATION  #####################
#####################################################

SEC0A <- read_dta('SEC0A.dta')

clust_info <- SEC0A %>%
  select("clust" ,"region" ,  "district" ,"eanum"  ) %>%
  mutate(region_name = case_when(region == 1 ~'Western',
                                 region == 2 ~'Central',
                                 region == 3 ~'Greater Accra', 
                                 region == 4 ~'Eastern',  
                                 region == 5~'Volta', 
                                 region == 6~'Ashanti', 
                                 region ==  7 ~'Brong Ahafo', 
                                 region == 8 ~'Northern',  
                                 region == 9~'Upper East', 
                                 region == 10 ~'Upper West' ))

### ------ load agriculture data and add primary key  ------ ###
### ------------------------------ ###

agri_land <- read_dta('data/glss4_new/sec8a1.dta') 
agri_land$new_HHID <- paste(agri_land$clust, agri_land$nh, sep="_") 


agri_plot <- read_dta('data/glss4_new/sec8b.dta') 
agri_plot$new_HHID <- paste(agri_plot$clust, agri_plot$nh, sep="_")


### ------ agriculture land ------ ###
### ------------------------------ ###


land <- select(agri_land, new_HHID, s8aq1, s8aq3, s8aq4, s8aq5) %>%
  mutate(total_land_area = ifelse(agri_land$s8aq3==3, agri_land$s8aq4 * 0.1, agri_land$s8aq4 * 1)) %>%
  filter(!is.na(total_land_area))


### -------------------------------------- ###
### ------ agriculture plot details ------ ###
### -------------------------------------- ###


plot <- select(agri_plot, new_HHID, s8bq4a, s8bq4b, s8bq5)  %>%
  mutate(plot_area = ifelse(agri_plot$s8bq4b %in% c("3") , agri_plot$s8bq4a * 0.1, agri_plot$s8bq4a * 1)) %>%
  group_by(new_HHID) %>%
  mutate(hh_plot_area = sum(plot_area)) %>%
  distinct(hh_plot_area, new_HHID)

### -------------------------------------- ###
### ------ popular crop details ------ ###
### -------------------------------------- ###
# figure out what crops got planted the most

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

crop_combined[is.na(crop_combined)]=0

### -------------------------------------- ###
### ------ local area characteritics ------ ###
### -------------------------------------- ###
SEC8C1 <- read_dta('SEC8C1.dta')
SEC8C1$new_HHID <- paste(SEC8C1$clust, SEC8C1$nh, sep = "_" )


########fix me, working on a local variable#######
Harvest <- SEC8C1 %>%
  select(new_HHID, s8cq6) %>%
  filter(!is.na(s8cq6)) %>% 
  mutate(market_name = case_when(s8cq6 == 1 ~'Pre-harvest contractor',
                                 s8cq6 == 2 ~'Farm gate buyer',
                                 s8cq6 == 3 ~'Market trader', 
                                 s8cq6 == 4 ~'Consumer',  
                                 s8cq6 == 5~'State trading organisation', 
                                 s8cq6 == 6~'Co-operatives'
  )) %>%
  pivot_wider(names_from = market_name , values_from = s8cq6, values_fn = list)


Harvest_TF <- Harvest %>%
  mutate(Pre_harvest_contractor = ifelse(is.null('Pre-harvest contractor') == TRUE, FALSE, TRUE)) %>%
  mutate(farm_gate_buyer = ifelse(is.null('Farm gate buyer') == TRUE, FALSE, TRUE)) %>%
  mutate(market_trader= ifelse('Market trader' == 'NULL', FALSE, TRUE)) %>%
  mutate(consumer = ifelse(is.null('Consumer') == TRUE, FALSE, TRUE)) %>%
  mutate(state_trading_org = ifelse(is.null('State trading organisation') == TRUE, FALSE, TRUE)) %>%
  mutate(co_op = ifelse(is.null('Co-operatives') == TRUE, FALSE, TRUE)) 





### ----------------------------------------------------------------- ###
### ------ merged agriculture land + plot + agriculture income ------ ###
### ----------------------------------------------------------------- ###

agri_merge <- full_join(land, plot, by = "new_HHID", all.y = TRUE) %>%
  full_join(crop_combined, by = "new_HHID", all.y = TRUE) %>%
  select(new_HHID,"total_land_area", "hh_plot_area", 
         "cassava", "maize", "minor_cassava","minor_Plantain"  )

agri_merge[is.na(agri_merge)]=0


### ----------------------------------------------------------------- ###
### ------ Education variables------ ###
### ----------------------------------------------------------------- ###

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





############## COMBINED DATASET  #####################
#####################################################

Final_df <- left_join(profit_join, educ_variables, by = "new_HHID")%>%
  left_join( agri_merge, by = "new_HHID") %>%
  mutate(clust = strtoi((substring(new_HHID, 1, 4))), base=4L) %>%
  left_join(clust_info,  by = 'clust') %>%
  mutate(profit_per_land = if_else(total_land_area != 0, (sum_profit / total_land_area), 0)) %>%
 mutate(profit_per_plot = if_else(hh_plot_area != 0, (sum_profit / hh_plot_area), 0))
  
###profit by region
ggplot(data = Final_df, aes(sum_profit, region_name)) +
  geom_boxplot()


#######LM set ups##########

###dependant variable -- sum profit
all_variables_lm <- lm(Final_df, formula = sum_profit ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                       + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                         cassava + maize + minor_cassava + minor_Plantain
                       + profit_per_land + profit_per_plot)

summary(all_variables_lm)


siginificant_variables_lm <- lm(Final_df, formula = sum_profit ~  highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                + total_land_area +  hh_plot_area +  minor_cassava + minor_Plantain)

summary(siginificant_variables_lm )


###land area profit######
all_variables_per_land_lm <- lm(Final_df, formula = profit_per_land ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                       + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                         cassava + maize + minor_cassava + minor_Plantain
                       )

summary(all_variables_per_land_lm)


###land area profit######
all_variables_per_plot_lm <- lm(Final_df, formula = profit_per_plot ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                                  cassava + maize + minor_cassava + minor_Plantain
)

summary(all_variables_per_plot_lm)


##these variables have the highest significant values 
#3 stars 

##hh_time_spent_going_to_school_avg ######
time_spent_going_to_school_avg_profit_lm <- lm(Final_df, formula = sum_profit ~  hh_time_spent_going_to_school_avg )

summary(time_spent_going_to_school_avg_profit_lm)

hist(rstandard((time_spent_going_to_school_avg_profit_lm)), 
     xlab = "Standardized residuals", main = 'Standardized Residuals
     of Average Time Spent 
     Going to School per Household' )

plot(fitted(time_spent_going_to_school_avg_profit_lm ), resid((time_spent_going_to_school_avg_profit_lm) ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Average Time
     Spent Going to School per Household',
     abline(h = 0, col = "blue"))

##dummies /specs 

time_spent_going_to_school_avg_profit_lm_poly <- lm(Final_df, formula = sum_profit  ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                                    + I(hh_time_spent_going_to_school_avg^2) + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                                                      cassava + maize + minor_cassava + minor_Plantain)


ggplot(data = Final_df, aes(x = I(hh_time_spent_going_to_school_avg ^2), y = sum_profit )) +
  geom_smooth() +
  xlab("Average of Time Spent Going to School Squared") +
  ylab(" Profits by household") +
  ggtitle("Average of Time Spent Going to School by 
          Profits per Household -- Polynomial")


###plot area ######
hh_plot_area_lm <- lm(Final_df, formula = sum_profit ~  hh_plot_area)

summary(hh_plot_area)

hist(rstandard((hh_plot_area_lm)), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of 
     Plot Area per Household' )

plot(fitted(hh_plot_area_lm), resid((hh_plot_area_lm) ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Plot Area
     per Household ',
     abline(h = 0, col = "blue"))

##dummies /specs 

hh_plot_area_lm_poly <- lm(Final_df, formula = sum_profit  ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                           + I(hh_plot_area^2) + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                             cassava + maize + minor_cassava + minor_Plantain)


ggplot(data = Final_df, aes(x = I(hh_plot_area ^2), y = sum_profit )) +
  geom_smooth() +
  xlab("Plot Area (in acres) Squared") +
  ylab(" Profits by household") +
  ggtitle("Plot Area (in Acres) per Household
          -- Polynomial")


##MINOR CASSAVA TRUE ######
minor_cassava_lm <- lm(Final_df, formula = sum_profit ~ minor_cassava )

summary(minor_cassava_lm)

hist(rstandard(minor_cassava_lm ), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of Average
     Highest Qualification' )

plot(fitted(minor_cassava_lm ), resid(minor_cassava_lm),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Average Highest 
     Qualifications in Household ',
     abline(h = 0, col = "blue"))





##2 stars 
##highest level avgs, total_land_area 


##highest level avgs 
highest_level_avgs_lm <- lm(Final_df, formula = sum_profit ~   highest_level_avgs )

summary( highest_level_avgs_lm)

hist(rstandard(( highest_level_avgs_lm )), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of Average
     Highest Level of Study
     per Household' )

plot(fitted( highest_level_avgs_lm ), resid(( highest_level_avgs_lm ) ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Average
     Highest Level of Study
     per Household',
     abline(h = 0, col = "blue"))

##dummies /specs 

highest_level_avgs_lm_poly <- lm(Final_df, formula = sum_profit  ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                 + I(hh_time_spent_going_to_school_avg^2) + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                                   cassava + maize + minor_cassava + minor_Plantain)


ggplot(data = Final_df, aes(x = I(highest_level_avgs  ^2), y = sum_profit )) +
  geom_smooth() +
  xlab("Average of Highest Level per Household Squared") +
  ylab(" Profits by household") +
  ggtitle("Average Highest Level of Study by 
          Profits per Household -- Polynomial")


##highest level avgs 
total_land_area_lm <- lm(Final_df, formula = sum_profit ~   total_land_area)

summary( total_land_area_lm )

hist(rstandard(( total_land_area_lm  )), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of
     Land Area per Household' )

plot(fitted( total_land_area_lm  ), resid(( total_land_area_lm  ) ),
     xlab = "Fitted", ylab = "Residuals",
     main = 'Fitted vs Residuals for Average Highest 
     Qualifications in Household ',
     abline(h = 0, col = "blue"))

##dummies /specs 

highest_level_avgs_lm_poly <- lm(Final_df, formula = sum_profit  ~ highest_level_avgs  + hh_time_spent_going_to_school_avg 
                                 + I(hh_time_spent_going_to_school_avg^2) + highest_qualification_avgs + hh_educ_expenses_avg + total_land_area +  hh_plot_area +    
                                   cassava + maize + minor_cassava + minor_Plantain)


ggplot(data = Final_df, aes(x = I(total_land_area  ^2), y = sum_profit )) +
  geom_smooth() +
  xlab("Average of Qualifications Squared") +
  ylab(" Profits by household") +
  ggtitle("Average Qualifications by 
          Profits per Household -- Polynomial")





