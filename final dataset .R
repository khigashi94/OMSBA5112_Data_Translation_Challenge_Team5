# final dataset 


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


##owe_money 
#savings_val    
##total_land_size_ropes_2_acres




