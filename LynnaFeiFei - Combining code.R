###Lynna and Feifei Combined 


###

##we ran feifeis codes 

agri_plot <- read_dta('sec8b.dta') 
agri_plot$new_HHID <- paste(agri_plot$clust, agri_plot$nh, sep="_") 
plot <- select(agri_plot, nh, clust, new_HHID , s8bq4a, s8bq4b, s8bq5, s8bq12a, s8bq12b)
plot$plot_ropes_2_acres = ifelse(plot$s8bq4b %in% c("3") , plot$s8bq4a * 0.1, plot$s8bq4a * 1)


plot_by_household <- plot%>%
  group_by(new_HHID) %>%
  summarise(household_land_size_totals = sum(plot_ropes_2_acres))

plot_by_household$new_HHID <- select(plot_by_household$new_hhid)
  

agri_merge <- left_join(land, plot_by_household, by = 'new_hhid', all.y = TRUE) %>% 
  select(nh, clust.x, s8aq2, s8aq3, s8aq4, land_ropes_2_acres, s8aq5, s8aq3, s8aq4, s8bq4a, plot_ropes_2_acres,s8bq4b, s8bq5, s8bq12a, s8bq12b)
agri_merge$new_HHID <- paste(agri_merge$clust.x, agri_merge$nh, sep="_") 


##run lynnas code 

lf_df_combined <- full_join(plot_by_household, profit_parents_df, by = "new_HHID")
lf_df_lm <- lm(lf_df_combined, formula = agri2c ~ household_land_size_totals + s1q15 + s1q19)
summary(lf_df_lm)

profit_timetoschool_lm <- lm(profit_timetoschool_df, formula =  totemp ~ timetotals)
