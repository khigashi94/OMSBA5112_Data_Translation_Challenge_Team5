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