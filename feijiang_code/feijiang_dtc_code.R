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


### ------------------------------ ###
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
plot <- select(agri_plot, new_HHID, s8bq4a, s8bq4b, s8bq5, s8bq12a, s8bq12b) 
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
select(new_HHID, s8bq12a)

Maize_crops_growing_three <- plot %>% 
  filter(s8bq12a == 22) %>% 
  select(new_HHID, s8bq12a)


### ----------------------------------------------------------------- ###
### ------ merged agriculture land + plot + agriculture income ------ ###
### ----------------------------------------------------------------- ###
agri_merge <- left_join(land, plot, by = "new_HHID", all.y = TRUE) %>% 
  group_by(new_HHID) %>%
  rename(HH_own_any_land = s8aq1, 
         was_land_bought_or_rented = s8aq5,
         farm_owned_by_HH_member = s8bq5,
         crop1_growing = s8bq12a,
         crop2_growing = s8bq12b) %>% 
  summarize(HH_own_any_land, 
          was_land_bought_or_rented, 
          farm_owned_by_HH_member, 
          crop1_growing, 
          crop2_growing, 
          total_land_size_ropes_2_acres = sum(land_size_ropes_2_acres), 
          total_plot_size_ropes_2_acres = sum(plot_size_ropes_2_acres))






