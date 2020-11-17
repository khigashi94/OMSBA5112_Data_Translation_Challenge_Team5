# feijiang_dtc_code.R
# Feifei Jiang
# Agriculture Land and Plot variables in data.pdf
# How much land that household has in acres? What crops are growing?

install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)


### ------ agriculture land ------ ###
agri_land <- read_dta('data/glss4_new/sec8a1.dta')
agri_land$new_pid <- paste(agri_land$clust, agri_land$nh, sep="_") 
land_info <- select(agri_land, nh, clust, s8aq3, s8aq4) %>% 
  filter(!is.na(s8aq3)) %>% 
  filter(!is.na(s8aq4))

# variables
plot_areas <- land_info$s8aq3      # unit of measure in acres, poles, ropes, other
land_owned <- land_info$s8aq4

# visualize by plot area
ggplot(data = agri_land, mapping = aes (x = s8aq3, y = nh)) + 
        geom_point(na.rm = TRUE)
#  geom_bar(mapping = aes(x = s8aq3)) 
#  geom_histogram(mapping = aes (x = s8aq4))
        

### ------ agriculture plot details ------ ###
agri_plot <- read_dta('data/glss4_new/sec8b.dta') 
agri_plot$new_pid <- paste(agri_plot$clust, agri_plot$nh, sep="_") 
plot_info <- select(agri_plot, nh, clust, s8bq4a, s8bq4b, s8bq5, s8bq12a, s8bq12b, s8bq13a, s8bq13b, s8bq13c, s8bq13d)

# variables
land_size <- plot_info$s8bq4a
unit_of_measure <- plot_info$s8bq4b    # unit of measure in acres, poles, ropes, other
farm_owned <- plot_info$s8bq5
crop1_growing <- plot_info$s8bq12a
crop2_growing <- plot_info$s8bq12b
crop_code1 <- plot_info$s8bq13a
crop_code2 <- plot_info$s8bq13b
crop_code3 <- plot_info$s8bq13c
crop_code4 <- plot_info$s8bq13d

# convert ropes to acres -- office hour
x <-  s8bq4b
if (x == 3) {
  "s8bq4b * .1111"
}  


### ----- merge agri land and plot by nh and clust. filter N/A ----- ###
agri_merge <- full_join(agri_land, agri_plot,  by = "nh", "clust", all.y = TRUE) %>% 
  select(nh, clust.x, s8aq3, s8aq4, s8bq4a, s8bq4b, s8bq5, s8bq12a, s8bq12b, s8bq13a, s8bq13b, s8bq13c, s8bq13d) %>% 
  filter(!is.na(s8aq3)) 

# visualize land owned by household, legend in land size
ggplot(agri_merge, aes(x = s8aq4, y = nh)) + 
  geom_point(aes(colour = s8bq4a)) 


# AGRI1C = HH agricultural income (1) - corrected
agg2_agri_income <- read_dta('data/glss4_new/aggregates/agg2.dta')
# variables
agri_income_agri1c <- agg2_agri_income$agri1c
  sd(agri_income)