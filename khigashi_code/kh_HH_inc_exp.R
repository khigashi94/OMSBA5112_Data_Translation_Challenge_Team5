



library(tidyverse)
library(here)
library(haven)


SEC1 <- read_dta("sec1.dta")
  SEC1$new_PID <- paste(SEC1$clust, SEC1$nh, SEC1$pid, sep = "_" )
  SEC1$new_HHID <- paste(SEC1$clust, SEC1$nh, sep = "_" )

## Employment ##

# screening questions and list of occupations (12 months)
SEC4A <- read_dta("sec4a.dta") 
  
# %>%
mutate(new_PID = paste(SEC4A$clust, SEC4A$nh, SEC4A$pid, sep = "_" )) %>%
#   mutate(SEC4A, new_HHID = paste(SEC4A$clust, SEC4A$nh, SEC4A$pid, sep = "_" ))paste(SEC4A$clust, SEC4A$nh, sep = "_" )

SEC4A_is_agric <- SEC4A
  SEC4A_is_agric$new <- paste(SEC4A$clust, SEC4A$nh, SEC4A$pid, sep = "_" )
  select(s4aq4, s4aq5, s4aq6, s4aq7)
# C", "S4AQ4", "S4AQ5", "S4AQ5", "S4AQ7")
  
  
  
# characteristics of main occupation
SEC4B <- read_dta("sec4b.dta")
  SEC4B$new_PID <- paste(SEC4B$clust, SEC4B$nh, SEC4B$pid, sep = "_" )
  SEC4B$new_HHID <- paste(SEC4B$clust, SEC4B$nh, sep = "_" )
  
  
  


# Employment search (12 mont)
SEC4F <- read_dta("sec4f.dta")
  SEC4F$new_PID <- paste(SEC4F$clust, SEC4F$nh, SEC4F$pid, sep = "_" )
  SEC4F$new_HHID <- paste(SEC4F$clust, SEC4F$nh, sep = "_" )


# emp history
SEC4H <- read_dta("sec4h.dta")



## Housing ##
SEC7 <- read_dta("sec7.dta")

SEC7$new_HHID <-paste(SEC7$clust, SEC7$nh, sep = "_" )

SEC12A1$new_HHID<-  paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )



# by_hh <- left_join(SEC7


by_hh <- left_join(SEC7, SEC12A2, by = "new_HHID")
## Respondent IDs ##
SEC6 <- read_dta("sec6.dta")



SEC12A1 <- read_dta("sec12a1.dta")
SEC12A1$new_HHID <- paste(SEC12A1$clust, SEC12A1$nh, sep = "_" )
SEC12A2 <- read_dta("sec12a2.dta")
SEC12A2$new_HHID <- paste(SEC12A2$clust, SEC12A2$nh, sep = "_" )
SEC12B <- read_dta("sec12b.dta")
SEC12B$new_HHID <- paste(SEC12B$clust, SEC12B$nh, sep = "_" )
SEC12C <- read_dta("sec12c.dta")
SEC12C$new_HHID <- paste(SEC12C$clust, SEC12C$nh, sep = "_" )

joined <- left_join(SEC7, 
                    left_join(SEC12A1
                                , left_join(SEC12A2
                                            , left_join(SEC12B, SEC12C
                                                        , by = "new_HHID")
                                            , by = "new_HHID")
                                , by = "new_HHID")
                    , by = "new_HHID")

## Expenditures ##
SEC9A11 <- read_dta("sec9a11.dta")
SEC9A12 <- read_dta("sec9a12.dta")
SEC9A2 <- read_dta("sec9a2.dta")
SEC9B0 <- read_dta("sec9b0.dta")
SEC9B <- read_dta("sec9b.dta")
SEC9C <- read_dta("sec9c.dta")

SEC10A <- read_dta("sec10a.dta")
SEC10B <- read_dta("sec10b.dta")
SEC10C <- read_dta("sec10c.dta")
SEC10D <- read_dta("sec10d.dta")
SEC10E <- read_dta("sec10e.dta")









# alt jobs
SEC4C <- read_dta("sec4c.dta")
SEC4D <- read_dta("sec4d.dta") 
SEC4E <- read_dta("sec4e.dta")
