#Lynna Tran

##This code file is used to out aside variables we are not using and
##save variables that we might use in our analysis later


# Lynna: Education in Data.pdf. 
# Page 1. Household size and education attainment per person, 
# per household. How much education that household has?

##if we're trying to study more individal characteristics such as gender 
individual_characteristics <-household %>%    
  select(sex,agey,  clust, nh, pid, new_PID) %>%
  mutate(male = (sex == 1))



##comparing sexes in level of study
gender_level_study <- left_join(level_study , individual_characteristics, by = "new_PID") 

gender_level_study_lm <- lm(gender_level_study, formula = s2aq2 ~ male + agey)
summary(gender_level_study_lm)



scholarship <- ed_general%>%
  filter(s2aq15 == 1) %>%
  select(clust, nh, pid,  s2aq15, s2aq16)

scholarship$new_PID <- paste(scholarship $clust, scholarship$nh, scholarship$pid, sep = "_" )


##Education: Literacy/Apprenticeship
ed_literacy <- read_dta('sec2c.dta')

#can read in either Ghanaian or English language
reading_literacy <- ed_literacy %>%
  filter(s2cq2 != 1 | s2cq1 == 1) %>%
  select(clust, nh, pid, s2cq1, s2cq2 )

reading_literacy$new_PID <- paste(reading_literacy$clust, reading_literacy$nh, reading_literacy$pid, sep = "_" )


#can read in English
read_english <- ed_literacy %>%
  filter(s2cq1 == 1) %>%
  select(clust, nh, pid, s2cq1 )

read_english$new_PID <- paste(read_english$clust, read_english$nh, read_english$pid, sep = "_" )


#can read in Ghana
read_ghana <- ed_literacy %>%
  filter(s2cq2 != 1 ) %>%
  select(clust, nh, pid, s2cq2 )

read_ghana$new_PID <- paste(read_ghana$clust, read_ghana$nh, read_ghana$pid, sep = "_" )


#can write in either Ghanaian or English language 
writing_literacy <- ed_literacy %>%
  filter(s2cq3 == 1 | s2cq4 != 1) %>%
  select(clust, nh, pid, s2cq3, s2cq4)

writing_literacy$new_PID <- paste(writing_literacy$clust, writing_literacy$nh, writing_literacy$pid, sep = "_" )

#can write in English language 
write_english<- ed_literacy %>%
  filter(s2cq3 == 1) %>%
  select(clust, nh, pid, s2cq3 )

write_english$new_PID <- paste(write_english$clust, write_english$nh, write_english$pid, sep = "_" )


##can write in Ghana language 
write_ghana <- ed_literacy %>%
  filter(s2cq4 != 1) %>%
  select(clust, nh, pid, s2cq4 )


write_ghana$new_PID <- paste(write_ghana$clust, write_ghana$nh, write_ghana$pid, sep = "_" )
