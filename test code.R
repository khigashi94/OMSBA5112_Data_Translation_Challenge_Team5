testing_lm <- lm(Final_df, formula = sum_profit ~  highest_level_avgs  + hh_time_spent_going_to_school_avg 
               + total_land_area +  hh_plot_area +    
                  minor_cassava + minor_Plantain)

summary(testing_lm)




SEC0A <- read_dta('SEC0A.dta')
SEC0B <- read_dta('SEC0B.dta')
SEC0C <- read_dta('SEC0C.dta')

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
                                 region == 10 ~'Upper West' )) %>%
  distinct(clust, region_name)

 
  
##if we want to add district 
   # mutate(district_name = case_when(region_name == 'Western' & district == 01 ~'Jomoro',
   #                                 region_name == 'Western' & district == 02 ~'Nzema',
   #                                 region_name == 'Western'  & district == 03 ~'Ahanta West',
   #                                 region_name == 'Western' & district == 04 ~'Shama-Ahanta East',
   #                                 region_name == 'Western' & district == 05 ~'Mpohor-Wassa East',
   #                                 region_name == 'Western' &  district == 06 ~'Wassa West',
   #                                 region_name == 'Western' & district == 07 ~'Wassa Amemfi',
   #                                 region_name == 'Western' & district == 08 ~'Aowin-Suaman',
   #                                 region_name == 'Western' & district == 09 ~'Juabeso - Bia',
   #                                 region_name == 'Western' & district == 10 ~'Sefwi-Wiawso',
   #                                 region_name == 'Western' & district == 11 ~'Bibiani/Anhwiaso/Wekwai',
   #                                 region_name == 'Central' & district == 01 ~'Komenda/Edina/Eguafo/Abirem',
   #                                 region_name == 'Central' & district == 02 ~'Cape coast',
   #                                 region_name == 'Central' & district == 03 ~'Abura/Asebu Kwamankese',
   #                                 region_name == 'Central' & district == 04 ~'Mfantsiman',
   #                                 region_name == 'Central' & district == 05 ~'Gomoa',
   #                                 region_name == 'Central' & district == 06 ~'Awutu/Efutu Senya',
   #                                 region_name == 'Central' & district == 07 ~'Agona',
   #                                 region_name == 'Central' & district == 08 ~'Asikuma/Odoben/Brakwa',
   #                                 region_name == 'Central' & district == 09 ~'Ajumako/Enyan/Essiam',
   #                                 region_name == 'Central' & district == 10 ~'Assin',
   #                                 region_name == 'Central' & district == 11 ~'Twifo/Heman/Lower Denkyira',
   #                                 region_name == 'Central' & district == 12 ~'Upper Denkyira'
   #                                                                      ))
                             
                                   
                                   
                                   
                               

colnames(SEC0A)

distinct_newhhis <- select(distinct(Final_df, new_HHID))




########testing crops variables with false ##########

Plantain_minor_crop<- agri_plot %>%
  select(new_HHID, s8bq12b) %>%
  mutate(minor_Plantain = (s8bq12b == 06))  %>% 
  filter(!is.na(minor_Plantain)) %>%
  distinct(minor_Plantain,new_HHID)



crop_combined <- full_join(Cassava_crops , Maize_crops, by = 'new_HHID') %>%
  full_join(Cassava_minor_crop ,by = 'new_HHID') %>%
  full_join(Plantain_minor_crop ,by = 'new_HHID') %>%
  distinct(new_HHID, .keep_all = TRUE)
