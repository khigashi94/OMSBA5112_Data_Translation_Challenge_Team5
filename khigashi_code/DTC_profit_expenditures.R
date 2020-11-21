# SEC9A11: Household Expenditure: ID’s of persons responsible for purchases
# SEC9A12: Household Expenditure: Non-food expenses-less frequently purchased items
# SEC9A2: Household Expenditure: Non-food expenses-frequently purchased items
# SEC9B0: Household Expenditure: ID’s of persons responsible for purchases
# SEC9B: Household Expenditure: Food expenses
# SEC9C: Household Expenditure: Availability of selected consumer items
# SEC10A: Non-Farm Enterprises: Basic characteristics of non-farm enterprises
# SEC10B: Non-Farm Enterprises: Expenditures on enterprises
# SEC10C: Non-Farm Enterprises: Assets of enterprises
# SEC10D: Non-Farm Enterprises: Revenue of enterprise
# SEC10E: Non-Farm Enterprises: Net income and inventory of enterprise
# SEC11A1: Income Transfers: Identification of households who gave out cash or
# goods as remittance (donors)
# SEC11A2: Income Transfers: Transfer payments made by households
# SEC11B1: Income Transfers: Identification of who received cash or goods as
# remittances (recipients)
# SEC11B2: Income Transfers: Transfer payments received by households
# SEC11C: Miscellaneous income
# SEC11D: Miscellaneous out-goings (expenditures)
# SEC12A1: Identification of households with credits
# SEC12A2: Credits
# SEC12B: Assets and durable consumer goods
# SEC12C: Savings (including Susu)



library(tidyverse)
library(here)
library(haven)
library(forcats)

SEC9A11 <- read_dta("SEC9A11.dta")
SEC9A12 <- read_dta("SEC9A12.dta")
SEC9A2 <- read_dta("SEC9A2.dta")
SEC9B0 <- read_dta("SEC9B0.dta")
SEC9B <- read_dta("SEC9B.dta")
SEC9C <- read_dta("SEC9C.dta")
SEC10A <- read_dta("SEC10A.dta")
SEC10B <- read_dta("SEC10B.dta")
SEC10C <- read_dta("SEC10C.dta")
SEC10D <- read_dta("SEC10D.dta")
SEC10E <- read_dta("SEC10E.dta")
SEC11A1 <- read_dta("SEC11A1.dta")
SEC11A2 <- read_dta("SEC11A2.dta")
SEC11B1 <- read_dta("SEC11B1.dta")
SEC11B2 <- read_dta("SEC11B2.dta")
SEC11C <- read_dta("SEC11C.dta")
SEC11D <- read_dta("SEC11D.dta")
SEC12A1 <- read_dta("SEC12A1.dta")
SEC12A2 <- read_dta("SEC12A2.dta")
SEC12B <- read_dta("SEC12B.dta")
SEC12C <- read_dta("SEC12C.dta")




INC2 <- read_dta("INC2.dta")
INC3 <- read_dta("INC3.dta")
INC4 <- read_dta("INC4.dta")
INC5 <- read_dta("INC5.dta")
INC6 <- read_dta("INC6.dta")
INC7 <- read_dta("INC7.dta")
INC8 <- read_dta("INC8.dta")
INC9 <- read_dta("INC9.dta")
INC10 <- read_dta("INC10.dta")
INC11 <- read_dta("INC11.dta")
INC12 <- read_dta("INC12.dta")
INC13 <- read_dta("INC13.dta")
INC14 <- read_dta("INC14.dta")
INC15 <- read_dta("INC15.dta")
INC16 <- read_dta("INC16.dta")

 <- read_dta("EXP1.dta")
EXP2 <- read_dta("EXP2.dta")
EXP3 <- read_dta("EXP3.dta")
EXP4 <- read_dta("EXP4.dta")
EXP5 <- read_dta("EXP5.dta")
EXP6 <- read_dta("EXP6.dta")
EXP7 <- read_dta("EXP7.dta")
EXP8 <- read_dta("EXP8.dta")
EXP9 <- read_dta("EXP9.dta")
EXP10 <- read_dta("EXP10.dta")
EXP11 <- read_dta("EXP11.dta")
EXP12 <- read_dta("EXP12.dta")
EXP13 <- read_dta("EXP13.dta")
EXP14 <- read_dta("EXP14.dta")
EXP15 <- read_dta("EXP15.dta")
EXP16 <- read_dta("EXP16.dta")
EXP17 <- read_dta("EXP17.dta")
EXP18 <- read_dta("EXP18.dta")
EXP19 <- read_dta("EXP19.dta")
EXP20 <- read_dta("EXP20.dta")
EXP21 <- read_dta("EXP21.dta")


SUBAGG1 <- read_dta("SUBAGG1.dta")
SUBAGG2 <- read_dta("SUBAGG2.dta")
SUBAGG3 <- read_dta("SUBAGG3.dta")
SUBAGG4 <- read_dta("SUBAGG4.dta")
SUBAGG5 <- read_dta("SUBAGG5.dta")
SUBAGG6 <- read_dta("SUBAGG6.dta")
SUBAGG7 <- read_dta("SUBAGG7.dta")
SUBAGG8 <- read_dta("SUBAGG8.dta")
SUBAGG9 <- read_dta("SUBAGG9.dta")
SUBAGG10 <- read_dta("SUBAGG10.dta")
SUBAGG11 <- read_dta("SUBAGG11.dta")
SUBAGG12 <- read_dta("SUBAGG12.dta")
SUBAGG13 <- read_dta("SUBAGG13.dta")
SUBAGG14 <- read_dta("SUBAGG14.dta")
SUBAGG15 <- read_dta("SUBAGG15.dta")
SUBAGG16 <- read_dta("SUBAGG16.dta")
SUBAGG17 <- read_dta("SUBAGG17.dta")
SUBAGG18 <- read_dta("SUBAGG18.dta")
SUBAGG19 <- read_dta("SUBAGG19.dta")
SUBAGG20 <- read_dta("SUBAGG20.dta")
SUBAGG21 <- read_dta("SUBAGG21.dta")
SUBAGG22 <- read_dta("SUBAGG22.dta")
SUBAGG23 <- read_dta("SUBAGG23.dta")                                                
SUBAGG24 <- read_dta("SUBAGG24.dta")
SUBAGG25 <- read_dta("SUBAGG25.dta")
SUBAGG26 <- read_dta("SUBAGG26.dta")
SUBAGG27 <- read_dta("SUBAGG27.dta")
SUBAGG28 <- read_dta("SUBAGG28.dta")
SUBAGG29 <- read_dta("SUBAGG29.dta")
SUBAGG30 <- read_dta("SUBAGG30.dta")
SUBAGG31 <- read_dta("SUBAGG31.dta")
SUBAGG32 <- read_dta("SUBAGG32.dta")
SUBAGG33 <- read_dta("SUBAGG33.dta")
SUBAGG34 <- read_dta("SUBAGG34.dta")
SUBAGG35 <- read_dta("SUBAGG35.dta")
SUBAGG36 <- read_dta("SUBAGG36.dta")
SUBAGG37 <- read_dta("SUBAGG37.dta")
SUBAGG38 <- read_dta("SUBAGG38.dta")
SUBAGG39 <- read_dta("SUBAGG39.dta")
SUBAGG40 <- read_dta("SUBAGG40.dta")

AGG1 <- read_dta("AGG1.dta")
AGG2 <- read_dta("AGG2.dta")
AGG3 <- read_dta("AGG3.dta")
AGG4 <- read_dta("AGG4.dta")
AGG5 <- read_dta("AGG5.dta")
AGG6 <- read_dta("AGG6.dta")
AGG7 <- read_dta("AGG7.dta")
AGG8 <- read_dta("AGG8.dta")
AGG9 <- read_dta("AGG9.dta")
AGG10 <- read_dta("AGG10.dta")
AGG11 <- read_dta("AGG11.dta")
AGG12 <- read_dta("AGG12.dta")

#### Income: ####
    AGG 2 (aggricultgure by HH)
        SUBAGG13
          INC10
        SUBAGG14
          INC11
        SUBAGG15
          INC12
        SUBAGG16
          INC13
        SUBAGG7
          INC2
          INC3
          INC4
          INC5
          
        SUBAGG26
          EXP7
        SUBAGG22
          EXP3
        SUBAGG23
          EXP4
        SUBAGG24
          EXP5
        SUBAGG25
          EXP6
        SUBAGG26
          EXP7
        SUBAGG31
          EXP12
dfAGG2_inc <- inner_join(INC2,INC3, by = 'nh') %>%
  inner_join(.,INC4,by = 'nh') %>%
  inner_join(.,INC5,by = 'nh') %>%
  inner_join(.,INC14,by = 'nh') %>%
  inner_join(.,INC15,by = 'nh')


dfAGG2_exp <- inner_join(EXP2,EXP3, by = 'nh')
%>%
  inner_join(.,EXP4,by = 'nh') 
# %>%
  # inner_join(.,EXP5,by = 'nh') %>%
  # inner_join(.,EXP6,by = 'nh') %>%
  # inner_join(.,EXP7,by = 'nh') %>%
  # inner_join(.,EXP12,by = 'nh')
  
    AGG1G 3,5 (nonfarm, remittances)
        SUBAGG17
          INC14
        SUBAGG8
          INC2
          INC3
          INC4
          INC5
        SUBAGG18
          INC15
        
        SUBAGG30
          EXP11
        SUBAGG37
          EXP19
        SUBAGG40
          EXP21
        SUBAGG32
          EXP13

    
    AGG 4 (imputed - rental)
        SUBAGG10
          INC7
        SUBAGG11
          INC8
        SUBAGG12
          INC9
        SUBAGG17
          INC14
        
        SUBAGG36
          EXP18
    
    AGG 6 (other)
        SUBAGG1
          INC1
        SUBAGG9
          INC6
        SUBAGG19
          INC16

#### Expenditures ####
    AGG 7, 8, 12 (food, housing, expenditure)
        SUBAGG29
          EXP10
        SUBAGGG36
          EXP18
        SUBAGG37
          EXP19
        SUBAGG38
          EXP17
        SUBAGG40
          EXP21
        SUGAGG33
          EXP14
        
    AGG 9 (other)
        SUBAGG21
          EXP2
        SUBAGG20
          EXP1
        SUBAGG28
          EXP9
        SUBAGG27
          EXP8
        SUBAGG34
          EXP15
        
    AGG 10, 11 (imputed - Food, other)
      SUBAGG6
          FD = J1K+J2K+J3KI+J4KI (from respectively INC2, INC3 and definition below)
          HO = J1H+J2HO+J3HO+J4HO (from INC2 and definition below)
          GD = J1TR+J1OTH+J2TROTH+J3TROTH+J4TROTH
          (from INC2 and definition below)
          SM = MJ1K+MJ1H+MJ1TR+MJ1OTH
          PK = MJ1K/SM
          PH = MJ1H/SM
          PTROTH = (MJ1TR+MJ1OTH)/SM
          J2HO = (PH/(PH+PTROTH))*J2H
          J2TROTH = (PRTOTH/(PH+PTROTH))*J2H
          IF J2HO LE .Z THEN J2HO = 0
          IF J2TROTH LE .Z THEN J2TROTH = 0
          J3KI = PK*J3K
          J3HO = PH*J3K
          J3TROTH = PROTH*J3K
          J4KI = PK*J4K
          J4HO = PH*J4K
          J4TROTH = PTROTH*J4K
        SUBAGG17
          INC14
        SUBAGG35
          EXP16
        SUBAGG26
          EXP7
          
