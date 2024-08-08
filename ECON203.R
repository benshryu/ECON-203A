library(ggplot2)
library(stargazer)
library(tidyverse)
library(broom)
library(stringr)
library(dplyr)
library(rddtools)
library(scales)
library(visreg)
rm(list = ls())
setwd('~/ECON203')
load('~/ECON203/dataframe_base_new.Rda')
load('~/ECON203/dataframe_repl_new.Rda')

# Replication Reg THperPop10k
regOH <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR),
               data=repl_df, weights = POPULATION)
summary(regOH , ROBUST = TRUE)
# Replication Reg FHperPop10k
regFH <- glm(FAMILY_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR),
             data=repl_df, weights = POPULATION)
summary(regFH , ROBUST = TRUE)
# Replication Reg IHperPop10k
regIH <- glm(INDIV_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR),
             data=repl_df, weights = POPULATION)
summary(regIH , ROBUST = TRUE)
# Replication Reg SHperPop10k
regSH <- glm(SHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR),
             data=repl_df, weights = POPULATION)
summary(regSH , ROBUST = TRUE)
# Replication Reg USHperPop10k
regUSH <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR),
              data=repl_df, weights = POPULATION)
summary(regUSH , ROBUST = TRUE)
stargazer(regOH, regFH, regIH, regSH, regUSH, type = 'text', title = 'Replication Result',
          keep=c("VACANCY_RATE","RENT_SHARE","MED_RENT","ACS_UNEMP", 'POVERTY_RATE'),
          covariate.labels=c('Vacant Rate', 'Rent Share', 'Median Rent', 'Unemployment Rate', 'Poverty Rate'),
          dep.var.labels=c("Total Homless","Family Homless", 'Individual Homeless', 'Sheltered Homeless', 'Unsheltered Homeless'))

#############replication with coc factor###################################################
regOH_coc <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                   ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                   SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) + factor(COC_NUMBER),
                 data=repl_df, weights = POPULATION)
summary(regOH_coc , ROBUST = TRUE)
# Replication Reg FHperPop10k
regFH_coc <- glm(FAMILY_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                   ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                   SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) + factor(COC_NUMBER),
                 data=repl_df, weights = POPULATION)
summary(regFH_coc , ROBUST = TRUE)
# Replication Reg IHperPop10k
regIH_coc <- glm(INDIV_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                   ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                   SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) + factor(COC_NUMBER),
                 data=repl_df, weights = POPULATION)
summary(regIH_coc , ROBUST = TRUE)
# Replication Reg SHperPop10k
regSH_coc <- glm(SHELTERED_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                   ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                   SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) + factor(COC_NUMBER),
                 data=repl_df, weights = POPULATION)
summary(regSH_coc , ROBUST = TRUE)
# Replication Reg UHperPop10k
regUSH_coc <- glm(UNSHELTERED_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                    ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                    SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) + factor(COC_NUMBER),
                  data=repl_df, weights = POP10K)
summary(regUSH_coc , ROBUST = TRUE)
stargazer(regOH_coc, regFH_coc, regIH_coc, regSH_coc, regUSH_coc, type = 'text',  
          keep=c("VACANCY_RATE","RENT_SHARE","MED_RENT","ACS_UNEMP", 'POVERTY_RATE'), title = 'Replication Result with community fixed effects',
          covariate.labels=c('Vacant Rate', 'Rent Share', 'Median Rent', 'Unemployment Rate', 'Poverty Rate'),
          dep.var.labels=c("Total Homless","Family Homless", 'Individual Homeless', 'Sheltered Homeless', 'Unsheltered Homeless'))
#################################################################################
base_df <- base_df %>%
  group_by(COC_NUMBER)%>%
  mutate(LAG_TOTAL_BEDS = lag(TOTAL_BEDS, n = 1)/POP10K)%>%
  mutate(LAG_TOTAL_BEDS2 = lag(TOTAL_BEDS, n = 2)/POP10K)%>%
  mutate(LAG_TOTAL_BEDS3 = lag(TOTAL_BEDS, n = 3)/POP10K)%>%
  mutate(LAG_TOTAL_BEDS4 = lag(TOTAL_BEDS, n = 4)/POP10K)%>%
  mutate(LAG_TOTAL_BEDS5 = lag(TOTAL_BEDS, n = 5)/POP10K)
base_df <- subset(base_df, base_df$YEAR < 2020)

# Reg THperPop10k
OGOH <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR),
          data=base_df, weights = POPULATION)
OH <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS,
             data=base_df, weights = POPULATION)
OH2 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS2,
          data=base_df, weights = POPULATION)
OH3 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS3,
          data=base_df, weights = POPULATION)
OH4 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
             ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
             SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS4,
           data=base_df, weights = POPULATION)
OH5 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
             ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
             SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS5,
           data=base_df, weights = POPULATION)
summary(OH , ROBUST = TRUE)
#  Reg FHperPop10k
FH <- glm(FAMILY_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+LAG_TOTAL_BEDS,
             data=base_df, weights = POPULATION)
summary(FH , ROBUST = TRUE)
#  Reg IHperPop10k
IH <- glm(INDIV_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+LAG_TOTAL_BEDS,
             data=base_df, weights = POPULATION)
summary(IH , ROBUST = TRUE)
#  Reg SHperPop10k
SH <- glm(SHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
               ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
               SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+LAG_TOTAL_BEDS,
             data=base_df, weights = POPULATION)
summary(SH , ROBUST = TRUE)
#  Reg USHperPop10k
USH <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS,
              data=base_df, weights = POPULATION)
USH2 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
             ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
             SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS2,
           data=base_df, weights = POPULATION)
USH3 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
             ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
             SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS3,
           data=base_df, weights = POPULATION)
USH4 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
             ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
             SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS4,
           data=base_df, weights = POPULATION)
USH5 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
              ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
              SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS5,
            data=base_df, weights = POPULATION)
summary(USH , ROBUST = TRUE)
stargazer(OGOH, OH, OH2, OH3, OH4, OH5, USH, USH2, USH3, USH4, USH5, type = 'text',  float = TRUE, title = 'Lagged Beds Effect Result',
          keep=c("VACANCY_RATE","RENT_SHARE","MED_RENT","ACS_UNEMP", 'POVERTY_RATE', 'LAG_TOTAL_BEDS', 'LAG_TOTAL_BEDS2', 'LAG_TOTAL_BEDS3', 'LAG_TOTAL_BEDS4'),
          covariate.labels=c('Vacant Rate', 'Rent Share', 'Median Rent', 'Unemployment Rate', 'Poverty Rate', 'Lag Available Beds'),
          dep.var.labels=c("Total Homless","Family Homless", 'Individual Homeless', 'Sheltered Homeless', 'Unsheltered Homeless'))
stargazer(OGOH, OH, FH, IH, SH, USH, type = 'latex',  float = TRUE, title = 'Lagged Beds Effect Result',
          keep=c("VACANCY_RATE","RENT_SHARE","MED_RENT","ACS_UNEMP", 'POVERTY_RATE', 'LAG_TOTAL_BEDS'),
          covariate.labels=c('Vacant Rate', 'Rent Share', 'Median Rent', 'Unemployment Rate', 'Poverty Rate', 'Lag Available Beds'),
          dep.var.labels=c("Total Homless","Family Homless", 'Individual Homeless', 'Sheltered Homeless', 'Unsheltered Homeless'))
################################################################################
# Reg THperPop10k
OGOH_coc <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ factor(COC_NUMBER),
              data=base_df, weights = POPULATION)
summary(OGOH_coc , ROBUST = TRUE)
OH_coc <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS+ factor(COC_NUMBER),
          data=base_df, weights = POPULATION)
OH_coc2 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS2+ factor(COC_NUMBER),
              data=base_df, weights = POPULATION)
OH_coc3 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS3+ factor(COC_NUMBER),
              data=base_df, weights = POPULATION)
OH_coc4 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS4+ factor(COC_NUMBER),
              data=base_df, weights = POPULATION)
OH_coc5 <- glm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
                ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                SINGLEPARENT + LIVINGALONE +  FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR) +LAG_TOTAL_BEDS5+ factor(COC_NUMBER),
              data=base_df, weights = POPULATION)
summary(OH_coc , ROBUST = TRUE)
#  Reg FHperPop10k
FH_coc <- glm(FAMILY_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+LAG_TOTAL_BEDS+ factor(COC_NUMBER),
          data=base_df, weights = POPULATION)
summary(FH_coc , ROBUST = TRUE)
#  Reg IHperPop10k
IH_coc <- glm(INDIV_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+LAG_TOTAL_BEDS+ factor(COC_NUMBER),
          data=base_df, weights = POPULATION)
summary(IH_coc , ROBUST = TRUE)
#  Reg SHperPop10k
SH_coc <- glm(SHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
            ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
            SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+LAG_TOTAL_BEDS+ factor(COC_NUMBER),
          data=base_df, weights = POPULATION)
summary(SH_coc , ROBUST = TRUE)
#  Reg USHperPop10k
USH_coc <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
             ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
             SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS+ factor(COC_NUMBER),
           data=base_df, weights = POPULATION)
USH_coc2 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
                 ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                 SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS2+ factor(COC_NUMBER),
               data=base_df, weights = POPULATION)
summary(USH_coc2 , ROBUST = TRUE)
USH_coc3 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
                 ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                 SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS3+ factor(COC_NUMBER),
               data=base_df, weights = POPULATION)
USH_coc4 <- glm(UNSHELTERED_HOMELESS_RATE ~  VACANCY_RATE + RENT_SHARE + MED_RENT + 
                 ACS_UNEMP +POVERTY_RATE + BLACK + HISPANIC + VETERAN +
                 SINGLEPARENT + LIVINGALONE + FAM_SUPP_HOUSING_PER_POP10K + factor(YEAR)+ LAG_TOTAL_BEDS3 + factor(COC_NUMBER),
               data=base_df, weights = POPULATION)

stargazer(OH_coc, OH_coc2, OH_coc3, USH_coc, USH_coc2, USH_coc3, USH_coc4, type = 'latex',  float = TRUE, title = 'Testing Different Lagged Varaibles With community Effect',
          keep=c("VACANCY_RATE","RENT_SHARE","MED_RENT","ACS_UNEMP", 'POVERTY_RATE', 'LAG_TOTAL_BEDS', 'LAG_TOTAL_BEDS2', 'LAG_TOTAL_BEDS3', 'LAG_TOTAL_BEDS4', 'LAG_TOTAL_BEDS5'),
          covariate.labels=c('Vacant Rate', 'Rent Share', 'Median Rent', 'Unemployment Rate', 'Poverty Rate', 'Lag Available Beds', 'Lag Available Beds t-2', 'Lag Available Beds t-3', 'Lag Available Beds t-4'),
          dep.var.labels=c("Total Homless",'Unsheltered Homeless'), keep.stat = c('n','rsq', 'adj.rsq'))
stargazer(OGOH_coc, OH_coc, FH_coc, IH_coc, SH_coc, USH_coc, type = 'latex', float = TRUE, title = 'Lagged Beds Effect Result With Community Effect',
          keep=c("VACANCY_RATE","RENT_SHARE","MED_RENT","ACS_UNEMP", 'POVERTY_RATE', 'LAG_TOTAL_BEDS'),
          covariate.labels=c('Vacant Rate', 'Rent Share', 'Median Rent', 'Unemployment Rate', 'Poverty Rate', 'Lag Available Beds'),
          dep.var.labels=c("Total Homless","Family Homless", 'Individual Homeless', 'Sheltered Homeless', 'Unsheltered Homeless'))
################################################################################


base_df_SF %>%
  ggplot(aes(x= YEAR, y= OVERALL_HOMELESS_RATE))+
  geom_point() +
  geom_vline(xintercept = 2016) + 
  labs(y = "Homelessness Rate", x = "Year", title = "Homelessness Rate in California in SF")

base_df_SF %>% 
  select(YEAR, OVERALL_HOMELESS_RATE) %>% 
  mutate(Policy = as.factor(ifelse(base_df_SF$YEAR >= 2016 , 1, 0))) %>% 
  ggplot(aes(x = YEAR, y = OVERALL_HOMELESS_RATE, color = Policy)) +
  geom_point() + 
  labs(y = "Homelessness Rate", x = "Year", title = "Homelessness Rate in California in SF")+
  geom_smooth(method = 'glm',  formula = y ~ poly(x, 2))
