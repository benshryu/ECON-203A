library(dplyr)
rm(list = ls())
base_path = 'C:/Users/Admin/Desktop/203A Hanratty Replication'

#Load data
load(paste(base_path, 'dataframe_base.Rda', sep=''))
load(paste(base_path, 'dataframe_repl.Rda', sep=''))

#Run regressions
#FAM_SUPP_HOUSING_PER_POP10K throws off the regressions by a ton! NEED TO FIX ASAP!!!!!
model.1.no.fe <- lm(OVERALL_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + ACS_UNEMP +
             POVERTY_RATE + BLACK + HISPANIC + VETERAN + SINGLEPARENT + LIVINGALONE  + factor(YEAR) +
               FAM_SUPP_HOUSING_PER_POP10K,
             data=repl_df, weights = POPULATION)
summary(model.1.no.fe)

model.2.no.fe <- lm(FAMILY_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + ACS_UNEMP +
                      POVERTY_RATE + BLACK + HISPANIC + VETERAN + SINGLEPARENT + LIVINGALONE  + factor(YEAR) +
                      FAM_SUPP_HOUSING_PER_POP10K,
                    data=repl_df, weights = POPULATION)
summary(model.2.no.fe)

model.3.no.fe <- lm(INDIV_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + ACS_UNEMP +
                      POVERTY_RATE + BLACK + HISPANIC + VETERAN + SINGLEPARENT + LIVINGALONE  + factor(YEAR) +
                      FAM_SUPP_HOUSING_PER_POP10K,
                    data=repl_df, weights = POPULATION)
summary(model.3.no.fe)

model.4.no.fe <- lm(SHELTERED_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + ACS_UNEMP +
                      POVERTY_RATE + BLACK + HISPANIC + VETERAN + SINGLEPARENT + LIVINGALONE  + factor(YEAR) +
                      FAM_SUPP_HOUSING_PER_POP10K,
                    data=repl_df, weights = POPULATION)
summary(model.4.no.fe)

model.5.no.fe <- lm(UNSHELTERED_HOMELESS_RATE ~ VACANCY_RATE + RENT_SHARE + MED_RENT + ACS_UNEMP +
                      POVERTY_RATE + BLACK + HISPANIC + VETERAN + SINGLEPARENT + LIVINGALONE  + factor(YEAR) +
                      FAM_SUPP_HOUSING_PER_POP10K,
                    data=repl_df, weights = POPULATION)
summary(model.5.no.fe)
