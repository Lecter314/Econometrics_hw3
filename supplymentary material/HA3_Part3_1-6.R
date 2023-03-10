#Prepared by Darya Pankova

#Installing packages#
##############################
install.packages('readxl', dependencies = TRUE)
install.packages('oglmx', dependencies = TRUE) # possble package 1
install.packages('MASS', dependencies = TRUE)  # possible package 2
install.packages("stargazer")
install.packages("plm")
##############################
# Disable scientific notation#
options(scipen = 999)

##############################
library('readxl')          # for reading excel files
library("MASS")            # ordered probit/logit (option 1)
library('oglmx')           # ordered probit/logit (option 2)
library(stargazer)
library("plm")
##############################

#Data preparation#
df <- as.data.frame(read_excel("/Users/dashenka/Downloads/hw3_preprocessed_dataset4xlsx/preprocessed_data_part3.xlsx"))

region <- df$region
assets_18 <- df$assets_18
assets_17 <- df$assets_17
bank_capital_18 <- df$bank_capital_18
bank_capital_17 <- df$bank_capital_17
net_income_18 <- df$net_income_18
net_income_17 <- df$net_income_17
credit_portfolio_18 <- df$credit_portfolio_18
credit_portfolio_17 <- df$credit_portfolio_17
ranking_multi <- df$ranking_multi

our_data <- data.frame(region, assets_18, assets_17, 
                       bank_capital_18, bank_capital_17, 
                       net_income_18, net_income_17, 
                       credit_portfolio_18, credit_portfolio_17, 
                       ranking_multi)


#Estimate the ordered probit#
results_oprob_1 <- oprobit.reg(ranking_multi ~ 
                             region + assets_18 + assets_17 +
                             bank_capital_18 + bank_capital_17 + 
                             net_income_18 + net_income_17 + 
                             credit_portfolio_18 + credit_portfolio_17,
                             data = our_data)

summary(results_oprob_1)

#Estimate the ordered probit only with constant#
results_oprob_1_with_intercept <- oprobit.reg(ranking_multi ~ 1,
                                data = our_data)

summary(results_oprob_1_with_intercept)

#Estimate the ordered logit model#
results_olog_1 <- ologit.reg(ranking_multi ~ 
                               region + assets_18 + assets_17 +
                               bank_capital_18 + bank_capital_17 + 
                               net_income_18 + net_income_17 + 
                               credit_portfolio_18 + credit_portfolio_17,
                             data = our_data)
summary(results_olog_1)

#Estimate the ordered logit only with constant#
results_olog_1_with_intercept <- ologit.reg(ranking_multi ~ 1,
                             data = our_data)
summary(results_olog_1_with_intercept)


#Probit_2: Exclude all variables except region and income#
results_oprob_2 <- oprobit.reg(ranking_multi ~ 
                                 region  +
                                 net_income_18 + net_income_17,
                               data = our_data)

summary(results_oprob_2)
#Logit_2: Exclude all variables except region and income#


results_olog_2 <- ologit.reg(ranking_multi ~ 
                               region + 
                               net_income_18 + net_income_17,
                             data = our_data)
summary(results_olog_2)




