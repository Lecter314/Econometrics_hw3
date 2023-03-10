library('readxl')          # for reading excel files
library("margins")         # calculates marginal effects for non-discrete variables
library('pROC')            # draws ROC curve
library('DescTools')       # metrics for probit/logit


df <- read_excel("~/Documents/econometrics hw3/preprocessed_data_part2.xlsx", 
                                      col_types = c("numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric"))


#rename variables
assets <- df$assets_18
capital <- df$bank_capital_18
net_income <- df$net_income_18
credit <- df$credit_portfolio_18
dif_assets <- df$change_assets
dif_capital <- df$change_capital
dif_income <-df$change_income
dif_credit <- df$change_credit
region <- df$region
ranking <- df$ranking

#evaluating final data frame for the model
df <- data.frame(ranking, region, assets, capital, net_income, credit, dif_capital,
                 dif_credit, dif_income, dif_assets)

#estimating LPM model
Lpm_mod <- lm(data = df, ranking ~ assets + capital + net_income
              + credit + dif_assets + dif_capital + dif_income + dif_credit)
summary(Lpm_mod)

#estimating logit model
Logit_mod <-  glm(data = df, ranking ~ assets + capital + net_income
                  + credit + dif_assets + dif_capital + dif_income + dif_credit,
                  family = binomial(link = "logit"))
summary(Logit_mod)


#estimating probit model
Probit_mod <-  glm(data = df, ranking ~ assets + capital + net_income
                   + credit + dif_assets + dif_capital + dif_income + dif_credit,
                  family = binomial(link = "probit"))
summary(Probit_mod)
