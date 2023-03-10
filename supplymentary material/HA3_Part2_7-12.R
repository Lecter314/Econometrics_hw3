
# Disable scientific notation
options(scipen = 999)
install.packages("stargazer")

library('readxl')          # for reading excel files
library("margins")         # calculates marginal effects for non-discrete variables
library('pROC')            # draws ROC curve
library('DescTools')       # metrics for probit/logit
library("stargazer")

df <- as.data.frame(read_excel("/Users/antonpolous/Downloads/preprocessed_data_part2.xlsx"))

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

df <- data.frame(ranking, region, assets, capital, net_income, credit, dif_capital,
                 dif_credit, dif_income, dif_assets)

#Logit model
model_logit <- glm(formula = ranking ~ assets + capital + net_income
                   + credit + dif_assets + dif_capital + dif_income + dif_credit,
                   data = df,
                   family = binomial(link = "logit"))
summary(model_logit)






#7. 
#construct a model with only an intercept
model_intercept <- glm(formula = ranking ~ 1,
                   data = df,
                   family = binomial(link = "logit"))
summary(model_intercept)

n <- length(ranking) #number of observations

#get the predictions from logit model
prob_est_logit <- predict(model_logit, type = "response")
ranking_pred_logit <- as.numeric(prob_est_logit > 0.5)  

#get the predictions from the model with only intercept
prob_est_intercept <- predict(model_intercept, type = "response")
ranking_pred_intercept <- as.numeric(prob_est_intercept > 0.5) 

#compute the share of correct predictions for both models
TP_logit <- sum((ranking_pred_logit == 1) & (ranking == 1))        # number of correct forecasts for ranking = 1
TN_logit <- sum((ranking_pred_logit == 0) & (ranking == 0))        # number of correct forecasts for ranking = 0
true_pred_logit <- TP_logit + TN_logit                             # total number of correct forecasts
true_pred_share_logit <- true_pred_logit / n                       # share of correct forecasts

TP_intercept <- sum((ranking_pred_intercept == 1) & (ranking == 1))        # number of correct forecasts for ranking = 1
TN_intercept <- sum((ranking_pred_intercept == 0) & (ranking == 0))        # number of correct forecasts for ranking = 0
true_pred_intercept <- TP_intercept + TN_intercept                         # total number of correct forecasts
true_pred_share_intercept <- true_pred_intercept / n                       # share of correct forecasts 

c("logit" = true_pred_share_logit, "only intercept" = true_pred_share_intercept) #get a vector to compare the results

#9.ROC curve
rocCurve_logit <- roc(ranking ~ predict(model_logit, type=c("response")), data = df)
AUC_logit <- rocCurve_logit$auc   # area under the curve

plot(rocCurve_logit)

FP_logit <- sum((ranking_pred_logit == 1) & (ranking == 0)) #False positive
FN_logit <- sum((ranking_pred_logit == 0) & (ranking == 1)) #False negative

sensitivity <- TP_logit/(TP_logit + FN_logit)
specificity <- TN_logit/(FP_logit + TN_logit)

#10. Other measures of quality

PseudoR2(model_logit, which = 'all')
PseudoR2(model_intercept, which = 'all')
PseudoR2(model_logit, which = 'McFaddenAdj')

HM <- sensitivity + specificity #Henricksson and Merton (1981)

#11. Adding/removing regressors
#Add to the model the interactions of regressors and their corresponding dynamics
#and the squares of all static parameters
new_logit <- glm(formula = ranking ~ assets + capital + net_income
                 + credit + dif_assets + dif_capital + dif_income + dif_credit + 
                   I(dif_assets*assets) + I(dif_capital*capital) + I(dif_income*net_income) +
                   I(dif_credit*credit) + I(assets^2) + I(capital^2) + I(net_income^2)
                 + I(credit^2),
                 data = df,
                 family = binomial(link = "logit"))
summary(new_logit)
PseudoR2(new_logit, which = 'McFaddenAdj')

#get the predictions from logit model
prob_est_new1 <- predict(new_logit, type = "response")
ranking_pred_new1 <- as.numeric(prob_est_new1 > 0.5) 

TP_new1 <- sum((ranking_pred_new1 == 1) & (ranking == 1))  # True positive
TN_new1 <- sum((ranking_pred_new1 == 0) & (ranking == 0))  # True negative
FP_new1 <- sum((ranking_pred_new1 == 1) & (ranking == 0))  # False positive
FN_new1 <- sum((ranking_pred_new1 == 0) & (ranking == 1))  # False negative

sensitivity_new1 <- TP_new1/(TP_new1 + FN_new1)
specificity_new1 <- TN_new1/(FP_new1 + TN_new1)

HM_new1 <- sensitivity_new1 + specificity_new1
HM_new1

#Delete the squares 
new_logit2 <- glm(formula = ranking ~ assets + capital + net_income
                  + credit + dif_assets + dif_capital + dif_income + dif_credit + 
                    I(dif_assets*assets) + I(dif_capital*capital) + I(dif_income*net_income) +
                    I(dif_credit*credit),
                  data = df,
                  family = binomial(link = "logit"))
summary(new_logit2)
PseudoR2(new_logit2, which = 'McFaddenAdj')

#get the predictions from logit model
prob_est_new2 <- predict(new_logit2, type = "response")
ranking_pred_new2 <- as.numeric(prob_est_new2 > 0.5) 

TP_new2 <- sum((ranking_pred_new2 == 1) & (ranking == 1))  # True positive
TN_new2 <- sum((ranking_pred_new2 == 0) & (ranking == 0))  # True negative
FP_new2 <- sum((ranking_pred_new2 == 1) & (ranking == 0))  # False positive
FN_new2 <- sum((ranking_pred_new2 == 0) & (ranking == 1))  # False negative

sensitivity_new2 <- TP_new2/(TP_new2 + FN_new2)
specificity_new2 <- TN_new2/(FP_new2 + TN_new2)

HM_new2 <- sensitivity_new2 + specificity_new2
HM_new2

#Add the dummy for region
new_logit3 <- glm(formula = ranking ~ region + assets + capital + net_income
                  + credit + dif_assets + dif_capital + dif_income + dif_credit + 
                    I(dif_assets*assets) + I(dif_capital*capital) + I(dif_income*net_income) +
                    I(dif_credit*credit),
                  data = df,
                  family = binomial(link = "logit"))
summary(new_logit3)
PseudoR2(new_logit3, which = 'McFaddenAdj')

#get the predictions from logit model
prob_est_new3 <- predict(new_logit3, type = "response")
ranking_pred_new3 <- as.numeric(prob_est_new3 > 0.5) 

TP_new3 <- sum((ranking_pred_new3 == 1) & (ranking == 1))  # True positive
TN_new3 <- sum((ranking_pred_new3 == 0) & (ranking == 0))  # True negative
FP_new3 <- sum((ranking_pred_new3 == 1) & (ranking == 0))  # False positive
FN_new3 <- sum((ranking_pred_new3 == 0) & (ranking == 1))  # False negative

sensitivity_new3 <- TP_new3/(TP_new3 + FN_new3)
specificity_new3 <- TN_new3/(FP_new3 + TN_new3)

HM_new3 <- sensitivity_new3 + specificity_new3
HM_new3

#Delete "region" dummy and two insignificant interaction variables 
new_logit4 <- glm(formula = ranking ~ assets + capital + net_income
                  + credit + dif_assets + dif_capital + dif_income + dif_credit + 
                    I(dif_assets*assets) + I(dif_income*net_income),
                  data = df,
                  family = binomial(link = "logit"))
summary(new_logit4)
PseudoR2(new_logit4, which = 'McFaddenAdj')

#get the predictions from logit model
prob_est_new4 <- predict(new_logit4, type = "response")
ranking_pred_new4 <- as.numeric(prob_est_new4 > 0.5) 

TP_new4 <- sum((ranking_pred_new4 == 1) & (ranking == 1))  # True positive
TN_new4 <- sum((ranking_pred_new4 == 0) & (ranking == 0))  # True negative
FP_new4 <- sum((ranking_pred_new4 == 1) & (ranking == 0))  # False positive
FN_new4 <- sum((ranking_pred_new4 == 0) & (ranking == 1))  # False negative

sensitivity_new4 <- TP_new4/(TP_new4 + FN_new4)
specificity_new4 <- TN_new4/(FP_new4 + TN_new4)

HM_new4 <- sensitivity_new4 + specificity_new4
HM_new4

#12. Quality of the final model

#get the predictions from the model
prob_est_final <- predict(new_logit4, type = "response")
ranking_pred_final <- as.numeric(prob_est_final > 0.5) 

TP_final <- sum((ranking_pred_final == 1) & (ranking == 1))  # True positive
TN_final <- sum((ranking_pred_final == 0) & (ranking == 0))  # True negative
FP_final <- sum((ranking_pred_final == 1) & (ranking == 0))  # False positive
FN_final <- sum((ranking_pred_final == 0) & (ranking == 1))  # False negative

sensitivity_final <- TP_final/(TP_final + FN_final)
specificity_final <- TN_final/(FP_final + TN_final)

HM_final <- sensitivity_final + specificity_final
HM_final

true_pred_final <- TP_final + TN_final              # total number of correct predictions
true_pred_share_final <- true_pred_final / n        # share of correct predictions

rocCurve_final <- roc(ranking ~ predict(new_logit4, type=c("response")), data = df)
AUC_final <- rocCurve_final$auc   # area under the curve
plot(rocCurve_final)

