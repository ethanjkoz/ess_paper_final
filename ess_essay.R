#### LIBRARIES AND LOADING PACKAGES ####
library(tidyverse)     # For data viz and management
library(countrycode)   # For country code conversion
library(psych)         # For Factor Analysis and related plots
library(FactoMineR)    # For PCA
library(factoextra)    # For PCA visualization
library(haven)         # For reading stata files
library(MASS)
library(polycor)
library(ordinal)

data_path <- "D:\\hw\\macs 30205\\ESS\\ess7-9\\ess7-9.dta"
raw <- read_dta(data_path)

context_ess7 = read_dta("D:\\hw\\macs 30205\\ESS\\ESS7MDWe02.2.stata\\ESS7MDWe02.2_F1.dta")
context_ess8 = read_dta("D:\\hw\\macs 30205\\ESS\\ESS8MDWe02.1.stata\\ESS8MDWe02.1_F1.dta")
context_ess9 = read_dta("D:\\hw\\macs 30205\\ESS\\ESS9MDWe03.1.stata\\ESS9MDWe03.1_F1.dta")

#### SELECTING DATA ####
variables_to_keep <- c(
  "name", "essround", "cntry", # admin
  "pplfair", "pplhlp", "ppltrst", # generalized trust 
  # "actrolg", "cptppol", "etapapl", "psppipl", "psppsgv", # civics/politics 
  "lrscale", # ideology
  "stfeco", # satisfied with econ (bad to good)
  "stfgov", # satisfied with gov (bad to good)
  "trstep", "trstun", # trust in multinational bodies
  "trstlgl", "trstplc", "trstplt", "trstprl", "trstprt", # trust in system  
  "vote", 
  "imsmetn", # allow migrants from same ethnic group as majority 
  "imdfetn", # allow migrants from other ethnic group from majority
  "impcntr", # allow migrants from poorer countries outside Europe
  "imbgeco", "imueclt", "imwbcnt", # mig attitudes  
  
  "blgetmg", # belong to majority
  "brncntr", # born in country 
  #"cntbrthc", "ctzcntr", "ctzshipc", 
  "rlgatnd", # religion
  "sclact", "sclmeet", # social activity 
  
  # IND LVL vars
  "gndr", "yrbrn", "agea", "anctry1", "edulvlb", "pdwrk", "uempla",
  #"crpdwk", "anctry2"
  
  "inwyys" # YEAR
  
  # # Other migrant variables for ESS7 
  # "acetalv", "admaimg", "algyplv", "aljewlv", "allbpe", "allbpne",
  # "almuslv", "alpfpe", "alpfpne", "dfegcf", "dfegcon", "dfeghbg", "eimpcnt", "fclcntr", "gvrfgap",
  # "gvtrimg", "icbrnct", "imbleco", "imdetbs", "imdetmr", "imtcjob", "imwbcrm", "lwdscwp",
  # "noimbro", "pplstrd", "qfimchr", "qfimcmt", "qfimedu", "qfimlng", "qfimwht", "qfimwsk",
  # "rfgbfml", "rfgfrpc", "rlgueim", "smctmbe", "smegbhw", "smegbli",
  
  #"reg9_loun_ths_2014"
)


ess7 <- context_ess7 %>%
  dplyr::select(all_of(variables_to_keep), 
                c_immg_tot_2010, c_immg_tot_2011, 
                c_immg_tot_2014, c_immg_tot_2015,
                c_tpopsz_2010, c_tpopsz_2011,
                c_tpopsz_2014, c_tpopsz_2015,
                c_loun_pc_act_2010, c_loun_pc_act_2011, 
                c_loun_pc_act_2012, c_loun_pc_act_2013,
                c_loun_pc_act_2014, c_loun_pc_act_2015) %>%
  mutate(
    # For 5-year average of differences in immigrant population totals
    mig_diff_5y_avg = case_when(
      inwyys == 2014 ~ (c_immg_tot_2014 / c_tpopsz_2014 - 
                          c_immg_tot_2010 / c_tpopsz_2010) / 5,
      inwyys == 2015 ~ (c_immg_tot_2015 / c_tpopsz_2015 - 
                          c_immg_tot_2011 / c_tpopsz_2011) / 5
    ),
    # For 5-year average of native-born unemployment rate
    unemp_5y_avg = case_when(
      inwyys == 2014 ~ (c_loun_pc_act_2010 + c_loun_pc_act_2011 + 
                          c_loun_pc_act_2012 + c_loun_pc_act_2013 + 
                          c_loun_pc_act_2014) / 5,
      inwyys == 2015 ~ (c_loun_pc_act_2011 + c_loun_pc_act_2012 + 
                          c_loun_pc_act_2013 + c_loun_pc_act_2014 + 
                          c_loun_pc_act_2015) / 5
    )
  ) %>%
  # Drop unnecessary columns
  dplyr::select(-c(c_immg_tot_2010, c_immg_tot_2011, 
                   c_immg_tot_2014, c_immg_tot_2015,
                   c_tpopsz_2010, c_tpopsz_2011,
                   c_tpopsz_2014, c_tpopsz_2015,
                   c_loun_pc_act_2010, c_loun_pc_act_2011, 
                   c_loun_pc_act_2012, c_loun_pc_act_2013,
                   c_loun_pc_act_2014, c_loun_pc_act_2015)) %>%
  na.omit()


ess8 <- context_ess8 %>%
  dplyr::select(all_of(variables_to_keep), 
                c_immg_tot_2012, c_immg_tot_2013, 
                c_immg_tot_2016, c_immg_tot_2017,
                c_tpopsz_2012, c_tpopsz_2013,
                c_tpopsz_2016, c_tpopsz_2017,
                c_loun_pc_act_2012, c_loun_pc_act_2013, 
                c_loun_pc_act_2014, c_loun_pc_act_2015,
                c_loun_pc_act_2016, c_loun_pc_act_2017) %>%
  mutate(
    # For 5-year average of differences in immigrant population totals
    mig_diff_5y_avg = case_when(
      inwyys == 2016 ~ (c_immg_tot_2016 / c_tpopsz_2016 - 
                          c_immg_tot_2012 / c_tpopsz_2012) / 5,
      inwyys == 2017 ~ (c_immg_tot_2017 / c_tpopsz_2017 - 
                          c_immg_tot_2013 / c_tpopsz_2013) / 5
    ),
    # For 5-year average of native-born unemployment rate
    unemp_5y_avg = case_when(
      inwyys == 2016 ~ (c_loun_pc_act_2012 + c_loun_pc_act_2013 + 
                          c_loun_pc_act_2014 + c_loun_pc_act_2015 + 
                          c_loun_pc_act_2016) / 5,
      inwyys == 2017 ~ (c_loun_pc_act_2013 + c_loun_pc_act_2014 + 
                          c_loun_pc_act_2015 + c_loun_pc_act_2016 + 
                          c_loun_pc_act_2017) / 5
    )
  ) %>%
  # Drop unnecessary columns
  dplyr::select(-c(c_immg_tot_2012, c_immg_tot_2013, 
                   c_immg_tot_2016, c_immg_tot_2017,
                   c_tpopsz_2012, c_tpopsz_2013,
                   c_tpopsz_2016, c_tpopsz_2017,
                   c_loun_pc_act_2012, c_loun_pc_act_2013, 
                   c_loun_pc_act_2014, c_loun_pc_act_2015,
                   c_loun_pc_act_2016, c_loun_pc_act_2017)) %>%
  na.omit()

ess9 <- context_ess9 %>%
  dplyr::select(all_of(variables_to_keep), 
                c_immg_tot_2014, c_immg_tot_2015, 
                c_immg_tot_2018, c_immg_tot_2019,
                c_tpopsz_2014, c_tpopsz_2015,
                c_tpopsz_2018, c_tpopsz_2019,
                c_loun_pc_act_2014, c_loun_pc_act_2015, 
                c_loun_pc_act_2016, c_loun_pc_act_2017,
                c_loun_pc_act_2018, c_loun_pc_act_2019) %>%
  mutate(
    # For 5-year average of differences in immigrant population totals
    mig_diff_5y_avg = case_when(
      inwyys == 2018 ~ (c_immg_tot_2018 / c_tpopsz_2018 - 
                          c_immg_tot_2014 / c_tpopsz_2014) / 5,
      inwyys == 2019 ~ (c_immg_tot_2019 / c_tpopsz_2019 - 
                          c_immg_tot_2015 / c_tpopsz_2015) / 5
    ),
    # For 5-year average of native-born unemployment rate
    unemp_5y_avg = case_when(
      inwyys == 2018 ~ (c_loun_pc_act_2014 + c_loun_pc_act_2015 + 
                          c_loun_pc_act_2016 + c_loun_pc_act_2017 + 
                          c_loun_pc_act_2018) / 5,
      inwyys == 2019 ~ (c_loun_pc_act_2015 + c_loun_pc_act_2016 + 
                          c_loun_pc_act_2017 + c_loun_pc_act_2018 + 
                          c_loun_pc_act_2019) / 5
    )
  ) %>%
  # Drop unnecessary columns
  dplyr::select(-c(c_immg_tot_2014, c_immg_tot_2015, 
                   c_immg_tot_2018, c_immg_tot_2019,
                   c_tpopsz_2014, c_tpopsz_2015,
                   c_tpopsz_2018, c_tpopsz_2019,
                   c_loun_pc_act_2014, c_loun_pc_act_2015, 
                   c_loun_pc_act_2016, c_loun_pc_act_2017,
                   c_loun_pc_act_2018, c_loun_pc_act_2019)) %>%
  na.omit()

combined_data <- bind_rows(ess7, ess8, ess9)

data <- combined_data %>%
  dplyr::select(all_of(variables_to_keep), mig_diff_5y_avg, unemp_5y_avg) %>%
  mutate(
    anctry1 = case_when(
      anctry1 < 20000 ~ "European",
      anctry1 < 444444 ~ "Non-European",
      anctry1 >= 444444 ~ NA_character_),
    edulvlb = case_when(
      edulvlb < 610 ~ "non-college",
      edulvlb <= 800 ~ "college+",
      edulvlb > 800 ~ NA_character_),
    # factorize
    ppltrst_binary = ifelse(ppltrst > 5, 1, 0),
    ppltrst = factor(ppltrst, ordered = TRUE), # untrust to trust for people
    anctry1 = factor(anctry1, levels = c("European", "Non-European")),
    edulvlb = factor(edulvlb, levels = c("non-college", "college+")),
    blgetmg = factor(blgetmg), # belong to minority
    brncntr = factor(brncntr), # born in country
    gndr = factor(gndr),
    stfgov_binary = ifelse(stfgov > 5, 1, 0),
    stfgov = factor(stfgov, ordered = TRUE), # dis to sat w/ gov
    stfeco_binary = ifelse(stfeco > 5, 1, 0),
    stfeco = factor(stfeco), # dis to sat w/ econ
    
    mig_temp = rowMeans(dplyr::select(., imbgeco, imueclt, imwbcnt), na.rm = TRUE), # average response to questions on immigrant effect sentiment
    mig_allow = rowMeans(dplyr::select(., imsmetn, imdfetn, impcntr), na.rm = TRUE) # average response to questions on allowing specific migrants
    ) %>% na.omit()

#### FACTOR ANALYSIS ####
# Select variables for factor analysis
trust_vars <- data %>%
  dplyr::select(trstep, trstun, trstlgl, trstplc, trstplt, trstprl, trstprt) %>%
  na.omit()

fa.parallel(trust_vars, fa = "fa", fm = "minres")

eigen_values <- eigen(cor(trust_vars))$values
plot(eigen_values, type = "b", main = "Scree Plot", xlab = "Factor", ylab = "Eigenvalue")
abline(h = 1, col = "red", lty = 2)

# Count how many eigenvalues are > 1
num_factors <- sum(eigen_values > 1)
print(num_factors)


trust_fa <- fa(trust_vars, nfactors = 3, rotate = "promax")

print(trust_fa)

# View the factor loadings
print(trust_fa$loadings)

# Add factor scores to the dataset
data$trust1 <- trust_fa$scores[, 1]
data$trust2 <- trust_fa$scores[, 2]
data$trust3 <- trust_fa$scores[, 3]
fa.diagram(trust_fa)

#### ORIGINAL MODELS, LOGISTIC REGRESSION ####
# Simple model (Model 1)
model1 <- glm(ppltrst_binary ~ mig_diff_5y_avg + agea + anctry1 + edulvlb, 
              data = data, family = binomial)

# Adding more predictors (Model 2)
model2 <- glm(ppltrst_binary ~ mig_diff_5y_avg + agea + anctry1 + edulvlb + 
                mig_temp + mig_allow + lrscale + stfeco_binary + unemp_5y_avg, 
              data = data, family = binomial)

# Adding further complexity (Model 3)
model3 <- glm(ppltrst_binary ~ mig_diff_5y_avg + agea + anctry1 + edulvlb + 
                mig_temp + mig_allow + lrscale + stfeco_binary + unemp_5y_avg + 
                trust1 + trust2 + trust3, 
              data = data, family = binomial)


summary(model1)
summary(model2)
summary(model3)

library(broom)       # For tidying model output
library(kableExtra)  # For creating nice tables

# Extract tidy summaries for each model
model1_tidy <- tidy(model1) %>% mutate(Model = "Model 1")
model2_tidy <- tidy(model2) %>% mutate(Model = "Model 2")
model3_tidy <- tidy(model3) %>% mutate(Model = "Model 3")

# Create a regression table
library(stargazer)
stargazer(
  model1, model2, model3,
  type = "text",                # Output in the console (use "html" or "latex" for other formats)
  title = "Hierarchical Logistic Regression Models",
  dep.var.labels = "High Trust (Binary)",  # Dependent variable label
  
  omit.stat = c("LL", "ser"),   # Omit log-likelihood and standard error
  ci = TRUE,                    # Include confidence intervals
  single.row = TRUE             # Use single row per variable
)


AIC(model1, model2, model3)

anova(model1, model2, test = "Chisq")  # Compare Model 1 and Model 2
anova(model2, model3, test = "Chisq")  # Compare Model 2 and Model 3

# Add predicted probabilities to the data
data <- data %>%
  mutate(
    pred_model1 = predict(model1, type = "response"),
    pred_model2 = predict(model2, type = "response"),
    pred_model3 = predict(model3, type = "response")
  )
library(pROC)

# ROC curve for Model 1
roc1 <- roc(data$ppltrst_binary, predict(model1, type = "response"))
plot(roc1, col = "red", main = "ROC Curves for Hierarchical Models")

# Add ROC for Model 2
roc2 <- roc(data$ppltrst_binary, predict(model2, type = "response"))
plot(roc2, col = "blue", add = TRUE)

# Add ROC for Model 3
roc3 <- roc(data$ppltrst_binary, predict(model3, type = "response"))
plot(roc3, col = "green", add = TRUE)

legend("bottomright", legend = c("Model 1", "Model 2", "Model 3"),
       col = c("red", "blue", "green"), lwd = 2)


# library(ggplot2)
# library(pscl)
# model_metrics <- data.frame(
#   Model = c("Model 1", "Model 2", "Model 3"),
#   AIC = c(AIC(model1), AIC(model2), AIC(model3)),
#   R2 = c(pR2(model1)$McFadden, pR2(model2)$McFadden, pR2(model3)$McFadden)
# )
# 
# # Plot AIC and R2
# ggplot(model_metrics, aes(x = Model)) +
#   geom_bar(aes(y = -AIC), stat = "identity", fill = "blue") +
#   geom_line(aes(y = R2 * 100, group = 1), color = "red") +
#   scale_y_continuous(name = "AIC (-ve)", sec.axis = sec_axis(~./100, name = "McFadden's R2")) +
#   ggtitle("Model Comparison: AIC and R2") +
#   theme_minimal()



data_z <- data %>%
  mutate(
    mig_diff_5y_avg_z = scale(mig_diff_5y_avg), # Standardize migration level
    agea_z = scale(agea),                       # Standardize age
    lrscale_z = scale(lrscale),                 # Standardize left-right scale
    unemp_5y_avg_z = scale(unemp_5y_avg),       # Standardize unemployment
    mig_temp_z = scale(mig_temp),               # Standardize mig temp
    mig_allow_z = scale(mig_allow),             # Standardize mig allow
    trust1_z = scale(trust1),                   # Standardize trust in institution 1
    trust2_z = scale(trust2),                   # Standardize trust in institution 2
    trust3_z = scale(trust3)                    # Standardize trust in institution 3
  )

# Simple model (Model 1)
model1z <- glm(ppltrst_binary ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb, 
               data = data_z, family = binomial)

# Adding more predictors (Model 2)
model2z <- glm(ppltrst_binary ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
                 mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z, 
               data = data_z, family = binomial)

# Adding further complexity (Model 3)
model3z <- glm(ppltrst_binary ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
                 mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z + 
                 trust1_z + trust2_z + trust3_z,
               data = data_z, family = binomial)

summary(model1z)
summary(model2z)
summary(model3z)

#### NEW ANALYSES ####

# Ordinal Regression
model4z <- polr(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb, 
                data = data_z, method="logistic")

# Adding more predictors (Model 2)
model5z <- polr(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
                  mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z, 
                data = data_z, method="logistic")

# Adding further complexity (Model 3)
model6z <- polr(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
                  mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z + 
                  trust1_z + trust2_z + trust3_z,
                data = data_z, method="logistic")
summary(model4z)
summary(model5z)
summary(model6z)


# # Simple model (Model 1)
# model7z <- clmm(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
#                   (1 + mig_diff_5y_avg_z | cntry), 
#                 data = data_z, link="logit")
# 
# # Adding more predictors (Model 2)
# model8z <- clmm(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
#                   (1 + mig_diff_5y_avg_z + unemp_5y_avg_z | cntry) +
#                   mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z, 
#                 data = data_z, link="logit")
# 
# # Adding further complexity (Model 3)
# model9z <- clmm(ppltrst ~ (1 | cntry) + mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
#                   mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z + 
#                   trust1_z + trust2_z + trust3_z + 
#                   (1 + mig_diff_5y_avg_z + unemp_5y_avg_z | cntry),
#                 data = data_z, link="logit")
# 
# summary(model7z)
# summary(model8z)
# summary(model9z)



# # Simple model (Model 1)
# model7z_no_slope <- clmm(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
#                            (1 | cntry), 
#                          data = data_z, link="logit")
# 
# # Adding more predictors (Model 2)
# model8z_no_slope <- clmm(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
#                            (1 | cntry) + 
#                            mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z, 
#                          data = data_z, link="logit")
# 
# # Adding further complexity (Model 3)
# model9z_no_slope <- clmm(ppltrst ~ mig_diff_5y_avg_z + agea_z + anctry1 + edulvlb + 
#                            mig_temp_z + mig_allow_z + lrscale_z + stfeco_binary + unemp_5y_avg_z + 
#                            trust1_z + trust2_z + trust3_z + 
#                            (1 | cntry),
#                          data = data_z, link="logit")
# 
# summary(model7z_no_slope)
# summary(model8z_no_slope)
# summary(model9z_no_slope)


#### Create a regression table ####
library(stargazer)
stargazer(
  model1z, model2z, model3z,
  type = "html",                # Output in the console (use "html" or "latex" for other formats)
  title = "Hierarchical Logistic Regression Models",
  dep.var.labels = "High Trust (Binary)",  # Dependent variable label
  
  omit.stat = c("LL", "ser"),   # Omit log-likelihood and standard error
  ci = TRUE,                    # Include confidence intervals
  single.row = TRUE             # Use single row per variable
)

stargazer(
  model4z, model5z, model6z,
  type = "html",                # Output in the console (use "html" or "latex" for other formats)
  title = "Hierarchical Logistic Regression Models",
  dep.var.labels = "High Trust (Ordinal)",  # Dependent variable label
  
  omit.stat = c("LL", "ser"),   # Omit log-likelihood and standard error
  ci = TRUE,                    # Include confidence intervals
  single.row = TRUE             # Use single row per variable
)

# stargazer(
#   model7z, model8z, model9z,
#   type = "text",                # Output in the console (use "html" or "latex" for other formats)
#   title = "Hierarchical Logistic Regression Models",
#   dep.var.labels = "High Trust (Binary)",  # Dependent variable label
#   
#   omit.stat = c("LL", "ser"),   # Omit log-likelihood and standard error
#   ci = TRUE,                    # Include confidence intervals
#   single.row = TRUE             # Use single row per variable
# )



