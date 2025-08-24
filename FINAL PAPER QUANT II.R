library(readxl)
library(tidyverse)
library(sensemakr)
library(rdrobust)
library(optmatch)
library(RItools)
library(broom)
library(rcompanion)
library(MASS)
library(DeclareDesign)
library(cobalt)

path_2014 <- "/Users/trevornorton/Desktop/First Year Paper/Data/2014/2014.xlsx"
path_2018 <- "/Users/trevornorton/Desktop/First Year Paper/Data/2018/2018.xlsx"

data_2014 <- read_excel(path_2014)
data_2018 <- read_excel(path_2018)

data_2014 <- data_2014 %>%
  mutate(across(c(D20_REND_DOMICILIO, D21A_PESSOAS_CASA, D1A_IDADE), as.numeric))

data_2018 <- data_2018 %>%
  mutate(across(c(D9, D20, D1A_ID), as.numeric))

# cleaning data -----------------------------------------------------------
cleaned_2014 <- data_2014 %>%
  transmute(
    # bf_beneficiary: 1 = Yes, 0 = No, NA = Don't know / No answer
    bf_beneficiary = case_when(
      PC18 == 1 ~ 1,
      PC18 == 2 ~ 0,
      PC18 %in% c(8, 9) ~ NA_real_
    ),
    
    # hh_income: raw monthly household income
    hh_income = D20_REND_DOMICILIO %>%
      na_if(9999998) %>%
      na_if(9999999),
    
    # hh_size: number of people in household; NA if 0
    hh_size = na_if(D21A_PESSOAS_CASA, 0),
    
    # marital_status: recoded into 4-category scheme per codebook
    # 1 = Married/living with partner, 2 = Widowed,
    # 3 = Divorced/Separated, 4 = Single, NA = DK/NA
    marital_status = case_when(
      D4_EST_CIVIL %in% c(1, 2) ~ 1,
      D4_EST_CIVIL == 6 ~ 2,
      D4_EST_CIVIL %in% c(4, 5) ~ 3,
      D4_EST_CIVIL == 3 ~ 4,
      D4_EST_CIVIL %in% c(98, 99) ~ NA_real_
    ),
    
    # male: 1 = Male, 0 = Female, NA = DK/NR
    male = case_when(
      D2_SEXO == 1 ~ 1,
      D2_SEXO == 2 ~ 0,
      D2_SEXO %in% c(98, 99) ~ NA_real_
    ),
    
    # education: 1–10 scale based on ladder in codebook
    education = D3_ESCOLA,
    
    # ideology: 0–10 scale, NA for 95, 98, 99
    ideology = ifelse(Q12 %in% c(95, 98, 99), NA, Q12),
    
    # vote_pt: 1 = PT (Dilma), 0 = Serra, NA = all other options
    vote_pt = case_when(
      Q6D == 1 ~ 1,
      Q6D == 2 ~ 0,
      Q6D %in% c(50, 60, 98, 99) ~ NA_real_
    ),
    
    # age: numeric, in years
    age = D1A_IDADE
  ) %>%
  # Derived variables
  mutate(
    income_pc = hh_income / hh_size,
    eligible = case_when(
      income_pc <= 154 & marital_status == 1 & hh_size >= 3 ~ 1,
      income_pc <= 154 & marital_status != 1 & hh_size >= 2 ~ 1,
      TRUE ~ 0
    )
  )

# Standardizing 2018 -----------------------------------------------------------
cleaned_2018 <- data_2018 %>%
  transmute(
    # bf_beneficiary: 1 = yes, 0 = no, NA = dk/nr
    bf_beneficiary = case_when(
      P29 == 1 ~ 1,
      P29 == 2 ~ 0,
      P29 %in% c(8, 9) ~ NA_real_
    ),
    
    # hh_income: household monthly income, NA for missing codes
    hh_income = D9 %>%
      na_if(9999998) %>%
      na_if(9999999),
    
    # hh_size: total household members; 0 is invalid
    hh_size = na_if(D20, 0),
    
    # marital_status: 1 = married/partnered, 2 = widowed, 3 = divorced/separated, 4 = single
    marital_status = case_when(
      D4 %in% 1:4 ~ D4,
      D4 %in% c(98, 99) ~ NA_real_
    ),
    
    # male: 1 = male, 0 = female, NA = dk/nr
    male = case_when(
      D2_SEXO == 1 ~ 1,
      D2_SEXO == 2 ~ 0,
      D2_SEXO %in% c(98, 99) ~ NA_real_
    ),
    
    # education: 1–10 scale
    education = D3_ESCOLA,
    
    # age: in years
    age = D1A_ID,
    
    # ideology: 0–10 scale; NA for 95, 98, 99
    ideology = ifelse(Q18 %in% c(95, 98, 99), NA, Q18),
    
    # vote_pt: 1 = Haddad (PT), 0 = Bolsonaro, NA = null/blank/other
    vote_pt = case_when(
      Q12P2_B == 1 ~ 1,
      Q12P2_B == 2 ~ 0,
      Q12P2_B %in% c(50, 60, 97, 98, 99) ~ NA_real_
    ),
    
    # corruption_perceived: 1 = very widespread, ..., 4 = rarely happens; 7–9 = NA
    corruption_perceived = case_when(
      Q7 %in% 1:4 ~ Q7,
      Q7 %in% 7:9 ~ NA_real_
    ),
    
    # lavajato_bias: 1–5 scale; NA for 8/9
    lavajato_bias = case_when(
      P19 %in% 1:5 ~ P19,
      P19 %in% c(8, 9) ~ NA_real_
    ),
    
    # lavajato_effective: 1 = combats corruption, 0 = does not; NA = dk/nr
    lavajato_effective = case_when(
      P20 == 1 ~ 1,
      P20 == 2 ~ 0,
      P20 %in% c(8, 9) ~ NA_real_
    ),
    
    # media_consumption: 1 = very closely, 4 = not at all; 7–8 = NA
    media_consumption = case_when(
      Q2 %in% 1:4 ~ Q2,
      Q2 %in% c(7, 8) ~ NA_real_
    ),
    
    # income_pc: per capita income
    income_pc = hh_income / hh_size,
    
    # eligible: binary rule per 2018 threshold
    eligible = case_when(
      income_pc <= 178 & marital_status == 1 & hh_size >= 3 ~ 1,
      income_pc <= 178 & marital_status != 1 & hh_size >= 2 ~ 1,
      TRUE ~ 0
    )
  )

# matching 2018 (h1) -------------------------------------------------

# prep dataset for matching
match_data_2018 <- cleaned_2018 %>%
  filter(if_all(c(bf_beneficiary, corruption_perceived, education, hh_income), ~ !is.na(.))) %>%
  mutate(treat = bf_beneficiary) %>%
  dplyr::select(treat, corruption_perceived, education, hh_income)

# compute mahalanobis distance
distance_2018 <- match_on(
  treat ~ education + hh_income,
  data = match_data_2018,
  method = "mahalanobis"
)

# run optimal pair matching
pairs_2018 <- pairmatch(distance_2018, data = match_data_2018)

# attach matched pair ids and keep matched sample
matched_data_2018 <- match_data_2018 %>%
  mutate(pair_id = pairs_2018) %>%
  filter(!is.na(pair_id))

# check covariate balance
balanceTest(
  treat ~ education + hh_income + strata(pair_id),
  data = matched_data_2018
)

# estimate att with pair fixed effects
model_att_2018 <- lm(corruption_perceived ~ treat + factor(pair_id), data = matched_data_2018)
summary(model_att_2018)
model_att_2018_h1 <- model_att_2018

# sensitivity analysis on att model
sensitivity_att_2018 <- sensemakr(
  model = model_att_2018,
  treatment = "treat",
  kd = 1:3
)

summary(sensitivity_att_2018)
plot(sensitivity_att_2018)

# match retention summary
match_data_2018 <- match_data_2018 %>%
  mutate(matched = !is.na(pairs_2018))

matched_summary <- match_data_2018 %>%
  group_by(treat, matched) %>%
  summarise(N = n(), .groups = "drop") %>%
  pivot_wider(names_from = matched, values_from = N, names_prefix = "matched_") %>%
  rename(`Treated?` = treat, Matched = matched_TRUE, Unmatched = matched_FALSE)

print(matched_summary)

# compare covariates between matched and unmatched units
compare_covariates <- match_data_2018 %>%
  mutate(match_group = ifelse(matched, "Matched", "Unmatched")) %>%
  group_by(treat, match_group) %>%
  summarise(
    mean_edu = mean(education, na.rm = TRUE),
    mean_income = mean(hh_income, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(treat, match_group)

print(compare_covariates)



# balance table -----------------------------------------------------------
# pre-matching balance check
balance_pre <- xBalance(
  treat ~ education + hh_income,
  data = match_data_2018,
  report = "all"
)

# post-matching balance check with strata
balance_post <- xBalance(
  treat ~ education + hh_income,
  strata = pairs_2018,
  data = match_data_2018,
  report = "all"
)

# convert results to data frames
balance_pre_df <- as.data.frame(balance_pre$results)
balance_post_df <- as.data.frame(balance_post$results)

# rename for clarity
names(balance_pre_df) <- paste0("Pre_", names(balance_pre_df))
names(balance_post_df) <- paste0("Post_", names(balance_post_df))

# combine and round
balance_combined <- cbind(balance_pre_df, balance_post_df)
balance_combined <- round(balance_combined, 3)

print(balance_combined)


# love plot ---------------------------------------------------------------
love.plot(
  treat ~ education + hh_income,
  data = match_data_2018,
  weights = pairs_2018,
  abs = TRUE,
  binary = "std",
  var.order = "unadjusted",
  sample.names = c("Unmatched", "Matched"),
  thresholds = c(m = 0.1),
  title = "Covariate Balance Before and After Matching (2018)",
  var.names = c(
    education = "Education",
    hh_income = "Household Income"
  )
)


# false positive ----------------------------------------------------------
set.seed(123)  # reproducibility
n_sim <- 1000  # number of simulations
p_vals <- numeric(n_sim)  # store p-values

for (i in 1:n_sim) {
  shuffled <- matched_data_2018 %>%
    group_by(pair_id) %>%
    mutate(treat_shuffled = sample(treat)) %>%
    ungroup()
  
  model <- lm(corruption_perceived ~ treat_shuffled + factor(pair_id), data = shuffled)
  p_vals[i] <- summary(model)$coefficients["treat_shuffled", "Pr(>|t|)"]
}

# calculate false positive rate
fp_rate <- mean(p_vals < 0.05)
cat("false positive rate (2018 corruption outcome):", round(fp_rate, 3), "\n")


# power and mse -----------------------------------------------------------
set.seed(123)
n_sim <- 1000
tau <- coef(model_att_2018)["treat"]

# estimate residual sd from original model
resid_sd <- sd(residuals(model_att_2018))

# adjust observed outcome to remove estimated treatment effect
base_outcome_2018 <- matched_data_2018 %>%
  mutate(corruption_base = corruption_perceived - tau * treat)

# storage
estimates_no_noise <- numeric(n_sim)
estimates_with_noise <- numeric(n_sim)

# simulations
for (i in 1:n_sim) {
  # version 1: no noise — shuffle treatment within pairs
  sim1 <- base_outcome_2018 %>%
    group_by(pair_id) %>%
    mutate(treat_shuffled = sample(treat)) %>%
    ungroup() %>%
    mutate(corruption_sim = corruption_base + ifelse(treat_shuffled == 1, tau, 0))
  
  model1 <- lm(corruption_sim ~ treat_shuffled + factor(pair_id), data = sim1)
  estimates_no_noise[i] <- coef(model1)["treat_shuffled"]
  
  # version 2: with random noise
  sim2 <- base_outcome_2018 %>%
    mutate(corruption_sim = corruption_base + ifelse(treat == 1, tau, 0) +
             rnorm(n(), mean = 0, sd = resid_sd))
  
  model2 <- lm(corruption_sim ~ treat + factor(pair_id), data = sim2)
  estimates_with_noise[i] <- coef(model2)["treat"]
}

# summary function
report_metrics <- function(estimates, tau, label) {
  mse <- mean((estimates - tau)^2)
  bias <- mean(estimates) - tau
  variance <- var(estimates)
  
  cat("\n----", label, "----\n")
  cat("estimated mse:", round(mse, 5), "\n")
  cat("bias:", round(bias, 5), "\n")
  cat("variance:", round(variance, 5), "\n")
}

# report results
report_metrics(estimates_no_noise, tau, "no noise (design-based uncertainty)")
report_metrics(estimates_with_noise, tau, "with random noise (realistic)")


# power curve -------------------------------------------------------------
set.seed(123)
taus <- seq(0, 0.5, by = 0.05)
alpha <- 0.05
n_sim <- 1000

# estimate treatment effect and residual sd
tau_hat <- coef(model_att_2018)["treat"]
resid_sd <- sd(residuals(model_att_2018))

# adjust base outcome
base_outcome_2018 <- matched_data_2018 %>%
  mutate(corruption_base = corruption_perceived - tau_hat * treat)

# storage
powers_no_noise <- numeric(length(taus))
powers_with_noise <- numeric(length(taus))

for (j in seq_along(taus)) {
  tau <- taus[j]
  
  # p-value storage
  p_vals_no_noise <- numeric(n_sim)
  p_vals_with_noise <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    # no noise
    sim1 <- base_outcome_2018 %>%
      mutate(corruption_sim = corruption_base + ifelse(treat == 1, tau, 0))
    model1 <- lm(corruption_sim ~ treat + factor(pair_id), data = sim1)
    p_vals_no_noise[i] <- summary(model1)$coefficients["treat", "Pr(>|t|)"]
    
    # with random noise
    sim2 <- base_outcome_2018 %>%
      mutate(corruption_sim = corruption_base +
               ifelse(treat == 1, tau, 0) +
               rnorm(n(), mean = 0, sd = resid_sd))
    model2 <- lm(corruption_sim ~ treat + factor(pair_id), data = sim2)
    p_vals_with_noise[i] <- summary(model2)$coefficients["treat", "Pr(>|t|)"]
  }
  
  # store power
  powers_no_noise[j] <- mean(p_vals_no_noise < alpha)
  powers_with_noise[j] <- mean(p_vals_with_noise < alpha)
}

# plot curves
plot(taus, powers_no_noise, type = "b", pch = 19, col = "black",
     ylim = c(0, 1), xlab = "true effect size (τ)",
     ylab = "statistical power", main = "power curves (2018: no noise vs. with noise)")
lines(taus, powers_with_noise, type = "b", pch = 17, col = "blue", lty = 2)
abline(h = 0.8, col = "red", lty = 2)
legend("bottomright", legend = c("no noise", "with random noise"),
       col = c("black", "blue"), pch = c(19, 17), lty = c(1, 2))

# power table
power_table_2018 <- data.frame(
  Tau = taus,
  Power_No_Noise = round(powers_no_noise, 3),
  Power_With_Noise = round(powers_with_noise, 3)
)

print(power_table_2018)


## matching 2014 (h2) -------------------------------------------------
match_data <- cleaned_2014 %>%
  filter(across(c(bf_beneficiary, vote_pt, male, education, hh_income), ~ !is.na(.))) %>%
  mutate(treat = bf_beneficiary) %>%
  dplyr::select(treat, vote_pt, male, education, hh_income)

# compute mahalanobis distance
distance <- match_on(
  treat ~ male + education + hh_income,
  data = match_data,
  method = "mahalanobis"
)

# run optimal pair matching
pairs <- pairmatch(distance, data = match_data)

# attach matched pair ids and keep matched sample
matched_data <- match_data %>%
  mutate(pair_id = pairs) %>%
  filter(!is.na(pair_id))

# check covariate balance
balanceTest(
  treat ~ male + education + hh_income + strata(pair_id),
  data = matched_data
)

# estimate att (treated vs control within pairs)
model_att <- lm(vote_pt ~ treat + factor(pair_id), data = matched_data)
summary(model_att)
model_att_2014_h2 <- model_att

# sensitivity analysis on att model
sensitivity_att <- sensemakr(
  model = model_att,
  treatment = "treat",
  kd = 1:3
)

# summarize and plot
summary(sensitivity_att)
plot(sensitivity_att)

# match retention summary
match_data <- match_data %>%
  mutate(matched = !is.na(pairs))

match_summary_2014 <- match_data %>%
  group_by(treat, matched) %>%
  summarise(N = n(), .groups = "drop") %>%
  pivot_wider(names_from = matched, values_from = N, names_prefix = "matched_") %>%
  rename(`Treated?` = treat, Matched = matched_TRUE, Unmatched = matched_FALSE)

print(match_summary_2014)

# compare covariate means (matched vs unmatched)
compare_covariates_2014 <- match_data %>%
  mutate(matched_group = ifelse(matched, "Matched", "Unmatched")) %>%
  group_by(treat, matched_group) %>%
  summarise(across(c(male, education, hh_income), mean, na.rm = TRUE), n = n(), .groups = "drop")

print(compare_covariates_2014)


# balance table -----------------------------------------------------------

# pre-match balance
balance_pre_2014 <- xBalance(
  treat ~ male + education + hh_income,
  data = match_data,
  report = "all"
)

# post-match balance
balance_post_2014 <- xBalance(
  treat ~ male + education + hh_income,
  strata = pairs,
  data = match_data,
  report = "all"
)

# convert to data frames
balance_pre_df_2014 <- as.data.frame(balance_pre_2014$results)
balance_post_df_2014 <- as.data.frame(balance_post_2014$results)

# rename columns
names(balance_pre_df_2014) <- paste0("Pre_", names(balance_pre_df_2014))
names(balance_post_df_2014) <- paste0("Post_", names(balance_post_df_2014))

# combine and round
balance_combined_2014 <- cbind(balance_pre_df_2014, balance_post_df_2014)
balance_combined_2014 <- round(balance_combined_2014, 3)

# print result
print(balance_combined_2014)


# love plot ---------------------------------------------------------------
love.plot(
  x = pairs,  
  formula = treat ~ male + education + hh_income,
  data = match_data,
  abs = TRUE,
  binary = "std",
  var.order = "unadjusted",
  sample.names = c("Unmatched", "Matched"),
  thresholds = c(m = 0.1),
  title = "covariate balance before and after matching (2014)",
  var.names = c(
    male = "Male",
    education = "Education",
    hh_income = "Household Income"
  )
)

# false positive ----------------------------------------------------------
set.seed(123) 
n_sim <- 1000  # number of simulations
p_vals <- numeric(n_sim)  # store p-values

for (i in 1:n_sim) {
  # shuffle treatment within pairs
  shuffled <- matched_data %>%
    group_by(pair_id) %>%
    mutate(treat_shuffled = sample(treat)) %>%
    ungroup()
  
  # placebo att with shuffled treatment
  model <- lm(vote_pt ~ treat_shuffled + factor(pair_id), data = shuffled)
  p_vals[i] <- summary(model)$coefficients["treat_shuffled", "Pr(>|t|)"]
}

# false positive rate
fp_rate <- mean(p_vals < 0.05)
cat("false positive rate (2014 pt vote outcome):", round(fp_rate, 3), "\n")

# Power and MSE -----------------------------------------------------------
set.seed(123)
n_sim <- 1000
alpha <- 0.05
taus <- seq(0, 0.5, by = 0.05)

tau_hat <- coef(model_att)["treat"]
resid_sd <- sd(residuals(model_att))

# create baseline outcome by removing estimated effect
base_outcome_2014 <- matched_data %>%
  mutate(vote_base = vote_pt - tau_hat * treat)

# simulate estimates under design-based uncertainty (no noise)
estimates_no_noise <- replicate(n_sim, {
  sim <- base_outcome_2014 %>%
    group_by(pair_id) %>%
    mutate(treat_shuffled = sample(treat)) %>%
    ungroup() %>%
    mutate(vote_sim = vote_base + ifelse(treat_shuffled == 1, tau_hat, 0))
  
  model <- lm(vote_sim ~ treat_shuffled + factor(pair_id), data = sim)
  coef(model)["treat_shuffled"]
})

# simulate estimates under outcome noise (realistic scenario)
estimates_with_noise <- replicate(n_sim, {
  sim <- base_outcome_2014 %>%
    mutate(vote_sim = vote_base + ifelse(treat == 1, tau_hat, 0) +
             rnorm(n(), mean = 0, sd = resid_sd))
  
  model <- lm(vote_sim ~ treat + factor(pair_id), data = sim)
  coef(model)["treat"]
})

# summarize performance
report_metrics <- function(estimates, true_tau, label) {
  mse <- mean((estimates - true_tau)^2)
  bias <- mean(estimates) - true_tau
  variance <- var(estimates)
  cat("----", label, "----\n")
  cat("Estimated MSE:", round(mse, 5), "\n")
  cat("Bias:", round(bias, 5), "\n")
  cat("Variance:", round(variance, 5), "\n\n")
}

report_metrics(estimates_no_noise, tau_hat, "No Noise (Design-Based Uncertainty)")
report_metrics(estimates_with_noise, tau_hat, "With Random Noise (Realistic Outcome Variation)")


# power curve -------------------------------------------------------------
set.seed(123)
n_sim <- 1000
alpha <- 0.05
taus <- seq(0, 0.5, by = 0.05)

tau_hat <- coef(model_att)["treat"]
resid_sd <- sd(residuals(model_att))

base_outcome_2014 <- matched_data %>%
  mutate(vote_base = vote_pt - tau_hat * treat)

powers_no_noise <- numeric(length(taus))
powers_with_noise <- numeric(length(taus))

for (j in seq_along(taus)) {
  tau <- taus[j]
  
  # noiseless
  pvals_nn <- replicate(n_sim, {
    sim <- base_outcome_2014 %>%
      mutate(vote_sim = vote_base + ifelse(treat == 1, tau, 0))
    model <- lm(vote_sim ~ treat + factor(pair_id), data = sim)
    summary(model)$coefficients["treat", "Pr(>|t|)"]
  })
  powers_no_noise[j] <- mean(pvals_nn < alpha)
  
  # with noise
  pvals_noise <- replicate(n_sim, {
    sim <- base_outcome_2014 %>%
      mutate(vote_sim = vote_base + ifelse(treat == 1, tau, 0) +
               rnorm(n(), mean = 0, sd = resid_sd))
    model <- lm(vote_sim ~ treat + factor(pair_id), data = sim)
    summary(model)$coefficients["treat", "Pr(>|t|)"]
  })
  powers_with_noise[j] <- mean(pvals_noise < alpha)
}

plot(taus, powers_no_noise, type = "b", pch = 19, col = "black", ylim = c(0, 1),
     xlab = "True Effect Size (τ)", ylab = "Statistical Power", main = "Power Curves (2014 PT Vote)")
lines(taus, powers_with_noise, type = "b", pch = 17, col = "blue", lty = 2)
abline(h = 0.8, col = "red", lty = 2)
legend("bottomright", legend = c("No Noise", "With Random Noise"),
       col = c("black", "blue"), pch = c(19, 17), lty = c(1, 2))

power_table_2014 <- data.frame(
  Tau = taus,
  Power_No_Noise = round(powers_no_noise, 3),
  Power_With_Noise = round(powers_with_noise, 3)
)

print(power_table_2014)


# matching 2018 (H2) -------------------------------------------------
match_data_2018 <- cleaned_2018 %>%
  filter(if_all(c(bf_beneficiary, vote_pt, male, education, hh_income, corruption_perceived), ~ !is.na(.))) %>%
  mutate(treat = bf_beneficiary) %>%
  dplyr::select(treat, vote_pt, male, education, hh_income, corruption_perceived)

# compute Mahalanobis distance
distance_2018 <- match_on(
  treat ~ male + education + hh_income,
  data = match_data_2018,
  method = "mahalanobis"
)

# perform optimal pair matching
pairs_2018 <- pairmatch(distance_2018, data = match_data_2018)

# attach matched pair IDs and retain matched sample
matched_data_2018 <- match_data_2018 %>%
  mutate(pair_id = pairs_2018) %>%
  filter(!is.na(pair_id))

# covariate balance check
balanceTest(
  treat ~ male + education + hh_income + strata(pair_id),
  data = matched_data_2018
)

# estimate ATT using matched-pair fixed effects
model_att_2018 <- lm(vote_pt ~ treat + factor(pair_id), data = matched_data_2018)
summary(model_att_2018)
model_att_2018_h2 <- model_att_2018

# estimate ATT using robust standard errors with matched-pair fixed effects
model_att_2018_robust <- lm_robust(vote_pt ~ treat + factor(pair_id), data = matched_data_2018)
summary(model_att_2018_robust)

# sensitivity analysis based on ATT model with fixed effects
sensitivity_att_2018 <- sensemakr(
  model = model_att_2018,
  treatment = "treat",
  kd = 1:3
)

# summarize and plot sensitivity analysis
summary(sensitivity_att_2018)
plot(sensitivity_att_2018)

# evaluate match retention for 2018
match_data_2018 <- match_data_2018 %>%
  mutate(matched = !is.na(pairs_2018))

# summarize counts of matched/unmatched by treatment
match_summary_2018 <- match_data_2018 %>%
  group_by(treat, matched) %>%
  summarise(N = n(), .groups = "drop") %>%
  pivot_wider(names_from = matched, values_from = N, names_prefix = "matched_") %>%
  rename(`Treated?` = treat, Matched = matched_TRUE, Unmatched = matched_FALSE)

print(match_summary_2018)

# compare covariate means for unmatched vs matched
compare_covariates_2018 <- match_data_2018 %>%
  mutate(matched_group = ifelse(matched, "Matched", "Unmatched")) %>%
  group_by(treat, matched_group) %>%
  summarise(across(c(male, education, hh_income), mean, na.rm = TRUE), n = n(), .groups = "drop")

print(compare_covariates_2018)

# estimate model with treatment-corruption interaction
model_interaction_2018 <- lm(vote_pt ~ treat * corruption_perceived + factor(pair_id), data = matched_data_2018)
summary(model_interaction_2018)

# sensitivity analysis based on interaction model
sensitivity_interaction_2018 <- sensemakr(
  model = model_interaction_2018,
  treatment = "treat:corruption_perceived",
  kd = 1:3
)

summary(sensitivity_interaction_2018)
plot(sensitivity_interaction_2018)


# balance table -----------------------------------------------------------
# pre-matching balance
balance_pre <- xBalance(
  treat ~ male + education + hh_income,
  data = match_data_2018,
  report = c("all")
)

# post-matching balance (with matched pairs as strata)
balance_post <- xBalance(
  treat ~ male + education + hh_income,
  strata = pairs_2018,
  data = match_data_2018,
  report = c("all")
)

balance_pre_df <- as.data.frame(balance_pre$results)
balance_post_df <- as.data.frame(balance_post$results)

names(balance_pre_df) <- paste0("Pre_", names(balance_pre_df))
names(balance_post_df) <- paste0("Post_", names(balance_post_df))

balance_combined <- cbind(balance_pre_df, balance_post_df)

balance_combined <- round(balance_combined, 3)

print(balance_combined)


# love plot ---------------------------------------------------------------
love.plot(
  x = pairs_2018,
  formula = treat ~ male + education + hh_income,
  data = match_data_2018,
  abs = TRUE,
  binary = "std",
  var.order = "unadjusted",
  sample.names = c("Unmatched", "Matched"),
  thresholds = c(m = 0.1),
  title = "Covariate Balance Before and After Matching (2018 – Vote Choice)",
  var.names = c(
    male = "Male",
    education = "Education",
    hh_income = "Household Income"
  )
)

# false positive ----------------------------------------------------------
set.seed(123)
n_sim <- 1000
p_vals_fp_2018 <- numeric(n_sim)

for (i in 1:n_sim) {
  # shuffle treatment labels within matched pairs
  shuffled <- matched_data_2018 %>%
    group_by(pair_id) %>%
    mutate(treat_shuffled = sample(treat)) %>%
    ungroup()
  
  # estimate placebo ATT
  model_fp <- lm(vote_pt ~ treat_shuffled + factor(pair_id), data = shuffled)
  p_vals_fp_2018[i] <- summary(model_fp)$coefficients["treat_shuffled", "Pr(>|t|)"]
}

#false pos
fp_rate_2018 <- mean(p_vals_fp_2018 < 0.05)
cat("False positive rate (2018 PT vote outcome):", round(fp_rate_2018, 3), "\n")


# power and mse -----------------------------------------------------------
set.seed(123)
n_sim <- 1000
alpha <- 0.05
taus <- seq(0, 0.5, by = 0.05)

# estimate tau from actual model
tau_hat_2018 <- coef(model_att_2018)["treat"]

# residual SD for noise simulation
resid_sd_2018 <- sd(residuals(model_att_2018))

# create a base outcome with estimated effect removed
base_outcome_2018 <- matched_data_2018 %>%
  mutate(vote_base = vote_pt - treat * tau_hat_2018)

# --- (a) No Noise (Design-based uncertainty: shuffle treatment within pairs)
estimates_no_noise_2018 <- replicate(n_sim, {
  sim <- base_outcome_2018 %>%
    group_by(pair_id) %>%
    mutate(treat_shuffled = sample(treat)) %>%
    ungroup() %>%
    mutate(vote_sim = vote_base + ifelse(treat_shuffled == 1, tau_hat_2018, 0))
  
  model <- lm(vote_sim ~ treat_shuffled + factor(pair_id), data = sim)
  coef(model)["treat_shuffled"]
})

# --- (b) With Noise (Realistic outcome variation)
estimates_with_noise_2018 <- replicate(n_sim, {
  sim <- base_outcome_2018 %>%
    mutate(vote_sim = vote_base + ifelse(treat == 1, tau_hat_2018, 0) +
             rnorm(n(), mean = 0, sd = resid_sd_2018))
  
  model <- lm(vote_sim ~ treat + factor(pair_id), data = sim)
  coef(model)["treat"]
})

# function to report metrics
report_metrics <- function(estimates, true_tau, label) {
  mse <- mean((estimates - true_tau)^2)
  bias <- mean(estimates) - true_tau
  variance <- var(estimates)
  cat("----", label, "----\n")
  cat("Estimated MSE:", round(mse, 5), "\n")
  cat("Bias:", round(bias, 5), "\n")
  cat("Variance:", round(variance, 5), "\n\n")
}

report_metrics(estimates_no_noise_2018, tau_hat_2018, "No Noise (Design-Based Uncertainty)")
report_metrics(estimates_with_noise_2018, tau_hat_2018, "With Random Noise (Realistic Outcome Variation)")

# power curve -------------------------------------------------------------
set.seed(123)
taus <- seq(0, 0.5, by = 0.05)
n_sim <- 1000
alpha <- 0.05
tau_hat_2018 <- coef(model_att_2018)["treat"]
resid_sd_2018 <- sd(residuals(model_att_2018))

# remove estimated effect for baseline
base_outcome_2018 <- matched_data_2018 %>%
  mutate(vote_base = vote_pt - treat * tau_hat_2018)

# storage
powers_no_noise_2018 <- numeric(length(taus))
powers_with_noise_2018 <- numeric(length(taus))

# simulations
for (j in seq_along(taus)) {
  tau <- taus[j]
  
  # no noise
  pvals_nn <- replicate(n_sim, {
    sim <- base_outcome_2018 %>%
      mutate(vote_sim = vote_base + ifelse(treat == 1, tau, 0))
    model <- lm(vote_sim ~ treat + factor(pair_id), data = sim)
    summary(model)$coefficients["treat", "Pr(>|t|)"]
  })
  powers_no_noise_2018[j] <- mean(pvals_nn < alpha)
  
  # with noise
  pvals_noise <- replicate(n_sim, {
    sim <- base_outcome_2018 %>%
      mutate(vote_sim = vote_base + ifelse(treat == 1, tau, 0) +
               rnorm(n(), mean = 0, sd = resid_sd_2018))
    model <- lm(vote_sim ~ treat + factor(pair_id), data = sim)
    summary(model)$coefficients["treat", "Pr(>|t|)"]
  })
  powers_with_noise_2018[j] <- mean(pvals_noise < alpha)
}

# plot curves
plot(taus, powers_no_noise_2018, type = "b", pch = 19, col = "black", ylim = c(0, 1),
     xlab = "true effect size (τ)", ylab = "statistical power", main = "power curves (2018 pt vote)")
lines(taus, powers_with_noise_2018, type = "b", pch = 17, col = "blue", lty = 2)
abline(h = 0.8, col = "red", lty = 2)
legend("bottomright", legend = c("no noise", "with random noise"),
       col = c("black", "blue"), pch = c(19, 17), lty = c(1, 2))

# power table
power_table_2018 <- data.frame(
  Tau = taus,
  Power_No_Noise = round(powers_no_noise_2018, 3),
  Power_With_Noise = round(powers_with_noise_2018, 3)
)

print(power_table_2018)

# difference between years ------------------------------------------------
# att
est_2014 <- tidy(model_att) %>% filter(term == "treat")
est_2018 <- tidy(model_att_2018) %>% filter(term == "treat")

# estimates and ses
tau_2014 <- est_2014$estimate
se_2014  <- est_2014$std.error

tau_2018 <- est_2018$estimate
se_2018  <- est_2018$std.error

# diff in att and test
tau_diff <- tau_2018 - tau_2014
se_diff  <- sqrt(se_2014^2 + se_2018^2)
z_score  <- tau_diff / se_diff
p_value  <- 2 * (1 - pnorm(abs(z_score)))

# output
cat("att difference (2018 - 2014):", round(tau_diff, 4), "\n")
cat("standard error of difference:", round(se_diff, 4), "\n")
cat("z score:", round(z_score, 3), "\n")
cat("p-value:", round(p_value, 4), "\n")


# diagnostics: income extremes (2018) ----------
diag_2018 <- cleaned_2018 %>%
  filter(!is.na(hh_income), !is.na(bf_beneficiary)) %>%
  mutate(
    z_income = as.numeric(scale(hh_income)), # mean-0, sd-1
    extreme_3sd = abs(z_income) >= 3 # flag at ±3 sd
  )

# counts by treatment
diag_2018 %>%
  group_by(bf_beneficiary, extreme_3sd) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(bf_beneficiary, desc(extreme_3sd)) %>%
  print()

# quick visuals
hist(diag_2018$z_income, breaks = 40,
     main = "2018 Household Income (z-scores)", xlab = "z(hh_income)")
boxplot(diag_2018$z_income, horizontal = TRUE,
        main = "2018 Income (z) — Boxplot")

# diagnostics: income extremes (2014) ----------
diag_2014 <- cleaned_2014 %>%
  filter(!is.na(hh_income), !is.na(bf_beneficiary)) %>%
  mutate(
    z_income = as.numeric(scale(hh_income)),
    extreme_3sd = abs(z_income) >= 3
  )

diag_2014 %>%
  group_by(bf_beneficiary, extreme_3sd) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(bf_beneficiary, desc(extreme_3sd)) %>%
  print()

hist(diag_2014$z_income, breaks = 40,
     main = "2014 Household Income (z-scores)", xlab = "z(hh_income)")
boxplot(diag_2014$z_income, horizontal = TRUE,
        main = "2014 Income (z) — Boxplot")


# att plots ---------------------------------------------------------------

# helper: pull att and se for "treat" from an lm
.att_se <- function(mod, term = "treat") {
  co <- coef(summary(mod))
  c(att = unname(co[term, "Estimate"]), se = unname(co[term, "Std. Error"]))
}

# grab from saved models
h1_2018 <- .att_se(model_att_2018_h1)   # 2018 perceived corruption
h2_2014 <- .att_se(model_att_2014_h2)   # 2014 pt vote
h2_2018 <- .att_se(model_att_2018_h2)   # 2018 pt vote

# assemble vectors
labels   <- c("H1: 2018", "H2: 2014", "H2: 2018")
ATT      <- c(h1_2018["att"], h2_2014["att"], h2_2018["att"])
SE       <- c(h1_2018["se"],  h2_2014["se"],  h2_2018["se"])

ci_mult  <- 1.96  # set to 1 for ±1 se bars
CI_low   <- ATT - ci_mult * SE
CI_high  <- ATT + ci_mult * SE


pos <- seq_along(ATT)
# set up plot region
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))
par(mar = c(4, 10, 3, 2))  # wider left margin for labels

xlim_range <- range(CI_low, CI_high, 0)  # include 0 for reference line
plot(ATT, pos,
     xlim = xlim_range,
     yaxt = "n", ylab = "", xlab = "ATT",
     pch = 19, cex = 1.2,
     main = "Estimated ATT of Bolsa Família (95% CI)")

# ci bars
segments(CI_low, pos, CI_high, pos, lwd = 2)
segments(CI_low, pos - 0.05, CI_low, pos + 0.05, lwd = 2)
segments(CI_high, pos - 0.05, CI_high, pos + 0.05, lwd = 2)

# zero line
abline(v = 0, lty = 2)

# points on top
points(ATT, pos, pch = 19, cex = 1.2)

# y-axis labels
axis(2, at = pos, labels = labels, las = 2)  # las=2 = vertical text