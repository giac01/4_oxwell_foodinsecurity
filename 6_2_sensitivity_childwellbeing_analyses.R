# Load Packages and Data -------------------------------------------------------
rm(list=ls())
source("0_2_load_packages.R")
source("0_4_custom_functions.R")

dat_imputed_long  =  readRDS(file=file.path("r_output_enc","sensitive_dat_imputed_long.Rdata"))

stan_ordinal_results = readRDS(file.path("6_childwellbeingresults.RDS"))

# dat_imputed_long = dat_imputed_long %>%
#   filter(X1010_year %in% c("Y07","Y08","Y09","Y10","Y11","Y12","Y13"))

# Create datafame with all draws -----------------------------------------------

all_draws = list()

for (i in seq_along(stan_ordinal_results)){
  
  all_draws[[i]] = list()
  
  for(i_imp in seq_along(stan_ordinal_results[[i]] )){
    all_draws[[i]][[i_imp]]               = posterior::as_draws_df(stan_ordinal_results[[i]][[i_imp]], inc_warmup = FALSE) %>%
      # The first imputation has a bunch of posterior predictive distribution draws 
      select(-starts_with("Y_rep"))
    all_draws[[i]][[i_imp]]$.imputation_n = i_imp
    all_draws[[i]][[i_imp]]$out = outcomes[i]
  }
  
  all_draws[[i]] = bind_rows(all_draws[[i]])
  
}

all_draws = bind_rows(all_draws)

# Combine results --------------------------------------------------------------

emm_stan_fe = list()

## child_wellbeing ----------------------------------------------------------------

emm_stan_fe[["scwbs_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long %>% filter(X1010_year %in% c("Y05","Y06")),
  input_model       = stan_ordinal_results[["scwbs_ss_onlyfoodpov"]],
  fixed_predictors  = predictors_primary[1:6],
  conf_level        = .99,
  outcome_vector    = 15:75,
  my_cdf            = plogis
)


emm_stan_fe$scwbs_ss[[2]]


emm_stan_fe[["scwbs_ss"]] 


## Regression Coefficients -----------------------------------------------------


all_draws %>%
  select(starts_with("b[")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  ggdist::mean_hdci(value)


all_draws %>%
  select(starts_with("b[")) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(pd = as.numeric(bayestestR::p_direction(value)))
