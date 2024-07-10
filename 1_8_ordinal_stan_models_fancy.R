# Pre-requisites ---------------------------------------------------------------
rm(list=ls(all.names = TRUE))
# load(file.path("r_output_enc","workspace_justimputation.Rdata"))

source("0_2_load_packages.R")

imputed_data_list =  readRDS(file=file.path("r_output_enc","sensitive_imputed_data_list.Rdata"))

library(rstan)
library(parallel)

length(imputed_data_list)

# imputed_data_list = imputed_data_list[1:8]

# Function to simplify running cmdstanmodels


# Convert year data to groups --------------------------------------------------

for (i in 1:length(imputed_data_list)){
  imputed_data_list[[i]]$X1010_year_grouped = forcats::fct_recode(
    imputed_data_list[[i]]$X1010_year,
    KS2 = "Y05",
    KS2 = "Y06",
    KS3 = "Y07",
    KS3 = "Y08",
    KS3 = "Y09",
    KS4 = "Y10",
    KS4 = "Y11",
    KS5 = "Y12",
    KS5 = "Y13"
  )
}

# Arguments for functions for testing purposes ---------------------------------
# input_dat = imputed_data_list[[1]]
# out = "rcads_dep_ss"
# fixed_predictors = predictors_secondary
# transform_outcome_function = ordinalise
# re_factor_1   = "school_ID_new"
# re_factor_2   = "X1030_age"
# re_factor_3   = "X1040_ethnicity"
# re_factor_4   = "X1020_gender"

# Warning = ages/year groups and school ids do not match up to the numbers used in the stan code!

# Data analysis functions ------------------------------------------------------

plus_one = function(x){return(x+1)}

ordinalise = function(x){return(match(x, sort(unique(x))))}

safe_vb <- function(...) {
  tryCatch({
    stan_results <- rstan::vb(...)
    return(stan_results)
  }, error = function(e) {
    message("There was an error: ", e$message)
    return(NULL)
  })
}

stan_model_pre = rstan::stan_model(file = "5_ordinal_model3_experimental.stan")   # Compile stan model

create_stan_data_file = function(
    input_dat, 
    out                        = NULL,
    fixed_predictors           = NULL,
    transform_outcome_function = NULL,
    prior_only                 = FALSE,
    re_factor_1                = "school_ID_new",
    re_factor_2                = "X1010_year_grouped",
    re_factor_3                = "X1040_ethnicity",
    re_factor_4                = "X1020_gender"
    
){
  
  input_dat = input_dat[!is.na(input_dat[[out]]),]                              # Remove rows with missing data on outcome (e.g., when just primary or just secondary data-analysis)
  
  Y = transform_outcome_function(input_dat[,out])
  
  
  
  dat_stan = list(
    N           = nrow(input_dat),
    Y           = Y,
    nthres      = length(unique(Y))-1,
    K           = length(fixed_predictors), # Number of population level effects
    X           = input_dat[,fixed_predictors],
    # Kc          = length(fixed_predictors),
    # school-level effects
    M_1         = 1,                                                              # Number of coefficients per level 1 unit
    
    J_1         = match(input_dat[,re_factor_1], sort(unique(input_dat[,re_factor_1]))),                                            
    J_2         = match(input_dat[,re_factor_2], sort(unique(input_dat[,re_factor_2]))),                                            
    J_3         = match(input_dat[,re_factor_3], sort(unique(input_dat[,re_factor_3]))),                                            
    J_4         = match(input_dat[,re_factor_4], sort(unique(input_dat[,re_factor_4]))),                                            
   
    N_1         = length(unique(input_dat[,re_factor_1])),
    N_2         = length(unique(input_dat[,re_factor_2])),
    N_3         = length(unique(input_dat[,re_factor_3])),
    N_4         = length(unique(input_dat[,re_factor_4])),
    
    Z_1_1       = rep(1, nrow(input_dat)),                                          # Level 1 covariate (intercept)
    # age-level effects 
    n_random_effects = 7,                                                              # Number of coefficients per level 2 unit (i.e. number of predictors, intercept + 6 coefficients  )
    Z_2_1       = rep(1, nrow(input_dat)),
    Z_2_2       = input_dat$X1440_foodpovSome,
    Z_2_3       = input_dat$X1440_foodpovOften,
    Z_2_4       = input_dat$X1470_foodpovSome,
    Z_2_5       = input_dat$X1470_foodpovOften,
    Z_2_6       = input_dat$X1500_foodpovSome,
    Z_2_7       = input_dat$X1500_foodpovOften,
    
    n_re_cors        = 21,                                                   # Number of random effect correlations (7^2-7)/2
    n_random_effects = 7,                                                    # 7 random effects - 1 intercept and 6 coefficients
    
    prior_only  = FALSE,
    conc        = 1/(0.8 + 0.35 * max(length(fixed_predictors), 3))
  )
}

run_stan_model       = function(...){
  
  dat_stan_imputation = create_stan_data_file(...)
  
  # fit model using variational inference 
  
  # if error with vb() return NULL
  
  stan_results = safe_vb(object   = stan_model_pre, 
                         data       = dat_stan_imputation,
                         output_samples = 2000,
                         seed           = 10,
                         tol_rel_obj    = .0005,
                         pars           = c(paste0("Cor_", 1:4),
                                            paste0("L_",   1:4),
                                            paste0("z_",   1:4)
                                            ),
                         include        = FALSE
                         # chains     = 2,
                         # warmup     = 1000,
                         # iter       = 2000,
                         # cores      = 2,
                         # refresh    = 400,
                         # init       = 0.5,
                         # alogorithm = "meanfield"
  )                   
  print("completed")
  return(stan_results)
}

# Run analysis using parallel loops --------------------------------------------

# parallel::stopCluster(cl)                                                       # close any previously running clusters
numJobs = 8                                                                     # Number of jobs to run simultaneously (each job needs 2 cores)
cl       = parallel::makeCluster(numJobs, type = "PSOCK")                       # Create cluster (FORK not supported on windows)
clusterExport(cl, c("run_stan_model", "imputed_data_list", 
                    "predictors_random_model",
                    "plus_one", "ordinalise", "create_stan_data_file","stan_model_pre", "safe_vb"))
clusterEvalQ(cl, library(rstan))

results = list()

outcomes = c("rcads_dep_ss", "rcads_anx_ss", "swemwbs_ss",  "pt_ss" , "lone_ss" , "scwbs_ss" )

## Track time taken for each analysis ------------------------------------------

time_taken = vector()

## RCADS Depression ------------------------------------------------------------
start_time = Sys.time()

results[["rcads_dep_ss"]] = parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "rcads_dep_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
    # stan_model_path            = "5_ordinal_model2.stan"                        # Path to just intercept model 
  )
})

time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## RCADS Anxiety----------------------------------------------------------------
start_time = Sys.time()

results[["rcads_anx_ss"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "rcads_anx_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})
time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## SWEMWBS ---------------------------------------------------------------------
start_time = Sys.time()

results[["swemwbs_ss"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "swemwbs_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})
time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## pt_ss ---------------------------------------------------------------------
start_time = Sys.time()

results[["pt_ss"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "pt_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})
time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## lone_ss ---------------------------------------------------------------------
start_time = Sys.time()

results[["lone_ss"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "lone_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})
time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## scwbs_ss ---------------------------------------------------------------------
start_time = Sys.time()

results[["scwbs_ss"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "scwbs_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})
time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## cog_ss ---------------------------------------------------------------------
start_time = Sys.time()

results[["cog_ss"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "cog_ss",
    fixed_predictors           = predictors_random_model,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})

time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

saveRDS(results, file = file.path("r_output_enc","ordinal_results_agerandomeffects_complex.RDS"))

sum(time_taken)/4*100/24

# close cluster

stopCluster(cl)


