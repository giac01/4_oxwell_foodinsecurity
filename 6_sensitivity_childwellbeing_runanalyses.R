
# This is the script used on June 17th 2024 (Time taken = 4.24 days)

rm(list=ls())

source("0_2_load_packages.R")

# imputed_data_list =  readRDS(file=file.path(save_location,"sensitive_imputed_data_list.Rdata"))
imputed_data_list =  readRDS(file=file.path("r_output_enc","sensitive_imputed_data_list.Rdata"))

length(imputed_data_list)

# imputed_data_list = imputed_data_list[1:8]

stan_model_pre = rstan::stan_model(file = "5_ordinal_model1_v3.stan")   # Compile stan model

fit_method_selection = "sampling"

# Warning = ages/year groups and school ids do not match up to the numbers used in the stan code!

# Data analysis functions ------------------------------------------------------

plus_one = function(x){return(x+1)}

ordinalise = function(x){return(match(x, sort(unique(x))))}

create_stan_data_file = function(input_dat, 
                                 out                        = NULL,   # out -> short for outcome! (variable name outcome is taken currently)
                                 fixed_predictors           = NULL,
                                 transform_outcome_function = NULL,
                                 prior_only                 = FALSE,
                                 ppc                        = ppc
                                 
){
  # browser()
  
  input_dat = input_dat[!is.na(input_dat[[out]]),]                              # Remove missing data rows when OUTCOME missing (e.g., when just primary or just secondary data-analysis
  
  Y = transform_outcome_function(input_dat[,out])
  
  dat_stan = list(
    N           = nrow(input_dat),
    Y           = Y,
    nthres      = length(unique(Y))-1,
    K           = length(fixed_predictors), # Number of fixed effects
    X           = input_dat[,fixed_predictors],
    # school-level effects
    N_1         = length(unique(input_dat$school_ID_new)),
    J_1         = match(input_dat$school_ID_new, sort(unique(input_dat$school_ID_new))),    # Grouping indicator for level 1 
    Z_1_1       = rep(1, nrow(input_dat)),                                                  # Level 1 covariate (intercept)
    prior_only  = prior_only,
    ppc         = ppc,                                                                      # If TRUE, run posterior predictive checks
    conc        = 1/(0.8 + 0.35 * max(length(fixed_predictors), 3))
  )
  
  return(dat_stan)
  
}

run_stan_model       = function(ppc, 
                                fit_method                 = "sampling",
                                ... ){
  # browser()
  dat_stan_imputation = create_stan_data_file( ppc, ... )
  
  exclude_pars = c("z_1","r_1_1")                                               # Deleting data on school-level random intercepts - as need to cut down on memory usage
  
  if(ppc == FALSE){ exclude_pars = c(exclude_pars, "Y_rep")}
  
  if (fit_method == "sampling") {
    stan_model = rstan::sampling(
      stan_model_pre,
      data     = dat_stan_imputation,      # named list of data
      chains   = 2,                        # number of Markov chains
      warmup   = 1000,                     # number of warmup iterations per chain
      iter     = 1500,                     # total number of iterations per chain
      cores    = 2,                        # number of cores (could use one per chain)
      refresh  = 400,
      init     = 0.5,
      pars     = exclude_pars,
      include  = FALSE,
      save_warmup = FALSE
    )
  }
  
  if (fit_method == "variational") {
    stan_model = rstan::vb(
      stan_model_pre,
      data        = dat_stan_imputation,      # named list of data
      iter        = 40000,                     # total number of iterations per chain
      tol_rel_obj = .00001,
      init        = 0.5,
      pars        = exclude_pars,
      algorithm   = "meanfield",
      output_samples = 1000
    )
  }
  
  return(stan_model)
  
}



# Run analysis using parallel loops --------------------------------------------

# parallel::stopCluster(cl)                                                     # close any previously running clusters
numJobs  = 7                                           # Number of jobs to run simultaneously (each job needs 2 cores)
cl       = parallel::makeCluster(numJobs, type = "PSOCK")                       # Create cluster (FORK not supported on windows)
# cl       = parallel::makeCluster(numJobs, type = "FORK")                        # Only for linux
clusterExport(cl, c("run_stan_model", "imputed_data_list", 
                    "predictors_allyears", "predictors_secondary", "predictors_primary",
                    "plus_one", "ordinalise", "create_stan_data_file",
                    "stan_model_pre", "fit_method_selection"))
clusterEvalQ(cl, library(rstan))

results = list()

## Track time taken for each analysis ------------------------------------------

time_taken = vector()

imputed_data_list[[1]] %>%
  select(all_of(outcomes)) %>%
  apply(.,2,table)

predictors_only_foodpov = predictors_primary[1:6]


# scwbs_ss ---------------------------------------------------------------------
start_time = Sys.time()

results[["scwbs_ss_onlyfoodpov"]] =  parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model(
    input_dat                  = imputed_data_list[[i]],
    out                        = "scwbs_ss",
    fixed_predictors           = predictors_primary[1:6],
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE,
    ppc                        = (i == 1),
    fit_method                 = fit_method_selection
  )
})

time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))


saveRDS(results, file = "6_childwellbeingresults.RDS")

