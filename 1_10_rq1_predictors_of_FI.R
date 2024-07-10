# Pre-requisites ---------------------------------------------------------------
rm(list=ls())

source("0_2_load_packages.R")
library(parallel)


imputed_data_list =  readRDS(file=file.path("sensitive_imputed_data_list.Rdata"))

# imputed_data_list = imputed_data_list[1:4]

fi_predictors = c("X1010_Y05","X1010_Y06","X1010_Y07","X1010_Y08" ,          "X1010_Y10","X1010_Y11" ,"X1010_Y12","X1010_Y13",   # YEAR 9 USED AS REFERENCE!! 
                  "X1020_gender_m", "X1020_gender_o",
                  "X1040_ethnicityMixed", "X1040_ethnicityAsian", "X1040_ethnicityBlack" ,"X1040_ethnicityOther",
                  "X1050_born_uk", 
                  "X1060_p_born_uk"
                  )

if (length(which(!(fi_predictors %in% colnames(imputed_data_list[[1]]))))>0) stop("Error - some vars in fi_predictors not in dataset")

# Functions to fit models in rstan ---------------------------------------------

plus_one = function(x){return(x+1)}

ordinalise = function(x){return(match(x, sort(unique(x))))}

create_stan_data_file = function(input_dat, 
                                 out                        = NULL,
                                 fixed_predictors           = NULL,
                                 transform_outcome_function = NULL,
                                 prior_only                 = FALSE
){
  # browser()
  input_dat = input_dat[!is.na(input_dat[[out]]),]                              # Remove missing data rows (e.g., when just primary or just secondary data-analysis
  
  Y = transform_outcome_function(input_dat[,out])
  
  dat_stan = list(
    N           = nrow(input_dat),
    Y           = Y,
    nthres      = length(unique(Y))-1,
    K           = length(fixed_predictors), # Number of population level effects
    X           = input_dat[,fixed_predictors],
    # Kc          = length(fixed_predictors),
    # school-level effects
    N_1         = length(unique(input_dat$school_ID_new)),
    J_1         = match(input_dat$school_ID_new, sort(unique(input_dat$school_ID_new))),                                        # Grouping indicator for level 1 
    prior_only  = FALSE,
    conc        = 1/(0.8 + 0.35 * max(length(fixed_predictors), 3)),
    ppc         = FALSE
  )
}


run_stan_model       = function(...){
  # browser()
  dat_stan_imputation = create_stan_data_file(...)
  
  stan_model <- rstan::sampling(
    object   = stan_model_compiled,          # Stan program
    data     = dat_stan_imputation,      # named list of data
    chains   = 1,                        # number of Markov chains
    warmup   = 1000,                     # number of warmup iterations per chain
    iter     = 1500,                     # total number of iterations per chain
    cores    = 1,                        # number of cores (could use one per chain)
    refresh  = 300,
    init     = 0.5,
    include = FALSE,
    pars    = c("z_1","r_1_1", "Y_rep") 
  )
  
  return(stan_model)
  
}

# FIT STAN MODELS --------------------------------------------------------------
stan_model_compiled = rstan::stan_model(file = "5_ordinal_model1_v3.stan")   # Compile stan model

if (exists("cl")) parallel::stopCluster(cl)                                                       # close any previously running clusters

numJobs = 8                                                                     # Number of jobs to run simultaneously 

cl       = parallel::makeCluster(numJobs, type = "PSOCK")                       # Create cluster (FORK not supported on windows)

parallel::clusterExport(cl, c("run_stan_model", "imputed_data_list", 
                    "fi_predictors",
                    "plus_one", "ordinalise", "create_stan_data_file","stan_model_compiled"))

parallel::clusterEvalQ(cl, library(rstan))

results = list()

outcomes = c("X1440_foodpov","X1470_foodpov","X1500_foodpov")

## Track time taken for each analysis ------------------------------------------

time_taken = vector()


## First Food Poverty Question -------------------------------------------------
start_time = Sys.time()

results[["X1440_foodpov"]] = parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "X1440_foodpov",
    fixed_predictors           = fi_predictors,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})

time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

## Second Food Poverty Question -------------------------------------------------
start_time = Sys.time()

results[["X1470_foodpov"]] = parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "X1470_foodpov",
    fixed_predictors           = fi_predictors,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})

time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))


## Third Food Poverty Question -------------------------------------------------
start_time = Sys.time()

results[["X1500_foodpov"]] = parLapply(cl, 1:length(imputed_data_list), function(i){
  run_stan_model( 
    input_dat                  = imputed_data_list[[i]], 
    out                        = "X1500_foodpov",
    fixed_predictors           = fi_predictors,
    transform_outcome_function = ordinalise,
    prior_only                 = FALSE
  )
})

time_taken = c(time_taken, as.numeric(difftime(Sys.time(), start_time, units = "hours")))

# Export Data-------------------------------------------------------------------

## Save everythign 

# Export Data - takes 342s 
system.time({
saveRDS(results, file = file.path("r_output_enc","1_10_re1_predictors_foodpov.RDS"))
  
})

results2 = results

draws = list()

draws[[1]] = lapply(results2[[1]], function(x) as.data.frame(extract(x)))
draws[[2]] = lapply(results2[[2]], function(x) as.data.frame(extract(x)))
draws[[3]] = lapply(results2[[3]], function(x) as.data.frame(extract(x)))

for(j in 1:3){
  for (i in 1:length(results[[1]])){
    draws[[j]][[i]]$.imp    = i
    draws[[j]][[i]]$outcome = outcomes[j]
  }
}

for(j in 1:3){
  draws[[j]] = do.call("rbind.data.frame",draws[[j]])
}

draws = do.call("rbind.data.frame", draws)

draws = draws %>%
        select(-matches("^z_1\\.\\d{1,3}$")) %>%
        select(-matches("^r_1_1\\.\\d{1,3}$")) %>%
        as.data.frame()


colnames(draws)

system.time({
  write.csv(draws, file = file.path("r_output_enc","1_10_re1_predictors_foodpov_draws.csv"))
  
})


sum(time_taken)/8*100

