# Code to calculate estimated marginal means and contrasts for brms models with "foodpov" variables. 

# input_model = rq2_age_subgroup_models$scwbs_ss[[1]]
# input_data = imputed_data_list_filtered_year[[1]]

# BRMS functions ---------------------------------------------------------------

calc_brms_normal = function(input_model, input_data){
  cov = rownames(brms::fixef(input_model)) %>%
          grep("foodpov", ., invert = TRUE, value = TRUE) %>%
          grep("Intercept", ., invert = TRUE, value = TRUE)
  
  covariate_means = 
  input_data %>%
    mutate(X1020_genderMale  = as.numeric(X1020_gender=="Male"),
           X1020_genderOther = as.numeric(X1020_gender=="Other"),
           X1010_yearY06     = as.numeric(X1010_year=="Y06"),
           X1010_yearY07     = as.numeric(X1010_year=="Y07"),
           X1010_yearY08     = as.numeric(X1010_year=="Y08"),
           X1010_yearY09     = as.numeric(X1010_year=="Y09"),
           X1010_yearY10     = as.numeric(X1010_year=="Y10"),
           X1010_yearY11     = as.numeric(X1010_year=="Y11"),
           X1010_yearY12     = as.numeric(X1010_year=="Y12"),
           X1010_yearY13     = as.numeric(X1010_year=="Y13"),
           X1040_ethnicitymixed = as.numeric(X1040_ethnicity == "mixed"),
           X1040_ethnicityasian = as.numeric(X1040_ethnicity == "asian"),
           X1040_ethnicityblack = as.numeric(X1040_ethnicity == "black"),
           X1040_ethnicityother = as.numeric(X1040_ethnicity == "other"),
           X1430_deprivationOften = as.numeric(X1430_deprivation == "Often"),
           X1430_deprivationSome  = as.numeric(X1430_deprivation == "Some"),
           X1450_deprivationOften = as.numeric(X1450_deprivation == "Often"),
           X1450_deprivationSome  = as.numeric(X1450_deprivation == "Some"),
           X1460_deprivationOften = as.numeric(X1460_deprivation == "Often"),
           X1460_deprivationSome  = as.numeric(X1460_deprivation == "Some"),
           X1480_deprivationOften = as.numeric(X1480_deprivation == "Often"),
           X1480_deprivationSome  = as.numeric(X1480_deprivation == "Some"),
           X1490_deprivationOften = as.numeric(X1490_deprivation == "Often"),
           X1490_deprivationSome  = as.numeric(X1490_deprivation == "Some")) %>%
    select(all_of(cov)) %>%
    apply(.,2, mean)  
  
  # All possible responses (not included for now!)
  food_insecurity_levels = 
    expand.grid(
      X1440_foodpovSome = 0:1, 
      X1440_foodpovOften = 0:1, 
      X1470_foodpovSome = 0:1, 
      X1470_foodpovOften = 0:1, 
      X1500_foodpovSome = 0:1, 
      X1500_foodpovOften = 0:1
    ) %>%
    filter(!((X1440_foodpovSome == 1 & X1440_foodpovOften == 1) |
             (X1470_foodpovSome == 1 & X1470_foodpovOften == 1) |
             (X1500_foodpovSome == 1 & X1500_foodpovOften == 1)))
  
  food_insecurity_levels_3cat = 
    expand.grid(
      X1440_foodpovSome = 0:1, 
      X1440_foodpovOften = 0:1, 
      X1470_foodpovSome = 0:1, 
      X1470_foodpovOften = 0:1, 
      X1500_foodpovSome = 0:1, 
      X1500_foodpovOften = 0:1
    ) %>%
    dplyr::filter(
      (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
    ) %>%
    slice(c(2,3,1))
  
  covariate_means_3 = matrix(rep(covariate_means, each = 3), ncol = length(covariate_means))
  colnames(covariate_means_3) = names(covariate_means)
  
  datagrid_3cat =cbind.data.frame(covariate_means_3, food_insecurity_levels_3cat)
  
  emm_3cat = brms::posterior_epred(
    object = input_model,
    # type = "response",
    newdata = datagrid_3cat,
    conf_level = .99,
    re_formula = NA        # formula containing group-level effects to be considered in the prediction. If NULL (default), include all group-level effects; if NA, include no group-level effects.
  ) 
  
  
}


calc_emm = function(input_model, input_data){
  
   datagrid_3cat = datagrid(
    X1440_foodpovSome = 0:1,
    X1470_foodpovSome = 0:1,
    X1500_foodpovSome = 0:1,
    X1440_foodpovOften= 0:1,
    X1470_foodpovOften= 0:1,
    X1500_foodpovOften= 0:1,
    model = input_model,
    newdata = input_data
  ) %>%
    dplyr::filter(
      (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
    ) %>%
    slice(c(2,3,1))
  
  datagrid_all = datagrid(
    X1440_foodpovSome = 0:1,
    X1470_foodpovSome = 0:1,
    X1500_foodpovSome = 0:1,
    X1440_foodpovOften = 0:1,
    X1470_foodpovOften = 0:1,
    X1500_foodpovOften = 0:1,
    model = input_model,
    newdata = input_data
  ) %>%
    rowwise() %>%
    mutate(x1 = sum(X1440_foodpovSome, X1440_foodpovOften*2),
           x2 = sum(X1470_foodpovSome, X1470_foodpovOften*2),
           x3 = sum(X1500_foodpovSome, X1500_foodpovOften*2)
    ) %>%
    filter(x1<3 & x2<3 & x3<3) %>%
    as.data.frame()
  
  emm_3cat = marginaleffects::predictions(
    model = input_model,
    type = "response",
    newdata = datagrid_3cat,
    conf_level = .99,
    re_formula = NA        # formula containing group-level effects to be considered in the prediction. If NULL (default), include all group-level effects; if NA, include no group-level effects.
  ) 
  
  # Warnings about re_formula can be ingored (see GitHub Issue)
  
  emm_all = marginaleffects::predictions(
    model = input_model,
    type = "response",
    newdata = datagrid_all,
    conf_level = .99,
    re_formula = NA
  ) 
  
  return(list(datagrid_3cat, datagrid_all, emm_3cat, emm_all))
  
}


contrast_calc = function(input_data, input_model, conf_level = .99){
  
  # input_grid  = emm_list[[1]][[3]]
  # input_model = brms_model1_list[[1]]
  # input_data  = dat_imputed_long 
  
  datagrid_3cat = datagrid(
    X1440_foodpovSome = 0:1,
    X1470_foodpovSome = 0:1,
    X1500_foodpovSome = 0:1,
    X1440_foodpovOften= 0:1,
    X1470_foodpovOften= 0:1,
    X1500_foodpovOften= 0:1,
    model = input_model,
    newdata = input_data
  ) %>%
    dplyr::filter(
      (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
    ) %>%
    slice(c(2,3,1))
  
  fit0_draws_sigma = input_model %>%
    as_draws_df(., variable = "sigma", inc_warmup = FALSE) %>%
    select(1) %>%
    as.matrix()
  
  fit0_draws = input_model %>%
    as_draws_df(., variable = "^b_", regex = TRUE, inc_warmup = FALSE) %>%
    dplyr::select(1:(ncol(.)-3)) %>% # Removes chain, iteration and draw column 
    as.matrix()
  
  predictors = gsub("b_","",grep("b_", get_variables(input_model), value = TRUE))[-1]
  
  # Raw data not needed for now! 
  
  # input_data_2 = input_data %>%
  #                select(all_of(predictors)) %>%
  #                tibble::add_column(Intercept = 1, .before = 1)
  
  # Define contrasts 
  
  input_grid_2 = datagrid_3cat %>%
    data.frame() %>%
    # select(-1:-5) %>%
    select(all_of(predictors)) %>%
    tibble::add_column(Intercept = 1, .before = 1)
  
  # Check colnames(input_grid_2) == colnames(fit0_draws)
  if (!identical(colnames(input_grid_2), gsub("b_","",colnames(fit0_draws)))) {warning("STOP")}
  
  contrast_1 = input_grid_2[2,] - input_grid_2[3,]
  contrast_2 = input_grid_2[1,] - input_grid_2[3,]
  contrast_3 = input_grid_2[1,] - input_grid_2[2,]
  
  emm      = as.data.frame(matrix(nrow = nrow(fit0_draws)))
  emm$con1 = as.vector(fit0_draws %*% t(contrast_1))
  emm$con2 = as.vector(fit0_draws %*% t(contrast_2))
  emm$con3 = as.vector(fit0_draws %*% t(contrast_3))
  emm$smd1 = as.vector(emm$con1  / fit0_draws_sigma)
  emm$smd2 = as.vector(emm$con2  / fit0_draws_sigma)
  emm$smd3 = as.vector(emm$con3  / fit0_draws_sigma)
  emm$c1   = as.vector(fit0_draws %*% t(input_grid_2[1,]))
  emm$c2   = as.vector(fit0_draws %*% t(input_grid_2[2,]))
  emm$c3   = as.vector(fit0_draws %*% t(input_grid_2[3,]))
  emm      = emm[-1]
  
  emm_summary = 
    apply(emm,2,function(x) ggdist::mean_hdi(x, .width = conf_level)) %>%
    do.call("rbind.data.frame",.)
  
  return(list(emm, emm_summary, input_grid_2))
}


contrast_calc_ordinal = function(input_data, 
                                 input_model, 
                                 conf_level = .99,
                                 outcome_vector = 0:8,
                                 my_cdf = plogis
                                 ){
  
  # input_data = dat_imputed_long
  # input_model = rq2_lone_ordinal 
  
  suppressWarnings({
    fit0_draws = input_model %>%
      as_draws_df(., inc_warmup = FALSE) %>%
      # dplyr::select(starts_with("b_")) %>% # Removes chain, iteration and draw column 
      as.matrix()
  })
  
  suppressWarnings({
    fit0_draws_nointercept = fit0_draws %>%
      as.data.frame() %>%
      select(starts_with("b_")) %>% 
      select(-contains("Intercept")) %>%
      as.matrix()
  })
  
  suppressWarnings({
    fit0_draws_intercepts = fit0_draws %>%
      as.data.frame() %>%
      select(starts_with("b_")) %>% 
      select(contains("Intercept")) %>% 
      tibble::add_column(`Intercept[0]` = -Inf, .before = 1) %>%
      tibble::add_column(`Intercept[last]` = Inf) %>%
      as.matrix()
  })
  
  # rm(fit0_draws)
  # 
  # head(fit0_draws)
  
  predictors = gsub("b_", "", grep("b_", get_variables(input_model), value = TRUE))
  predictors = grep("Intercept", predictors, value = TRUE, invert = TRUE)
  
  datagrid_3cat = datagrid(
    X1440_foodpovSome = 0:1,
    X1470_foodpovSome = 0:1,
    X1500_foodpovSome = 0:1,
    X1440_foodpovOften= 0:1,
    X1470_foodpovOften= 0:1,
    X1500_foodpovOften= 0:1,
    model = input_model,
    newdata = input_data
  ) %>%
    dplyr::filter(
      (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
        (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
    ) %>%
    slice(c(2,3,1))
  
  # Caution - school id is in here!
  
  
  
  # Define contrasts 
  
  datagrid_3cat = datagrid_3cat %>%
    data.frame() %>%
    select(all_of(predictors)) 
  
  # Check colnames(datagrid_3cat) == colnames(fit0_draws)
  if (!identical(colnames(datagrid_3cat), gsub("b_","",colnames(fit0_draws_nointercept)))) {warning("STOP")}
  
  contrast_1 = datagrid_3cat[2,] - datagrid_3cat[3,]   # Never -> Sometimes
  contrast_2 = datagrid_3cat[1,] - datagrid_3cat[3,]   # Never -> Always
  contrast_3 = datagrid_3cat[1,] - datagrid_3cat[2,]   # Sometimes -> Always
  
    # Estimate outcome probabilities 
  
  outcome_probabilities_never = 
    sapply(1:(ncol(fit0_draws_intercepts)-1), function(i) {
      tau = as.numeric(as.matrix(datagrid_3cat[3,]) %*% t(fit0_draws_nointercept) )
      my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
    }) %>%
    `colnames<-`(paste0("prob_y_",1:ncol(.),"_never"))
  
  outcome_probabilities_sometimes = 
    sapply(1:(ncol(fit0_draws_intercepts)-1), function(i) {
      tau = as.numeric(as.matrix(datagrid_3cat[2,]) %*% t(fit0_draws_nointercept) )
      my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
    }) %>%
    `colnames<-`(paste0("prob_y_",1:ncol(.),"_sometimes"))
  
  outcome_probabilities_always = 
    sapply(1:(ncol(fit0_draws_intercepts)-1), function(i) {
      tau = as.numeric(as.matrix(datagrid_3cat[1,]) %*% t(fit0_draws_nointercept) )
      my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
    }) %>%
    `colnames<-`(paste0("prob_y_",1:ncol(.),"_always"))
  
  outcome_probabilities = cbind.data.frame(outcome_probabilities_never, outcome_probabilities_sometimes, outcome_probabilities_always)
  
  emm      = as.data.frame(matrix(nrow = nrow(fit0_draws)))
  emm$smd1 = as.vector(fit0_draws_nointercept %*% t(contrast_1))
  emm$smd2 = as.vector(fit0_draws_nointercept %*% t(contrast_2))
  emm$smd3 = as.vector(fit0_draws_nointercept %*% t(contrast_2))
  emm$mean1   = outcome_probabilities_always %*% as.matrix(outcome_vector)
  emm$mean2   = outcome_probabilities_sometimes %*% as.matrix(outcome_vector)
  emm$mean3   = outcome_probabilities_never %*% as.matrix(outcome_vector)
  emm$con1 = emm$mean2 - emm$mean3 
  emm$con2 = emm$mean1 - emm$mean3
  emm$con3 = emm$mean1 - emm$mean2
  emm      = emm[-1]
  
  emm_summary = 
    apply(emm,2,function(x) ggdist::mean_hdi(x, .width = conf_level)) %>%
    do.call("rbind.data.frame",.)
  
  return(list(emm, emm_summary, datagrid_3cat))
}


# STAN functions ---------------------------------------------------------------


analyse_stan_models = function(
    input_data, 
    input_model, 
    fixed_predictors  = predictors_allyears,
    conf_level        = .99,
    outcome_vector    = 0:8,
    calc_gender       = FALSE,
    my_cdf            = plogis
){
  # input_data        = dat_imputed_long
  # input_model       = stan_ordinal_results[["rcads_dep_ss"]]
  # fixed_predictors  = predictors_allyears
  # conf_level        = .99
  # outcome_vector    = 0:15
  # my_cdf            = plogis

  # Combine draws across stan models on different multiply imputed datasets  
  input_model_draws = lapply(input_model, function(x) posterior::as_draws_df(x, inc_warmup = FALSE))
  
  for (i in seq_along(input_model)){
    input_model_draws[[i]]$.imputation_n = i
    
    input_model_draws[[i]] = select(input_model_draws[[i]], -starts_with("Y_rep"))
  }
  
  input_model_draws = dplyr::bind_rows(input_model_draws) %>% 
                      as.data.frame()
  
  fit0_draws = input_model_draws %>%
    as.matrix()
  
  fit0_draws_nointercept = input_model_draws %>%
    select(starts_with("b")) %>% 
    select(-contains("Intercept")) %>%
    as.matrix()
  
  fit0_draws_intercepts = input_model_draws %>%
    select(starts_with("b_")) %>% 
    select(contains("Intercept")) %>% 
    tibble::add_column(`Intercept[0]` = -Inf, .before = 1) %>%
    tibble::add_column(`Intercept[last]` = Inf) %>%
    as.matrix()
  
  pred_means = input_data %>%
    select(all_of(fixed_predictors)) %>%
    apply(.,2, mean) 
  
  # Create datagrid with low, medium and high food insecurity 
     # Note the order of food pov variables here - depends on the predictor matrix! 
  datagrid_3cat = cbind.data.frame(pred_means,
                                   pred_means,
                                   pred_means) %>% 
                    t() %>%
                    `rownames<-`((NULL))
  
  datagrid_3cat[,1:6] = matrix(c(0, 1, 0, 1, 0, 1,
                                 1, 0, 1, 0, 1, 0,
                                 0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE) 

  
  # Create contrasts 
  
  if (!identical(colnames(datagrid_3cat), fixed_predictors)) {warning("STOP")}
  
  contrast_1 = datagrid_3cat[2,] - datagrid_3cat[3,]   # Never -> Sometimes
  contrast_2 = datagrid_3cat[1,] - datagrid_3cat[3,]   # Never -> Always
  contrast_3 = datagrid_3cat[1,] - datagrid_3cat[2,]   # Sometimes -> Always
  
  # Estimate outcome probabilities 
  n_outcome_levels = ncol(fit0_draws_intercepts)-1
  
  outcome_probabilities_never = 
    sapply(1:n_outcome_levels, function(i) {
      tau = as.numeric((datagrid_3cat[3,]) %*% t(fit0_draws_nointercept) )      # Caculate latent logistic mean
      my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)  # Caclulate probability of Y = k response  
    }) %>%
    `colnames<-`(paste0("prob_y_",1:ncol(.),"_never"))
  
  outcome_probabilities_sometimes = 
    sapply(1:n_outcome_levels, function(i) {
      tau = as.numeric((datagrid_3cat[2,]) %*% t(fit0_draws_nointercept) )
      my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
    }) %>%
    `colnames<-`(paste0("prob_y_",1:ncol(.),"_sometimes"))
  
  outcome_probabilities_always = 
    sapply(1:n_outcome_levels, function(i) {
      tau = as.numeric((datagrid_3cat[1,]) %*% t(fit0_draws_nointercept) )
      my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
    }) %>%
    `colnames<-`(paste0("prob_y_",1:ncol(.),"_always"))
  
  outcome_probabilities = cbind.data.frame(outcome_probabilities_never, outcome_probabilities_sometimes, outcome_probabilities_always)
  
  # Calculate Estimated Marginal Means
  
  emm         = as.data.frame(matrix(nrow = nrow(fit0_draws)))
  # Differences on the linear predictor term 
  emm$smd1    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_1))
  emm$smd2    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_2))
  emm$smd3    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_3))
  emm$mean1   = outcome_probabilities_always     %*% as.matrix(outcome_vector)
  emm$mean2   = outcome_probabilities_sometimes  %*% as.matrix(outcome_vector)
  emm$mean3   = outcome_probabilities_never      %*% as.matrix(outcome_vector)
  emm$con1    = emm$mean2 - emm$mean3 
  emm$con2    = emm$mean1 - emm$mean3
  emm$con3    = emm$mean1 - emm$mean2
  emm         = emm[-1]
  
  if (identical(my_cdf,plogis)){              # If logistic model, divide mean difference by standard deviation of standard logistic distribution
    emm$smd1 = emm$smd1 / sqrt(pi^2 / 3)
    emm$smd2 = emm$smd2 / sqrt(pi^2 / 3)
    emm$smd3 = emm$smd3 / sqrt(pi^2 / 3)
  }
  
  emm_summary = 
    apply(emm,2,function(x) ggdist::mean_hdci(x, .width = conf_level)) %>%
    do.call("rbind.data.frame",.) 
  
  # Probability of Direction
  
  pd = emm %>%                
    bayestestR::pd() %>%
    as.data.frame() %>%
    pull(pd)
  
  emm_summary$pd = pd
  
  return_list = list(emm, emm_summary, datagrid_3cat, outcome_probabilities)
  
  # Calculate Outcome Probabilities For men and women separtely (for RCADS in particular)
  
  if(calc_gender){
    
    datagrid_3cat_female = datagrid_3cat
    datagrid_3cat_female[,"X1020_gender_m"] = 0
    datagrid_3cat_female[,"X1020_gender_o"] = 0
    
    datagrid_3cat_male = datagrid_3cat
    datagrid_3cat_male[,"X1020_gender_m"] = 1
    datagrid_3cat_male[,"X1020_gender_o"] = 0
    
    outcome_probabilities_never_female = 
      sapply(1:n_outcome_levels, function(i) {
        tau = as.numeric((datagrid_3cat_female[3,]) %*% t(fit0_draws_nointercept) )
        my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
      }) %>%
      `colnames<-`(paste0("prob_y_",1:ncol(.),"_never"))
    
    outcome_probabilities_sometimes_female = 
      sapply(1:n_outcome_levels, function(i) {
        tau = as.numeric((datagrid_3cat_female[2,]) %*% t(fit0_draws_nointercept) )
        my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
      }) %>%
      `colnames<-`(paste0("prob_y_",1:ncol(.),"_sometimes"))
    
    outcome_probabilities_always_female = 
      sapply(1:n_outcome_levels, function(i) {
        tau = as.numeric((datagrid_3cat_female[1,]) %*% t(fit0_draws_nointercept) )
        my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
      }) %>%
      `colnames<-`(paste0("prob_y_",1:ncol(.),"_always"))
    
    outcome_probabilities_female = cbind.data.frame(outcome_probabilities_never_female, outcome_probabilities_sometimes_female, outcome_probabilities_always_female)
    
    outcome_probabilities_never_male = 
      sapply(1:n_outcome_levels, function(i) {
        tau = as.numeric((datagrid_3cat_male[3,]) %*% t(fit0_draws_nointercept) )
        my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
      }) %>%
      `colnames<-`(paste0("prob_y_",1:ncol(.),"_never"))
    
    outcome_probabilities_sometimes_male = 
      sapply(1:n_outcome_levels, function(i) {
        tau = as.numeric((datagrid_3cat_male[2,]) %*% t(fit0_draws_nointercept) )
        my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
      }) %>%
      `colnames<-`(paste0("prob_y_",1:ncol(.),"_sometimes"))
    
    outcome_probabilities_always_male = 
      sapply(1:n_outcome_levels, function(i) {
        tau = as.numeric((datagrid_3cat_male[1,]) %*% t(fit0_draws_nointercept) )
        my_cdf(as.numeric(fit0_draws_intercepts[,i+1]) - tau) - my_cdf(as.numeric(fit0_draws_intercepts[,i]) - tau)
      }) %>%
      `colnames<-`(paste0("prob_y_",1:ncol(.),"_always"))
    
    outcome_probabilities_male = cbind.data.frame(outcome_probabilities_never_male, outcome_probabilities_sometimes_male, outcome_probabilities_always_male)

    return_list = list(
      emm                          = emm,
      emm_summary                  = emm_summary,
      datagrid_3cat                = datagrid_3cat,
      outcome_probabilities        = outcome_probabilities,
      outcome_probabilities_female = outcome_probabilities_female,
      outcome_probabilities_male   = outcome_probabilities_male
    )
    
  }
  
  return(return = return_list)
  
}


analyse_stan_models_re = function(
    input_data, 
    input_model, 
    fixed_predictors  = predictors_allyears,
    conf_level        = .99,
    outcome_vector    = 0:8,
    my_cdf            = plogis
){
  
  did_model_fit = sapply(input_model, function(x) !is.null(x))
  
  # Remove elements of list that did not fit (==NULL)
  # 
  input_model = input_model[did_model_fit]
  
  # Combine draws across stan models on different multiply imputed datasets 
  input_model_draws = lapply(input_model, function(x) posterior::as_draws_df(x, inc_warmup = FALSE))
  for (i in seq_along(input_model)){
    input_model_draws[[i]]$.imputation_n = i
  }
  input_model_draws = do.call("rbind.data.frame",input_model_draws) %>%
    as.data.frame()
  
  fit0_draws = input_model_draws %>%
    as.matrix()
  
  fit0_draws_nointercept = input_model_draws %>%
    select(starts_with("b")) %>% 
    select(-contains("Intercept")) %>%
    as.matrix()
  
  fit0_draws_intercepts = input_model_draws %>%
    select(starts_with("b_")) %>% 
    select(contains("Intercept")) %>% 
    tibble::add_column(`Intercept[0]` = -Inf, .before = 1) %>%
    tibble::add_column(`Intercept[last]` = Inf) %>%
    as.matrix()
  
  datagrid_3cat = input_data %>%
    select(all_of(fixed_predictors)) %>%
    apply(.,2, mean) 
  
  datagrid_3cat = cbind.data.frame(datagrid_3cat,datagrid_3cat,datagrid_3cat) %>% 
    t() %>%
    `rownames<-`((NULL))
  
  # Note the order of food pov variables here - depends on the predictor matrix! 
  datagrid_3cat[,1:6] = matrix(c(0, 1, 0, 1, 0, 1,
                                 1, 0, 1, 0, 1, 0,
                                 0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE) 
  
  n_groups = colnames(fit0_draws) %>%
    grep("^r_2_1\\[\\d+\\]$", ., value = TRUE) %>%
    gsub("^r_2_1\\[(\\d+)\\]$", "\\1", .) %>%
    as.numeric(.) %>%
    max()
  
  # Calculate Beta-Coefficients for each GROUP 
  
  fit0_draws_list = list()          
  for (i in 1:n_groups){
    fit0_draws_list[[i]] = 
      input_model_draws %>%
      select(starts_with("b")) 
    
    # Adjust the beta-coefficients for the 6 foodpov variables, adding the random slope term to each effect
    fit0_draws_list[[i]]$`b[1]` = fit0_draws_list[[i]]$`b[1]` + input_model_draws[[paste0("r_2_2[",i,"]")]]
    fit0_draws_list[[i]]$`b[2]` = fit0_draws_list[[i]]$`b[2]` + input_model_draws[[paste0("r_2_3[",i,"]")]]
    fit0_draws_list[[i]]$`b[3]` = fit0_draws_list[[i]]$`b[3]` + input_model_draws[[paste0("r_2_4[",i,"]")]]
    fit0_draws_list[[i]]$`b[4]` = fit0_draws_list[[i]]$`b[4]` + input_model_draws[[paste0("r_2_5[",i,"]")]]
    fit0_draws_list[[i]]$`b[5]` = fit0_draws_list[[i]]$`b[5]` + input_model_draws[[paste0("r_2_6[",i,"]")]]
    fit0_draws_list[[i]]$`b[6]` = fit0_draws_list[[i]]$`b[6]` + input_model_draws[[paste0("r_2_7[",i,"]")]]
    
    # Adjust intercepts for each group 
    fit0_draws_list[[i]] = fit0_draws_list[[i]] %>%
      mutate_at(vars(starts_with("b_Intercept")), ~ . - input_model_draws[[paste0("r_2_1[",i,"]")]])
    
    # Check this worked!
    # input_model_draws %>%
    #   select(all_of(c("b_Intercept[1]","b_Intercept[2]","b_Intercept[3]","r_2_1[1]"))) %>%
    #   head()
    # 
    # fit0_draws_list[[i]] %>%
    #   select(starts_with("b_Int")) %>%
    #   head()
    
  }
  
  # # Check colnames(datagrid_3cat) == colnames(fit0_draws)
  if (!identical(colnames(datagrid_3cat), fixed_predictors)) {warning("STOP")}

  contrast_1 = datagrid_3cat[2,] - datagrid_3cat[3,]   # Never -> Sometimes
  contrast_2 = datagrid_3cat[1,] - datagrid_3cat[3,]   # Never -> Always
  contrast_3 = datagrid_3cat[1,] - datagrid_3cat[2,]   # Sometimes -> Always

  smd1 = sapply(1:n_groups, function(i) 
    as.vector( as.matrix(select(fit0_draws_list[[i]], -starts_with("b_Intercept"))) %*% as.matrix(contrast_1))
  ) %>%
    `colnames<-`(paste0("smd1_group_",1:n_groups))
  
  smd2 = sapply(1:n_groups, function(i) 
    as.vector( as.matrix(select(fit0_draws_list[[i]], -starts_with("b_Intercept"))) %*% as.matrix(contrast_2))
  ) %>%
    `colnames<-`(paste0("smd2_group_",1:n_groups))
  
  smd3 = sapply(1:n_groups, function(i) 
    as.vector( as.matrix(select(fit0_draws_list[[i]], -starts_with("b_Intercept"))) %*% as.matrix(contrast_3))
  ) %>%
    `colnames<-`(paste0("smd3_group_",1:n_groups))
  
  if (identical(my_cdf,plogis)){              # If logistic model, divide mean difference by standard deviation of standard logistic distribution
    smd1 = smd1 / sqrt(pi^2 / 3)
    smd2 = smd2 / sqrt(pi^2 / 3)
    smd3 = smd3 / sqrt(pi^2 / 3)
  }
  emm = cbind.data.frame(smd1, smd2, smd3)
  
  # # Differences on the linear predictor term 
  # emm$smd1    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_1))
  # emm$smd2    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_2))
  # emm$smd3    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_3))
  # emm$mean1   = outcome_probabilities_always     %*% as.matrix(outcome_vector)
  # emm$mean2   = outcome_probabilities_sometimes  %*% as.matrix(outcome_vector)
  # emm$mean3   = outcome_probabilities_never      %*% as.matrix(outcome_vector)
  # emm$con1    = emm$mean2 - emm$mean3 
  # emm$con2    = emm$mean1 - emm$mean3
  # emm$con3    = emm$mean1 - emm$mean2
  # emm         = emm[-1]
  # 
  # 
  # 
  
  emm_summary =
    apply(emm,2,function(x) ggdist::mean_hdci(x, .width = conf_level)) %>%
    do.call("rbind.data.frame",.)
  
  emm_summary$n_draws = apply(emm,2,function(x) length(x))
  
  emm_summary$n_models = length(which(did_model_fit))
  
  
  # ggdist::mean_hdci(as.numeric(emm$smd2_group_6), .width = conf_level)

  pd = emm %>%                # Probability of direction
    bayestestR::pd() %>%
    as.data.frame() %>%
    pull(pd)

  emm_summary$pd = pd
  # 
  return(list(emm_summary = emm_summary))
}



analyse_stan_models_re_complicated = function(
    input_data, 
    input_model, 
    fixed_predictors  = predictors_allyears,
    conf_level        = .99,
    outcome_vector    = 0:8,
    my_cdf            = plogis
){
  
  did_model_fit = sapply(input_model, function(x) !is.null(x))
  
  # Remove elements of list that did not fit (==NULL)
  # 
  input_model = input_model[did_model_fit]
  
  # Combine draws across stan models on different multiply imputed datasets 
  input_model_draws = lapply(input_model, function(x) posterior::as_draws_df(x, inc_warmup = FALSE))
  for (i in seq_along(input_model)){
    input_model_draws[[i]]$.imputation_n = i
  }
  input_model_draws = do.call("rbind.data.frame",input_model_draws) %>%
    as.data.frame()
  
  # input_model_draws = input_model_draws %>%
  #                     select(-matches("z_[1-4]\\[")) %>%
  #                     select(-matches("L_[1-4]\\["))
  # 
  # fit0_draws = input_model_draws %>%
  #   as.matrix()
  
  fit0_draws_nointercept = input_model_draws %>%
    select(starts_with("b")) %>% 
    select(-contains("Intercept")) %>%
    as.matrix()
  
  fit0_draws_intercepts = input_model_draws %>%
    select(starts_with("b_")) %>% 
    select(contains("Intercept")) %>% 
    tibble::add_column(`Intercept[0]` = -Inf, .before = 1) %>%
    tibble::add_column(`Intercept[last]` = Inf) %>%
    as.matrix()
  
  datagrid_3cat = input_data %>%
    select(all_of(fixed_predictors)) %>%
    apply(.,2, mean) 
  
  datagrid_3cat = cbind.data.frame(datagrid_3cat,datagrid_3cat,datagrid_3cat) %>% 
    t() %>%
    `rownames<-`((NULL))
  
  # Note the order of food pov variables here - depends on the predictor matrix! 
  datagrid_3cat[,1:6] = matrix(c(0, 1, 0, 1, 0, 1,
                                 1, 0, 1, 0, 1, 0,
                                 0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE) 
  
  # Number of distinct YEAR groups
  n_groups_re2 = colnames(input_model_draws) %>%
    grep("^r_2_1\\[\\d+\\]$", ., value = TRUE) %>%
    gsub("^r_2_1\\[(\\d+)\\]$", "\\1", .) %>%
    as.numeric(.) %>%
    max()
  
  # Number of distinct ETHNICITY groups  
  n_groups_re3 = colnames(input_model_draws) %>%
    grep("^r_3_1\\[\\d+\\]$", ., value = TRUE) %>%
    gsub("^r_3_1\\[(\\d+)\\]$", "\\1", .) %>%
    as.numeric(.) %>%
    max()
  
  # Number of distinct GENDER groups
  n_groups_re4 = colnames(input_model_draws) %>%
    grep("^r_4_1\\[\\d+\\]$", ., value = TRUE) %>%
    gsub("^r_4_1\\[(\\d+)\\]$", "\\1", .) %>%
    as.numeric(.) %>%
    max()
  
  n_groups = c(n_groups_re2, n_groups_re3, n_groups_re4)
  
  
  # Calculate Beta-Coefficients for each subgroup 
  
  fit0_draws_list = list()
  for (re_i in 1:3){
    fit0_draws_list[[re_i]] = list()
    
    add_year_8          = 1
    add_ethnicity_white = 1
    add_gender_female   = 1
    
    if (re_i==1){add_year_8 = 0}  # year 8 just means the youngest age group - now might refer to KS2/KS3
    if (re_i==2){add_ethnicity_white = 0}
    if (re_i==3){add_gender_female = 0}
    
    for (i in 1:n_groups[re_i]){
      fit0_draws_list[[re_i]][[i]] = 
        input_model_draws %>%
        select(starts_with("b")) 
      # Adjust the beta-coefficients for the 6 foodpov variables, adding the random slope term to each effect
      # guide to how rstan has labelled the random effects 
      # r_a_b[c]
      # A) 1 - School Random Effect, 2 - Year Random Effect , 3 - Ethnicity Random Effects, 4 - Gender Random Effects
      # B) 1 - Random Intercept, 2 - X1440_foodpovSome 3- X1440_foodpovOften 4 - X1470_foodpovSome 5 - X1470_foodpovOften 6 -  X1500_foodpovSome 7 - X1500_foodpovOften
      # C) Group Indicator (e.g., 1 = Year 5, 2 = Year 6, etc.... ) 
      #                                                                                                                                   [4] = earliest age group                         [1] = White                                                 [1] = female                                        
      fit0_draws_list[[re_i]][[i]]$`b[1]` = fit0_draws_list[[re_i]][[i]]$`b[1]` + input_model_draws[[paste0("r_",re_i+1,"_2[",i,"]")]]  + add_year_8*input_model_draws[["r_2_2[1]"]] + add_ethnicity_white*input_model_draws[["r_3_2[1]"]] + add_gender_female*input_model_draws[["r_4_2[1]"]]
      fit0_draws_list[[re_i]][[i]]$`b[2]` = fit0_draws_list[[re_i]][[i]]$`b[2]` + input_model_draws[[paste0("r_",re_i+1,"_3[",i,"]")]]  + add_year_8*input_model_draws[["r_2_3[1]"]] + add_ethnicity_white*input_model_draws[["r_3_3[1]"]] + add_gender_female*input_model_draws[["r_4_3[1]"]]
      fit0_draws_list[[re_i]][[i]]$`b[3]` = fit0_draws_list[[re_i]][[i]]$`b[3]` + input_model_draws[[paste0("r_",re_i+1,"_4[",i,"]")]]  + add_year_8*input_model_draws[["r_2_4[1]"]] + add_ethnicity_white*input_model_draws[["r_3_4[1]"]] + add_gender_female*input_model_draws[["r_4_4[1]"]]
      fit0_draws_list[[re_i]][[i]]$`b[4]` = fit0_draws_list[[re_i]][[i]]$`b[4]` + input_model_draws[[paste0("r_",re_i+1,"_5[",i,"]")]]  + add_year_8*input_model_draws[["r_2_5[1]"]] + add_ethnicity_white*input_model_draws[["r_3_5[1]"]] + add_gender_female*input_model_draws[["r_4_5[1]"]]
      fit0_draws_list[[re_i]][[i]]$`b[5]` = fit0_draws_list[[re_i]][[i]]$`b[5]` + input_model_draws[[paste0("r_",re_i+1,"_6[",i,"]")]]  + add_year_8*input_model_draws[["r_2_6[1]"]] + add_ethnicity_white*input_model_draws[["r_3_6[1]"]] + add_gender_female*input_model_draws[["r_4_6[1]"]]
      fit0_draws_list[[re_i]][[i]]$`b[6]` = fit0_draws_list[[re_i]][[i]]$`b[6]` + input_model_draws[[paste0("r_",re_i+1,"_7[",i,"]")]]  + add_year_8*input_model_draws[["r_2_7[1]"]] + add_ethnicity_white*input_model_draws[["r_3_7[1]"]] + add_gender_female*input_model_draws[["r_4_7[1]"]]
      
      # Adjust intercepts for each group 
      fit0_draws_list[[re_i]][[i]] = fit0_draws_list[[re_i]][[i]] %>%
        mutate_at(vars(starts_with("b_Intercept")), ~ . - input_model_draws[[paste0("r_",re_i+1,"_1[",i,"]")]])

      names(fit0_draws_list[[re_i]])[i] = paste0(re_i,"_",i)
    }
  }
  
  # fit0_draws_long = do.call("rbind.data.frame", fit0_draws_list[[1]])
  # fit0_draws_long = rbind.data.frame(fit0_draws_long, do.call("rbind.data.frame", fit0_draws_list[[2]]) )
  # fit0_draws_long = rbind.data.frame(fit0_draws_long, do.call("rbind.data.frame", fit0_draws_list[[3]]) )
  # 
  fit0_draws_list = unlist(fit0_draws_list, recursive = FALSE)
  
    # rm(input_model_draws, fit0_draws_long)
  
  # # Check colnames(datagrid_3cat) == colnames(fit0_draws)
  if (!identical(colnames(datagrid_3cat), fixed_predictors)) {warning("STOP")}
  
  contrast_1 = datagrid_3cat[2,] - datagrid_3cat[3,]   # Never -> Sometimes
  contrast_2 = datagrid_3cat[1,] - datagrid_3cat[3,]   # Never -> Always
  contrast_3 = datagrid_3cat[1,] - datagrid_3cat[2,]   # Sometimes -> Always
  
  
  smd1 = sapply(1:sum(n_groups), function(i) 
    as.vector( as.matrix(select(fit0_draws_list[[i]], -starts_with("b_Intercept"))) %*% as.matrix(contrast_1))
  ) %>%
    `colnames<-`(paste0("smd1_group_",names(fit0_draws_list)))
  
  smd2 = sapply(1:sum(n_groups), function(i) 
    as.vector( as.matrix(select(fit0_draws_list[[i]], -starts_with("b_Intercept"))) %*% as.matrix(contrast_2))
  ) %>%
    `colnames<-`(paste0("smd2_group_",names(fit0_draws_list)))
  
  smd3 = sapply(1:sum(n_groups), function(i) 
    as.vector( as.matrix(select(fit0_draws_list[[i]], -starts_with("b_Intercept"))) %*% as.matrix(contrast_3))
  ) %>%
    `colnames<-`(paste0("smd3_group_",names(fit0_draws_list)))
  
  if (identical(my_cdf,plogis)){              # If logistic model, divide mean difference by standard deviation of standard logistic distribution
    smd1 = smd1 / sqrt(pi^2 / 3)
    smd2 = smd2 / sqrt(pi^2 / 3)
    smd3 = smd3 / sqrt(pi^2 / 3)
  }
  emm = cbind.data.frame(smd1, smd2, smd3)
  
  
  # # Differences on the linear predictor term 
  # emm$smd1    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_1))
  # emm$smd2    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_2))
  # emm$smd3    = as.vector(fit0_draws_nointercept %*% as.matrix(contrast_3))
  # emm$mean1   = outcome_probabilities_always     %*% as.matrix(outcome_vector)
  # emm$mean2   = outcome_probabilities_sometimes  %*% as.matrix(outcome_vector)
  # emm$mean3   = outcome_probabilities_never      %*% as.matrix(outcome_vector)
  # emm$con1    = emm$mean2 - emm$mean3 
  # emm$con2    = emm$mean1 - emm$mean3
  # emm$con3    = emm$mean1 - emm$mean2
  # emm         = emm[-1]
  # 
  # 
  # 
  emm_summary =
    apply(emm,2,function(x) ggdist::mean_hdci(x, .width = conf_level)) %>%
    do.call("rbind.data.frame",.)
  
  emm_summary$n_draws = apply(emm,2,function(x) length(x))
  
  emm_summary$n_models = length(which(did_model_fit))
  
  
  # ggdist::mean_hdci(as.numeric(emm$smd2_group_6), .width = conf_level)
  
  pd = emm %>%                # Probability of direction
    bayestestR::pd() %>%
    as.data.frame() %>%
    pull(pd)
  
  emm_summary$pd = pd
  # 
  return(list(emm_summary = emm_summary))
}







# 
# 
# input = stan_ordinal_results$rcads_dep_ss
# 
# combined_draws = lapply(input, function(x)  extract(x)$b  )
# combined_draws = do.call("rbind.data.frame", combined_draws)
# 
# 
# print(x[[1]])
# 
# stan_diag(x[[1]])
# 
# sampling(x[[1]])
# 
# zz = extract(x[[1]])
# z = extract(x[[1]])$b
# 
# 

# 
# 
# 
# 
# # OLD ---------------



# calc_emm = function(input_model, input_data){
  
  ## 3 groups ("Never", "Sometimes", "Always")
  
#   datagrid_3cat = datagrid(
#     X1440_foodpovSome = 0:1,
#     X1470_foodpovSome = 0:1,
#     X1500_foodpovSome = 0:1,
#     X1440_foodpovOften= 0:1,
#     X1470_foodpovOften= 0:1,
#     X1500_foodpovOften= 0:1,
#     model = input_model,
#     newdata = input_data
#   ) %>%
#     dplyr::filter(
#       (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
#         (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
#         (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
#     ) %>%
#     slice(c(2,3,1))
#   
#   datagrid_all = datagrid(
#     X1440_foodpovSome = 0:1,
#     X1470_foodpovSome = 0:1,
#     X1500_foodpovSome = 0:1,
#     X1440_foodpovOften = 0:1,
#     X1470_foodpovOften = 0:1,
#     X1500_foodpovOften = 0:1,
#     model = input_model,
#     newdata = input_data
#   ) %>%
#     rowwise() %>%
#     mutate(x1 = sum(X1440_foodpovSome, X1440_foodpovOften*2),
#            x2 = sum(X1470_foodpovSome, X1470_foodpovOften*2),
#            x3 = sum(X1500_foodpovSome, X1500_foodpovOften*2)
#     ) %>%
#     filter(x1<3 & x2<3 & x3<3) %>%
#     as.data.frame()
#   
#   emm_3cat = marginaleffects::predictions(
#     model = input_model,
#     type = "response",
#     newdata = datagrid_3cat,
#     conf_level = .99,
#     re_formula = NA        # formula containing group-level effects to be considered in the prediction. If NULL (default), include all group-level effects; if NA, include no group-level effects.
#   ) 
#   
#   # Warnings about re_formula can be ingored (see GitHub Issue)
#   
#   emm_all = marginaleffects::predictions(
#     model = input_model,
#     type = "response",
#     newdata = datagrid_all,
#     conf_level = .99,
#     re_formula = NA
#   ) 
#   
#   return(list(datagrid_3cat, datagrid_all, emm_3cat, emm_all))
#   
# }




# 
# 
# contrast_calc_ordinal = function(input_data, input_model, conf_level = .99, 
#                                  
#                                  ){
#   
#   suppressWarnings({
#     fit0_draws = input_model %>%
#       as_draws_df(., inc_warmup = FALSE) %>%
#       dplyr::select(starts_with("b_")) %>% # Removes chain, iteration and draw column 
#       as.matrix()
#   })
#   
#   predictors = gsub("b_", "", grep("b_", get_variables(input_model), value = TRUE))
#   predictors = grep("Intercept", predictors, value = TRUE, invert = TRUE)
#   
#   fit0_draws_nointercept = fit0_draws %>%
#     as.data.frame() %>%
#     select(-contains("Intercept")) %>%
#     rename_with(~gsub("b_", "", .), .cols = everything()) %>%
#     select(all_of(predictors)) %>%
#     as.matrix()
#   
#   binary_values <- c(0, 1)
#   
#   # Generate every possible combination
#   combinations_all <- expand.grid(
#     X1440_foodpovSome = binary_values,
#     X1470_foodpovSome = binary_values,
#     X1500_foodpovSome = binary_values,
#     X1440_foodpovOften = binary_values,
#     X1470_foodpovOften = binary_values,
#     X1500_foodpovOften = binary_values
#   )
#   
#   # View the first few rows of the dataset
#   head(combinations)
#   
#   combinations_3comp = combinations_all %>%
#     dplyr::filter(
#       (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
#         (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
#         (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
#     ) %>%
#     slice(c(3,2,1))
#   
#   input_data = 
#     input_data %>%
#     select(all_of(predictors))
#   
#   for(i in 1:nrow(combinations_3comp)){
#     
#     dat = input_data
#     dat[,food_pov_variables] = combinations_3comp[i,]
#     dat = as.matrix(dat)
#     
#     x = vector()
#     for (i in 1:nrow(fit0_draws_nointercept)){
#       x[i] = mean(dat %*% as.matrix(fit0_draws_nointercept[i,]))
#     }
#     x = dat %*% as.matrix(fit0_draws_nointercept[1,])
#     x = dat %*% as.matrix(t(fit0_draws_nointercept))
#     
#     
#   }
#   
#   
#   datagrid_3cat = datagrid(
#     X1440_foodpovSome = 0:1,
#     X1470_foodpovSome = 0:1,
#     X1500_foodpovSome = 0:1,
#     X1440_foodpovOften= 0:1,
#     X1470_foodpovOften= 0:1,
#     X1500_foodpovOften= 0:1,
#     model = input_model,
#     newdata = input_data
#   ) %>%
#     dplyr::filter(
#       (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
#         (X1440_foodpovSome==1 & X1470_foodpovSome==1 & X1500_foodpovSome==1 & X1440_foodpovOften==0 & X1470_foodpovOften==0 & X1500_foodpovOften==0) |
#         (X1440_foodpovSome==0 & X1470_foodpovSome==0 & X1500_foodpovSome==0 & X1440_foodpovOften==1 & X1470_foodpovOften==1 & X1500_foodpovOften==1) 
#     ) %>%
#     slice(c(2,3,1))
#   
#   # fit0_draws_sigma = input_model %>%
#   #   as_draws_df(., variable = "sigma", inc_warmup = FALSE) %>%
#   #   select(1) %>%
#   #   as.matrix()
#   
#   
#   
#   
#   
#   # Raw data not needed for now! 
#   
#   # input_data_2 = input_data %>%
#   #                select(all_of(predictors)) %>%
#   #                tibble::add_column(Intercept = 1, .before = 1)
#   
#   # Define contrasts 
#   
#   input_grid_2 = datagrid_3cat %>%
#     data.frame() %>%
#     # select(-1:-5) %>%
#     select(all_of(predictors)) %>%
#     tibble::add_column(Intercept = 1, .before = 1)
#   
#   # Check colnames(input_grid_2) == colnames(fit0_draws)
#   if (!identical(colnames(input_grid_2), gsub("b_","",colnames(fit0_draws)))) {warning("STOP")}
#   
#   contrast_1 = input_grid_2[2,] - input_grid_2[3,]
#   contrast_2 = input_grid_2[1,] - input_grid_2[3,]
#   contrast_3 = input_grid_2[1,] - input_grid_2[2,]
#   
# 
#   
#   emm      = as.data.frame(matrix(nrow = nrow(fit0_draws)))
#   emm$con1 = as.vector(fit0_draws %*% t(contrast_1))
#   emm$con2 = as.vector(fit0_draws %*% t(contrast_2))
#   emm$con3 = as.vector(fit0_draws %*% t(contrast_3))
#   emm$smd1 = as.vector(emm$con1  / fit0_draws_sigma)
#   emm$smd2 = as.vector(emm$con2  / fit0_draws_sigma)
#   emm$smd3 = as.vector(emm$con3  / fit0_draws_sigma)
#   emm$c1   = as.vector(fit0_draws %*% t(input_grid_2[1,]))
#   emm$c2   = as.vector(fit0_draws %*% t(input_grid_2[2,]))
#   emm$c3   = as.vector(fit0_draws %*% t(input_grid_2[3,]))
#   emm      = emm[-1]
#   
#   emm_summary = 
#     apply(emm,2,function(x) ggdist::mean_hdi(x, .width = conf_level)) %>%
#     do.call("rbind.data.frame",.)
#   
#   return(list(emm, emm_summary, input_grid_2))
# }
# 
# 
