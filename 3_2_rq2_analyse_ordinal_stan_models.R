# Load Packages and Data -------------------------------------------------------
rm(list=ls())
source("0_2_load_packages.R")
source("0_4_custom_functions.R")

USE_LOCAL_DATA_CACHE = TRUE

remote_path       =  "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\"  # Path to Oxford Server
remote_path = ""
dat_imputed_long  =  readRDS(file=file.path("r_output_enc","sensitive_dat_imputed_long.Rdata"))

# if (USE_LOCAL_DATA_CACHE){
  stan_ordinal_results = readRDS(file.path("r_output_enc","1_6_ordinal_results.RDS"))
#   warning("Using local cache of stan model results (much quicker to read) - check UP TO DATE")
# } else{
#   # stan_ordinal_results = readRDS(file = file.path("r_output_enc","1_6_ordinal_results.RDS"))
# }

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

# Because this does NOT have any data on participants, we will store it locally so loading it is much quicker:

# saveRDS(all_draws, file = file.path("r_output_enc", paste0("all_draws_", format(Sys.Date(), "%Y-%m-%d"),".Rds"))) # TAKES FOREVER!! 
saveRDS(all_draws, file = paste0("all_draws_", format(Sys.Date(), "%Y-%m-%d"),".Rds"))

# Combine results --------------------------------------------------------------

if (FALSE){
  # Check distribution of each outcome
  
  x = dat_imputed_long %>%
    select(all_of(outcomes)) 
  
  x %>%
    apply(.,2,table)
  
  sapply(1:6, function(i) length(names(table(x[i]))))
  
  sapply(1:6, function(i) max(x[i], na.rm = TRUE) - min(x[i], na.rm = TRUE) + 1)
}

emm_stan_fe = list()

## rcads_dep_ss ----------------------------------------------------------------
emm_stan_fe[["rcads_dep_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["rcads_dep_ss"]], 
  fixed_predictors  = predictors_allyears,
  conf_level        = .99,
  outcome_vector    = 0:15,
  calc_gender       = TRUE,
  my_cdf            = plogis
)

## rcads_anx_ss ----------------------------------------------------------------

emm_stan_fe[["rcads_anx_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["rcads_anx_ss"]], 
  fixed_predictors  = predictors_allyears,
  conf_level        = .99,
  outcome_vector    = 0:18,
  calc_gender       = TRUE,
  my_cdf            = plogis
)

## swemwbs_ss ------------------------------------------------------------------

emm_stan_fe[["swemwbs_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["swemwbs_ss"]], 
  fixed_predictors  = predictors_secondary,
  conf_level        = .99,
  outcome_vector    = 7:35,
  my_cdf            = plogis
)

## pt_ss -----------------------------------------------------------------------

emm_stan_fe[["pt_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["pt_ss"]], 
  fixed_predictors  = predictors_secondary,
  conf_level        = .99,
  outcome_vector    = 0:32,
  my_cdf            = plogis
)

## lone_ss ---------------------------------------------------------------------

emm_stan_fe[["lone_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["lone_ss"]], 
  fixed_predictors  = predictors_secondary,
  conf_level        = .99,
  outcome_vector    = 0:8,
  my_cdf            = plogis
)

## scwbs_ss --------------------------------------------------------------------

emm_stan_fe[["scwbs_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["scwbs_ss"]], 
  fixed_predictors  = predictors_primary,
  conf_level        = .99,
  outcome_vector    = 15:75,
  my_cdf            = plogis
)

## cog_ss --------------------------------------------------------------------

emm_stan_fe[["cog_ss"]] = analyse_stan_models(
  input_data        = dat_imputed_long, 
  input_model       = stan_ordinal_results[["cog_ss"]], 
  fixed_predictors  = predictors_secondary,
  conf_level        = .99,
  outcome_vector    = 1:10,
  my_cdf            = plogis
)

# visualise results ------------------------------------------------------------

## Standardised mean differences dot-plot --------------------------------------

main_results = lapply(emm_stan_fe, function(x) x[[2]])

for(i in seq_along(main_results)){
  main_results[[i]]$outcome = outcomes[i]
  main_results[[i]] = tibble::rownames_to_column(main_results[[i]], var = "effect")
}

main_results = do.call(rbind, main_results) %>%
  `rownames<-`((NULL))


main_results %>%
  filter(grepl("smd[1-2]",effect)) %>%
  mutate(x_pos = 1:14,
         outcome_label = fct_rev(factor(outcomes_labels[match(outcome, outcomes)], levels = outcomes_labels)),
         effect_label  = factor(effect, levels = paste0("smd",1:3), labels = c("Low -> Medium",
                                                                               "Low -> High",
                                                                               "Medium -> High"))
  ) %>%
  filter(outcome != "cog_ss") %>%
  ggplot(aes(x = y, xmin = ymin, xmax = ymax, y = outcome_label, col = effect_label)) +
  geom_vline(aes(xintercept=0)) +
  geom_errorbar(size = 1.8) + 
  geom_point(size = 6) +
  jtools::theme_apa() +
  # coord_flip() +
  theme(
    axis.ticks.x = element_line(size = 0.5),  
    panel.grid.major.x = element_line(color = 'grey90', linewidth = 1),
    panel.grid.minor.x = element_line(color = 'grey90', linetype = "dashed", linewidth = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "top"
    # legend.position = c(0.8, 0.2),  # set legend position
    # legend.key.size = unit(.4, "cm")  # make the legend half its original size
  ) + 
  labs(x = "Standardised Mean Diffference", y = NULL) +
  coord_cartesian(xlim = c(-1.4,1.4)) +
  # geom_text(aes(label=sprintf("%.2f [%.2f, %.2f]", y, ymin, ymax)), nudge_x = 0.05, size = 3, hjust = 0)  # This line is added to display the annotations.
  geom_text(aes(label=sprintf("%.2f [%.2f, %.2f]", y, ymin, ymax), 
                hjust=ifelse(effect_label=="Low -> Medium", 0, 1),
                x=ifelse(effect_label=="Low -> Medium", -1.5, 1.5)), 
            size = 5) +
  scale_x_continuous(breaks = seq(-.5,.5, by = .5), minor_breaks = seq(-.7, .7, by =.1)) +
  scale_color_manual(values = c("Low -> Medium" = "#0072B2", "Low -> High" = "#D55E00")) +
  theme(panel.grid.minor.x = element_line(size = 0.75))


ggsave(filename = file.path("plots","3_2_stan_standardised_mean_differences_simple.pdf"), width = 8.6, height = 3.2)
ggsave(filename = file.path("plots","3_2_stan_standardised_mean_differences_simple.png"), width = 8.6, height = 3.2)

## Plot distributions under different effects ----------------------------------

outcome_probabilities = lapply(emm_stan_fe, function(x) x[[4]])

outcome_vector_list = list(
  0:15,  # Depression
  0:18,  # Anxiety
  7:35,  # SWEMWBS
  0:32,  # Positive Thoughts
  0:8,   # Loneliness
  15:75, # SCWBS
  1:10       # Cognition
)


for (i in 1:length(outcome_probabilities)) {
  
  outcome_probabilities[[i]] = apply(outcome_probabilities[[i]], 2, mean) %>%
    t() %>%
    data.frame()
  
  outcome_probabilities[[i]]$outcome = outcomes_labels[i]
  
  outcome_probabilities[[i]] = outcome_probabilities[[i]] %>%
    pivot_longer(
      cols = starts_with("prob_y_"),
      names_to = c("y", "fi_level"),
      names_pattern = "prob_y_(\\d+)_(.*)",
      values_to = "probability"
    )
  
  if (length(unique(outcome_probabilities[[i]]$y))!=length(outcome_vector_list[[i]])) stop("ERROR")
  
  outcome_probabilities[[i]]$y = outcome_vector_list[[i]][as.numeric(outcome_probabilities[[i]]$y)]
  
}

outcome_probabilities2 = do.call("rbind.data.frame",outcome_probabilities) %>%
  mutate(y = as.numeric(y)) %>%
  mutate(fi_level = factor(fi_level, levels = c("always","sometimes","never"))) %>%
  mutate(
    `Food Insecurity Level` = fct_recode(fi_level,
                                         Low = "never",
                                         Medium = "sometimes",
                                         High = "always")
  ) %>%
  mutate(ribbon_ymin = case_when(
    `Food Insecurity Level` == "High" ~ 0,
    `Food Insecurity Level` == "Medium" ~ max(probability[`Food Insecurity Level` == "High"]),
    `Food Insecurity Level` == "Low" ~ max(probability[`Food Insecurity Level` == "Medium"]),
    TRUE ~ 0
  ))


# Define custom colors for each level
custom_colors <- c("High" = "#D55E00", # Red
                   "Medium" = "#0072B2", # Green
                   "Low" = "#009E73") # Blue

custom_colors_lighter <- c("High" = "#ffc599", # Red
                           "Medium" = "#99daff", # Green
                           "Low" = "#99ffe4") # Blue

outcome_probabilities2 %>%
  mutate(outcome = factor(outcome, levels = outcomes_labels)) %>%
  filter(outcome!="Cognition") %>%
  # filter(outcome %in% outcomes_labels[1:4]) %>%
  ggplot(aes(x = `y`, 
             y = probability, 
             fill   = `Food Insecurity Level`,
             col    = `Food Insecurity Level`,
             group = `Food Insecurity Level`)
  ) +
  theme_minimal() +
  geom_ribbon(aes(ymin = 0, ymax = probability, fill = `Food Insecurity Level`), alpha = 1) +
  scale_y_continuous(labels = scales::label_percent(), minor_breaks = seq(0,1,by=.01), breaks = seq(0,1,by=.02)) +
  scale_x_continuous(minor_breaks = 1:100,breaks = c(seq(1,100,by=2))) +
  scale_fill_manual(values = custom_colors_lighter) +
  scale_color_manual(values = custom_colors) +
  geom_point(aes(shape =`Food Insecurity Level` ), size = 4) +
  geom_line(size =1.5) +
  facet_wrap(~(outcome), scales = "free", ncol = 2) +
  labs (
    title = "Model-predicted probabilties of each outcome score at different levels of food insecurity",
    x = "Score"
    ) +
  theme(panel.background = element_rect(fill = NA)) + # This makes the panel background transparent +
  theme(legend.position = c(1, 1),      # Positioning the legend
      legend.justification = c(1, 1)) # Adjusting the justification

ggsave(filename = file.path("plots","3_2_stan_marginal_effects_lineplot.pdf"), width = 9.5, height = 11)
ggsave(filename = file.path("plots","3_2_stan_marginal_effects_lineplot.png"), width = 9.5, height = 11)


# Format data for word table ---------------------------------------------------

main_results_emm = main_results %>%
  filter(grepl("mean[1-3]",effect)) %>%
  mutate(outcome_label = outcomes_labels[match(outcome, outcomes)],
         outcome_label = factor(outcome_label, levels = outcomes_labels),
         effect_label  = c("Always","Sometimes","Never")[match(effect, paste0("mean",1:3))],
         effect_label  = factor(effect_label, levels =  c("Never","Sometimes","Always")),
  ) %>%
  dplyr::arrange(outcome_label, effect_label) %>%
  select(outcome_label, effect_label, y, ymin, ymax) %>%
  # colwise() %>%
  mutate(outcome_label = ifelse(duplicated(outcome_label), "", as.character(outcome_label)))
main_results_emm


change_emm = main_results %>%
  filter(grepl("con[1-3]",effect)) %>%
  mutate(
    outcome_label = outcomes_labels[match(outcome, outcomes)],
    outcome_label = factor(outcome_label, levels = outcomes_labels),
    effect = factor(effect, levels = c("con3","con1","con2"))
  ) %>%
  arrange(outcome_label, effect) %>%
  select(outcome_label, effect, y, ymin, ymax) 

change_emm[which(as.character(change_emm$effect) == "con3"),c("y","ymin", "ymax")] = ""
change_emm

smd_table = main_results %>%
  filter(grepl("smd[1-3]",effect)) %>%
  mutate(
    outcome_label = outcomes_labels[match(outcome, outcomes)],
    outcome_label = factor(outcome_label, levels = outcomes_labels),
    effect = factor(effect, levels = c("smd3","smd1","smd2"))
  ) %>%
  arrange(outcome_label, effect) %>%
  select(outcome_label, effect, y, ymin, ymax) 
smd_table[which(as.character(smd_table$effect) == "smd3"),c("y","ymin", "ymax")] = ""
smd_table

probability_direction = main_results %>%
  filter(grepl("con[1-3]",effect)) %>%
  mutate(
    outcome_label = outcomes_labels[match(outcome, outcomes)],
    outcome_label = factor(outcome_label, levels = outcomes_labels),
    effect = factor(effect, levels = c("con3","con1","con2")),
    pd = ifelse(effect == "con3", "", gbtoolbox::apa_num(pd, n_decimal_places = 3))
  ) %>%
  arrange(outcome_label, effect) %>%
  select(outcome, effect, pd) 

combined_results_table = cbind.data.frame(main_results_emm, change_emm, smd_table) %>% 
  `colnames<-`(c("Outcome","Food Insecurity Level","M","LB","UB","del1","del2","Δ Est","Δ LB", "Δ UB","del3", "del4","Δ smd","smd LB","smd UB")) %>%
  select(-contains("del")) %>%
  mutate(across(3:ncol(.), function(x) gbtoolbox::apa_num(x,n_decimal_places = 2))) %>%
  mutate(pd = probability_direction$pd)

combined_results_table %>%
  write.csv(file.path("results","3_2_stan_main_results_table.csv"))

# Calculate % above cut-off for RCADS ------------------------------------------

all_draws_rcads = all_draws %>%
  filter(out %in% outcomes[1:2]) %>%
  data.frame()

all_draws_rcads_betas = all_draws_rcads %>% 
                        select(starts_with("b.")) %>%
                        as.matrix()

pred_means = dat_imputed_long %>%
  select(all_of(predictors_allyears)) %>%
  apply(.,2, mean) 

## Calculate Reference Grids ---------------------------------------------------

datagrid_3cat = cbind.data.frame(pred_means,
                                 pred_means,
                                 pred_means) %>% 
  t() %>%
  `rownames<-`((NULL))

datagrid_3cat[,1:6] = matrix(c(0, 1, 0, 1, 0, 1,
                               1, 0, 1, 0, 1, 0,
                               0, 0, 0, 0, 0, 0), nrow = 3, byrow = TRUE) 

datagrid_3cat_males = datagrid_3cat %>% data.frame()
  datagrid_3cat_males$X1020_gender_m = 1
  datagrid_3cat_males$X1020_gender_o = 0
  
datagrid_3cat_females = datagrid_3cat %>% data.frame()
  datagrid_3cat_females$X1020_gender_m = 0
  datagrid_3cat_females$X1020_gender_o = 0
  

## Calculate latent mean scores (Tau) ----------------------------------------------------------------------

  all_draws_rcads$tau_males_highfi = all_draws_rcads_betas %*% matrix(t(datagrid_3cat_males[1,]))
  all_draws_rcads$tau_males_medfi  = all_draws_rcads_betas %*% matrix(t(datagrid_3cat_males[2,]))
  all_draws_rcads$tau_males_lowfi  = all_draws_rcads_betas %*% matrix(t(datagrid_3cat_males[3,]))
  
  all_draws_rcads$tau_females_highfi = all_draws_rcads_betas %*% matrix(t(datagrid_3cat_females[1,]))
  all_draws_rcads$tau_females_medfi  = all_draws_rcads_betas %*% matrix(t(datagrid_3cat_females[2,]))
  all_draws_rcads$tau_females_lowfi  = all_draws_rcads_betas %*% matrix(t(datagrid_3cat_females[3,]))
  
  # https://link.springer.com/article/10.1007/s10802-021-00817-w
  # For adolescent-report, we recommend using cut-off scores of ≥ 5 for adolescent boys, and ≥ 9 for adolescent girls (anxiety score), ≥ 8 for adolescent boys, and ≥ 9 for adolescent girls (depression score), and ≥ 9 for adolescent boys and ≥ 14 for adolescent girls (total score). Using additional impact questions, the cut-off for the total score increases to ≥ 14 for adolescent boys and ≥ 18 for adolescent girls. When using parent-report, we recommend using cut-offs of ≥ 5 (adolescent boys) and ≥ 7 (adolescent girls) for the anxiety score, ≥ 6 (adolescent boys) and ≥ 7 (adolescent girls) for the depression score, ≥ 8 (adolescent boys) and ≥ 11 (adolescent girls) for total score and ≥ 13 (adolescent boys) and ≥ 15 (adolescent girls) for total score with additional impact items.
  
  # Anxiety cuts offs- 
  ## male >=5 
  ## Female >= 9
  # Depression cut offs - 
  ## Male >= 8
  ## Female >= 9
  
  all_draws_rcads$males_threshold = NA
  all_draws_rcads[all_draws_rcads$out=="rcads_dep_ss","males_threshold"] = all_draws_rcads[all_draws_rcads$out=="rcads_dep_ss", "b_Intercept.7."] 
  all_draws_rcads[all_draws_rcads$out=="rcads_anx_ss","males_threshold"] = all_draws_rcads[all_draws_rcads$out=="rcads_anx_ss", "b_Intercept.4."] 
  
  all_draws_rcads$females_threshold = NA
  all_draws_rcads[all_draws_rcads$out=="rcads_dep_ss","females_threshold"] = all_draws_rcads[all_draws_rcads$out=="rcads_dep_ss", "b_Intercept.8."] 
  all_draws_rcads[all_draws_rcads$out=="rcads_anx_ss","females_threshold"] = all_draws_rcads[all_draws_rcads$out=="rcads_anx_ss", "b_Intercept.8."] 

## Calculate marginal probabilities & relative risks  -----------------------------------------------------
  
  all_draws_rcads$prob_males_highfi = 1 - plogis(q = all_draws_rcads$males_threshold, location = all_draws_rcads$tau_males_highfi)
  all_draws_rcads$prob_males_medfi  = 1 - plogis(q = all_draws_rcads$males_threshold, location = all_draws_rcads$tau_males_medfi)
  all_draws_rcads$prob_males_lowfi  = 1 - plogis(q = all_draws_rcads$males_threshold, location = all_draws_rcads$tau_males_lowfi)
  
  all_draws_rcads$prob_RR_males_highlow = all_draws_rcads$prob_males_highfi/all_draws_rcads$prob_males_lowfi
  all_draws_rcads$prob_RR_males_medlow  = all_draws_rcads$prob_males_medfi /all_draws_rcads$prob_males_lowfi
  
  all_draws_rcads$prob_females_highfi = 1 - plogis(q = all_draws_rcads$females_threshold, location = all_draws_rcads$tau_females_highfi)
  all_draws_rcads$prob_females_medfi  = 1 - plogis(q = all_draws_rcads$females_threshold, location = all_draws_rcads$tau_females_medfi)
  all_draws_rcads$prob_females_lowfi  = 1 - plogis(q = all_draws_rcads$females_threshold, location = all_draws_rcads$tau_females_lowfi)
  
  all_draws_rcads$prob_RR_females_highlow = all_draws_rcads$prob_females_highfi/all_draws_rcads$prob_females_lowfi
  all_draws_rcads$prob_RR_females_medlow  = all_draws_rcads$prob_females_medfi /all_draws_rcads$prob_females_lowfi
  
  all_draws_rcads %>% 
    pivot_longer(cols = starts_with("prob_")) %>%
    group_by(name, out) %>%
    summarise(
      mean_hdci = list(mean_hdci(value, .width = c(0.99)))
    ) %>%
    unnest(mean_hdci) %>%
    arrange(out)
  
  ggplot(data = dat_imputed_long, aes(x = rcads_dep_ss)) +
    geom_histogram(aes(y = ..density.. ), binwidth = 1) + # Multiply density by 100 for percentage
    facet_wrap(~X1020_gender) +
    scale_y_continuous(labels = scales::percent_format())  # Use percent format for y-axis labels
  
  dat_imputed_long %>%
    filter(X1020_gender=="Male") %>%
    pull(rcads_dep_ss) %>%
    `>=`(8) %>%
    table() %>%
    `/`(sum(.))
  
  dat_imputed_long %>%
    filter(X1020_gender=="Male") %>%
    pull(rcads_anx_ss) %>%
    `>=`(5) %>%
    table() %>%
    `/`(sum(.))
  
  dat_imputed_long %>%
    filter(X1020_gender=="Female") %>%
    pull(rcads_dep_ss) %>%
    `>=`(9) %>%
    table() %>%
    `/`(sum(.))
  
  dat_imputed_long %>%
    filter(X1020_gender=="Female") %>%
    pull(rcads_anx_ss) %>%
    `>=`(9) %>%
    table() %>%
    `/`(sum(.))
  
# Gender Line Plot -------------------------------------------------------------
  gc()
  
  emm_stan_fe$rcads_dep_ss$outcome_probabilities_female$gender = "Female"
  emm_stan_fe$rcads_dep_ss$outcome_probabilities_female$outcome = "rcads_dep_ss"
    
  emm_stan_fe$rcads_dep_ss$outcome_probabilities_male$gender = "Male"
  emm_stan_fe$rcads_dep_ss$outcome_probabilities_male$outcome = "rcads_dep_ss"
  
  emm_stan_fe$rcads_anx_ss$outcome_probabilities_female$gender = "Female"
  emm_stan_fe$rcads_anx_ss$outcome_probabilities_female$outcome = "rcads_anx_ss"
  
  emm_stan_fe$rcads_anx_ss$outcome_probabilities_male$gender = "Male"
  emm_stan_fe$rcads_anx_ss$outcome_probabilities_male$outcome = "rcads_anx_ss"
  
  outcome_probabilities_list = list()
  
  outcome_probabilities_list = list(emm_stan_fe$rcads_dep_ss$outcome_probabilities_female,
                                    emm_stan_fe$rcads_dep_ss$outcome_probabilities_male,
                                    emm_stan_fe$rcads_anx_ss$outcome_probabilities_female,
                                    emm_stan_fe$rcads_anx_ss$outcome_probabilities_male
                                    )
  
  outcome_probabilities_list = lapply(outcome_probabilities_list, function(x) 
    pivot_longer(x, cols = starts_with("prob_y_"),
                 names_to = c("y", "fi_level"),
                 names_pattern = "prob_y_(\\d+)_(.*)",
                 values_to = "probability")
    )
  
  outcome_probabilities_long = dplyr::bind_rows(outcome_probabilities_list)
  
  outcome_probabilities_summarised = outcome_probabilities_long %>%
    group_by(gender, outcome, y, fi_level) %>%
    summarise(
      mean_hdci = list(ggdist::mean_hdci(probability, .width = c(0.99)))
    ) %>%
    unnest(mean_hdci, names_repair = "unique") %>%
    dplyr::rename(
      y=`y...3`,
      probability = `y...5`
    )
  
  rm(outcome_probabilities_list)  
  
  # Set shading area for each plot
  
  outcome_probabilities_summarised$xmin = NA
  outcome_probabilities_summarised$xmin[outcome_probabilities_summarised$gender=="Female" & outcome_probabilities_summarised$outcome=="rcads_anx_ss"] = 9
  outcome_probabilities_summarised$xmin[outcome_probabilities_summarised$gender=="Male"   & outcome_probabilities_summarised$outcome=="rcads_anx_ss"] = 5
  outcome_probabilities_summarised$xmin[outcome_probabilities_summarised$gender=="Female" & outcome_probabilities_summarised$outcome=="rcads_dep_ss"] = 9
  outcome_probabilities_summarised$xmin[outcome_probabilities_summarised$gender=="Male"   & outcome_probabilities_summarised$outcome=="rcads_dep_ss"] = 8
  
  
  # copied code from previously:
  outcome_probabilities_summarised2 = outcome_probabilities_summarised %>%
    ungroup() %>%
    mutate(y = as.numeric(y)) %>%
    mutate(adjusted_ymin = ifelse(as.numeric(y) >= xmin, 0, NA)) %>%
    mutate(fi_level = factor(fi_level, levels = c("always","sometimes","never"))) %>%
    mutate(outcome = fct_rev((outcome))) %>%
    mutate(
      `Food Insecurity Level` = fct_recode(fi_level,
                                           Low = "never",
                                           Medium = "sometimes",
                                           High = "always")
    ) %>%
    mutate(
      `Outcome` = fct_recode(outcome,
                             `RCADS-11 Anxiety` = "rcads_anx_ss",
                             `RCADS-11 Depression` = "rcads_dep_ss"
      )
    ) %>%
    mutate(ribbon_ymin = case_when(
      `Food Insecurity Level` == "High" ~ 0,
      `Food Insecurity Level` == "Medium" ~ max(probability[`Food Insecurity Level` == "High"]),
      `Food Insecurity Level` == "Low" ~ max(probability[`Food Insecurity Level` == "Medium"]),
      TRUE ~ 0
    )) 
  
  # Define custom colors for each level
  custom_colors <- c("High" = "#D55E00", # Red
                     "Medium" = "#0072B2", # Green
                     "Low" = "#009E73") # Blue
  
  custom_colors_lighter <- c("High" = "#ffc599", # Red
                             "Medium" = "#99daff", # Green
                             "Low" = "#99ffe4") # Blue
  
  outcome_probabilities_summarised2 %>%
    # filter(outcome %in% outcomes_labels[1:4]) %>%
    ggplot(aes(x = `y`, 
               y = probability, 
               fill   = `Food Insecurity Level`,
               col    = `Food Insecurity Level`,
               group = `Food Insecurity Level`)
    ) +
    theme_minimal() +
    geom_ribbon(aes(ymin = adjusted_ymin, ymax = probability, fill = `Food Insecurity Level`), alpha = 1) +
    geom_vline(aes(xintercept = xmin)) +
    scale_y_continuous(labels = scales::label_percent(), minor_breaks = seq(0,1,by=.01), breaks = seq(0,1,by=.02)) +
    scale_x_continuous(minor_breaks = 1:100,breaks = seq(1,100,by=2)) +
    scale_fill_manual(values = custom_colors_lighter) +
    scale_color_manual(values = custom_colors) +
    geom_point(aes(shape =`Food Insecurity Level` ), size = 4) +
    geom_line(size =1.5) +
    # facet_wrap(Outcome + gender, scales = "fixed", ncol = 2) +
    facet_grid(rows = vars(gender), cols = (vars(Outcome))) +
    labs (x = "Score", y = "Probability Mass Functions") +
    theme(strip.text = element_text(size = 13)) +
    theme(panel.background = element_rect(fill = NA)) # This makes the panel background transparent
  
  ggsave(filename = file.path("plots","3_2_stan_rcads_gender_lineplot.pdf"), width = 9, height = 7)
  ggsave(filename = file.path("plots","3_2_stan_rcads_gender_lineplot.png"), width = 9, height = 7)
  
# Posterior Predictive Checks --------------------------------------------------

#  x = extract(stan_ordinal_results[[1]][[1]])
# 
# x$Y_rep
# 
# dat_imputed_long_first = dat_imputed_long %>% 
#   filter(.imp == 1)
# 
# 
# extract(stan_ordinal_results[[1]][[1]])
# 
# get_variables(stan_ordinal_results[[1]][[1]])
# 
# ordinalise = function(x){return(match(x, sort(unique(x))))}

dat_imputed_long_first = dat_imputed_long %>%
  filter(.imp == 1)

pp_check_list = lapply(1:7, function(i){
  # browser()
  print(i)
  y_rep    = as.matrix(extract(stan_ordinal_results[[i]][[1]])$Y_rep)[1:20,]
  y        = as.numeric(unlist(dat_imputed_long_first[outcomes[i]]))
  y        = as.numeric(na.omit(y))
  y_unique = sort(unique(y))
  
  y_rep = apply(y_rep, 2, function(x) y_unique[x] )     # map our y_rep values back on the original scale
  
  # y_rep = t(apply(y_rep, 1, ordinalise))
  
  bayesplot::ppc_dens_overlay(
    y = y,
    yrep   = y_rep) + 
    labs(title = outcomes_labels[i])
})

pp_check_list[[3]] + pp_check_list[[6]] + pp_check_list[[5]] +
  pp_check_list[[4]] + pp_check_list[[2]] + pp_check_list[[1]] +
  pp_check_list[[7]]

ggsave(file.path("plots","3_2_ORDINAL_posterior_predictive_checks.png"),  width = 7*1.4, height = 5*1.4)

# x =stan_ordinal_results$rcads_dep_ss
# 
# i = 1
# 
# dat_imputed_long[outcomes[i]]
