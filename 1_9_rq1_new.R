# Pre-requisites ---------------------------------------------------------------
rm(list=ls(all.names = TRUE))

source("0_2_load_packages.R")
foodpov_measures = c("X1440_foodpov","X1470_foodpov","X1500_foodpov")

# cmdstanr::install_cmdstan(dir = "C:\\Users\\giaco\\.cmdstan\\", overwrite = TRUE)
# cmdstanr::set_cmdstan_path("C:\\Users\\giaco\\.cmdstan\\cmdstan-2.35.0\\cmdstan-2.35.0\\cmdstan-2.35.0\\")
# df_impute =  readRDS(file=file.path("r_output_enc","sensitive_df_impute.Rdata"))

remote_path =  "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\"  # Path to Oxford Server 
df          =  readRDS(file=file.path(remote_path, "r_output_enc","sensitive_df_impute.Rdata"))

df_weights          =  readRDS(file=file.path(remote_path, "r_output_enc","sensitive_df.Rdata"))

df_weights$WEIGHT.GENDER3AGE %>% sum(., na.rm = TRUE)

df$weights = df_weights$WEIGHT.GENDER3AGE[match(df$RID, df_weights$RID)]

table(df_weights$WEIGHT.GENDER2AGE, df_weights$X1030_age)

# Useful Functions -------------------------------------------------------------

# Softmax function for brms (first category set to 0)
# see documentaiton here: https://bookdown.org/content/3686/nominal-predicted-variable.html
g_softmax = function(x){
  x = c(0,x)
  return(exp(x)/sum(exp(x)))
}

# Unweighted estiamtes ---------------------------------------------------------

# foodpov_questions = c("")

models_unweighted = list()
model_emmeans     = list()

for(i in 1:3){
  models_unweighted[[i]] = brm(
    brms::brmsformula(as.formula(paste0(c(foodpov_measures[i]," ~ 1"), collapse = ""))), 
    data = df,
    family = categorical(),
    cores = 2,
    chains = 2,
    iter   = 8000,
    threads = threading(4), # for some reason cmdstanr doesn't work
    backend = "rstan",
    prior = c(set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muSome"),
              set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muOften"))
    
  )
  
  model_emmeans[[i]] = models_unweighted[[i]] %>%
    spread_draws(b_muSome_Intercept, b_muOften_Intercept) %>% 
    rowwise() %>%
    mutate(p1 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[1],
           p2 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[2],
           p3 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[3],
    ) %>%
    ungroup() %>%
    pivot_longer(cols = p1:p3) %>% 
    group_by(name) %>%
    ggdist::mean_hdi(value,.width = .99)
  
  print(i)
}

saveRDS(models_unweighted,file.path("results","models_unweighted_allstudents.Rds"))
saveRDS(model_emmeans, file.path("results","model_emmeans.Rds"))


## Secondary School Only --------------------------------------------------------

models_unweighted_secondary = list()
model_emmeans_secondary     = list()

for(i in 1:3){
  models_unweighted_secondary[[i]] = brm(
    brms::brmsformula(as.formula(paste0(c(foodpov_measures[i]," ~ 1"), collapse = ""))), 
    data = data.frame(filter(df, X1010_year %in% c("Y07","Y08","Y09","Y10","Y11", "Y12", "Y13"))),
    family = categorical(),
    cores = 2,
    chains = 2,
    iter   = 8000,
    threads = threading(4), # for some reason cmdstanr doesn't work
    backend = "rstan",
    prior = c(set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muSome"),
              set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muOften"))
    
  )
  
  model_emmeans_secondary[[i]] = models_unweighted_secondary[[i]] %>%
    spread_draws(b_muSome_Intercept, b_muOften_Intercept) %>% 
    rowwise() %>%
    mutate(p1 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[1],
           p2 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[2],
           p3 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[3],
    ) %>%
    ungroup() %>%
    pivot_longer(cols = p1:p3) %>% 
    group_by(name) %>%
    ggdist::mean_hdi(value,.width = .99)
  
  print(i)
}

saveRDS(models_unweighted_secondary,file.path("results","models_unweighted_secondary.Rds"))
saveRDS(model_emmeans_secondary, file.path("results","model_emmeans_secondary.Rds"))

## Primary School Only --------------------------------------------------------

models_unweighted_primary = list()
model_emmeans_primary     = list()

for(i in 1:3){
  models_unweighted_primary[[i]] = brm(
    brms::brmsformula(as.formula(paste0(c(foodpov_measures[i]," ~ 1"), collapse = ""))), 
    data = data.frame(filter(df, X1010_year %in% c("Y05","Y06"))),
    family = categorical(),
    cores = 2,
    chains = 2,
    iter   = 8000,
    threads = threading(4), # for some reason cmdstanr doesn't work
    backend = "rstan",
    prior = c(set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muSome"),
              set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muOften"))
    
  )
  
  model_emmeans_primary[[i]] = models_unweighted_primary[[i]] %>%
    spread_draws(b_muSome_Intercept, b_muOften_Intercept) %>% 
    rowwise() %>%
    mutate(p1 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[1],
           p2 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[2],
           p3 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[3],
    ) %>%
    ungroup() %>%
    pivot_longer(cols = p1:p3) %>% 
    group_by(name) %>%
    ggdist::mean_hdi(value,.width = .99)
  
  print(i)
}

saveRDS(models_unweighted_primary,file.path("results","models_unweighted_primary.Rds"))
saveRDS(model_emmeans_primary, file.path("results","model_emmeans_primary.Rds"))

# Weighted estimates -----------------------------------------------------------

## Calculate Weights -----------------------------------------------------------

library(survey)

## Fit Model -------------------------------------------------------------------
  
df_weighted = df %>%
    select(all_of(foodpov_measures),weights) 
  
df_weighted$weights = df_weighted$weights/sum(df_weighted$weights)*nrow(df_weighted)

sum(df_weighted$weights) == nrow(df_weighted)
 
models_weighted = list()
model_emmeans_weighted = list()

for(i in 1:3){
    models_weighted[[i]] = brm(
      brms::brmsformula(as.formula(paste0(c(foodpov_measures[i],"| weights(weights) ~ 1"), collapse = ""))), 
      data = df_weighted,
      family = categorical(),
      cores = 2,
      chains = 2,
      iter   = 8000,
      threads = threading(4), # for some reason cmdstanr doesn't work
      backend = "rstan",
      prior = c(set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muSome"),
                set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "muOften"))
      
    )
    
    model_emmeans_weighted[[i]] = models_weighted[[i]] %>%
      spread_draws(b_muSome_Intercept, b_muOften_Intercept) %>% 
      rowwise() %>%
      mutate(p1 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[1],
             p2 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[2],
             p3 = g_softmax(c(b_muSome_Intercept,  b_muOften_Intercept))[3],
      ) %>%
      ungroup() %>%
      pivot_longer(cols = p1:p3) %>% 
      group_by(name) %>%
      ggdist::mean_hdi(value,.width = .99)
    
    print(i)
}
  
saveRDS(models_weighted,file.path("results","models_weighted.Rds"))
saveRDS(model_emmeans_weighted, file.path("results","model_emmeans_weighted.Rds"))

# Final Table ------------------------------------------------------------------
if (FALSE){
  # rm(list = ls(all.names = TRUE))
  models_unweighted = readRDS(file.path("results","models_unweighted_allstudents.Rds"))
  model_emmeans = readRDS(file.path("results","model_emmeans.Rds"))

  models_unweighted_secondary = readRDS(file.path("results","models_unweighted_secondary.Rds"))
  model_emmeans_secondary = readRDS(file.path("results","model_emmeans_secondary.Rds"))

  models_unweighted_primary = readRDS(file.path("results","models_unweighted_primary.Rds"))
  model_emmeans_primary = readRDS(file.path("results","model_emmeans_primary.Rds"))

  models_weighted = readRDS(file.path("results","models_weighted.Rds"))
  model_emmeans_weighted = readRDS(file.path("results","model_emmeans_weighted.Rds"))
}

model_emmeans2 = model_emmeans
model_emmeans2[[1]]$outcome = food_pov_questions[1]
model_emmeans2[[2]]$outcome = food_pov_questions[2]
model_emmeans2[[3]]$outcome = food_pov_questions[3]
model_emmeans2 = do.call("rbind.data.frame",model_emmeans2)
model_emmeans2$group = "All Students"

model_emmeans_secondary2 = model_emmeans_secondary
model_emmeans_secondary2[[1]]$outcome = food_pov_questions[1]
model_emmeans_secondary2[[2]]$outcome = food_pov_questions[2]
model_emmeans_secondary2[[3]]$outcome = food_pov_questions[3]
model_emmeans_secondary2 = do.call("rbind.data.frame", model_emmeans_secondary2)
model_emmeans_secondary2$group = "Secondary School Students"

model_emmeans_primary2 = model_emmeans_primary
model_emmeans_primary2[[1]]$outcome = food_pov_questions[1]
model_emmeans_primary2[[2]]$outcome = food_pov_questions[2]
model_emmeans_primary2[[3]]$outcome = food_pov_questions[3]
model_emmeans_primary2 = do.call("rbind.data.frame", model_emmeans_primary2)
model_emmeans_primary2$group = "Primary School Students"

model_emmeans_weighted2 = model_emmeans_weighted
model_emmeans_weighted2[[1]]$outcome = food_pov_questions[1]
model_emmeans_weighted2[[2]]$outcome = food_pov_questions[2]
model_emmeans_weighted2[[3]]$outcome = food_pov_questions[3]
model_emmeans_weighted2 = do.call("rbind.data.frame", model_emmeans_weighted2)
model_emmeans_weighted2$group = "Weighted Secondary School Estimates"

# Incidence in students aged 11-16
population_size = 3576255
  
model_incidence_weighted2              = model_emmeans_weighted
model_incidence_weighted2[[1]]$outcome = food_pov_questions[1]
model_incidence_weighted2[[2]]$outcome = food_pov_questions[2]
model_incidence_weighted2[[3]]$outcome = food_pov_questions[3]
model_incidence_weighted2              = do.call("rbind.data.frame", model_incidence_weighted2)
model_incidence_weighted2$value        = population_size*model_incidence_weighted2$value
model_incidence_weighted2$.lower       = population_size*model_incidence_weighted2$.lower
model_incidence_weighted2$.upper       = population_size*model_incidence_weighted2$.upper
model_incidence_weighted2$group = "Estimated Incidence in students aged 11-16"

simple_table = do.call("rbind.data.frame", list(model_emmeans2,model_emmeans_secondary2,model_emmeans_primary2,model_emmeans_weighted2,model_incidence_weighted2)) %>%
  mutate(outcome = factor(outcome),
         group   = factor(group)
  ) %>% 
  pivot_wider(names_from = name, values_from = c(value,.lower,.upper)) %>%
  select(outcome, group,
         value_p1, .lower_p1, .upper_p1, 
         value_p2, .lower_p2, .upper_p2,
         value_p3, .lower_p3, .upper_p3) 
# `colnames<-`(c("Outcome","%","LB","UB","%","LB","UB","%","LB","UB")) %>%

simple_table %>%
  gt() %>%
  tab_row_group(
    label = "Estimated Incidence in students aged 11-16",
    rows = (group=="Estimated Incidence in students aged 11-16")
  ) %>% 
  tab_row_group(
    label = "Weighted Secondary School Estimates",
    rows = (group=="Weighted Secondary School Estimates")
  ) %>% 
  tab_row_group(
    label = "Primary School Students",
    rows = (group=="Primary School Students")
  ) %>% 
  tab_row_group(
    label = "Secondary School Students",
    rows = (group=="Secondary School Students")
  ) %>% 
  tab_row_group(
    label = "All Students",
    rows = (group=="All Students")
  ) %>% 
  tab_spanner(label = "Never or hardly ever",
              columns = c("value_p1",	".lower_p1",	".upper_p1")) %>%
  tab_spanner(label = "Some of the time",
              columns = c("value_p2",	".lower_p2",	".upper_p2")) %>%
  tab_spanner(label = "Often",
              columns = c("value_p3",	".lower_p3",	".upper_p3")) %>%
  cols_label(
    value_p1 = "%",
    .lower_p1 = "99%CI LB",
    .upper_p1 = "99%CI UB",
    value_p2 = "%",
    .lower_p2 = "99%CI LB",
    .upper_p2 = "99%CI UB",
    value_p3 = "%",
    .lower_p3 = "99%CI LB",
    .upper_p3 = "99%CI UB",
  ) %>%
  fmt_percent(
    columns = everything(),
    decimals = 1
  ) %>%
  # tab_header(
  #   title = md("Unweighted Proportions")
  # ) %>%
  tab_footnote(
    footnote = "Note. ...") %>%
  tab_stubhead(label = "Outcome") %>%
  tab_options(table.font.size = 11,
              table_body.vlines.color = "black",
              table_body.hlines.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.color  = "black",
              table.border.bottom.color = "black",
              heading.border.bottom.color = "black",
              column_labels.vlines.color = "black",
              column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              stub.border.color              = "black",
              summary_row.border.color       = "black",
              footnotes.border.bottom.color      = "black"
              # grand_summary_row.border.color  = "black"
  ) %>%
  cols_align(
    align = "left",
    columns = outcome
  ) %>%
  cols_width(
    outcome ~ px(200),
    everything() ~ px(40),
  ) %>%
  cols_hide(group) %>%
  gtsave(filename = file.path("results","1_9_rq1_all_proprtions.html"))



write.csv(simple_table, file = file.path("results","1_9_prevalence_table.csv"))
