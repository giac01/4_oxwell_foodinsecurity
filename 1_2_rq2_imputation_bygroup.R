# Load data --------------------------------------------------------------------
rm(list=ls(all.names = TRUE))

source("0_2_load_packages.R")
save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

df_impute = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))
df_impute_split = readRDS(file=file.path(save_location,"sensitive_df_impute_split.Rdata"))

## Set up predictor matrix ----------------------------------------------------

ignore_vars = c("school_ID_new",
                # "X1010_Y05", "X1010_Y06", "X1010_Y07", "X1010_Y08", "X1010_Y09","X1010_Y10","X1010_Y11","X1010_Y12","X1010_Y13",
                # "X1030_age",
                "X1010_year",
                "RID",
                "original_rownames"
)


colnames(df_impute)[!(colnames(df_impute) %in% ignore_vars)]

gsub("_.*","",colnames(df_impute)) %>% duplicated %>% table

# Tell R which variables to impute and use in imputation

pred_matrix = matrix(1, ncol(df_impute), ncol(df_impute))

colnames(pred_matrix) = rownames(pred_matrix) = colnames(df_impute)
  diag(pred_matrix) = 0
  pred_matrix[,ignore_vars] = 0
  pred_matrix[ignore_vars,] = 0

# Specify different predictorMatrix for primary and secondary school children. 
# Primary School:
pred_matrix_primary = pred_matrix
  pred_matrix_primary[secondary_specific_variables,] = 0
  pred_matrix_primary[,secondary_specific_variables] = 0
# Secondary School:
pred_matrix_secondary = pred_matrix
  pred_matrix_secondary[primary_specific_variables,] = 0
  pred_matrix_secondary[,primary_specific_variables] = 0
  
## Run imputation model --------------------------------------------------------
  
timea = Sys.time()
mice_list = list()

for (i in 1:9){
  rm(x)
  imputation_number = 100
  plan(multisession, workers = min(c(imputation_number, 8)))
  
  if(i<3) { x = pred_matrix_primary}
  if(i>2) { x = pred_matrix_secondary}
  
  # Running mice in parallel seems to ignore the where input!! 
  mice_list[[i]] = mice::futuremice(data = df_impute_split[[i]], 
                             parallelseed  = 10,
                             m = imputation_number, 
                             method = "pmm", 
                             predictorMatrix = x,
                             n.core = min(c(imputation_number,8)),
  )
  
  print(paste0("Completed ", i))
}
# rbind_mice = do.call("rbind", mice_list)   # This doesn't work! 

rbind_mice = mice_list[[1]] 
  rbind_mice = rbind(rbind_mice, mice_list[[2]])
  rbind_mice = rbind(rbind_mice, mice_list[[3]])
  rbind_mice = rbind(rbind_mice, mice_list[[4]])
  rbind_mice = rbind(rbind_mice, mice_list[[5]])
  rbind_mice = rbind(rbind_mice, mice_list[[6]])
  rbind_mice = rbind(rbind_mice, mice_list[[7]])
  rbind_mice = rbind(rbind_mice, mice_list[[8]])
  rbind_mice = rbind(rbind_mice, mice_list[[9]])

plan(sequential)

timeb = Sys.time()

timeb - timea

# Takes 8 minutes for 8 parallel imputations 
# Takes 3 minutes for 2 parallel imputations

# imputed_dataset_long = dat_imputed %>%
#                        complete(action = "long", include = FALSE) %>%
#                        filter(.imp>0) 



if( TRUE ){
  imputed_dataset = rbind_mice %>%
    complete(action = "all", include = TRUE) %>%
    .[[2]] 
  
  original_dataset = rbind_mice %>%
    complete(action = "all", include = TRUE) %>%
    .[[1]] 
  
  table(original_dataset$X2870_SCWBS, imputed_dataset$X2870_SCWBS, useNA = "always")
  
  table(original_dataset$X1450_deprivation, imputed_dataset$X1450_deprivation, useNA = "always")
  
  imputed_dataset %>%
    gbtoolbox::plot_pairwise_missing(., textadjust=1.5 , divisor = 1000) + 
    labs(title = "Pairwise Missing Plot: Post Imputation") 
    ggsave(file.path("plots","3_imputation_plotpairwisemissing_imputed_all.png"), width = 11.5, height = 11.5, dpi = 600)
  
  imputed_dataset %>%
    filter(!(X1010_year %in% c("Y05","Y06"))) %>%
    gbtoolbox::plot_pairwise_missing(., textadjust=1.5 , divisor = 1000) + 
    labs(title = "Pairwise Missing Plot: Post Imputation, Secondary Students") 
    ggsave(file.path("plots","3_imputation_plotpairwisemissing_imputed_secondary.png"), width = 11.5, height = 11.5, dpi = 600)
  
  imputed_dataset %>%
    filter((X1010_year %in% c("Y05","Y06"))) %>%
    gbtoolbox::plot_pairwise_missing(., textadjust=1.5 , divisor = 1000) + 
    labs(title = "Pairwise Missing Plot: Post Imputation, Primary Students") 
    ggsave(file.path("plots","3_imputation_plotpairwisemissing_imputed_primary.png"), width = 11.5, height = 11.5, dpi = 600)
  
  original_dataset %>%
    gbtoolbox::plot_pairwise_missing(., textadjust=1.5 , divisor = 1000) + 
    labs(title = "Pairwise Missing Plot: Pre Imputation")
    ggsave(file.path("plots","3_imputation_plotpairwisemissing_pre-imputation_all.png"), width = 11.5, height = 11.5, dpi = 600)

}


## Add sumscores to mice object ------------------------------------------------


imputed_data_list = rbind_mice %>%
                    complete("all", include = FALSE)


imputed_data_list = lapply(seq_along(imputed_data_list), function(i){
                           dataset = imputed_data_list[[i]]
                           
                           #SWEMWBS -------------------------------------------------
                           if(i ==1 ) cat(("\nSWEMWBS \n"))
                           dataset$swemwbs_ss     = gbtoolbox::sum_score(dataset[,c("X1860_SWEMWBS", "X1870_SWEMWBS", "X1880_SWEMWBS", "X1890_SWEMWBS", "X1900_SWEMWBS", "X1910_SWEMWBS", "X1920_SWEMWBS")],
                                                                         print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                         print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                         plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                         
                                                                         
                                                                         )
                           if(i ==1 ){print(table(dataset$swemwbs_ss))}
                           
                           #RCADS Depression -------------------------------------------------
                           if(i ==1 ) cat(("\nRCADS Depression \n"))
                           dataset$rcads_dep_ss   = gbtoolbox::sum_score(dataset[,c("X1940_RCADS", "X1970_RCADS", "X1990_RCADS", "X2000_RCADS", "X2020_RCADS")],
                                                                         print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                         print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                         plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                         
                                                                         
                           )
                           if(i ==1 ){print(table( dataset$rcads_dep_ss))}
 
                           #RCADS Anxiety -------------------------------------------------
                           if(i ==1 ) cat(("\nRCADS Anxiety \n"))
                           dataset$rcads_anx_ss   = gbtoolbox::sum_score(dataset[,c("X1930_RCADS", "X1950_RCADS", "X1960_RCADS", "X1980_RCADS", "X2010_RCADS", "X2030_RCADS")],
                                                                         print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                         print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                         plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                         
                                                                         
                           )
                           if(i ==1 ){print(table( dataset$rcads_anx_ss))}
                           
                           #Positive Thoughts -------------------------------------------------
                           if(i ==1 ) cat(("\nPositive Thoughts \n"))
                           dataset$pt_ss   = gbtoolbox::sum_score(dataset[,c("X2460_pt", "X2470_pt", "X2480_pt", "X2490_pt", "X2500_pt", "X2510_pt", "X2520_pt", "X2530_pt")],
                                                                         print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                         print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                         plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                         
                                                                         
                           )
                           if(i ==1 ){print(table( dataset$pt_ss))}
                           
                           #Stirling Child Wellbeing Scale -------------------------------------------------
                           if(i ==1 ) cat(("\nStirling Child Wellbeing Scale \n"))
                           dataset$scwbs_ss = gbtoolbox::sum_score(dataset[,c("X2830_SCWBS", "X2840_SCWBS", "X2850_SCWBS", "X2860_SCWBS", "X2870_SCWBS", "X2880_SCWBS", "X2890_SCWBS", "X2900_SCWBS", "X2910_SCWBS", "X2920_SCWBS",
                                                                              "X2930_SCWBS", "X2940_SCWBS", "X2950_SCWBS", "X2960_SCWBS", "X2970_SCWBS")],
                                                                  print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                  print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                  plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                  
                                                                  
                           )
                           if(i ==1 ){print(table( dataset$scwbs_ss))}
                           
                           # Loneliness -------------------------------------------------
                           if(i ==1 ) cat(("\n Loneliness \n"))
                           dataset$lone_ss = gbtoolbox::sum_score(dataset[,c("X1750_lone", "X1760_lone", "X1770_lone", "X1780_lone")],
                                                                   max_percent_missing_allowed = 0,
                                                                   print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                   print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                   plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                  
                           ) 
                           
                           
                           if(i ==1 ){print(table( dataset$lone_ss))}
                           
                           # Cognition -------------------------------------------------
                           # browser()
                           if(i ==1 ) cat(("\n Cognition \n"))
                           dataset$cog_sumscore = gbtoolbox::sum_score(dataset[,c("X2740_cog", "X2750_cog", "X2760_cog")],
                                                                  max_percent_missing_allowed = 0,
                                                                  print_missing_table  = ifelse(i==1, TRUE, FALSE), 
                                                                  # print_missing_each_input_var = ifelse(i==1, TRUE, FALSE),
                                                                  plot_scores = ifelse(i==1, TRUE, FALSE)
                                                                  
                           ) 
                           
                           cog_breaks <- quantile(dataset$cog_sumscore, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
                           
                           dataset$cog_ss = cut(dataset$cog_sumscore , breaks = cog_breaks, include.lowest = TRUE, labels = FALSE)

                           # if(i ==1 ){print(table( round(dataset$cog_sumscore/30)))}
                           if(i ==1 ){print(table( dataset$cog_ss))}
                           
                           # Age -------------------------------------------------

                           age_data = dataset %>%
                             mutate_at(vars(X1030_age), as.factor) %>%
                             model.matrix(~ 0 + X1030_age, data = .) %>%
                             as.data.frame()
                           dataset = cbind.data.frame(dataset, age_data)
                           
                           # Year -----------------------------------------------
                           year_data = dataset %>%
                             mutate(X1010_ = factor(X1010_year)) %>%
                             model.matrix(~ 0 + X1010_, data = .) %>%
                             as.data.frame()
                           dataset = cbind.data.frame(dataset, year_data)
                           
                           # Ethnicity -----------------------------------------
                           ethnicity_data = dataset %>%
                             mutate(X1040_ethnicity = factor(X1040_ethnicity)) %>%
                             model.matrix(~ 0 + X1040_ethnicity, data = .) %>%
                             as.data.frame()
                           dataset = cbind.data.frame(dataset, ethnicity_data)

                           # Gender --------------------------------------------
                           dataset = dataset %>%
                                     mutate(
                                       X1020_gender_m = as.numeric(X1020_gender=="Male"),
                                       X1020_gender_f = as.numeric(X1020_gender=="Female"),
                                       X1020_gender_o = as.numeric(X1020_gender=="Other")
                                     )
                           table(dataset$X1020)
                           
                           
                           # Deprivation ---------------------------------------

                           dep = dataset %>% select(contains("_deprivation"))

                           deprivation = 
                           lapply(colnames(dep), function(i)  model.matrix( as.formula(paste0("~ 0 + ", i)) , data = dep))  %>%
                             do.call("cbind.data.frame",.) %>%
                             tibble(.,) %>%
                             select(-contains("Never"))
                           
                           dataset = cbind.data.frame(dataset, deprivation)
                           
                           # Food poverty --------------------------------------
                           fp = dataset %>% select(contains("_foodPov"))
                           
                           
                           fp = 
                             lapply(colnames(fp), function(i)  model.matrix( as.formula(paste0("~ 0 + ", i)) , data = fp))  %>%
                             do.call("cbind.data.frame",.) %>%
                             tibble(.,) %>%
                             select(-contains("Never"))
                           
                           dataset = cbind.data.frame(dataset, fp)

                           
                           return(dataset)
                           }
                           )

# Create Long version of imputed dataset ----------------------------------------

dat_imputed_long = imputed_data_list %>%
  map2(1:length(imputed_data_list), ~ .x %>% mutate(.imp = .y)) %>%
  do.call("rbind.data.frame",.)

saveRDS(imputed_data_list, file=file.path(save_location,"sensitive_imputed_data_list.Rdata"))
saveRDS(dat_imputed_long,  file=file.path(save_location,"sensitive_dat_imputed_long.Rdata"))



# save.image(file=file.path("r_output_enc","workspace_justimputation.Rdata"))
