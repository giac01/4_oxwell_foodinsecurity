# Load Data --------------------------------------------------------------------
rm(list = ls(all.names = TRUE))

source("0_2_load_packages.R")
save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

df              = readRDS(file=file.path(save_location,"sensitive_df.Rdata"))
df_impute       = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))

dat_imputed_long  =  readRDS(file=file.path(save_location,"sensitive_dat_imputed_long.Rdata"))

# Correlations between all variables -------------------------------------------

# NOT COMPLETED THIS 

# dep_qs = df_impute %>%
#   select(ends_with("_deprivation"), ends_with("foodpov"), X1020_gender) %>%
#   mutate(across(!matches("X1020_gender"), ~ factor(., levels = c("Never", "Some", "Often")))) %>%
#   mutate(X1020_gender = X1020_gender=="Other") %>%
#   mutate(across(everything(), as.numeric)) %>%
#   data.frame()
# 
# # Polychoric correlations between deprivaiton and non-binary gender identity
# 
# sapply(1:9, function(i)
#   polycor::polychor(x=dep_qs[,i], y=dep_qs$X1020_gender)
#   )

# df_impute %>%
#   select(all_of(c(predictors, outcomes))) %>% 
#   gbtoolbox::plot_correlations(suppress_warning_message = TRUE)



## Sumscore function -----------------------------------------------------------

sumscore = function(df_input=NULL,
                    percent_missing_allowed      = 0.2,
                    # verbosity
                    check_item_loadings          = TRUE,
                    plot_scores                  = TRUE,
                    print_missing_table          = TRUE,
                    print_missing_each_input_var = FALSE
){
  
  percent_missing = apply(df_input, 1, function(x) length(which(is.na(x)))/length(x))
  
  if (print_missing_table){
    percent_missing_table =  table(percent_missing)
    names(percent_missing_table) = gbtoolbox:::apa_num(as.numeric(names(percent_missing_table)))
    cat("Percent missing on the calculated sumscore variable:\n")
    print(percent_missing_table)
    cat("\n")
  }
  
  if (print_missing_each_input_var) print(apply(df_input, 2,table))
  
  if (check_item_loadings){
    item_loadings = stats::loadings(stats::princomp(covmat = stats::cov(df_input, use = "pairwise.complete.obs"), fix_sign = TRUE, cor = TRUE, scores = FALSE) )[,1] # Need to check this later!
    
    if (length(which(item_loadings<0))>0){cat(c("\n\nWARNING, negative items loadings:", which(item_loadings<0),"\n\n"))}
  }
  
  scores = apply(df_input, 1, function(x) mean(x, na.rm=TRUE)*ncol(df_input))
  
  scores[percent_missing > percent_missing_allowed] = NA
  
  if (plot_scores) hist(scores)
  
  return(scores)
}


# Correlations between food insecurity quesitons -------------------------------

# df %>%
#   select("X1440","X1470","X1500") %>%
#   mutate_all(~ recode(.,
#                       "Never or hardly ever" = 1,
#                       "Some of the time" = 2,
#                       "Often" = 3,
#                       .default = NA_real_)) %>%
#   psych::polychoric()
# # cor(., use = "pairwise.complete.obs")
# 
# colnames(df_impute)

poly_cors = 
df_impute %>%
  select("X1440_foodpov","X1470_foodpov","X1500_foodpov",
         ) %>%
  # `colnames<-`(food_pov_questions) %>%
  mutate_all(~ recode(.,
                      "Never" = 1,
                      "Some" = 2,
                      "Often" = 3,
                      .default = NA_real_)) %>%
  psych::polychoric(., global = FALSE)

poly_table = poly_cors$rho %>%
             data.frame() %>%
             mutate_all(.funs = ~gbtoolbox::apa_num(.))
             

diag(poly_table) = df_impute %>%
                   select("X1440_foodpov","X1470_foodpov","X1500_foodpov") %>%
                   apply(.,2,function(x) length(which(!is.na(x))))

poly_table
poly_table[1,2]  = df_impute %>% select("X1440_foodpov","X1470_foodpov",) %>% na.omit() %>% nrow()
poly_table[1,3]  = df_impute %>% select("X1440_foodpov","X1500_foodpov") %>% na.omit() %>% nrow()
poly_table[2,3]  = df_impute %>% select("X1470_foodpov","X1500_foodpov") %>% na.omit() %>% nrow()

poly_table %>%
  `rownames<-`(paste0(1:3,paste0(") ",food_pov_questions))) %>%
  `colnames<-`(paste0(1:3,")")) %>%
  write.csv(file.path("results","2_2_polychoric_correlations.csv"))

df_impute %>%
  select("X1440_foodpov","X1470_foodpov","X1500_foodpov",
  ) %>%
gbtoolbox::plot_pairwise_missing()


# Plots of each outcome variables ----------------------------------------------

dat_imputed_long %>%
  select(all_of(outcomes)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(label = factor(outcomes_labels[match(name, outcomes)], levels = outcomes_labels)) %>%
  ggplot(aes(x=value)) + 
  geom_histogram() +
  facet_wrap(~ label, scales = "free", nrow = 2)

ggsave(file.path("plots","2_2_outcome_distributions.png"), width = 10, height = 4)

# Frequency plot of food poverty -----------------------------------------------

#Vertical plto -----------------------------------------------------------------

df %>%
  select("X1440","X1470","X1500") %>%
  mutate_all(~na_if(., "NoResponse")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(label = food_pov_questions[match(name, c("X1440","X1470","X1500"))]) %>%
  na.omit %>%
  group_by(label, value) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  mutate(label = factor(label, levels = c("My family uses food banks","At school, I am unable to afford to eat","At home, I go to bed hungry because there is not enough food in the house"))) %>%
  ggplot(., aes(x = label, y = percentage, fill = value)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(sprintf("%.1f%%", percentage), " (", count, ")")),
    position = position_stack(vjust = 0.5)
  ) +
  xlab(NULL) +
  ylab("Percentage") +
  ggtitle(NULL) +
  theme(axis.text.x = element_text(angle = 12, hjust = 1, vjust=1)) +
  scale_fill_brewer(palette = "Set2")

ggsave(file.path("plots","2_2_food_insecurity_barplot.png"), width = 12, height = 6)

## Horizontal Plot -------------------------------------------------------------

df %>%
  select("X1440","X1470","X1500") %>%
  mutate_all(~na_if(., "NoResponse")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(label = food_pov_questions[match(name, c("X1440","X1470","X1500"))]) %>%
  na.omit %>%
  group_by(label, value) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  mutate(label = factor(label, levels = c("My family uses food banks","At school, I am unable to afford to eat","At home, I go to bed hungry because there is not enough food in the house"))) %>%
  ggplot(., aes(y = label, x = percentage, fill = value)) +
  theme_bw() +
  geom_bar(stat = "identity", width  =.9) +
  geom_text(
    aes(label = paste0(sprintf("%.1f%%", percentage), " (", count, ")")),
    position = position_stack(vjust = 0.5),
    angle = 90
  ) +
  ylab(NULL) +
  xlab("Percentage") +
  ggtitle(NULL) +
  # theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust=1, size = 10)) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = c(0,5,10, 25, 50, 75, 100),  labels = scales::percent_format(scale = 1))

ggsave(file.path("plots","2_2_food_insecurity_barplot_horizontal.pdf"), width = 14, height = 3.8)


# Distribution of ages ---------------------------------------------------------
df_impute$X1030_age %>% table

df_impute %>%
  filter(!is.na(X1030_age)) %>%  
  ggplot(aes( x = X1030_age)) + 
  geom_bar() +
  scale_x_discrete(breaks = 9:19, labels = 9:19) +
  facet_wrap(~ X1010_year)

ggsave(file.path("plots","2_2_age_dist_by_year.png"), width = 10, height = 6)

df_impute %>%
  filter(!is.na(X1030_age)) %>%  
  ggplot(aes( x = X1030_age)) + 
  geom_bar() + 
  scale_x_discrete(breaks = 9:19, labels = 9:19) 

ggsave(file.path("plots","2_2_age_dist.png"), width = 10, height = 6)

## Table -----------------------------------------------------------------------
# Load necessary libraries
library(tidyverse)

# We cannot use the df_impute variable because the age variable in there, ages of 8 have been switched to 9! 

df_temp = df[match(df_impute$RID, df$RID),]
df_temp$X1030 = df_temp$X1030 %>%
  na_if("NoResponse") %>%
  gsub("A","",.) %>% 
  gsub("\\+","",.) %>% 
  as.numeric()

# (as.numeric(df_temp$RID) == df_impute$RID) %>% table()

df_temp %>%
  select(X1030) %>%
  # # Remove NA values
  # filter(!is.na(X1030_age)) %>%
  # # Calculate the frequency distribution
  mutate(
    X1030 = ifelse(is.na(X1030),"Missing", X1030),
    X1030 = factor(X1030, levels = c("Missing",8:20))
  ) %>%
  group_by(X1030) %>%
  summarise(
    Count = n(),
    Percentage = (Count / nrow(df_temp))
  ) %>%
  # Arrange by age
  arrange(X1030) %>%
  gt() %>%
  fmt_percent(columns = Percentage) %>%
  gt::cols_label(
    X1030 = "Age"
  ) %>%
  # gt::grand_summary_rows(
  #   columns = c(Count, Percentage),
  #   fns = ~ sum(.),
  #   # fmt = ~ fmt_percent(.),
  #   side = "bottom"
  # )
  gt::gtsave(file.path("results","2_2_agetable.html"))

# Display the table
print(age_distribution)





# Outcomes: Descriptive Statistics Table ---------------------------------------
# Helper function
mutate_school_level <- function(df) {
  df %>% 
    mutate(school_level = X1010_year %in% c("Y07","Y08","Y09","Y10","Y11","Y12","Y13"),
           school_level = factor(school_level, levels = c(FALSE,TRUE), labels = c("Primary", "Secondary"))
    )
}

percent_notmissing = function(df) {
  x = c(t(df))
  x = length(which(!is.na(x))) / length(x)
  return(x)
}

# List of Questionnaire Items
sumscore_items = list(
  rcads_dep_ss = c("X1940_RCADS", "X1970_RCADS", "X1990_RCADS", "X2000_RCADS", "X2020_RCADS"),
  rcads_anx_ss = c("X1930_RCADS", "X1950_RCADS", "X1960_RCADS", "X1980_RCADS", "X2010_RCADS", "X2030_RCADS"),
  swemwbs_ss   = c("X1860_SWEMWBS", "X1870_SWEMWBS", "X1880_SWEMWBS", "X1890_SWEMWBS", "X1900_SWEMWBS", "X1910_SWEMWBS", "X1920_SWEMWBS"),
  pt_ss        = c("X2460_pt", "X2470_pt", "X2480_pt", "X2490_pt", "X2500_pt", "X2510_pt", "X2520_pt", "X2530_pt"),
  lone_ss      = c("X1750_lone", "X1760_lone", "X1770_lone", "X1780_lone"),
  scwbs_ss     = c("X2830_SCWBS", "X2840_SCWBS", "X2850_SCWBS", "X2860_SCWBS", "X2870_SCWBS", "X2880_SCWBS", "X2890_SCWBS", "X2900_SCWBS", "X2910_SCWBS", "X2920_SCWBS", "X2930_SCWBS", "X2940_SCWBS", "X2950_SCWBS", "X2960_SCWBS", "X2970_SCWBS"),
  cog_ss       = c("X2740_cog", "X2750_cog", "X2760_cog" )
)

unlist(sumscore_items) %in% colnames(df_impute)
table(duplicated(unlist(sumscore_items)))

# Calculate the proportion of missing data

primary_notmissing = 
  sapply(1:length(outcomes), function(i) {
    df_impute %>%
      mutate_school_level() %>%
      filter(school_level == "Primary") %>%
      select(all_of(sumscore_items[[i]])) %>%
      percent_notmissing
  })

secondary_notmissing = 
  sapply(1:length(outcomes), function(i) {
    df_impute %>%
      mutate_school_level() %>%
      filter(school_level == "Secondary") %>%
      select(all_of(sumscore_items[[i]])) %>%
      percent_notmissing
  })

# Calculate means for the participants with complete data 

df_sumscores_complete = df_impute %>%
  select(all_of(as.character(unlist(sumscore_items)))) %>%
  mutate_all(as.numeric) %>%
  as.data.frame()

percent_missing_allowed_input = 0

for (i in seq_along(outcomes)) {
  df_sumscores_complete[[outcomes[i]]] = 
    gbtoolbox::sum_score(
      input                        = df_impute[,sumscore_items[[i]]],
      max_percent_missing_allowed  = .2,
      plot_scores                  = TRUE,
      print_missing_table          = TRUE,
      print_missing_each_input_var = FALSE
    )
}
cog_breaks <- quantile(df_sumscores_complete$cog_ss, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

df_sumscores_complete$cog_ss =  cut(df_sumscores_complete$cog_ss , breaks = cog_breaks, include.lowest = TRUE, labels = FALSE)

# 
# df_sumscores_complete$rcads_dep_ss = gbtoolbox::sum_score( 
#   df_input                     = df_impute[,sumscore_items[[1]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )
# df_sumscores_complete$rcads_anx_ss = gbtoolbox::sum_score( 
#   df_input                     = df_impute[,sumscore_items[[2]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )
# df_sumscores_complete$swemwbs_ss   = gbtoolbox::sum_score( 
#   df_input                     = df_impute[,sumscore_items[[3]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )
# df_sumscores_complete$pt_ss        = gbtoolbox::sum_score( 
#   df_input                     = df_impute[,sumscore_items[[4]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )
# df_sumscores_complete$lone_ss      = gbtoolbox::sum_score( 
#   df_input         = df_impute[,sumscore_items[[5]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )
# df_sumscores_complete$scwbs_ss     = gbtoolbox::sum_score( 
#   df_input         = df_impute[,sumscore_items[[6]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )
# df_sumscores_complete$cog     = gbtoolbox::sum_score( 
#   df_input         = df_impute[,sumscore_items[[7]]], 
#   max_percent_missing_allowed  = .2,
#   plot_scores                  = TRUE,
#   print_missing_table          = TRUE,
#   print_missing_each_input_var = TRUE
# )

df_sumscores_complete = df_sumscores_complete %>%
  mutate(X1010_year = df_impute$X1010_year) %>%
  mutate_school_level()

stats_not_imputed_df = 
  df_sumscores_complete %>%
  group_by(school_level) %>%
  summarise(
    across(all_of(outcomes), list(N = ~length(which(!is.na(.x))),
                                  `Mean` = ~(mean(.x, na.rm = TRUE)),
                                  SD = ~(sd(.x, na.rm = TRUE)))
    )
  ) %>%
  pivot_longer(
    cols = -school_level, 
    names_to = c("outcome", "statistic"), 
    names_pattern = "([a-z_]*)_(.*)"
  ) %>%
  pivot_wider(
    names_from = c(statistic, school_level),
    values_from = value
  ) 

# Calculate N, M, SD 
stats_df <- dat_imputed_long %>%
  mutate_school_level() %>%
  group_by(school_level) %>%
  summarise(
    across(all_of(outcomes), list(N    = ~length(which(!is.na(.x))) / max(dat_imputed_long$.imp),
                                  Mean = ~(mean(.x, na.rm = TRUE)),
                                  SD   = ~(sd(.x, na.rm = TRUE)))
    )
  ) %>%
  pivot_longer(
    cols = -school_level, 
    names_to = c("outcome", "statistic"), 
    names_pattern = "([a-z_]*)_(.*)"
  ) %>%
  pivot_wider(
    names_from = c(statistic, school_level),
    values_from = value
  ) %>%
  # Percentage of completions 
  mutate(perc_not_missing_p = primary_notmissing, .after = 2) %>%
  mutate(perc_not_missing_s = secondary_notmissing, .after = 6) %>%
  # Summary stats from non-imputed data
  mutate(n_ni_p = stats_not_imputed_df$N_Primary, .after = 5) %>%
  mutate(mean_ni_p = stats_not_imputed_df$Mean_Primary, .after = 6) %>%
  mutate(sd_ni_p = stats_not_imputed_df$SD_Primary, .after = 7) %>%
  
  mutate(n_ni_s = stats_not_imputed_df$N_Secondary, .after = 12) %>%
  mutate(mean_ni_s = stats_not_imputed_df$Mean_Secondary, .after = 13) %>%
  mutate(sd_ni_s = stats_not_imputed_df$SD_Secondary, .after = 14) %>%
  mutate(outcome = outcomes_labels[match(outcome, outcomes)])



# 
# # Convert 0, NaN or NA to "" 
# mutate(across(where(is.numeric), ~replace_na(.x, ""))) %>%
# mutate(across(where(is.numeric), ~replace(.x, .x == 0, ""))) %>%
# mutate(across(where(is.numeric), ~replace(.x, is.nan(.x), "")))
# 


# Prettify table using gt

stats_df %>%
  gt() %>%   
  tab_spanner(
    label = md("**Imputed Data**"),
    columns = c( N_Primary,perc_not_missing_p, Mean_Primary, SD_Primary),
    id = "imputed_data_p",
    level = 1
  ) %>%
  tab_spanner(
    label = md("**Imputed Data**"),
    columns = c( N_Secondary,perc_not_missing_s, Mean_Secondary, SD_Secondary),
    id = "imputed_data_s",
    level = 1
  ) %>%
  tab_spanner(
    label = md("**Not-Imputed Data**"),
    columns = c( n_ni_p, mean_ni_p, sd_ni_p),
    id = "not_imputed_data_p",
    level = 1
  ) %>%
  tab_spanner(
    label = md("**Not-Imputed Data**"),
    columns = c(  n_ni_s, mean_ni_s, sd_ni_s),
    id = "not_imputed_data_s",
    level = 1
  ) %>%
  tab_spanner(
    label = md("**Primary School**"),
    columns = c( N_Primary,perc_not_missing_p, Mean_Primary, SD_Primary, n_ni_p, mean_ni_p, sd_ni_p ),
    level   = 2
  ) %>%
  tab_spanner(
    label = md("**Secondary School**"),
    columns = c( N_Secondary,perc_not_missing_s, Mean_Secondary, SD_Secondary, n_ni_s, mean_ni_s, sd_ni_s),
    level   = 2
  ) %>%
  cols_label(
    outcome        ~ md("**Outcome**"),
    N_Primary      ~ md("**N**"),
    N_Secondary    ~ md("**N**"),
    n_ni_p         ~ md("**N**"),
    n_ni_s         ~ md("**N**"),
    mean_ni_p      ~ md("**Mean**"),
    mean_ni_s      ~ md("**Mean**"),
    Mean_Primary   ~ md("**Mean**"),
    Mean_Secondary ~ md("**Mean**"),
    SD_Primary     ~ md("**SD**"),
    SD_Secondary   ~ md("**SD**"),
    sd_ni_p        ~ md("**SD**"),
    sd_ni_s        ~ md("**SD**"),
    perc_not_missing_p ~ md("**Complete**"),
    perc_not_missing_s ~ md("**Complete**")
  ) %>%  
  fmt(
    columns = everything(),
    fns = function(x) ifelse(is.na(x), "", x)
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2
    
  ) %>%
  fmt_number(
    columns = c(N_Primary,N_Secondary, n_ni_p, n_ni_s),
    decimals = 0
  ) %>%
  fmt_percent(
    columns  = c(perc_not_missing_p,perc_not_missing_s),
    decimals = 1
  ) %>%
  tab_options(table.font.size = 13,
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
  cols_width(
    outcome ~ px(100),
    n_ni_s             ~ px(50),
    n_ni_p             ~ px(50),
    N_Secondary        ~ px(50),
    N_Primary          ~ px(50),
    perc_not_missing_p ~ px(70),
    perc_not_missing_s ~ px(70),
    everything() ~ px(40),
  ) %>%
  cols_align(align = c("center"), columns = !contains("outcome") ) %>%
  tab_footnote(
    footnote = md("**Complete** = percentage of non-missing values on individual questionnaire items. To calculate non-imputed summary scores, only data was retained when participants completed all items of the respective questionnaire."),
  ) %>%
  gtsave(filename = file.path("results","2_2_descriptive_table_outcomes.html"))#, zoom = 4, expand = 10)




stats_df %>%
  write.csv(file.path("results","2_2_descriptive_stats.csv"), row.names = FALSE)


# Demographics : Descriptive Statistics Table ----------------------------------

remove_pps = !(df$RID %in% df_impute$RID)


table(df$X1020_gender, useNA = "always")

table(df$X1050, useNA = "always")
table(df$X1020_gender, useNA = "always")

total_primary = df %>%
  filter(!remove_pps) %>%
  mutate_school_level() %>%
  filter(school_level == "Primary") %>%
  nrow()

total_secondary = df %>%
  filter(!remove_pps) %>%
  mutate_school_level() %>%
  filter(school_level == "Secondary") %>%
  nrow()

set_gender_to_na = (df$X1020_gender=="Other" & df$gender_silly_response)
df$X1020_gender[set_gender_to_na] = NA

demog_table = df %>%
  filter(!remove_pps) %>%
  mutate_school_level() %>%
  select(school_level, X1020_gender, X1040_ethnicity, X1050, X1060) %>%
  mutate(X1050 = recode(X1050,
                        `Yes`   = "UK*",
                        `No` = "Not UK",
                        .default = "Missing Data")) %>%
  mutate(X1060 = recode(X1060,
                        `Yes, one parent`   = "Yes, one parent*",
                        `Yes, both parents` = "Yes, both parents*",
                        `Neither parent`    = "Neither parent",
                        .default = "Missing Data")) %>%
  mutate(X1040_ethnicity = recode(as.character(X1040_ethnicity),
                                  Asian = "Asian",
                                  Black = "Black",
                                  Mixed = "Mixed",
                                  Other = "Other",
                                  White = "White*",
                                  .missing = "Missing Data",
                                  .default = "Missing Data")) %>%
  mutate(X1020_gender = recode(as.character(X1020_gender),
                               Female   = "Female*",
                               Male     = "Male",
                               Other    = "Gender Diverse",
                               .missing = "Missing Data",
                               .default = "Missing Data")) %>%
  
  `colnames<-`(c("school_level", "Gender","Ethnicity", "Child Born UK", "Parent Born UK")) %>%
  mutate(across(-school_level, as.character)) %>%  # Convert all columns except school_level to character
  pivot_longer(cols = -school_level, names_to = "category", values_to = "level") %>%
  count(school_level, category, level) %>%
  pivot_wider(names_from = school_level, values_from = n, values_fill = list(n = 0)) %>%
  group_by(category) %>%
  mutate(
    Primary_percentage   = ifelse(level=="Missing Data", NA, Primary)/sum(ifelse(level=="Missing Data", 0, Primary)),
    Secondary_percentage = ifelse(level=="Missing Data", NA, Secondary)/sum(ifelse(level=="Missing Data", 0, Secondary)),
  ) %>%
  ungroup() %>%
  add_row(category = "Total",
          level = "",
          Primary = total_primary,
          Primary_percentage = 1,
          Secondary = total_secondary,
          Secondary_percentage = 1) %>%
  mutate(category = factor(category, levels = c("Gender", "Ethnicity", "Child Born UK", "Parent Born UK", "Total"))) %>%
  mutate(level    = factor(level,    levels = c("Female*", "Male", "Gender Diverse", "White*", "Asian", "Black","Mixed","UK*","Not UK", "Yes, one parent*","Yes, both parents*","Neither parent","Other","Missing Data",""))) %>%
  arrange(category, level) %>%
  select(category, level, Primary, Primary_percentage, Secondary, Secondary_percentage)

demog_table


# add row at the end with totals

demog_table %>% 
  group_by(category) %>%
  gt(.,
     # rowname_col = "category"
  ) %>%
  fmt(
    columns = contains("percent"),
    fns = function(x) ifelse(is.na(x), "", x)
  ) %>%
  fmt_percent(
    columns  = contains("percent"),
    decimals = 2
  ) %>%
  cols_label(
    level       ~ md("**Subgroup**"),
    Primary        ~ md("**N**"),
    Secondary      ~ md("**N**"),
    Primary_percentage   ~ md("**%**"),
    Secondary_percentage ~ md("**%**")
  ) %>%
  tab_spanner(
    label = md("**Primary School**"),
    columns = contains("primary"),
    level = 1
  ) %>%
  tab_spanner(
    label = md("**Secondary School**"),
    columns = contains("secondary"),
    level = 1
  ) %>%
  # add footnote:
  tab_footnote(
    footnote = ("*Reference group for dummy coding. Prefer not to say respones are coded as missing."),
    placement = "left"
  ) %>%
  
  tab_options(table.font.size = 13,
              table_body.vlines.color = "black",
              table_body.hlines.color = "black",
              table_body.border.top.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.color  = "black",
              table.border.bottom.color = "black",
              # heading.border.top.color = "black",
              heading.border.bottom.color = "black",
              heading.border.lr.color = "red",
              column_labels.vlines.color = "black",
              column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              stub.border.color              = "black",
              summary_row.border.color       = "black",
              footnotes.border.bottom.color      = "black",
              row_group.border.top.color = "black",
              row_group.border.bottom.color = "black"
              # grand_summary_row.border.color  = "black"
  ) %>%
  # gtsave(filename = file.path("results","2_2_descriptive_table_demographics.pdf"), zoom = 4, expand = 1)

gtsave(filename = file.path("results","2_2_descriptive_table_demographics.html"))#, zoom = 4, expand = 10)


# Data on Age -------------------------------------------------------------------

