rm(list = ls())
library(tidyverse)
library(gt)
library(survey)

save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

census_counts   = readxl::read_xlsx(file.path("results","census_freq.xlsx"))
df              = readRDS(file=file.path(save_location,"sensitive_df.Rdata"))
df_impute       = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))

df_impute$SCHOOLTYPE        = df$SCHOOLTYPE[match(df_impute$RID, df$RID)]

df_impute$SCHOOLTYPE_BINARY =  case_when(
  df_impute$SCHOOLTYPE =="Independent school"     ~ "Independent",
  df_impute$SCHOOLTYPE =="State-funded secondary" ~ "State",
  df_impute$SCHOOLTYPE =="State-funded primary"   ~ "State",
  TRUE                                            ~ NA_character_
)

# df_impute$SCHOOLTYPE        = df$SCHOOLTYPE[match(df_impute$RID, df$RID)]
# df_impute$SCHOOLTYPE_BINARY = df$SCHOOLTYPE_BINARY[match(df_impute$RID, df$RID)]

df_impute$SCHOOLTYPE %>% unique()

table(df_impute$SCHOOLTYPE, useNA = "always")

sample_counts = 
df_impute %>%
  filter(X1010_year %in% c("Y05","Y06","Y07","Y08","Y09","Y10","Y11")) %>%
  mutate(X1010_year = factor(X1010_year)) %>%
  # select(SCHOOLTYPE, X1010_year) %>%
  mutate(SCHOOLTYPE = case_when(
    SCHOOLTYPE =="Independent school"     ~ "Independent",
    SCHOOLTYPE =="State-funded secondary" ~ "State",
    SCHOOLTYPE =="State-funded primary"   ~ "State",
    TRUE                                            ~ NA_character_
  ))

sample_counts_all = sample_counts %>%
  select(SCHOOLTYPE, X1010_year) %>%
  table() %>%
  data.frame() %>%
  arrange(SCHOOLTYPE) %>%
  rename(Freq_all = Freq)
  
sample_counts_fi1 = sample_counts %>%
  filter(!is.na(X1440_foodpov)) %>%
  select(SCHOOLTYPE, X1010_year) %>%
  table() %>%
  data.frame() %>%
  arrange(SCHOOLTYPE) %>%
  rename(Freq_fi1 = Freq)

sample_counts_fi2 = sample_counts %>%
  filter(!is.na(X1470_foodpov)) %>%
  select(SCHOOLTYPE, X1010_year) %>%
  table() %>%
  data.frame() %>%
  arrange(SCHOOLTYPE) %>%
  rename(Freq_fi2 = Freq)

sample_counts_fi3 = sample_counts %>%
  filter(!is.na(X1500_foodpov)) %>%
  select(SCHOOLTYPE, X1010_year) %>%
  table() %>%
  data.frame() %>%
  arrange(SCHOOLTYPE) %>%
  rename(Freq_fi3 = Freq)

sample_counts_combined = do.call("cbind.data.frame", list(sample_counts_all,sample_counts_fi1,sample_counts_fi2, sample_counts_fi3)) %>%
  select(c(1:3,6,9,12)) %>%
  mutate(
    `Year Group` = as.numeric(gsub("Y","", X1010_year))
  )

# Creat Weights --------------------------------------------------------------

census_counts_clean = census_counts %>%
  mutate(SCHOOLTYPE = ifelse(`School Type`=="Independent","Independent","State"))

sample_counts_all %>%
  data.frame() %>% 
  arrange(SCHOOLTYPE)

weights = 
dplyr::full_join(census_counts_clean, sample_counts_combined, by = c("SCHOOLTYPE", "Year Group")) %>%
  select(-`Non-Adusted Numbers`,- `Adjustment Factor`, - census_percent) %>%
  mutate(
    weight_all = census_headcount/Freq_all,
    weight_fi1 = census_headcount/Freq_fi1,
    weight_fi2 = census_headcount/Freq_fi2,
    weight_fi3 = census_headcount/Freq_fi3,
  ) %>%
  mutate(
    group = paste(SCHOOLTYPE, `Year Group`, sep = "_")
  )

write.csv(weights, file = file.path("results","1_11_weights.csv"))

# Add weights to complete dataset

df_impute$group = paste(df_impute$SCHOOLTYPE_BINARY,as.numeric(gsub("Y","", df_impute$X1010_year)), sep = "_")
# df$group        = paste(       df$SCHOOLTYPE_BINARY,as.numeric(gsub("Y","",        df$X1010_year)), sep = "_")

df_impute$weight_all = weights$weight_all[match(df_impute$group, weights$group)]
df_impute$weight_fi1 = weights$weight_fi1[match(df_impute$group, weights$group)]
df_impute$weight_fi2 = weights$weight_fi2[match(df_impute$group, weights$group)]
df_impute$weight_fi3 = weights$weight_fi3[match(df_impute$group, weights$group)]


census_counts_clean$census_headcount %>% sum()

df_impute$weight_fi1 %>% is.na %>% table


SVY_fi1 <- svydesign(ids=~1,weights=~weight_fi1, data=df_impute[!is.na(df_impute$X1440_foodpov) & !is.na(df_impute$weight_all),])
SVY_fi2 <- svydesign(ids=~1,weights=~weight_fi2, data=df_impute[!is.na(df_impute$X1470_foodpov) & !is.na(df_impute$weight_all),])
SVY_fi3 <- svydesign(ids=~1,weights=~weight_fi3, data=df_impute[!is.na(df_impute$X1500_foodpov) & !is.na(df_impute$weight_all),])

results_fi1 = svytotal(~X1440_foodpov,design=SVY_fi1, na.rm = TRUE, level = .99) 
results_fi2 = svytotal(~X1470_foodpov,design=SVY_fi2, na.rm = TRUE, level = .99) 
results_fi3 = svytotal(~X1500_foodpov,design=SVY_fi3, na.rm = TRUE, level = .99) 

results_fi1 %>% 
  data.frame() %>%
  pull(total) %>%
  sum()

results_fi2 %>% 
  data.frame() %>%
  pull(total) %>%
  sum()

results_fi3 %>% 
  data.frame() %>%
  pull(total) %>%
  sum()

results_total_list = list()
results_total_list[["fi1"]] = cbind.data.frame(data.frame(results_fi1), data.frame(confint(results_fi1, level = .99)))
results_total_list[["fi2"]] = cbind.data.frame(data.frame(results_fi2), data.frame(confint(results_fi2, level = .99)))
results_total_list[["fi3"]] = cbind.data.frame(data.frame(results_fi3), data.frame(confint(results_fi3, level = .99)))

do.call("cbind.data.frame",results_total_list) %>%
  write.csv(file.path("results","1_11_results_total_list.csv"))

results_percent_list = results_total_list
results_percent_list[[1]] = results_percent_list[[1]] / sum(results_percent_list[[1]]$total)
results_percent_list[[2]] = results_percent_list[[2]] / sum(results_percent_list[[2]]$total)
results_percent_list[[3]] = results_percent_list[[3]] / sum(results_percent_list[[3]]$total)

do.call("cbind.data.frame",results_percent_list) %>%
  write.csv(file.path("results","1_11_results_percent_list.csv"))



results_total_list %>%
  imap_dfr(~.x %>% 
             as_tibble(rownames = "category") %>% 
             mutate(question = .y)) %>%
  mutate(category = str_remove(category, "X\\d+_foodpov"),
         question = str_remove(question, "fi")) %>%
  select(question, category, total,  X0.5.., X99.5..) %>%
  pivot_wider(names_from = category, 
              values_from = c(total,  X0.5.., X99.5..)) %>%
  # rename_with(~str_replace(., "total_", "")) %>%
  rename_with(~str_replace(., "X0.5.._", "LB_")) %>%
  rename_with(~str_replace(., "X99.5.._", "UB_")) %>%
  select(question, 
         total_Never, LB_Never, UB_Never, 
         total_Some,  LB_Some,  UB_Some,
         total_Often, LB_Often, UB_Often
         ) %>%
  write.csv(file.path("results","1_11_results_total_list_2.csv"))


results_percent_list %>%
  imap_dfr(~.x %>% 
             as_tibble(rownames = "category") %>% 
             mutate(question = .y)) %>%
  mutate(category = str_remove(category, "X\\d+_foodpov"),
         question = str_remove(question, "fi")) %>%
  select(question, category, total,  X0.5.., X99.5..) %>%
  pivot_wider(names_from = category, 
              values_from = c(total,  X0.5.., X99.5..)) %>%
  # rename_with(~str_replace(., "total_", "")) %>%
  rename_with(~str_replace(., "X0.5.._", "LB_")) %>%
  rename_with(~str_replace(., "X99.5.._", "UB_")) %>%
  select(question, 
         total_Never, LB_Never, UB_Never, 
         total_Some,  LB_Some,  UB_Some,
         total_Often, LB_Often, UB_Often
  ) %>%
  write.csv(file.path("results","1_11_results_percent_list_2.csv"))

