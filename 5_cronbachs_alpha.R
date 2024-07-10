# Load data --------------------------------------------------------------------
rm(list=ls(all.names = TRUE))

source("0_2_load_packages.R")
save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

df_impute = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))


#

vars = list(
  rcads_dep_ss = c("X1940_RCADS", "X1970_RCADS", "X1990_RCADS", "X2000_RCADS", "X2020_RCADS"),
  rcads_anx_ss = c("X1930_RCADS", "X1950_RCADS", "X1960_RCADS", "X1980_RCADS", "X2010_RCADS", "X2030_RCADS"),
  swemwbs_ss   = c("X1860_SWEMWBS", "X1870_SWEMWBS", "X1880_SWEMWBS", "X1890_SWEMWBS", "X1900_SWEMWBS", "X1910_SWEMWBS", "X1920_SWEMWBS"),
  pt_ss        = c("X2460_pt", "X2470_pt", "X2480_pt", "X2490_pt", "X2500_pt", "X2510_pt", "X2520_pt", "X2530_pt"),
  lone_ss      = c("X1750_lone", "X1760_lone", "X1770_lone", "X1780_lone"),
  scwbs_ss     = c("X2830_SCWBS", "X2840_SCWBS", "X2850_SCWBS", "X2860_SCWBS", "X2870_SCWBS", "X2880_SCWBS", "X2890_SCWBS", "X2900_SCWBS", "X2910_SCWBS", "X2920_SCWBS",
                   "X2930_SCWBS", "X2940_SCWBS", "X2950_SCWBS", "X2960_SCWBS", "X2970_SCWBS"),
  cog_ss       = c("X2740_cog", "X2750_cog", "X2760_cog")
)

cronbach_alpha_results = list()

for (i in seq_along(vars)){
  
  cronbach_alpha_results[[i]] = psych::alpha(
    df_impute[vars[[i]]],
    check.keys = TRUE
)
  
}

cronbach_alpha_results[[1]]$feldt$alpha

alpha = sapply(cronbach_alpha_results, function(x) x$total$std.alpha)
feldt_alpha = sapply(cronbach_alpha_results, function(x) x$feldt$alpha) %>% unlist()
feldt_alpha_lci = sapply(cronbach_alpha_results, function(x) x$feldt$lower.ci)%>% unlist()
feldt_alpha_uci = sapply(cronbach_alpha_results, function(x) x$feldt$upper.ci) %>% unlist()

data.frame(outcomes_labels, alpha, feldt_alpha, feldt_alpha_lci, feldt_alpha_uci) %>%
  gt() %>%
  gt::fmt_percent(decimals = 0) %>%
  gt::gtsave(filename = file.path("results", "5_alpha_nodec.html"))

data.frame(outcomes_labels, alpha, feldt_alpha, feldt_alpha_lci, feldt_alpha_uci) %>%
  gt() %>%
  gt::fmt_percent(decimals = 2) %>%
  gt::gtsave(filename = file.path("results", "5_alpha_2dec.html"))
