# Load Packages ----------------------------------------------------------------

source("0_2_load_packages.R")

save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

df              = readRDS(file=file.path(save_location,"sensitive_df.Rdata"))
df_impute       = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))

## Pairwwise missing plots ------------------------------------------------------

df_impute %>%
  select(all_of(var_main_nodummy)) %>%
  gbtoolbox::plot_pairwise_missing(textadjust = 1.5, divisor = 100) +
  ggplot2::labs(title = "Whole Sample", y = 'Sample Size x 1/100', x = 'Sample Size x 1/100')
ggsave(file.path("plots","3_descriptive_plotpairwisemissing_all.png"), width = 11, height = 11, dpi = 600)

df_impute %>%
  filter(X1010_year %in% c("Y05","Y06")) %>%
  select(all_of(var_main_nodummy)) %>%
  gbtoolbox::plot_pairwise_missing(textadjust = 1.5, divisor = 100) + 
  ggplot2::labs(title = "Primary School Only", y = 'Sample Size x 1/100', x = 'Sample Size x 1/100')
ggsave(file.path("plots","3_descriptive_plotpairwisemissing_primary.png"), width = 11, height = 11, dpi = 600)

df_impute %>%
  filter(X1010_year %in% c("Y07","Y08","Y09","Y10","Y11","Y12","Y13")) %>%
  select(all_of(var_main_nodummy)) %>%
  gbtoolbox::plot_pairwise_missing(textadjust = 1.5, divisor = 100) + 
  ggplot2::labs(title = "Secondary School Only", y = 'Sample Size x 1/100', x = 'Sample Size x 1/100')
ggsave(file.path("plots","3_descriptive_plotpairwisemissing_secondary.png"), width = 11, height = 11, dpi = 600)


## Plot Missing Correlations ---------------------------------------------------

df %>%
  select(all_of(var_main_nodummy)) %>%
  gbtoolbox::plot_missing_correlations(textadjust = 1, cluster_variables = FALSE, p_threshold_col = .05*2/(40*40-40)) +
  ggplot2::labs(title = "Whole Sample")
ggsave(file.path("plots","3_descriptive_plotmissingcorrelations2.png"), width = 10, height = 10, dpi = 600)

df %>%
  filter(X1010_year %in% c("Y05","Y06")) %>%
  select(all_of(var_main_brief)) %>% 
  gbtoolbox::plot_missing_correlations(textadjust = 1, cluster_variables = FALSE, p_threshold_col = .05*2/(40*40-40)) +
  ggplot2::labs(title = "Primary School Only")
ggsave(file.path("plots","3_descriptive_plotmissingcorrelations_primary.png"), width = 10, height = 10, dpi = 600)

df %>%
  filter(secondary_school==1) %>%
  select(all_of(var_main_brief)) %>% 
  gbtoolbox::plot_missing_correlations(textadjust = 1, cluster_variables = FALSE, p_threshold_col = .05*2/(40*40-40)) +
  ggplot2::labs(title = "Secondary School Only")
ggsave(file.path("plots","3_descriptive_plotmissingcorrelations_primary.png"), width = 10, height = 10, dpi = 600)

# Histogram of all data --------------------------------------------------------

skimr::skim(df_impute)

create_plots_ggplot2 <- function(df) {
  numeric_vars <- sapply(df, is.numeric)
  factor_vars <- sapply(df, is.factor)
  
  plots <- list()
  
  # Histograms for numeric variables
  for (var in names(df)[numeric_vars]) {
    p <- ggplot(df, aes_string(x = var)) + 
      geom_histogram(fill = "skyblue", color = "black") +
      ggtitle(paste("", var)) +
      xlab(var)
    plots[[var]] <- p
  }
  
  # Bar plots for factor variables
  for (var in names(df)[factor_vars]) {
    p <- ggplot(df, aes_string(x = var)) + 
      geom_bar(fill = "lightgreen", color = "black") +
      ggtitle(paste("", var)) +
      xlab(var)
    plots[[var]] <- p
  }
  
  return(plots)
}

# Create the plots
library(gridExtra)
plots = df_impute %>%
  select(-RID) %>%
  create_plots_ggplot2()

plot_grid = do.call(grid.arrange, c(plots, ncol = 10))

ggsave(file.path("plots","2_1_histogram.png"), plot_grid, width = 19, height = 14, dpi = 600)


# OLD STUFF --------------------------------------------------------------------

## Number Missing on key variables ---------------------------------------------

df %>%
  select(all_of(var_main_brief)) %>%
  apply(1, function(x) length(which(!is.na(x)))) %>%
  table()

df %>%
  select(ends_with("_missing")) %>%
  mutate_all(~na_if(.,TRUE)) %>%
  gbtoolbox::plot_pairwise_missing()

## Missing age data -----------------------------------------------------------

df %>%
  select(contains("age")) %>% 
  gbtoolbox::plot_pairwise_missing(textadjust = 1.5, divisor = 1) 

df %>%
  filter(!remove_pps) %>%
  filter(is.na(X1030_age)) %>%
  select(all_of(var_main)) %>% 
  gbtoolbox::plot_pairwise_missing(textadjust = 1.5, divisor = 1) 

# Remove participants with no data on food poverty OR well-being! 


table(is.na(df$X1020_gender_f),is.na(df$X1020_gender_o))


df %>%
  select(all_of(var_main)) %>%
  # select(1:50) %>%
  # select(1:50) %>%
  # sapply(., function(x) sd(x, na.rm = TRUE)) %>% sort()
  # cor(., use = "pairwise.complete.obs")
  
  gbtoolbox::plot_correlations(., confidence_interval = FALSE, cluster_variables = FALSE)
  
# df %>%
#   select(var_main) %>%
#   # select(1:50) %>%
#   # sapply(., function(x) sd(x, na.rm = TRUE)) %>% sort()
#   
#   mice::md.pattern()



  
  gbtoolbox::plot_correlations(., confidence_interval = FALSE, cluster_variables = FALSE)

df %>%
  select(var_main) %>%
  gbtools::plot_missing_correlations(textadjust = 1, cluster_variables = FALSE)

df %>%
  select(all_of(var_main)) %>% 
  filter(X1030_age14==1) %>% 
  gbtoolbox::plot_missing_correlations(textadjust = 1, cluster_variables = FALSE, )



# Calculate how many imputations are needed ------------------------------------

df_impute %>%
  select(-contains("X1010")) %>% 
  select(-contains("original_rownames")) %>%
  apply(., 1, function(x) length(which(is.na(x)))/length(x)) %>%
  hist(., main = "Percent Missing data for each participant")

## RCADS -----------------------------------------------------------------------

# df_impute %>%
#   select(-contains("X1010")) %>% 
#   select(-contains("original_rownames")) %>%
#   # select(-ends_with("RCADS")) %>%
#   select(-ends_with("SWEMWBS")) %>%
#   select(-ends_with("_pt")) %>%
#   select(-ends_with("SCWBS")) %>%
#   apply(., 1, function(x) length(which(is.na(x)))>0) %>%
#   table() %>%
#   prop.table(.) %>%
#   `names<-`(c("%participants without missing data","participants with missing data"))

df %>%
  filter(!remove_pps) %>%
  # filter(primary_school==1) %>%
  select(all_of(c(var_main))) %>%
  select(-contains("X1010")) %>% 
  select(-contains("original_rownames")) %>%
  # select(-ends_with("RCADS")) %>%
  select(-ends_with("SWEMWBS")) %>%
  select(-ends_with("_pt")) %>%
  select(-ends_with("SCWBS")) %>% 
  select(-ends_with("_lone")) %>% 
  apply(., 1, function(x) length(which(is.na(x)))>0) %>%
  table() %>%
  prop.table(.) %>%
  `names<-`(c("%participants without missing data","participants with missing data"))

## SWEMWBS ---------------------------------------------------------------------

df %>%
  filter(!remove_pps) %>%
  filter(secondary_school==1) %>%
  select(all_of(c(var_main))) %>%
  select(-contains("X1010")) %>% 
  select(-contains("original_rownames")) %>%
  select(-ends_with("RCADS")) %>%
  # select(-ends_with("SWEMWBS")) %>%
  select(-ends_with("_pt")) %>%
  select(-ends_with("SCWBS")) %>% 
  select(-ends_with("_lone")) %>% 
  apply(., 1, function(x) length(which(is.na(x)))>0) %>%
  table() %>%
  prop.table(.) %>%
  `names<-`(c("%participants without missing data","participants with missing data"))

## pt --------------------------------------------------------------------------

df %>%
  filter(!remove_pps) %>%
  filter(secondary_school==1) %>%
  select(all_of(c(var_main))) %>%
  select(-contains("X1010")) %>% 
  select(-contains("original_rownames")) %>%
  select(-ends_with("RCADS")) %>%
  select(-ends_with("SWEMWBS")) %>%
  # select(-ends_with("_pt")) %>%
  select(-ends_with("SCWBS")) %>% 
  select(-ends_with("_lone")) %>% 
  apply(., 1, function(x) length(which(is.na(x)))>0) %>%
  table() %>%
  prop.table(.) %>%
  `names<-`(c("%participants without missing data","participants with missing data"))

## SC ---------------------------------------------------------------------

df %>%
  filter(!remove_pps) %>%
  filter(primary_school==1) %>%
  select(all_of(c(var_main))) %>%
  select(-contains("X1010")) %>% 
  select(-contains("original_rownames")) %>%
  select(-ends_with("RCADS")) %>%
  select(-ends_with("SWEMWBS")) %>%
  select(-ends_with("_pt")) %>%
  # select(-ends_with("SCWBS")) %>% 
  select(-ends_with("_lone")) %>% 
  apply(., 1, function(x) length(which(is.na(x)))>0) %>%
  table() %>%
  prop.table(.) %>%
  `names<-`(c("%participants without missing data","participants with missing data"))


## Loneliness ---------------------------------------------------------------------

df %>%
  filter(!remove_pps) %>%
  filter(secondary_school==1) %>%
  select(all_of(c(var_main))) %>%
  select(-contains("X1010")) %>% 
  select(-contains("original_rownames")) %>%
  select(-ends_with("RCADS")) %>%
  select(-ends_with("SWEMWBS")) %>%
  select(-ends_with("_pt")) %>%
  select(-ends_with("SCWBS")) %>% 
  # select(-ends_with("_lone")) %>% 
  apply(., 1, function(x) length(which(is.na(x)))>0) %>%
  table() %>%
  prop.table(.) %>%
  `names<-`(c("%participants without missing data","participants with missing data"))











