# Insatll niche packages from github

# remotes::install_github("giac01/gbtoolbox", ref = "48a04860c62c21866850cf512f77c9728b7b00d5")
options(max.print = 10000)

# Load Packages 
library(tidyverse)
library(forcats)
library(fastDummies)

# devtools::install_github("giac01/gbtoolbox@e3e4ad79e3b7686d05083b9871755df0b7f2c1b6")
# testthat::test_package("gbtoolbox")
library(gbtoolbox)


library(ggmice)
library(mice)
library(brms)
library(future)

# library(miceadds)
library(beepr)
library(marginaleffects)

library(tidybayes)

library(knitr)

library(bayesplot)

library(patchwork)

library(psych)

library(bayestestR)

library(rstan)

library(survey)

library(emmeans)

library(gt)

library(parallel)

# Other Useful Stuff

outcomes = c("rcads_dep_ss", "rcads_anx_ss", "swemwbs_ss",  "pt_ss" , "lone_ss" , "scwbs_ss" , "cog_ss")

outcomes_labels = c("RCADS-11 Depression", "RCADS-11 Anxiety", "Adolescent Wellbeing SWEMWBS", "Positive Thoughts", "Loneliness", "Child Wellbeing SCWBS", "Cognition")

food_pov_variables = c("X1440_foodpovSome", "X1470_foodpovSome", "X1500_foodpovSome",
                       "X1440_foodpovOften","X1470_foodpovOften","X1500_foodpovOften")

food_pov_questions = c("My family uses food banks","At school, I am unable to afford to eat","At home, I go to bed hungry because there is not enough food in the house")

# removed age/yeargroup, gender and ethnicity as these are now included as random intercepts! 
predictors_random_model = c(
  "X1440_foodpovSome", "X1440_foodpovOften", 
  "X1470_foodpovSome", "X1470_foodpovOften", 
  "X1500_foodpovSome", "X1500_foodpovOften",
  # "X1020_gender_m", "X1020_gender_o",
  # "X1040_ethnicitymixed", "X1040_ethnicityasian", "X1040_ethnicityblack", "X1040_ethnicityother",
  "X1050_born_uk", "X1060_p_born_uk",
  "X1430_deprivationOften", "X1430_deprivationSome", 
  "X1450_deprivationOften", "X1450_deprivationSome", 
  "X1460_deprivationOften", "X1460_deprivationSome", 
  "X1480_deprivationOften", "X1480_deprivationSome", 
  "X1490_deprivationOften", "X1490_deprivationSome"
)

predictors = c(
  "X1440_foodpovSome", "X1440_foodpovOften", 
  "X1470_foodpovSome", "X1470_foodpovOften", 
  "X1500_foodpovSome", "X1500_foodpovOften",
  "X1020_gender_m", "X1020_gender_o",
  "X1040_ethnicityMixed", "X1040_ethnicityAsian", "X1040_ethnicityBlack", "X1040_ethnicityOther",
  "X1050_born_uk", "X1060_p_born_uk",
  "X1430_deprivationOften", "X1430_deprivationSome", 
  "X1450_deprivationOften", "X1450_deprivationSome", 
  "X1460_deprivationOften", "X1460_deprivationSome", 
  "X1480_deprivationOften", "X1480_deprivationSome", 
  "X1490_deprivationOften", "X1490_deprivationSome"
)

# Add age variables to predictors

predictors_allyears = c(
  predictors,
  # "X1030_age10", "X1030_age11", "X1030_age12", "X1030_age13", "X1030_age14", "X1030_age15", "X1030_age16", "X1030_age17", "X1030_age18", "X1030_age19"
  "X1010_Y06", "X1010_Y07", "X1010_Y08", "X1010_Y09","X1010_Y10","X1010_Y11","X1010_Y12","X1010_Y13"
  )


predictors_secondary = c(
  predictors,
  # "X1010_Y08", "X1010_Y09","X1010_Y10","X1010_Y11","X1010_Y12","X1010_Y13",
  "X1010_Y08", "X1010_Y09","X1010_Y10","X1010_Y11","X1010_Y12","X1010_Y13"
)

predictors_primary = c(
  predictors,
  "X1010_Y06"
)


predictors_labels =  c("Food Bank - Sometimes", "Food Bank - Often", 
                       "Unable to afford to eat - Sometimes", "Unable to afford to eat - Often",
                       "Go to bed hungry - Sometimes", "Go to bed hungry - Often",
                       "Gender - Male", "Gender - Other", 
                       "Ethnicity - Mixed", "Ethnicity - Asian", "Ethnicity - Black", "Ethnicity - Other", 
                       "Born in UK", "Parent born in UK",
                       "X1430 - Often", "X1430 - Sometimes", "X1450 - Often", "X1450 - Sometimes",  "X1460 - Often", "X1460 - Some",
                       "X1480 - Often", "X1480 - Sometimes",  "X1490 - Often", "X1490 - Sometimes")

predictors_allyears_labels  = c(predictors_labels, paste0("School Y",6:13))
predictors_secondary_labels = c(predictors_labels, paste0("School Y",8:13))
predictors_primary_labels   = c(predictors_labels, paste0("School Y",6))


# VARIABLES FOR IMPUTATION:

var_main_nodummy <- c(
  "X1020_gender", 
  
  "X1010_year",
  
  "X1030_age",

  "X1040_ethnicity",
  
  "X1050_born_uk", "X1060_p_born_uk", 
  
  "X1430_deprivation", 
  "X1450_deprivation",
  "X1460_deprivation", 
  "X1480_deprivation", 
  "X1490_deprivation",
  
  "school_ID_new", 
  
  "X1440_foodpov", 
  "X1470_foodpov", 
  "X1500_foodpov",

  "X1930_RCADS", "X1940_RCADS", "X1950_RCADS", "X1960_RCADS", "X1970_RCADS", "X1980_RCADS", "X1990_RCADS", "X2000_RCADS", "X2010_RCADS", "X2020_RCADS", "X2030_RCADS",
  
  "X1860_SWEMWBS", "X1870_SWEMWBS", "X1880_SWEMWBS", "X1890_SWEMWBS", "X1900_SWEMWBS", "X1910_SWEMWBS", "X1920_SWEMWBS", 
  
  "X2460_pt", "X2470_pt", "X2480_pt", "X2490_pt", "X2500_pt", "X2510_pt", "X2520_pt", "X2530_pt",
  
  "X1750_lone", "X1760_lone", "X1770_lone", "X1780_lone",
  
  "X2830_SCWBS", "X2840_SCWBS", "X2850_SCWBS", "X2860_SCWBS", "X2870_SCWBS", "X2880_SCWBS", "X2890_SCWBS", "X2900_SCWBS", "X2910_SCWBS", "X2920_SCWBS",
  "X2930_SCWBS", "X2940_SCWBS", "X2950_SCWBS", "X2960_SCWBS", "X2970_SCWBS", 
  
  "X2740_cog", "X2750_cog", "X2760_cog"
)

primary_specific_variables   = c("X2830_SCWBS", "X2840_SCWBS", "X2850_SCWBS", "X2860_SCWBS", "X2870_SCWBS", "X2880_SCWBS", "X2890_SCWBS", "X2900_SCWBS", "X2910_SCWBS", "X2920_SCWBS",
                                 "X2930_SCWBS", "X2940_SCWBS", "X2950_SCWBS", "X2960_SCWBS", "X2970_SCWBS")

secondary_specific_variables = c("X1860_SWEMWBS", "X1870_SWEMWBS", "X1880_SWEMWBS", "X1890_SWEMWBS", "X1900_SWEMWBS", "X1910_SWEMWBS", "X1920_SWEMWBS", 
                                 "X2460_pt", "X2470_pt", "X2480_pt", "X2490_pt", "X2500_pt", "X2510_pt", "X2520_pt", "X2530_pt",
                                 "X1750_lone", "X1760_lone", "X1770_lone", "X1780_lone")

save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

