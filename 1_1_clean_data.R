# Important Notes --------------------------------------------------------------

# for the variable X1030_age (but not X1030) - 8yo have been converted to 9yo since there were only 11 cases of 8 year olds
# options(na.action="na.pass") used to make sure that the matrix of dummy variables created keeps rows with NA rather than omits them! 



# Load Packages ----------------------------------------------------------------

# source("0_decrypt_data.R") # Run this script first. The source() function doesn't work here as it needs the user to input the cyphr password

# This section allows the script to be re-run from scratch without re-running "0_decrypt_data.R" which takes a very long time
objects_to_remove <- setdiff(ls(all.names = TRUE), c("DATA", "DATAOLD","cols_add", "FULL"))
rm(list = objects_to_remove) 

# source("0_6_gender_delete_responses.R")
source("0_2_load_packages.R")

# Cleaning ---------------------------------------------------------------------

DATAOLD2 = DATAOLD[match(DATA$RID, DATAOLD$RID),]

if (FALSE){
  table(DATAOLD2$SCHOOL.ID==DATA$SCHOOL.ID)
  table(DATAOLD2$SWEMWS,DATA$SWEMWS)
}

df = DATA %>%
     as_tibble(., .name_repair = "unique")

attr(df$X1120,"meta")

## Demographic Variables -------------------------------------------------------
df$X1010_year        = factor(
  as.character(df$X1010), 
  levels = c("Y05", "Y06", "Y07", "Y08", 
             "Y09", "Y10", "Y11", "Y12", 
             "Y13")
  )

options(na.action="na.pass")
year_dummy         = df %>%
  model.matrix( ~ 0 + X1010_year , data = .) %>%
  as.data.frame()
options(na.action="na.omit")

df = cbind.data.frame(df, year_dummy)

df$primary_school    = as.numeric(df$X1010 %in% c("Y04","Y05","Y06"))

df$secondary_school  = as.numeric(df$X1010 %in% c("Y07","Y08","Y09","Y10","Y11","Y12","Y13"))

df$X1030_age           = df$X1030 %>%
  na_if("NoResponse") %>%
  gsub("A","",.) %>% 
  gsub("\\+","",.) %>% 
  as.numeric() %>%
  ifelse(.==8, 9, .) # Only 11 8-year-olds, which have been moved to the 9-year-old group. 

df$X1030_age = factor(df$X1030_age, levels = 9:19)
options(na.action="na.pass")
age_dummy = df %>%
  mutate(X1030_age = factor(X1030_age, levels = paste0(9:19))) %>% 
  model.matrix( ~ 0 + X1030_age , data = .) %>%
  as.data.frame()
options(na.action="na.omit")

df = cbind.data.frame(df, age_dummy)

df$X1040_ethnicity   = forcats::fct_recode(
  df$X1040,
  "White" = "White - English/Welsh/Scottish/Northern Irish/British",
  "White" = "White - Irish",
  "White" = "White - Gypsy or Irish Traveller",
  "White" = "White - Other",
  "Mixed" = "Mixed/Multiple Ethnic Groups - White and Black Caribbean",
  "Mixed" = "Mixed/Multiple Ethnic Groups - White and Black African",
  "Mixed" = "Mixed/Multiple Ethnic Groups - White and Asian",
  "Mixed" = "Mixed/Multiple Ethnic Groups - Other",
  "Asian" = "Asian/Asian British - Indian",
  "Asian" = "Asian/Asian British - Pakistani",
  "Asian" = "Asian/Asian British - Bangladeshi",
  "Asian" = "Asian/Asian British - Chinese",
  "Asian" = "Asian/Asian British - Other",
  "Black" = "Black/Black British/African/Caribbean - African",
  "Black" = "Black/Black British/African/Caribbean - Caribbean",
  "Black" = "Black/Black British/African/Caribbean - Other",
  "Other" = "Arab",
  "Other" = "Other ethnic group",
  NULL = "NoResponse(Stopped_vA)",
  NULL = "NoResponse"
) %>% factor()
options(na.action="na.pass")
ethnicity = model.matrix( ~ 0 + X1040_ethnicity , data = df)
options(na.action="na.omit")
df = cbind.data.frame(df, ethnicity)

df$X1050_born_uk      = df$X1050 %>%
  recode(
    "Yes" = 0,
    "No" = 1,
    .default = NA_real_
  )

df$X1060_p_born_uk    = df$X1060 %>%
  recode(
    "Yes, one parent" = 0,
    "Yes, both parents" = 0,
    "Neither parent" = 1,
    .default = NA_real_
  )

deprivation           = df %>%
  select(X1430, X1450, X1460, X1480, X1490) %>%
  mutate_all(~na_if(., "NoResponse(Stopped_vA)")) %>%
  mutate_all(~na_if(., "NoResponse")) %>%
  mutate_all(~gsub("\\s.*$","",.)) %>%
  mutate_all(~factor(., levels = c("Never", "Often", "Some"))) %>%
  rename_with(~paste0(., "_deprivation")) 
options(na.action="na.pass")
deprivation_dummy   = lapply(colnames(deprivation), function(i)  model.matrix( as.formula(paste0("~ 0 + ", i)) , data = deprivation))  %>%
  do.call("cbind.data.frame",.) %>%
  tibble(.,) %>%
  select(-contains("Never"))
options(na.action="na.omit")
df = cbind.data.frame(df, deprivation, deprivation_dummy)

# deprivation_questions =  sapply(deprivation,  function(x) attr(x, "meta")$text) 

df$school_ID_new      = match(df$SCHOOL.ID, unique(df$SCHOOL.ID))

## Gender ----------------------------------------------------------------------

table(DATA$X1020, 
      DATA$GENDER.broad,
      useNA = "always"
)


df$X1020_gender      = df$GENDER.broad %>%
  na_if("Prefer not to say") %>%
  na_if("Silly/malicious/not gender") %>%
  na_if("Skipped(GW_NoResponse)") %>%
  na_if("Unsure") %>% 
  case_match(
    "Boy" ~ "Male",
    "Girl" ~ "Female",
    "Gender diverse" ~ "Other",
    .default = NA
  ) %>% 
  factor(., levels = c("Female", "Male", "Other"))
df$X1020_gender_m   = as.numeric(df$X1020_gender=="Male")
df$X1020_gender_f   = as.numeric(df$X1020_gender=="Female")
df$X1020_gender_o   = as.numeric(df$X1020_gender=="Other")

# df$gender_gave_free_text_response = as.numeric(df$X1021F!="NULL")
# df$gender_silly_response          = tolower(df$X1021F) %in% delete_responses
# df$X1020_gender_o[df$gender_silly_response] = NA




# Data Checks

# df$X1021F %>% tolower() %>% unique %>% sort %>%  paste0(., collapse = '" , "') 

# table(df$X1020_gender)
# table(df$X1021F) %>% sort()
# sort(unique(df$X1021F[!df$gender_silly_response]))
# sort(unique(df$X1021F[df$gender_silly_response]))
# table(df$X1020_gender, df$gender_silly_response)

## FOOD POVERTY QUESTIONS ------------------------------------------------------

food_poverty = df %>% 
               select(X1440, X1470, X1500) %>%
               mutate_all(~na_if(., "NoResponse")) %>%
               mutate_all(~na_if(., "NoResponse(Stopped_vA)")) %>%
               mutate_all(~gsub("\\s.*$","",.)) %>%
               mutate_all(~factor(., levels = c("Never","Some", "Often"))) %>%
               rename_with(~paste0(., "_foodpov"))
  options(na.action="na.pass")
  food_poverty_dummy = lapply(colnames(food_poverty), function(i) model.matrix( as.formula(paste0("~ 0 + ", i)) , data = food_poverty))  %>%
    do.call("cbind.data.frame",.) %>%
    tibble(.,) %>%
    select(-contains("Never"))
  options(na.action="na.omit")
  
  food_questions = df %>% 
                    select(X1440, X1470, X1500) %>%
                    sapply(.,  function(x) attr(x, "meta")$text)
  
  df = cbind.data.frame(df, food_poverty,food_poverty_dummy)
  
## Outcomes --------------------------------------------------------------------

### SWEMWBS -----------------------------------------------------------------------
SWEMWBS = df %>%
          select(X1860, X1870, X1880, X1890, X1900, X1910, X1920) %>% 
          mutate_all(~na_if(., "NoResponse")) %>%
          mutate_all(~na_if(., "NoResponse(Stopped_vA)")) %>%
          mutate_all(~ recode(.,
                              "None of the time" = 1,
                              "Rarely" = 2,
                              "Some of the time" = 3,
                              "Often" = 4,
                              "All of the time" = 5,
                              .default = NA_real_)) %>%
          rename_with(~paste0(., "_SWEMWBS"))
  SWEMWBS_questions = df %>%
                      select(X1860, X1870, X1880, X1890, X1900, X1910, X1920) %>%
                      sapply(.,  function(x) attr(x, "meta")$text)
  df = cbind.data.frame(df, SWEMWBS)

### Stirling Children's well-being scale -----------------------------------------

SCWBS   = DATAOLD2 %>%
          # select(paste0("X",seq(2830,2970, by = 10)))
          select(all_of(c(paste0("X6_15_0",1:9),paste0("X6_15_",10:15) ))) %>%
          mutate_all(~ recode(.,
                              "NEVER" = 1,
                              "NOTMUCH" = 2,
                              "SOMETIMES" = 3,
                              "OFTEN" = 4,
                              "ALWAYS" = 5,
                              .default = NA_real_)) %>%
          `colnames<-`(paste0("X",seq(2830,2970, by = 10))) %>%
           rename_with(~paste0(., "_SCWBS"))
  df = cbind.data.frame(df, SCWBS)

  table(SCWBS$X2830_SCWBS, DATAOLD2$X6_15_01, useNA = "always")

  
### RCADS ----------------------------------------------------------------------
  
RCADS = df %>%
        select("X1930", "X1940", "X1950", "X1960", "X1970", "X1980", "X1990", "X2000", "X2010", "X2020", "X2030") %>%
        mutate_all(~ recode(.,
                            "Never" = 0,
                            "Sometimes" = 1,
                            "Often" = 2,
                            "Always" = 3,
                            .default = NA_real_)) %>%
        rename_with(~paste0(., "_RCADS"))
  df = cbind.data.frame(df, RCADS)
  RCADS_questions = sapply(RCADS,  function(x) attr(x, "meta")$text)
  table(RCADS$X1930_RCADS, df$X1930, useNA = "always" )

# x=apply(FULL,2,function(x) grepl("easy",tolower(x)))
# 
# xx = apply(x,2,function(x) length(which(x)))>0 %>% as.vector() 
# 
# x = df[df$X1010_year=="Y10",] %>%
#     select(matches("[X,Q]{1}[0-9]{1-2}_")) %>%
#     select(-X3_03_15)
# 
# x[1:50] %>% apply(.,2,table)
# x[51:120] %>% apply(.,2,table)
# x[120:200] %>% apply(.,2,table)
# x[200:217] %>% apply(.,2,table)
# 
# 
# 
# df$X3_03_15 %>% table()
# which(xx)

### Positive Thoughts Scale -----------------------------------------------------
## Less information about how to score this one! 
positive = df %>%
           # select(all_of(paste0("X12_01_0",1:8))) %>%
           select(paste0("X",seq(2460,2530, by = 10))) %>%
           mutate_all(~ recode(.,
                              "Do not believe it" = 0,
                              "Believe it slightly" = 1,
                              "Believe it moderately" = 2,
                              "Believe it very much" = 3,
                              "Believe it totally" = 4,
                              .default = NA_real_)) %>%
           # `colnames<-`(paste0("X",seq(2460,2530, by = 10))) %>%
           rename_with(~paste0(., "_pt"))
  df = cbind.data.frame(df, positive)
  # table(positive$X2460_pt, df$X12_01_01 , useNA = "always" )
  
### Loneliness -----------------------------------------------------------------
  
  lone = df %>%
    select(X1750, X1760, X1770, X1780) %>%
    mutate_all(~ recode(.,
                        "Never or hardly ever" = 0,
                        "Some of the time" = 1,
                        "Often" = 2,
                        .default = NA_real_)) %>%
    rename_with(~paste0(., "_lone"))
  df = cbind.data.frame(df, lone)
  # table(lone$X1780_lone, useNA = "always" )
  # lone_questions = sapply(lone,  function(x) attr(x, "meta")$text)
  # attr(df$X1750, "meta")
  
### Cognition ------------------------------------------------------------------
  
  cog = df %>%
    select(X2740, X2750, X2760) %>%
    # mutate_all(~na_if(.,"NULL")) %>%
    mutate_all(~as.numeric(.)) %>%
    # `colnames<-`(c("X2740","X2750","X2760")) %>%
    rename_with(~paste0(., "_cog"))
  df = cbind.data.frame(df, cog)
  # c(t(conc)) %>% hist()
  
## Computed Variables ----------------------------------------------------------
  
### missingness ----------------------------------------------------------------  

  df$foodpov_missing =  df %>%
    select(
      X1440_foodpovSome, X1440_foodpovOften, X1470_foodpovSome,
      X1470_foodpovOften, X1500_foodpovSome, X1500_foodpovOften
    ) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)
  
  df$swemwbs_missing =  df %>%
    select(
      X1860_SWEMWBS, X1870_SWEMWBS, X1880_SWEMWBS, X1890_SWEMWBS, X1900_SWEMWBS, X1910_SWEMWBS, X1920_SWEMWBS
    ) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)
  
  df$scwbs_missing   =  df %>%
    select(
      X2830_SCWBS, X2840_SCWBS, X2850_SCWBS, X2860_SCWBS, X2870_SCWBS, X2880_SCWBS, X2890_SCWBS, X2900_SCWBS, X2910_SCWBS, X2920_SCWBS,
      X2930_SCWBS, X2940_SCWBS, X2950_SCWBS, X2960_SCWBS, X2970_SCWBS
    ) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)

  df$rcads_missing   =  df %>%
    select(
      X1930_RCADS, X1940_RCADS, X1950_RCADS, X1960_RCADS, X1970_RCADS, X1980_RCADS, X1990_RCADS, X2000_RCADS, X2010_RCADS, X2020_RCADS, X2030_RCADS
    ) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)
  
  df$pt_missing      =  df %>%
    select(
      X2460_pt, X2470_pt, X2480_pt, X2490_pt, X2500_pt, X2510_pt, X2520_pt, X2530_pt
    ) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)
  
  df$lone_missing =  df %>%
    select(X1750_lone, X1760_lone, X1770_lone, X1780_lone) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)
  
  df$cog_missing =  df %>%
    select(X2740_cog,X2750_cog,X2760_cog) %>%
    apply(.,1, function(x) length(which(!is.na(x)))==0)
  
### sumscores ------------------------------------------------------------------
  # Here it's not advisable to use the sum_score function within a mutate() function as warnings and error messages are hidden
  
  df$swemwbs_ss = df %>%
    select(ends_with("_SWEMWBS")) %>% 
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = T)

  df$scwbs_ss   = df %>%
    select(ends_with("_SCWBS")) %>%
    select(-2, -7, -13) %>%
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = T)
  
  df$rcads_anx_ss= df %>%
    select(ends_with("_RCADS")) %>%
    select(1, 3, 4, 6, 9, 11) %>%
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = T)
  
  df$rcads_dep_ss=df %>%
    select(ends_with("_RCADS")) %>%
    select(2, 5, 7, 8, 10) %>%
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = T)
  
  df$pt_ss   =    df %>%
    select(ends_with("_pt")) %>% 
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = T)    
  
  df$lone_ss =    df %>%
    select(ends_with("_lone")) %>% 
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = T)    
  
  df$cog_ss  =    df %>%
    select(ends_with("_cog")) %>% 
    gbtoolbox::sum_score(input = ., print_missing_each_input_var = F)    
  
# Imputation Data --------------------------------------------------------------
  
  
## Define Participants to remove from analyses ----------------------------------
  var_main_nodummy[!(var_main_nodummy %in% colnames(df))] # Check all column_names are in the dataset

  missing_data = df %>%
    select(ends_with("_missing")) %>%
    apply(.,1, function(x) length(which(x==FALSE))==0)
  
  x = df[missing_data,]
  
  fast_completion = as.numeric(df$FAB.SURVEYDURATION)<10 & df$FAB.SURVEYCOMPLETED=="Yes"
  
  consent = (df$FAB.FLAGCONSENT == "PASS")
  
  remove_pps = !consent  
  remove_pps[fast_completion] = TRUE
  remove_pps[missing_data] = TRUE
  
  # ALL UP TO DATE (26/June/2024)
  nrow(df)                    # 43735 pps
  table(remove_pps)           # 1520 removed due to lack of consent
  table(remove_pps)           # 112 additionally excluded removed due to fast completion
  table(remove_pps)           # 3673 additionally excluded removed due to lack of data
  
  # Final Numbers:
  # 
  # remove_pps
  # FALSE  TRUE 
  # 38302  5433
  # 
  
## Subset main dataset with important row and cols -----------------------------
  
  df_impute = df %>%
    select(all_of(sort(unique(c("RID",var_main_nodummy))))) %>%
    mutate(original_rownames = 1:nrow(.)) %>%
    filter(!remove_pps)
  
  df_impute_split = split(df_impute, df_impute$X1010_year)
  
  names(df_impute_split)

## Save objects ----------------------------------------------------------------

# remove items that are not necessary 
  
df = df %>%
     select( -X1021F, -X2126F, -X2204F,-X2218F,-X2220rF,-X2230oF,-X2240gF)
  
save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

if(TRUE){
  # save.image(file=file.path("r_output_enc","workspace_preimputation.Rdata"))
  saveRDS(df, file=file.path(save_location,"sensitive_df.Rdata"))
  saveRDS(df_impute, file=file.path(save_location,"sensitive_df_impute.Rdata"))
  saveRDS(df_impute_split, file=file.path(save_location,"sensitive_df_impute_split.Rdata"))
}
  
  