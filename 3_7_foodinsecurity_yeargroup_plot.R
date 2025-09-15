# Load Data --------------------------------------------------------------------
rm(list = ls(all.names = TRUE))

source("0_2_load_packages.R")
save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

df              = readRDS(file=file.path(save_location,"sensitive_df.Rdata"))
df_impute       = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))

# Clean data for year groups ---------------------------------------------------

df_impute %>%
  select(X1010_year,X1440_foodpov, X1470_foodpov, X1500_foodpov) %>%
  slice(1:200)

df_long <- df_impute %>%
    select(X1010_year, X1440_foodpov, X1470_foodpov, X1500_foodpov) %>%
    rename(
      "At home, I go to bed hungry because there is not enough food in the house" = X1500_foodpov,
      "At school, I am unable to afford to eat" = X1470_foodpov,
      "My family uses food banks" = X1440_foodpov
    ) %>%
    pivot_longer(cols = -X1010_year, 
                 names_to = "question", 
                 values_to = "response") %>%
    filter(!is.na(response))
  
  # Calculate percentages
  df_percentages <- df_long %>%
    group_by(X1010_year, question, response) %>%
    summarise(count = n()) %>%
    group_by(X1010_year, question) %>%
    mutate(
      percentage = count / sum(count) * 100,
      total_n = sum(count),
      response = factor(response, levels = c("Never", "Some", "Often"), labels = c("Never", "Sometimes","Often")),
      question = forcats::fct_rev(factor(question))
      ) %>%
    arrange(X1010_year, question, response)
  
  # Create the bar chart
  df_percentages %>%
    ungroup() %>%
    as.data.frame() %>%
    ggplot(aes(x = X1010_year, y = percentage, fill = response)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = sprintf("%.1f%%\n(n = %d)", percentage, count)),  # Cap y position at 17
              position = position_stack(vjust = .5),
              size = 3,
              angle = 0,
              hjust = .5) +
    geom_text( data = filter(df_percentages, response == "Never"),
              aes(label = sprintf("Total N=\n %d",total_n),
                  y = 14.5),
              size = 3) + 
    labs(
         title = "Food Insecurity Responses by Year Group",
         x = "School Year Group",
         y = "Percentage",
         fill = "Response") +
    theme_minimal() +
    coord_cartesian(ylim=c(0,15)) +
    # scale_y_break(c(20, 80), scales = 0.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0,20,by=2.5), labels = scales::percent_format(scale = 1)) +
    # facet_grid(rows = vars(question))
    facet_wrap(~ forcats::fct_rev(question), ncol = 1)

  ggsave(file.path("plots","3_7_yeargroup_foodinsecurity.png"), width = 7, height = 10)  
  ggsave(file.path("plots","3_7_yeargroup_foodinsecurity.pdf"), width = 7, height = 10)  
  