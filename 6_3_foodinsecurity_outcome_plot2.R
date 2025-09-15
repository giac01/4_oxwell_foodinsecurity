# Load Data --------------------------------------------------------------------
rm(list = ls(all.names = TRUE))

source("0_2_load_packages.R")
save_location = "X:\\OxWell\\CoreTeamAnalyses\\Analyses_PleaseFirstReadSOP&ReferToDPIA\\Oxwell_2023_giacomo\\food_insecurity_2023\\r_output_enc\\"

save_location = "r_output_enc"

df              = readRDS(file=file.path(save_location,"sensitive_df.Rdata"))
df_impute       = readRDS(file=file.path(save_location,"sensitive_df_impute.Rdata"))
# dat_imputed_long  =  readRDS(file=file.path(save_location,"sensitive_dat_imputed_long.Rdata"))

# Associaton between each FI question and each outcome -------------------------------------------
library(ggdist)

df %>%
  filter(df$RID %in% df_impute$RID) %>% 
  select(ends_with("foodPov"), outcomes[1:6]) %>% 
  pivot_longer(cols=contains("foodPov"), names_to = "fi_question", values_to = "fi_response") %>% 
  pivot_longer(cols=ends_with("_ss"), names_to = "outcome_question", values_to = "outcome_value") %>%
  filter(!is.na(outcome_value)) %>%
  filter(!is.na(fi_response)) %>% 
  # filter(!is.na(scwbs_ss)) %>%
  mutate(fi_question = factor(fi_question, 
                              levels = c("X1440_foodpov", "X1470_foodpov", "X1500_foodpov"), 
                              labels = stringr::str_wrap(food_pov_questions,20))) %>%
  mutate(
    outcome_question = factor(outcome_question, levels = outcomes, labels = outcomes_labels),
    outcome_value    = as.numeric(outcome_value),
    fi_response      = recode(fi_response,
                              "Some" = "Sometimes"
    )
  ) %>%
  ggplot(aes(x=fi_response, y = outcome_value, fill = fi_response)) + 
  stat_halfeye(
    position = position_nudge(x=.14),
    adjust = 6, 
    normalize = "groups", 
    width =.4,
    point_interval = "mean_qi",
    .width = 0
  ) +
  geom_jitter(
    shape = 16,
    size = .45,
    alpha = 0.3,
    width = .04
  ) +
  stat_summary(
    fun.data = function(x) {
      data.frame(
        y = min(x) + (max(x)-min(x))/2,
        # y = 10,
        # Use paste with collapse to create very tight spacing
        label = paste(
          sprintf("M:%.2f", mean(x)),
          sprintf("SD:%.3f", sd(x)),
          sprintf("SE:%.3f", sd(x)/sqrt(length(x))),
          sprintf("N:%d", length(x)),
          sep = "\n"
        )
      )
    },
    position = position_nudge(x=-.31),
    geom = "text",
    size = 2.2,
    lineheight = 0.8  # Reduce line spacing (default is 1.2)
  ) +
  stat_summary(
    fun.data = function(x) {
      qt_val <- qt(0.995, df = length(x) - 1)  
      mean_x <- mean(x)
      se_x <- sd(x) / sqrt(length(x))
      ci <- qt_val * se_x
      data.frame(
        y = mean_x,
        ymin = mean_x - ci,
        ymax = mean_x + ci
      )
    },
    geom = "errorbar",
    width = 0.19,  # Width of error bar ends
    linewidth = .75,
    color = "black",
    position = position_nudge(x=.14)
  ) +
  labs(
    x = "Food Insecurity Level",
    y = "Stirling Children's Wellbeing Scale Sum-Score"
  ) +
  facet_grid(
    cols = vars(fi_question), 
    rows = vars(outcome_question),
    scales = "free_y"
  ) + 
  theme_bw() +
  theme(legend.position = "none")

ggsave(file.path("plots","2_2_fi_outcome_violinplots2.png"), height = 13, width = 10, dpi = 300)
