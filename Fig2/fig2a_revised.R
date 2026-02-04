library(ggplot2)
library(reshape2)
library(dplyr)

hc_data <- read.csv("HC_data.csv")
aav_data <- read.csv("AAV_data_2.csv")
ad_data <- read.csv("AD_data_2.csv")
covid_data <- read.csv("COVID_data.csv")
sle_data <- read.csv("SLE_data_2.csv")
ssc_data <- read.csv("SSc_data.csv")

hc_subset <- hc_data[, which(colnames(hc_data) == "A4GALT"):ncol(hc_data)]
hc_mean <- colMeans(hc_subset, na.rm = TRUE)
hc_sd <- apply(hc_subset, 2, sd, na.rm = TRUE)

calculate_zscores <- function(data) {
  subset <- data[, which(colnames(data) == "A4GALT"):ncol(data)]
  zscores <- sweep(subset, 2, hc_mean, "-") / hc_sd
  return(zscores)
}

aav_zscores <- calculate_zscores(aav_data)
ad_zscores <- calculate_zscores(ad_data)
covid_zscores <- calculate_zscores(covid_data)
sle_zscores <- calculate_zscores(sle_data)
ssc_zscores <- calculate_zscores(ssc_data)

thresholds <- c(3, 4, 5)

all_results <- data.frame()
all_stats <- data.frame()

for (z_limit in thresholds) {
  
  count_func <- function(zscores, limit) {
    apply(zscores >= limit, 1, sum, na.rm = TRUE)
  }
  
  hc_c <- count_func(hc_subset, z_limit)
  aav_c <- count_func(aav_zscores, z_limit)
  ad_c <- count_func(ad_zscores, z_limit)
  covid_c <- count_func(covid_zscores, z_limit)
  sle_c <- count_func(sle_zscores, z_limit)
  ssc_c <- count_func(ssc_zscores, z_limit)
  
  temp_df <- data.frame(
    Class = factor(c(rep("HC", length(hc_c)), rep("COVID19", length(covid_c)), 
                     rep("AD", length(ad_c)), rep("AAV", length(aav_c)), 
                     rep("SSc", length(ssc_c)), rep("SLE", length(sle_c))),
                   levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE")),
    Count = c(hc_c, covid_c, ad_c, aav_c, ssc_c, sle_c),
    Threshold = paste0("Z >= ", z_limit)
  )
  all_results <- rbind(all_results, temp_df)
  
  # 統計テストの実行
  groups <- list("AAV"=aav_c, "AD"=ad_c, "COVID19"=covid_c, "SLE"=sle_c, "SSc"=ssc_c)
  for (g_name in names(groups)) {
    p_val <- wilcox.test(hc_c, groups[[g_name]])$p.value
    sig <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "ns")))
    
    all_stats <- rbind(all_stats, data.frame(
      Group = g_name,
      P_Value = p_val,
      Significance = sig,
      Threshold = paste0("Z >= ", z_limit),
      y_pos = max(temp_df$Count) * 1.05
    ))
  }
}

global_max <- max(all_results$Count, na.rm = TRUE)
y_limit <- global_max * 1.15
all_stats$y_pos <- global_max * 1.05

library(ggplot2)

ggplot(all_results, aes(x = Class, y = Count, fill = Class)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ Threshold) + 
  scale_y_continuous(limits = c(0, y_limit)) + 
  theme_minimal() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  labs(title = "Number of autoantibody positivity",
       x = "Disease Class",
       y = "Count of Positive Autoantibodies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(data = all_stats, aes(x = Group, y = y_pos, label = Significance), 
            inherit.aes = FALSE, size = 3, vjust = 0)
