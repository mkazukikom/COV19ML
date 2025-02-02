# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Load the data
hc_data <- read.csv("HC_data.csv")
aav_data <- read.csv("AAV_data_2.csv")
ad_data <- read.csv("AD_data_2.csv")
covid_data <- read.csv("COVID_data.csv")
sle_data <- read.csv("SLE_data_2.csv")
ssc_data <- read.csv("SSc_data.csv")

# Calculate mean and standard deviation for HC data (from 'A4GALT' onward)
hc_subset <- hc_data[, which(colnames(hc_data) == "A4GALT"):ncol(hc_data)]
hc_mean <- colMeans(hc_subset, na.rm = TRUE)
hc_sd <- apply(hc_subset, 2, sd, na.rm = TRUE)

# Function to calculate Z-scores based on HC statistics
calculate_zscores <- function(data) {
  subset <- data[, which(colnames(data) == "A4GALT"):ncol(data)]
  zscores <- sweep(subset, 2, hc_mean, "-") / hc_sd
  return(zscores)
}

# Calculate Z-scores for each dataset
aav_zscores <- calculate_zscores(aav_data)
ad_zscores <- calculate_zscores(ad_data)
covid_zscores <- calculate_zscores(covid_data)
sle_zscores <- calculate_zscores(sle_data)
ssc_zscores <- calculate_zscores(ssc_data)

# Count the number of Z-scores >= 4 for each sample
count_above_threshold <- function(zscores) {
  apply(zscores >= 4, 1, sum, na.rm = TRUE)
}

hc_counts <- count_above_threshold(hc_subset)
aav_counts <- count_above_threshold(aav_zscores)
ad_counts <- count_above_threshold(ad_zscores)
covid_counts <- count_above_threshold(covid_zscores)
sle_counts <- count_above_threshold(sle_zscores)
ssc_counts <- count_above_threshold(ssc_zscores)

# Combine counts with class labels for boxplot
boxplot_data <- data.frame(
  Class = c(rep("HC", length(hc_counts)),
            rep("AAV", length(aav_counts)),
            rep("AD", length(ad_counts)),
            rep("COVID19", length(covid_counts)),
            rep("SLE", length(sle_counts)),
            rep("SSc", length(ssc_counts))),
  Zscore_Above_4_Count = c(hc_counts, aav_counts, ad_counts, covid_counts, sle_counts, ssc_counts)
)

# Specify the order of diseases
boxplot_data$Class <- factor(boxplot_data$Class, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# Perform statistical tests between HC and each disease group
perform_test <- function(hc_counts, group_counts) {
  wilcox.test(hc_counts, group_counts, alternative = "two.sided")
}

aav_test <- perform_test(hc_counts, aav_counts)
ad_test <- perform_test(hc_counts, ad_counts)
covid_test <- perform_test(hc_counts, covid_counts)
sle_test <- perform_test(hc_counts, sle_counts)
ssc_test <- perform_test(hc_counts, ssc_counts)

# Collect test results and determine significance levels
stat_results <- data.frame(
  Group = c("AAV", "AD", "COVID19", "SLE", "SSc"),
  P_Value = c(aav_test$p.value, ad_test$p.value, covid_test$p.value, sle_test$p.value, ssc_test$p.value),
  Significance = c(
    ifelse(aav_test$p.value < 0.001, "***", ifelse(aav_test$p.value < 0.01, "**", ifelse(aav_test$p.value < 0.05, "*", "ns"))),
    ifelse(ad_test$p.value < 0.001, "***", ifelse(ad_test$p.value < 0.01, "**", ifelse(ad_test$p.value < 0.05, "*", "ns"))),
    ifelse(covid_test$p.value < 0.001, "***", ifelse(covid_test$p.value < 0.01, "**", ifelse(covid_test$p.value < 0.05, "*", "ns"))),
    ifelse(sle_test$p.value < 0.001, "***", ifelse(sle_test$p.value < 0.01, "**", ifelse(sle_test$p.value < 0.05, "*", "ns"))),
    ifelse(ssc_test$p.value < 0.001, "***", ifelse(ssc_test$p.value < 0.01, "**", ifelse(ssc_test$p.value < 0.05, "*", "ns")))
  )
)

# Add statistical test results to boxplot
boxplot_plot <- ggplot(boxplot_data, aes(x = Class, y = Zscore_Above_4_Count, fill = Class)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  labs(title = "Number of autoantibody positivity",
       x = "Disease Class",
       y = "Count of Z-scores >= 4") +
  geom_text(data = stat_results, aes(x = Group, y = max(boxplot_data$Zscore_Above_4_Count) + 1, 
                                     label = Significance), 
            inherit.aes = FALSE, color = "black", size = 5, vjust = -0.5)

# Save the plot
print(boxplot_plot)
