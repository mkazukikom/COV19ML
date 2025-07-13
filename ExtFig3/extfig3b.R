library(ggplot2)
library(dplyr)

# 正規化＋前処理
read_and_process_sex_age_norm <- function(file_path, disease_name) {
  data <- read.csv(file_path)
  start_col <- which(colnames(data) == "A4GALT")
  numeric_data <- data[, start_col:ncol(data)]
  numeric_columns <- sapply(numeric_data, is.numeric)
  numeric_data <- numeric_data[, numeric_columns]
  
  # Min-Max 正規化
  normalized_data <- as.data.frame(lapply(numeric_data, function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(rep(0, length(x)))  # 定数列を0に
    (x - rng[1]) / diff(rng)
  }))
  
  data$Sum_norm <- rowSums(normalized_data, na.rm = TRUE)
  return(data.frame(Disease = disease_name, Sum_norm = data$Sum_norm, sex = data$sex, Age = data$age))
}

# Wilcoxon検定関数
perform_rank_sum_test <- function(data) {
  diseases <- unique(data$Disease)
  hc_sums <- filter(data, Disease == "HC")$Sum_norm
  results <- lapply(diseases, function(disease) {
    if (disease == "HC") return(NULL)
    disease_sums <- filter(data, Disease == disease)$Sum_norm
    test_result <- wilcox.test(hc_sums, disease_sums, exact = FALSE)
    data.frame(Group = disease, p_value = test_result$p.value)
  })
  results <- do.call(rbind, results)
  results$stars <- sapply(results$p_value, function(p) {
    if (p < 0.001) "***"
    else if (p < 0.01) "**"
    else if (p < 0.05) "*"
    else "ns"
  })
  return(results)
}

# ファイル読み込み
data_paths <- list(
  HC = "HC_data.csv",
  COVID19 = "COVID_data.csv",
  AD = "AD_data_2.csv",
  AAV = "AAV_data_2.csv",
  SLE = "SLE_data_2.csv",
  SSc = "SSc_data.csv"
)

all_data_sex_age <- do.call(rbind, lapply(names(data_paths), function(disease) {
  read_and_process_sex_age_norm(data_paths[[disease]], disease)
}))

# 年齢グループを追加
all_data_sex_age <- all_data_sex_age %>%
  mutate(AgeGroup = if_else(Age < 50, "<50", ">=50"),
         Group = factor(Disease, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE")))

# 年齢別データ
data_young <- filter(all_data_sex_age, AgeGroup == "<50")
data_old <- filter(all_data_sex_age, AgeGroup == ">=50")

# 検定
p_values_young <- perform_rank_sum_test(data_young)
p_values_old <- perform_rank_sum_test(data_old)

# <50歳箱ひげ図
plot_young <- ggplot(data_young, aes(x = Group, y = Sum_norm, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan",
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  labs(title = "Age < 50",
       x = "Group", y = "Normalized SAL") +
  theme_minimal() +
  geom_text(data = p_values_young, aes(x = Group, y = 7000, label = stars),
            inherit.aes = FALSE, size = 5, color = "black")

print(plot_young)

# ≥50歳箱ひげ図
plot_old <- ggplot(data_old, aes(x = Group, y = Sum_norm, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan",
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  labs(title = "(Age >= 50)",
       x = "Group", y = "Normalized SAL") +
  theme_minimal() +
  geom_text(data = p_values_old, aes(x = Group, y = 7000, label = stars),
            inherit.aes = FALSE, size = 5, color = "black")

print(plot_old)
