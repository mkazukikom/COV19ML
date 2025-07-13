# 必要なライブラリの読み込み
library(ggplot2)
library(dplyr)

# データ読み込みと前処理関数
read_and_process_sex <- function(file_path, disease_name) {
  data <- read.csv(file_path)
  start_col <- which(colnames(data) == "A4GALT")
  numeric_data <- data[, start_col:ncol(data)]
  numeric_columns <- sapply(numeric_data, is.numeric)
  numeric_data <- numeric_data[, numeric_columns]
  
  normalized_data <- as.data.frame(lapply(numeric_data, function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }))
  
  data$Sum_norm <- rowSums(normalized_data, na.rm = TRUE)
  processed_data <- data.frame(Disease = disease_name, Sum_norm = data$Sum_norm, sex = data$sex)
  return(processed_data)
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

# データ結合
all_data_sex <- do.call(rbind, lapply(names(data_paths), function(Disease) {
  read_and_process_sex(data_paths[[Disease]], Disease)
}))

# 因子型の設定
all_data_sex$Group <- factor(all_data_sex$Disease, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))
all_data_sex$sex <- factor(all_data_sex$sex, levels = c("M", "F"))

# Wilcoxon rank sum test 関数
perform_rank_sum_test <- function(data) {
  diseases <- unique(data$Disease)
  hc_sums <- filter(data, Disease == "HC")$Sum_norm
  
  results <- lapply(diseases, function(disease) {
    if (disease == "HC") return(NULL)
    disease_sums <- filter(data, Disease == disease)$Sum_norm
    test_result <- wilcox.test(hc_sums, disease_sums, exact = FALSE)
    return(data.frame(Group = disease, P_Value = test_result$p.value))
  })
  results <- do.call(rbind, results)
  return(results)
}

# 全体・性別別検定
p_values_all <- perform_rank_sum_test(all_data_sex)
p_values_male <- perform_rank_sum_test(filter(all_data_sex, sex == "M"))
p_values_female <- perform_rank_sum_test(filter(all_data_sex, sex == "F"))

# 検定結果を整理
stat_results <- p_values_all
stat_results$Significance <- sapply(stat_results$P_Value, function(p) {
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "ns"
})
stat_results$Group <- factor(stat_results$Group, levels = c("COVID19", "AD", "AAV", "SSc", "SLE"))

# 箱ひげ図の描画
boxplot_plot <- ggplot(all_data_sex, aes(x = Group, y = Sum_norm, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  labs(title = "Sum of Normalized Autoantibody Levels", x = "Group", y = "Normalized SAL") +
  geom_text(data = stat_results, aes(x = Group, y = 7000, label = Significance), 
            inherit.aes = FALSE, color = "black", size = 5)

print(boxplot_plot)

# -----------------------------------------
# 男性・女性のp値テーブル作成とSignificance列付加
# -----------------------------------------
# 男性
p_values_male <- perform_rank_sum_test(filter(all_data_sex, sex == "M"))
stat_results_male <- p_values_male
stat_results_male$Significance <- sapply(stat_results_male$P_Value, function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "ns"
})
stat_results_male$Group <- factor(stat_results_male$Group, levels = c("COVID19", "AD", "AAV", "SSc", "SLE"))
stat_results_male$y_pos <- tapply(filter(all_data_sex, sex == "M")$Sum_norm,
                                  filter(all_data_sex, sex == "M")$Group,
                                  max)[as.character(stat_results_male$Group)] + 0.05

# 女性
p_values_female <- perform_rank_sum_test(filter(all_data_sex, sex == "F"))
stat_results_female <- p_values_female
stat_results_female$Significance <- sapply(stat_results_female$P_Value, function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "ns"
})
stat_results_female$Group <- factor(stat_results_female$Group, levels = c("COVID19", "AD", "AAV", "SSc", "SLE"))
stat_results_female$y_pos <- tapply(filter(all_data_sex, sex == "F")$Sum_norm,
                                    filter(all_data_sex, sex == "F")$Group,
                                    max)[as.character(stat_results_female$Group)] + 0.05

# -----------------------------------------
# 男性の箱ひげ図
# -----------------------------------------
plot_male <- ggplot(filter(all_data_sex, sex == "M"), aes(x = Group, y = Sum_norm, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan",
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  labs(title = "Male", x = "Group", y = "Normalized Sum") +
  geom_text(data = stat_results_male, aes(x = Group, y = 7000, label = Significance),
            inherit.aes = FALSE, color = "black")

print(plot_male)

# -----------------------------------------
# 女性の箱ひげ図
# -----------------------------------------
plot_female <- ggplot(filter(all_data_sex, sex == "F"), aes(x = Group, y = Sum_norm, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan",
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  labs(title = "Female", x = "Group", y = "Normalized Sum") +
  geom_text(data = stat_results_female, aes(x = Group, y = 7000, label = Significance),
            inherit.aes = FALSE, color = "black")

print(plot_female)

# Wilcoxon rank sum test を実行する関数（先に定義）
perform_rank_sum_test <- function(data) {
  diseases <- unique(data$Disease)
  hc_sums <- filter(data, Disease == "HC")$Sum
  results <- lapply(diseases, function(disease) {
    if (disease == "HC") return(NULL)
    disease_sums <- filter(data, Disease == disease)$Sum
    test_result <- wilcox.test(hc_sums, disease_sums, exact = FALSE)
    return(data.frame(Group = disease, p_value = test_result$p.value))
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

# データ読み込みと前処理
read_and_process_sex_age <- function(file_path, disease_name) {
  data <- read.csv(file_path)
  numeric_data <- data[, which(colnames(data) == "A4GALT"):ncol(data)]
  numeric_columns <- sapply(numeric_data, is.numeric)
  data$Sum <- rowSums(numeric_data[, numeric_columns], na.rm = TRUE)
  processed_data <- data.frame(Disease = disease_name, Sum = data$Sum, sex = data$sex, Age = data$age)
  return(processed_data)
}

data_paths <- list(
  HC = "HC_data.csv",
  COVID19 = "COVID_data.csv",
  AD = "AD_data_2.csv",
  AAV = "AAV_data_2.csv",
  SLE = "SLE_data_2.csv",
  SSc = "SSc_data.csv"
)

all_data_sex_age <- do.call(rbind, lapply(names(data_paths), function(disease) {
  read_and_process_sex_age(data_paths[[disease]], disease)
}))

# 年齢グループの列を追加
all_data_sex_age <- all_data_sex_age %>%
  mutate(AgeGroup = if_else(Age < 50, "<50", ">=50"),
         Group = factor(Disease, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE")))

# 50歳未満の検定と最大値の取得
data_young <- filter(all_data_sex_age, AgeGroup == "<50")
p_values_young <- perform_rank_sum_test(data_young)
group_max_young <- aggregate(Sum ~ Group, data = data_young, max)
p_values_young$y_pos <- group_max_young$Sum[match(p_values_young$Group, group_max_young$Group)] + 1000

# 50歳以上の検定と最大値の取得
data_old <- filter(all_data_sex_age, AgeGroup == ">=50")
p_values_old <- perform_rank_sum_test(data_old)
group_max_old <- aggregate(Sum ~ Group, data = data_old, max)
p_values_old$y_pos <- group_max_old$Sum[match(p_values_old$Group, group_max_old$Group)] + 1000

# 描画（<50）
ggplot(data_young, aes(x = Group, y = Sum, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  labs(title = "Age < 50", x = "Group", y = "Normalized SAL") +
  geom_text(data = p_values_young, aes(x = Group, y = 7000, label = stars),
            inherit.aes = FALSE, size = 5)

# 描画（>=50）
ggplot(data_old, aes(x = Group, y = Sum, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  labs(title = "Age >= 50", x = "Group", y = "Normalized SAL") +
  geom_text(data = p_values_old, aes(x = Group, y = 7000, label = stars),
            inherit.aes = FALSE, size = 5)

