library(ggplot2)
library(dplyr)

# データ読み込みと前処理関数（性別情報を含むように修正）
read_and_process_sex <- function(file_path, disease_name) {
  data <- read.csv(file_path)
  # A4GALT列の位置を見つける
  start_col <- which(colnames(data) == "A4GALT")
  # A4GALT列以降で、数値型の列のみを選択
  numeric_data <- data[, start_col:ncol(data)]
  numeric_columns <- sapply(numeric_data, is.numeric)
  # 数値型の列のみを含むデータで合計を計算
  data$Sum <- rowSums(numeric_data[, numeric_columns], na.rm = TRUE)
  # Diseaseとsex列を含むデータフレームを返す
  processed_data <- data.frame(Disease = disease_name, Sum = data$Sum, sex = data$sex)
  
  return(processed_data)
}

# データセットのパスと疾患名
data_paths <- list(
  HC = "HC_data.csv",
  COVID19 = "COVID_data.csv",
  AD = "AD_data_2.csv",
  AAV = "AAV_data_2.csv",
  SLE = "SLE_data_2.csv",
  SSc = "SSc_data.csv"
)

# 全データを読み込み、結合
all_data_sex <- do.call(rbind, lapply(names(data_paths), function(Disease) {
  read_and_process_sex(data_paths[[Disease]], Disease)
}))

# 疾患の順番を指定
all_data_sex$Group <- factor(all_data_sex$Disease, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))
all_data_sex$sex <- factor(all_data_sex$sex, levels = c("M", "F"))

# 性別ごとにデータを分割
all_data_male <- filter(all_data_sex, sex == "M")
all_data_female <- filter(all_data_sex, sex == "F")

# Wilcoxon rank sum testを実施する関数
perform_rank_sum_test <- function(data) {
  diseases <- unique(data$Disease)
  hc_sums <- filter(data, Disease == "HC")$Sum
  results <- lapply(diseases, function(disease) {
    if (disease == "HC") {
      return(NULL)  # HCの場合は結果を返さない
    } else {
      disease_sums <- filter(data, Disease == disease)$Sum
      test_result <- wilcox.test(hc_sums, disease_sums, exact = FALSE)
      return(list(disease = disease, p_value = test_result$p.value))
    }
  })
  results <- do.call(rbind, results)  # 結果を結合
  results <- as.data.frame(results)  # データフレームに変換
  rownames(results) <- NULL  # 行名をリセット
  results$p_value <- as.numeric(results$p_value)
  results$stars <- sapply(results$p_value, function(p) {
    if (p < 0.001) {
      "***"
    } else if (p < 0.01) {
      "**"
    } else if (p < 0.05) {
      "*"
    } else {
      "ns"
    }
  })
  return(results)
}

# 男性でのHCとの比較
p_values_male <- perform_rank_sum_test(all_data_male)

# 女性でのHCとの比較
p_values_female <- perform_rank_sum_test(all_data_female)

# 男性のデータに対して箱ひげ図を描画
ggplot(all_data_male, aes(x = Group, y = Sum, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  ylim(c(0, 20000)) +
  labs(title = "Male", x = "Group", y = "SAL [AU]") +
  annotate("text", x = 2:6, y = 19000, label = p_values_male$stars)

# 女性のデータに対して箱ひげ図を描画
ggplot(all_data_female, aes(x = Group, y = Sum, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  ylim(c(0, 20000)) +
  labs(title = "Female", x = "Group", y = "SAL [AU]") +
  annotate("text", x = 2:6, y = 19000, label = p_values_female$stars)

library(ggplot2)
library(dplyr)

# データ読み込みと前処理関数（年齢情報を含むように修正）
read_and_process_sex_age <- function(file_path, disease_name) {
  data <- read.csv(file_path)
  # A4GALT列以降の数値型の列の合計を計算
  numeric_data <- data[, which(colnames(data) == "A4GALT"):ncol(data)]
  numeric_columns <- sapply(numeric_data, is.numeric)
  data$Sum <- rowSums(numeric_data[, numeric_columns], na.rm = TRUE)
  # Disease, Sum, sex, および Age 列を含むデータフレームを返す
  processed_data <- data.frame(Disease = disease_name, Sum = data$Sum, sex = data$sex, Age = data$age)
  
  return(processed_data)
}

# データセットのパスと疾患名（ここは既存のリストを使用）
data_paths <- list(
  HC = "HC_data.csv",
  COVID19 = "COVID_data.csv",
  AD = "AD_data_2.csv",
  AAV = "AAV_data_2.csv",
  SLE = "SLE_data_2.csv",
  SSc = "SSc_data.csv"
)

# 全データを読み込み、結合
all_data_sex_age <- do.call(rbind, lapply(names(data_paths), function(disease) {
  read_and_process_sex_age(data_paths[[disease]], disease)
}))

# 年齢グループの列を追加
all_data_sex_age <- all_data_sex_age %>%
  mutate(AgeGroup = if_else(Age < 50, "<50", ">=50"))

# 疾患の順番を指定
all_data_sex_age$Group <- factor(all_data_sex_age$Disease, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# 50歳未満のデータでのHCとの比較
all_data_young <- filter(all_data_sex_age, AgeGroup == "<50")
p_values_young <- perform_rank_sum_test(all_data_young)

# 50歳以上のデータでのHCとの比較
all_data_old <- filter(all_data_sex_age, AgeGroup == ">=50")
p_values_old <- perform_rank_sum_test(all_data_old)

# Wilcoxon rank sum testを実施する関数
perform_rank_sum_test <- function(data) {
  diseases <- unique(data$Disease)
  hc_sums <- filter(data, Disease == "HC")$Sum
  results <- lapply(diseases, function(disease) {
    if (disease == "HC") {
      return(NULL)  # HCの場合は結果を返さない
    } else {
      disease_sums <- filter(data, Disease == disease)$Sum
      test_result <- wilcox.test(hc_sums, disease_sums, exact = FALSE)
      return(list(disease = disease, p_value = test_result$p.value))
    }
  })
  results <- do.call(rbind, results)  # 結果を結合
  results <- as.data.frame(results)  # データフレームに変換
  rownames(results) <- NULL  # 行名をリセット
  results$p_value <- as.numeric(results$p_value)
  results$stars <- sapply(results$p_value, function(p) {
    if (p < 0.001) {
      "***"
    } else if (p < 0.01) {
      "**"
    } else if (p < 0.05) {
      "*"
    } else {
      "ns"
    }
  })
  return(results)
}

# 50歳未満のデータに対して箱ひげ図を描画
ggplot(all_data_sex_age %>% filter(AgeGroup == "<50"), aes(x = Group, y = Sum, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  ylim(c(0, 20000)) +
  labs(title = "Age < 50", x = "Group", y = "SAL [AU]") +
  annotate("text", x = 2:6, y = 19000, label = p_values_young$stars)

# 50歳以上のデータに対して箱ひげ図を描画
ggplot(all_data_sex_age %>% filter(AgeGroup == ">=50"), aes(x = Group, y = Sum, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("HC" = "gray", "COVID19" = "magenta", "AD" = "cyan", 
                               "AAV" = "green", "SSc" = "orange", "SLE" = "royalblue")) +
  theme_minimal() +
  ylim(c(0, 20000)) +
  labs(title = "Age >= 50", x = "Group", y = "SAL [AU]") +
  annotate("text", x = 2:6, y = 19000, label = p_values_old$stars)

