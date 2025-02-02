# 必要なライブラリをロード
library(ggplot2)
library(dplyr)

# データを読み込む
data <- read.csv("combined_data_age_sex.csv")

# COVID_moderate と COVID_severe を COVID_moderate_severe に統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# 関連するクラスのみをフィルタリング（男性データ）
relevant_classes <- c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")
data_filtered_male <- data %>%
  filter(CLASS %in% relevant_classes, sex == "M") %>%
  select(CLASS, KAT2A)

# クラスを指定された順序で因子に変換
data_filtered_male <- data_filtered_male %>%
  mutate(CLASS = factor(CLASS, levels = c("HC", "COVID_mild",  "COVID_moderate_severe", "AD","AAV", "SSc", "SLE")))

# HCと他のグループ間でMann-Whitney U検定を実施（男性）
hc_data_male <- data_filtered_male %>%
  filter(CLASS == "HC") %>%
  pull(KAT2A)

comparison_results_male <- data_filtered_male %>%
  filter(CLASS != "HC") %>%
  group_by(CLASS) %>%
  summarise(p_value = wilcox.test(hc_data_male, KAT2A, exact = FALSE)$p.value)

# 検定結果のフォーマット（男性）
comparison_results_male <- comparison_results_male %>%
  mutate(label = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns"))))

# 男性データのプロット
ggplot(data_filtered_male, aes(x = CLASS, y = KAT2A, fill = CLASS)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-KAT2A (Male)", x = "Class", y = "[AU]") +
  geom_hline(yintercept = 30.58432629, linetype = 2, color = 'red') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 60)) +
  geom_text(data = comparison_results_male, 
            aes(x = CLASS, y = max(data_filtered_male$KAT2A, na.rm = TRUE) * 1.1, label = label), 
            inherit.aes = FALSE, color = "black")

# 関連するクラスのみをフィルタリング（女性データ）
data_filtered_female <- data %>%
  filter(CLASS %in% relevant_classes, sex == "F") %>%
  select(CLASS, KAT2A)

# クラスを指定された順序で因子に変換
data_filtered_female <- data_filtered_female %>%
  mutate(CLASS = factor(CLASS, levels = c("HC", "COVID_mild", "COVID_moderate_severe", "AD", "AAV", "SSc", "SLE")))

# HCと他のグループ間でMann-Whitney U検定を実施（女性）
hc_data_female <- data_filtered_female %>%
  filter(CLASS == "HC") %>%
  pull(KAT2A)

comparison_results_female <- data_filtered_female %>%
  filter(CLASS != "HC") %>%
  group_by(CLASS) %>%
  summarise(p_value = wilcox.test(hc_data_female, KAT2A, exact = FALSE)$p.value)

# 検定結果のフォーマット（女性）
comparison_results_female <- comparison_results_female %>%
  mutate(label = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns"))))

# 女性データのプロット
ggplot(data_filtered_female, aes(x = CLASS, y = KAT2A, fill = CLASS)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-KAT2A (Female)", x = "Class", y = "[AU]") +
  geom_hline(yintercept = 30.58432629, linetype = 2, color = 'red') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 60)) +
  geom_text(data = comparison_results_female, 
            aes(x = CLASS, y = max(data_filtered_female$KAT2A, na.rm = TRUE) * 0.9, label = label), 
            inherit.aes = FALSE, color = "black")

