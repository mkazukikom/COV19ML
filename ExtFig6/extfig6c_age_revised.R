# 必要なライブラリをロード
library(ggplot2)
library(dplyr)

# データを読み込む
data <- read.csv("combined_data_age_sex.csv")  # ファイル名を適宜修正してください

# COVID関連クラスの統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

#年齢グループを定義
data <- data %>%
  mutate(age_group = ifelse(age < 50, "<50", ">=50"))

# フィルタリングと選択
relevant_classes <- c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")
data_filtered <- data %>%
  filter(CLASS %in% relevant_classes) %>%
  select(CLASS, BCORP1, age_group)  # 性別の列も選択する

# クラスの順序を指定し、それに対応する色を定義
class_order <- factor(data_filtered$CLASS, levels = c("HC", "COVID_moderate_severe", "COVID_mild", "AD", "AAV", "SSc", "SLE"))
class_colors <- c("SSc" = "orange", "AAV" = "green", "SLE" = "royalblue", "COVID_mild" = "lightcoral", "COVID_moderate_severe" = "magenta4", "HC" = "grey", "AD" = "cyan")

# 年齢（age）で別々にグラフを作成（50未満と50以上）
ggplot(data_filtered, aes(x = class_order, y = BCORP1, fill = class_order)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = paste("Anti-BCORP1 by Age Group:"), x = "Class", y = "[AU]") +
  geom_hline(yintercept=2.258800045, linetype=2, color='red') +
  theme_minimal() +
  facet_wrap(~age_group, scales = "free", ncol = 1)  # 年齢で分ける

# ノンパラメトリック検定（Mann-Whitney U検定）を年齢グループごとに実施
p_values <- data.frame()
for (age_grp in unique(data_filtered$age_group)) {
  hc_data <- data_filtered %>% filter(CLASS == "HC" & age_group == age_grp)
  for (class in relevant_classes[-1]) {
    test_data <- data_filtered %>% filter(CLASS == class & age_group == age_grp)
    if (nrow(test_data) > 0) {
      p_value <- wilcox.test(hc_data$BCORP1, test_data$BCORP1, exact = FALSE)$p.value
      significance <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns")))
      p_values <- rbind(p_values, data.frame(Group = class, Age_Group = age_grp, P_Value = p_value, Significance = significance))
    }
  }
}

# 50未満のデータのみでクラスの順序を再定義
data_under_50 <- data_filtered %>% filter(age_group == "<50")
data_under_50$CLASS <- factor(data_under_50$CLASS, levels = relevant_classes)

# 50未満のデータのみでグラフを作成
plot_under_50 <- ggplot(data_under_50, aes(x = CLASS, y = BCORP1, fill = CLASS)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-BCORP1 for Age <50", x = "Class", y = "[AU]") +
  geom_hline(yintercept=2.258800045, linetype=2, color='red') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 30)) +
  geom_text(data = p_values %>% filter(Age_Group == "<50"), 
            aes(x = Group, y = 1.3 * max(data_under_50$BCORP1[data_under_50$CLASS == Group], na.rm = TRUE), 
                label = Significance), 
            inherit.aes = FALSE, color = "black", size = 5, vjust = -0.5)

plot_under_50

# 50以上のデータのみでクラスの順序を再定義
data_above_50 <- data_filtered %>% filter(age_group == ">=50")
data_above_50$CLASS <- factor(data_above_50$CLASS, levels = relevant_classes)

# 50以上のデータのみでグラフを作成
plot_above_50 <- ggplot(data_above_50, aes(x = CLASS, y = BCORP1, fill = CLASS)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-BCORP1 for Age >=50", x = "Class", y = "[AU]") +
  geom_hline(yintercept=2.258800045, linetype=2, color='red') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 30)) +
  geom_text(data = p_values %>% filter(Age_Group == ">=50"), 
            aes(x = Group, y = 1.3 * max(data_above_50$BCORP1[data_above_50$CLASS == Group], na.rm = TRUE), 
                label = Significance), 
            inherit.aes = FALSE, color = "black", size = 5, vjust = -0.5)

plot_above_50

