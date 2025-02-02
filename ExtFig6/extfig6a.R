# 必要なライブラリをロード
library(ggplot2)
library(ggpubr)

# データを読み込む
data <- read.csv("combined_data_age_sex.csv")  # ファイル名は適宜修正してください

# Spearman's 相関係数とP値を計算
cor_test <- cor.test(data$WPA_KAT2A, data$ELISA_KAT2A, method = "spearman")

# 散布図を作成
ggplot(data, aes(x = WPA_KAT2A, y = ELISA_KAT2A)) +
  geom_point(alpha = 0.7) +  # 散布図
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # 回帰直線と信頼区間
  labs(
    title = paste0(
      "Spearman's r: ", 
      round(cor_test$estimate, 2), 
      ", p-value: ", 
      format(cor_test$p.value, scientific = FALSE, digit=2)
    ),
    x = "WPA_KAT2A",
    y = "ELISA_KAT2A"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
