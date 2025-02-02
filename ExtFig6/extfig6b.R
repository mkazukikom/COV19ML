# 必要なライブラリをロード
library(ggplot2)
library(ggpubr)

# データを読み込む
data <- read.csv("COVID_elisa.csv")  # ファイル名は適宜修正してください

# Spearman's 相関係数とP値を計算
cor_test <- cor.test(data$WPA_BCORP1, data$ELISA_BCOR, method = "spearman")

# P値を有効数字2桁にフォーマット
formatted_p_value <- formatC(cor_test$p.value, digits = 2)

# 散布図を作成
ggplot(data, aes(x = WPA_BCORP1, y = ELISA_BCOR)) +
  geom_point(alpha = 0.7) +  # 散布図
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # 回帰直線と信頼区間
  labs(
    title = paste0(
      "Spearman's r: ", 
      round(cor_test$estimate, 2), 
      ", p-value: ", 
      formatted_p_value
    ),
    x = "WPA_BCORP1 [AU]",
    y = "ELISA_BCOR [AU]"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
