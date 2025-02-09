# 必要なライブラリの読み込み
library(tidyverse)
library(ggplot2)
library(readr)

# データの読み込み
early_df <- read.csv("early_covid.csv")
late_df <- read.csv("late_covid.csv")

# A4GALT 以降の列のインデックスを取得
a4galt_index_early <- which(colnames(early_covid) == "A4GALT")
a4galt_index_late <- which(colnames(late_covid) == "A4GALT")

# A4GALT 以降の列の総和を計算
early_sums <- rowSums(early_covid[, a4galt_index_early:ncol(early_covid)], na.rm = TRUE)
late_sums <- rowSums(late_covid[, a4galt_index_late:ncol(late_covid)], na.rm = TRUE)

# データフレームに変換
sums_df <- data.frame(EarlySums = early_sums, LateSums = late_sums)

# ウィルコクソンの符号付き順位和検定を実施
test_result <- wilcox.test(sums_df_clean$EarlySums, sums_df_clean$LateSums, paired = TRUE)

# 結果を表示
print(paste("Wilcoxon test p-value:", test_result$p.value))

# 時点情報を追加してデータフレームを作成
sums_df <- data.frame(Sum = c(early_sums, late_sums),
                        Timepoint = factor(rep(c("Early", "Late"), 
                                               times = c(length(early_sums), length(late_sums)))))

# 欠損値を含む行を除外
sums_df_clean <- na.omit(sums_df)

# 箱ひげ図を描画
ggplot(sums_df_clean, aes(x = Timepoint, y = Sum, fill = Timepoint)) +
  geom_boxplot() +
  labs(title = "Sum of Autoantibody Levels",
       y = "[AU]", x = "Timepoint") +
  ylim(c(0,15000)) +
  theme_minimal()
