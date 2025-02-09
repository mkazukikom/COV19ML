# 必要なライブラリを読み込む
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# データを読み込む
early_covid <- read_csv("early_covid.csv")
late_covid <- read_csv("late_covid.csv")

# early と late データセットを準備
early_melted <- gather(early_covid, key="Marker", value="Value", SARSCoV2_N, SARSCoV2_S1, SARSCoV2_RBD)
early_melted$Timepoint <- 'Early'

late_melted <- gather(late_covid, key="Marker", value="Value", SARSCoV2_N, SARSCoV2_S1, SARSCoV2_RBD)
late_melted$Timepoint <- 'Late'

# 両方を結合
combined_data <- rbind(early_melted, late_melted)

# 欠損値を含む行を除外
combined_data_clean <- na.omit(combined_data)

# 箱ひげ図を描画
ggplot(combined_data_clean, aes(x=Marker, y=Value, fill=Timepoint)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title="Antibodies against SARS-CoV-2",
       y="Serum levels [AU]") +
  geom_hline(yintercept = 10, linetype=2, color='red') +
  theme_minimal()

# name を使って early と late のデータセットをペアリング
paired_data <- merge(early_covid, late_covid, by="name", suffixes=c("_early", "_late"))

# 欠損値を含む行を除外
paired_data_clean <- na.omit(paired_data)

# ウィルコクソンの符号付き順位和検定を実施
for (marker in c("SARSCoV2_N", "SARSCoV2_S1", "SARSCoV2_RBD")) {
  early_col <- paste(marker, "early", sep="_")
  late_col <- paste(marker, "late", sep="_")
  
  test_result <- wilcox.test(paired_data_clean[[early_col]], paired_data_clean[[late_col]], paired=TRUE)
  
  print(paste(marker, "Wilcoxon test p-value:", test_result$p.value))
}
