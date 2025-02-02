# 必要なパッケージをロード
library(ggplot2)
library(dplyr)

# データの読み込み
covid_data <- read.csv("COVID_data.csv")
hc_data <- read.csv("HC_data.csv")

# A4GALT以降の列のみを分析対象とする
columns_to_analyze <- colnames(covid_data)[which(colnames(covid_data) == "A4GALT"):length(colnames(covid_data))]

# 結果を格納するデータフレームの初期化
results_df <- data.frame(Gene = character(), Log2FC = numeric(), PValue = numeric(), stringsAsFactors = FALSE)

# 各列について計算
for (column in columns_to_analyze) {
  covid_values <- covid_data[[column]]
  hc_values <- hc_data[[column]]
  
  # Mann-Whitney U検定を実施
  mw_test_result <- wilcox.test(covid_values, hc_values, alternative = "two.sided")
  
  # Log2 Fold Changeを計算
  mean_covid <- mean(covid_values, na.rm = TRUE)
  mean_hc <- mean(hc_values, na.rm = TRUE)
  log2fc <- log2(mean_covid / mean_hc)
  
  # 結果を追加
  results_df <- rbind(results_df, data.frame(
    Gene = column,
    Log2FC = log2fc,
    PValue = mw_test_result$p.value
  ))
}

# P値をFDR（Benjamini-Hochberg法）で補正
results_df$FDR <- p.adjust(results_df$PValue, method = "BH")

# FDRが0.05未満かつLog2 Fold Changeが1を超える項目を抽出しCSVに出力
significant_genes <- results_df %>% 
  filter(FDR < 0.05, Log2FC > 1)
write.csv(significant_genes, "significant_genes_covid_fdr_mann.csv", row.names = FALSE)

# Volcano Plotの作成 (縦軸をFDRに変更)
ggplot(results_df, aes(x = Log2FC, y = -log10(FDR))) +
  geom_point(aes(color = (FDR < 0.05 & Log2FC > 1)), alpha = 0.5) +
  scale_color_manual(values = c("TRUE" = "magenta", "FALSE" = "black")) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "red") +
  labs(title = "COVID-19", x = "Log2 Fold Change", y = "-log10(FDR)") +
  xlim(c(-5, 10)) +
  ylim(c(0, 30)) +
  theme_minimal() +
  theme(legend.position = "none")
