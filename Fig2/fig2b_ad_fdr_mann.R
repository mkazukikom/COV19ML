# 必要なパッケージをロード
library(ggplot2)
library(dplyr)

# データの読み込み
ad_data <- read.csv("AD_data_2.csv")
hc_data <- read.csv("HC_data.csv")

# A4GALT以降の列のみを分析対象とする
columns_to_analyze <- colnames(ad_data)[which(colnames(ad_data) == "A4GALT"):length(colnames(ad_data))]

# 結果を格納するデータフレームの初期化
results_df <- data.frame(Gene = character(), Log2FC = numeric(), PValue = numeric(), TestUsed = character(), stringsAsFactors = FALSE)

# 各列について計算
for (column in columns_to_analyze) {
  ad_values <- ad_data[[column]]
  hc_values <- hc_data[[column]]
  
  # 正規性検定（Shapiro-Wilk検定）
  shapiro_ad <- shapiro.test(ad_values)$p.value
  shapiro_hc <- shapiro.test(hc_values)$p.value
  
  # 使用する統計検定を選択
  if (shapiro_ad > 0.05 & shapiro_hc > 0.05) {
    # 正規性が満たされている場合：t検定を使用
    test_result <- t.test(ad_values, hc_values, alternative = "two.sided", var.equal = FALSE)
    test_used <- "t-test"
  } else {
    # 正規性が満たされていない場合：Mann-Whitney U検定を使用
    test_result <- wilcox.test(ad_values, hc_values, alternative = "two.sided")
    test_used <- "Mann-Whitney U test"
  }
  
  # Log2 Fold Changeを計算
  mean_ad <- mean(ad_values, na.rm = TRUE)
  mean_hc <- mean(hc_values, na.rm = TRUE)
  log2fc <- log2(mean_ad / mean_hc)
  
  # 結果を追加
  results_df <- rbind(results_df, data.frame(
    Gene = column,
    Log2FC = log2fc,
    PValue = test_result$p.value,
    TestUsed = test_used
  ))
}

# P値をFDR（Benjamini-Hochberg法）で補正
results_df$FDR <- p.adjust(results_df$PValue, method = "BH")

# FDRが0.05未満かつLog2 Fold Changeが1を超える項目を抽出しCSVに出力
significant_genes <- results_df %>% 
  filter(FDR < 0.05, Log2FC > 1)
write.csv(significant_genes, "significant_genes_ad_fdr_mann.csv", row.names = FALSE)

# Volcano Plotの作成（縦軸をFDRに変更、ハイライト条件をLog2FC > 1に設定）
ggplot(results_df, aes(x = Log2FC, y = -log10(FDR))) +
  geom_point(aes(color = (FDR < 0.05 & Log2FC > 1)), alpha = 0.5) +
  scale_color_manual(values = c("TRUE" = "cyan", "FALSE" = "black")) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "red") +
  labs(title = "AD", x = "Log2 Fold Change", y = "-log10(FDR)") +
  xlim(c(-5, 10)) +
  ylim(c(0, 30)) +
  theme_minimal() +
  theme(legend.position = "none")
