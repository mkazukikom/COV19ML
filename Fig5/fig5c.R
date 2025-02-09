library(dplyr)
library(ggplot2)

# データの読み込み
early_df <- read.csv("early_covid.csv")
late_df <- read.csv("late_covid.csv")

# 'name'列でマージ
merged_df <- merge(early_df, late_df, by="name", suffixes = c("_early", "_late"))

# A4GALT以降の列に対する処理
genes <- names(early_df)[which(names(early_df) == "A4GALT"):ncol(early_df)]

# 結果を格納するためのリストを初期化
results <- list()

for (gene in genes) {
  early_vals <- merged_df[[paste(gene, "early", sep = "_")]]
  late_vals <- merged_df[[paste(gene, "late", sep = "_")]]
  
  # 対応のあるt検定
  test_result <- t.test(early_vals, late_vals, paired = TRUE)
  
  # Log2 Fold Changeの計算
  log2_fc <- log2(mean(late_vals + 1e-9) / (mean(early_vals) + 1e-9))
  
  # 結果をリストに追加
  results[[gene]] <- c(p_value = test_result$p.value, Log2FoldChange = log2_fc)
}

# 結果のデータフレーム化
results_df <- bind_rows(results, .id = "Gene")

# p-valueとLog2FoldChangeに基づいて色を割り当て
results_df$Color <- case_when(
  results_df$p_value < 0.01 & results_df$Log2FoldChange > 0 ~ "red",
  results_df$p_value < 0.01 & results_df$Log2FoldChange < 0 ~ "blue",
  TRUE ~ "black"
)

# Volcano plotの作成
ggplot(results_df, aes(x = Log2FoldChange, y = -log10(p_value), color = Color)) +
  geom_point() +
  scale_color_identity() +
  geom_hline(yintercept = -log10(0.01), linetype="dashed", color = "black") +
#  geom_vline(xintercept = -1, linetype="dashed", color = "blue") +
#  geom_vline(xintercept = 1, linetype="dashed", color = "red") +
  labs(title = "Volcano Plot", x = "Log2 Fold Change", y = "-log10(p-value)") +
  theme_minimal()
