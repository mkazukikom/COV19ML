# 必要なパッケージをロード
library(ggplot2)
library(reshape2)

# データの読み込み
data <- read.csv("HC_data.csv")

# Min-Max正規化の実施
norm_data <- data[, which(colnames(data) == "A4GALT"):ncol(data)]
norm_data <- as.data.frame(lapply(norm_data, function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))  # 定数列なら0
  (x - rng[1]) / diff(rng)
}))

# サンプルID列を追加
norm_data$Sample <- seq_len(nrow(norm_data))

# 長い形式に変換
melted_data <- melt(norm_data, id.vars = "Sample", variable.name = "Gene", value.name = "NormValue")

# 各遺伝子の最大値でソート順を決定
gene_max_values <- aggregate(NormValue ~ Gene, data = melted_data, max)
gene_order <- gene_max_values$Gene[order(-gene_max_values$NormValue)]
melted_data$Gene <- factor(melted_data$Gene, levels = gene_order)

# ヒストグラム
histogram_plot <- ggplot(melted_data, aes(x = NormValue)) +
  geom_histogram(bins = 50, fill = "gray", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Normalized Values",
       x = "Normalized Value",
       y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))

print(histogram_plot)
