# 必要なパッケージをロード
library(ggplot2)
library(reshape2)

# データの読み込み
data <- read.csv("HC_data.csv")

# Z-scoreの計算
zscore_data <- data[, which(colnames(data) == "A4GALT"):ncol(data)]
zscore_data <- scale(zscore_data)

# Z-scoreデータフレームを作成し、サンプル列を追加
zscore_df <- as.data.frame(zscore_data)
zscore_df$Sample <- seq_len(nrow(zscore_df))  # サンプル列を行番号で作成

# データを長い形式に変換
melted_data <- reshape2::melt(zscore_df, id.vars = "Sample", variable.name = "Gene", value.name = "Zscore")

# 各遺伝子のZ-score最大値を計算
gene_max_values <- aggregate(Zscore ~ Gene, data = melted_data, max)

# 最大値に基づいて遺伝子を並べ替える順序を決定
gene_order <- gene_max_values$Gene[order(-gene_max_values$Zscore)]  # 最大値の降順
melted_data$Gene <- factor(melted_data$Gene, levels = gene_order)

# Manhattanプロットの作成
manhattan_plot <- ggplot(melted_data, aes(x = as.numeric(Gene), y = Zscore)) +
  geom_point(alpha = 0.5, size = 0.5) +
  theme_minimal() +
  labs(title = "Manhattan Plot of Z-scores by Sample (Sorted by Max Z-score)",
       x = "Gene Index (Sorted by Max Z-score)",
       y = "Z-score") +
  scale_x_continuous(breaks = seq(0, max(as.numeric(melted_data$Gene)), by = 1000),
                     labels = seq(0, max(as.numeric(melted_data$Gene)), by = 1000)) +
  scale_y_continuous(breaks = seq(floor(min(melted_data$Zscore)), ceiling(max(melted_data$Zscore)), by = 2)) +
  geom_hline(yintercept = 4, color = "red", linetype = "dashed")  # Z-score = 4 に赤い点線を追加

# プロットを表示
print(manhattan_plot)

# Histogram of Z-scores
histogram_plot <- ggplot(melted_data, aes(x = Zscore)) +
  geom_histogram(bins = 50, fill = "gray", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Z-scores",
       x = "Z-score",
       y = "Frequency")+
  scale_x_continuous(breaks = seq(floor(min(melted_data$Zscore)), ceiling(max(melted_data$Zscore)), by = 2))+
  geom_vline(xintercept = 4, color = "red", linetype = "dashed") 

print(histogram_plot)

# Cumulative histogram of Z-scores
cumulative_histogram_plot <- ggplot(melted_data, aes(x = Zscore)) +
  stat_ecdf(geom = "step", color = "blue") +
  theme_minimal() +
  labs(title = "Cumulative Distribution of Z-scores",
       x = "Z-score",
       y = "Cumulative Percentage (%)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(floor(min(melted_data$Zscore)), ceiling(max(melted_data$Zscore)), by = 2)) +
  geom_hline(yintercept = 0.99, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 4, color = "red", linetype = "dashed")

print(cumulative_histogram_plot)

