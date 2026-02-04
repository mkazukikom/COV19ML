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

# --- 閾値の定義と割合の計算 ---
thresholds <- c(3, 4, 5)

# 各閾値を超えるデータの割合（％）を計算
z_values <- melted_data$Zscore
pct_above <- sapply(thresholds, function(t) {
  sum(z_values >= t, na.rm = TRUE) / length(z_values) * 100
})

# 表示用のテキストラベル作成
label_text <- paste0("Z >= ", thresholds, ": ", sprintf("%.2f", pct_above), "%")
colors <- c("blue", "red", "darkgreen") # 3=青, 4=赤, 5=緑

# 1. Manhattanプロットの更新
manhattan_plot <- ggplot(melted_data, aes(x = as.numeric(Gene), y = Zscore)) +
  geom_point(alpha = 0.3, size = 0.5, color = "gray40") +
  theme_minimal() +
  labs(title = "Manhattan Plot: Sensitivity Analysis of Thresholds",
       x = "Gene Index (Sorted by Max Z-score)", y = "Z-score") +
  # 3, 4, 5 のラインを追加
  geom_hline(yintercept = thresholds, color = colors, linetype = "dashed", linewidth = 0.8) +
  # 割合をグラフ左上に表示
  annotate("text", x = 0, y = thresholds + 0.5, label = label_text, 
           hjust = 0, color = colors, fontface = "bold", size = 4) +
  scale_y_continuous(breaks = seq(-4, ceiling(max(melted_data$Zscore)), by = 2))

# 2. ヒストグラムの更新
histogram_plot <- ggplot(melted_data, aes(x = Zscore)) +
  geom_histogram(bins = 100, fill = "gray80", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Z-scores with Thresholds", 
       x = "Z-score", y = "Frequency") +
  
  # 垂直ラインを追加
  geom_vline(xintercept = thresholds, color = colors, linetype = "dashed", linewidth = 0.8) +
  
  # 割合をラベルとして表示
  annotate("text", 
           x = thresholds + 0.2, 
           y = 0,                # y軸の基点を0に設定
           label = label_text, 
           hjust = -0.1,         # テキストの開始位置を少し右（上方向）へずらす
           vjust = 0.5,          # 垂直ラインに対して中央に配置
           angle = 90,           # 90度回転して縦書きに
           color = colors, 
           fontface = "bold", 
           size = 3.5) +
  
  # y軸の設定：下側の隙間をなくし、上側に少し余裕を持たせる
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     breaks = scales::pretty_breaks()) +
  
  scale_x_continuous(breaks = seq(floor(min(melted_data$Zscore)), 
                                  ceiling(max(melted_data$Zscore)), by = 2))

# 3. 累積ヒストグラム（ECDF）の更新
cumulative_histogram_plot <- ggplot(melted_data, aes(x = Zscore)) +
  stat_ecdf(geom = "step", color = "black", linewidth = 1) +
  theme_minimal() +
  labs(title = "Cumulative Distribution of Z-scores", 
       x = "Z-score", y = "Cumulative Percentage (%)") +
  
  # y軸の設定（0〜100%）
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1),
                     expand = expansion(mult = c(0, 0.05))) +
  
  # 垂直ラインを追加 [cite: 147-149]
  geom_vline(xintercept = thresholds, color = colors, linetype = "dashed", linewidth = 0.8) +
  
  # 割合のラベルを下揃え（y=0基点）で表示
  annotate("text", 
           x = thresholds + 0.2, 
           y = 0,                # y軸の基点を0に設定
           label = label_text, 
           hjust = -0.1,         # 下端から少し浮かせる
           vjust = 0.5, 
           angle = 90, 
           color = colors, 
           fontface = "bold", 
           size = 4) +
  
  scale_x_continuous(breaks = seq(floor(min(melted_data$Zscore)), 
                                  ceiling(max(melted_data$Zscore)), by = 2))

# プロットの出力
print(manhattan_plot)
print(histogram_plot)
print(cumulative_histogram_plot)
