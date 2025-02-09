# 必要なパッケージをインストールしてロード
install.packages("VennDiagram")
library(VennDiagram)
library(dplyr)

# データを読み込む
early_covid <- read.csv("early_covid.csv")  # 適切なパスに置き換えてください
late_covid <- read.csv("late_covid.csv")  # 適切なパスに置き換えてください

# 'name'列で統合
merged_data <- merge(early_covid, late_covid, by = "name", suffixes = c("_early", "_late"))

# A4GALT列以降の列に対してt検定を実施
start_col <- match("A4GALT_early", colnames(merged_data))
significant_items <- list()

for (i in start_col:length(colnames(merged_data))) {
  feature <- colnames(merged_data)[i]
  if (grepl("_early$", feature)) {
    late_feature <- sub("_early$", "_late", feature)
    if (late_feature %in% colnames(merged_data)) {
      early_values <- merged_data[[feature]]
      late_values <- merged_data[[late_feature]]
      t_test <- t.test(early_values, late_values, na.action = na.omit)
      
      # Log2 Fold Changeを計算
      log2_fold_change <- mean(late_values, na.rm = TRUE) / mean(early_values, na.rm = TRUE)
      log2_fold_change <- log2(log2_fold_change)
      
      if (t_test$p.value < 0.01 && log2_fold_change > 1) {
        significant_items[[feature]] <- list(PValue = t_test$p.value, Log2FC = log2_fold_change)
      }
    }
  }
}

# 結果をデータフレームに変換
significant_items_df <- bind_rows(significant_items, .id = "Feature") %>%
  select(Feature, PValue, Log2FC)

# 結果をCSVファイルに出力
write.csv(significant_items_df, "significant_items_increased.csv", row.names = FALSE)



# CSVファイルからデータを読み込む
significant_genes_covid <- read.csv("significant_genes_covid.csv", header=TRUE)$gene
significant_genes_negative <- read.csv("significant_genes_negative.csv", header=TRUE)$gene
significant_genes_positive <- read.csv("significant_genes_positive.csv", header=TRUE)$gene

# リストに格納
gene_lists <- list(
  COVID = significant_genes_covid,
  Negative = significant_genes_negative,
  Positive = significant_genes_positive
)

# ベン図を作成
venn.plot <- venn.diagram(
  x = gene_lists,
  category.names = c("Elevated in COVID19", "Decreased", "Increased"),
  filename = NULL,
  output = TRUE,
  fill = c("magenta", "blue", "red"),
  cat.col = c("magenta", "blue", "red"),
  cat.cex = 0.6,
  cat.pos = c(0,0,0),
  cat.fontfamily = "Helvetica",
  fontfamily = "Helvetica",
  scaled=TRUE,
  inverted=TRUE,
  margin = 0.05
)

# ベン図を表示
grid.draw(venn.plot)
