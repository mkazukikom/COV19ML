# 必要なパッケージをロード（インストールされていない場合は先にインストール）
library(ComplexHeatmap)
library(circlize)
library(readr)
library(dplyr)

# データを読み込む
combined_data <- read_csv("combined_data.csv")
cytokines <- read_csv("receptors.csv")

# COVID_moderateとCOVID_severeをCOVIDにまとめる
combined_data$CLASS[combined_data$CLASS %in% c("COVID_moderate", "COVID_severe")] <- "COVID_moderate_severe"

# cytokinesの列名を取得
cytokines_columns <- colnames(cytokines)

# A4GALT以降のサイトカイン列を抽出
combined_data_filtered <- combined_data %>%
  select(A4GALT:ZZZ3) %>%
  select(all_of(cytokines_columns))

# 正規化関数の定義
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# データを正規化
normalized_data <- as.data.frame(lapply(combined_data_filtered, normalize))

# 二値化関数の定義（1.5625を閾値として0か1に）
binarize <- function(x) {
  return(ifelse(x > 1.5625, 1, 0))
}

# データを二値化
binarized_data <- as.data.frame(lapply(combined_data_filtered, binarize))

# 行名をセット
row.names(combined_data_filtered) <- combined_data$ID
row.names(binarized_data) <- combined_data$ID

# CLASS情報を因子として追加
class_info <- factor(combined_data$CLASS)

# sample_colorsの計算を修正
disease_colors <- setNames(c("grey", "magenta4", "lightcoral", "royalblue", "green", "orange", "cyan"),
                           c("HC", "COVID_moderate_severe", "COVID_mild", "SLE", "AAV", "SSc", "AD"))
sample_colors <- sapply(combined_data$CLASS, function(x) disease_colors[x])

# ヒートマップの生成
col_fun = colorRamp2(c(1.5625, 3.125), c("black", "red"))

heatmap_list <- Heatmap(as.matrix(combined_data_filtered), 
                        name = "[AU]", 
                        col = col_fun,
                        left_annotation = rowAnnotation(CLASS = anno_block(gp = gpar(fill = disease_colors))),
                        show_row_names = FALSE, 
                        show_column_names = TRUE, 
                        cluster_rows = FALSE, 
                        cluster_columns = TRUE,
                        row_split = class_info,
                        column_title = "Targeted cytokine receptors")

# ヒートマップの描画
draw(heatmap_list, heatmap_legend_side = "bot", annotation_legend_side = "bot")

