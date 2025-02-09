library(ComplexHeatmap)
library(circlize)
library(dplyr)

# データの読み込み
data <- read.csv("combined_data.csv")

# COVID関連クラスの統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# HCとCOVID19のデータのみを選択
data_filtered <- data %>%
  filter(CLASS %in% c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")) %>%
  select(CLASS, "BCORP1", "ZADH2", "TOP1", "CLIP4", "SRSF7", "MVK", "QDPR", "SNRPA", "KAT2A", "PECR",
         "CENPP", "CCDC74B", "TRIM21", "SNRPB", "KCNAB1", "CENPB", "BCL7A", "POLR2A", "PDLIM7", "FYTTD1")

# データの正規化（列ごと）
normalize_data <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data_norm <- as.data.frame(lapply(data_filtered[, -c(1)], normalize_data)) # 最初の2列（ID, Disease）を除外
data_norm$CLASS <- data_filtered$CLASS

# 疾患ごとに行をスライスするための準備
# 疾患名でソートし、疾患ごとにデータを分割
diseases_sorted <- sort(unique(data$CLASS))
data$CLASS <- factor(data$CLASS, levels = diseases_sorted)
data_sorted <- data[order(data$CLASS), ]

# 正規化されたデータにも同じ順番を適用
data_norm_sorted <- data_norm[order(data$CLASS), ]

# 疾患ファクターを正規化されたデータに追加
data_norm$CLASS <- factor(data_filtered$CLASS, levels = diseases_sorted)

# sample_colorsの計算を修正
disease_colors <- setNames(c("grey","lightcoral","magenta4","cyan","green","royalblue","orange"),
                           c("HC", "COVID_mild", "COVID_moderate_severe", "AD","AAV","SLE","SSc"))
sample_colors <- sapply(data_norm_sorted$CLASS, function(x) disease_colors[x])

# 疾患ごとにスライスされたヒートマップの作成
Heatmap(as.matrix(data_norm_sorted[, -which(names(data_norm_sorted) == "CLASS")]), # CLASS列を除外
        split = data_norm_sorted$CLASS, # split引数を修正
        column_km = 3,
        col = c("black", "red"),
        name = "[AU]",
        top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = c("white", "sienna", "orchid")),
                                                            labels = c("I", "II", "III"), 
                                                            labels_gp = gpar(col = "black", fontsize = 10))),
        left_annotation = rowAnnotation(CLASS = anno_block(gp = gpar(fill = disease_colors))),
        column_names_gp = gpar(fontsize = 10),
        row_names_gp = gpar(col = NA), # 行名を非表示にする
        column_title = "Targeted Autoantigens",
        row_title = "Samples",
        cluster_rows = TRUE,
        cluster_columns = TRUE)

