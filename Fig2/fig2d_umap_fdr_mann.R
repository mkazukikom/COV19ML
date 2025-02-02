# 必要なライブラリをロード
library(umap)
library(ggplot2)
library(readr)
library(dplyr)
library(ggExtra)

# filtered_combined_dataを読み込む
data <- read_csv("filtered_combined_data_fdr_mann.csv")

# CLASSの中でCOVID_mild/moderate/severeをCOVID19に統合
data$CLASS <- as.factor(ifelse(grepl("COVID", data$CLASS), "COVID19", as.character(data$CLASS)))

# 分析に使用するデータを選択 (ID, CLASS, COVID列を除外)
data_for_umap <- select(data, -ID, -COVID) %>%
  filter(CLASS %in% c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# CLASS列を別に保存し、解析対象の数値型データのみを抽出
classes <- data_for_umap$CLASS
data_for_umap <- select(data_for_umap, -CLASS)

# 数値型に変換
data_for_umap <- as.data.frame(lapply(data_for_umap, as.numeric))

# NA値を0で置換
data_for_umap[is.na(data_for_umap)] <- 0

# UMAP解析を実施
set.seed(20) # 結果の再現性のため
umap_result <- umap(data_for_umap)

# UMAP結果をデータフレームに変換し、CLASS情報を追加
umap_data <- as.data.frame(umap_result$layout) %>%
  setNames(c("UMAP1", "UMAP2")) %>%
  mutate(CLASS = classes)

# 判例の順番を指定
umap_data$CLASS <- factor(umap_data$CLASS, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# UMAPプロットをCLASSで色分けして描画
p <- ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = CLASS)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c(
    "HC" = "grey",
    "COVID19" = "magenta",
    "AD" = "cyan",
    "AAV" = "green",
    "SSc" = "orange",
    "SLE" = "royalblue"
  )) +
  theme_minimal() +
  labs(title = "UMAP Plot", x = "UMAP1", y = "UMAP2") +
  theme(legend.title = element_blank())

# マージナルヒストグラムを追加
ggExtra::ggMarginal(
  p,
  type = "density",
  margins = "both",
  size = 3,
  groupColour = TRUE,
  groupFill = TRUE
)

# 必要なライブラリをロード
library(Rtsne)
library(ggplot2)
library(readr)
library(dplyr)
library(ggExtra)

# filtered_combined_dataを読み込む
data <- read_csv("filtered_combined_data_fdr.csv")

# CLASSの中でCOVID_mild/moderate/severeをCOVID19に統合
data$CLASS <- as.factor(ifelse(grepl("COVID", data$CLASS), "COVID19", as.character(data$CLASS)))

# 分析に使用するデータを選択 (ID, CLASS, COVID列を除外)
data_for_tsne <- select(data, -ID, -COVID) %>%
  filter(CLASS %in% c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# CLASS列を別に保存し、解析対象の数値型データのみを抽出
classes <- data_for_tsne$CLASS
data_for_tsne <- select(data_for_tsne, -CLASS)

# 数値型に変換
data_for_tsne <- as.data.frame(lapply(data_for_tsne, as.numeric))

# NA値を0で置換
data_for_tsne[is.na(data_for_tsne)] <- 0

# t-SNE解析を実施
set.seed(10) # 再現性確保
tsne_result <- Rtsne(data_for_tsne, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# t-SNE結果をデータフレームに変換し、CLASS情報を追加
tsne_data <- as.data.frame(tsne_result$Y) %>%
  setNames(c("tSNE1", "tSNE2")) %>%
  mutate(CLASS = classes)

# 判例の順番を指定
tsne_data$CLASS <- factor(tsne_data$CLASS, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# t-SNEプロットをCLASSで色分けして描画
p <- ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = CLASS)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c(
    "HC" = "grey",
    "COVID19" = "magenta",
    "AD" = "cyan",
    "AAV" = "green",
    "SSc" = "orange",
    "SLE" = "royalblue"
  )) +
  theme_minimal() +
  labs(title = "t-SNE Plot", x = "t-SNE1", y = "t-SNE2") +
  theme(legend.title = element_blank())

# マージナルヒストグラムを追加
ggExtra::ggMarginal(
  p,
  type = "density",
  margins = "both",
  size = 3,
  groupColour = TRUE,
  groupFill = TRUE
)

# 必要なライブラリをロード
library(ggplot2)
library(readr)
library(dplyr)
library(ggExtra)

# filtered_combined_dataを読み込む
data <- read_csv("filtered_combined_data_fdr.csv")

# CLASSの中でCOVID_mild/moderate/severeをCOVID19に統合
data$CLASS <- as.factor(ifelse(grepl("COVID", data$CLASS), "COVID19", as.character(data$CLASS)))

# 分析に使用するデータを選択 (ID, CLASS, COVID列を除外)
data_for_pca <- select(data, -ID, -COVID) %>%
  filter(CLASS %in% c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# CLASS列を別に保存し、解析対象の数値型データのみを抽出
classes <- data_for_pca$CLASS
data_for_pca <- select(data_for_pca, -CLASS)

# 数値型に変換
data_for_pca <- as.data.frame(lapply(data_for_pca, as.numeric))

# NA値を0で置換
data_for_pca[is.na(data_for_pca)] <- 0

# PCA解析を実施
pca_result <- prcomp(data_for_pca, center = TRUE, scale. = TRUE)

# PCA結果をデータフレームに変換し、CLASS情報を追加
pca_data <- as.data.frame(pca_result$x[, 1:2]) %>%
  setNames(c("PC1", "PC2")) %>%
  mutate(CLASS = classes)

# 判例の順番を指定
pca_data$CLASS <- factor(pca_data$CLASS, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# PCAプロットをCLASSで色分けして描画
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = CLASS)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c(
    "HC" = "grey",
    "COVID19" = "magenta",
    "AD" = "cyan",
    "AAV" = "green",
    "SSc" = "orange",
    "SLE" = "royalblue"
  )) +
  theme_minimal() +
  labs(
    title = "PCA Plot",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "% variance)")
  ) +
  theme(legend.title = element_blank())

# マージナルヒストグラムを追加
ggExtra::ggMarginal(
  p,
  type = "density",
  margins = "both",
  size = 3,
  groupColour = TRUE,
  groupFill = TRUE
)
