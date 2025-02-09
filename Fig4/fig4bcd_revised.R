library(ggplot2)
library(ComplexHeatmap)
library(circlize)
library(dplyr)
library(ggExtra)

# データの読み込み
data <- read.csv("combined_data.csv")

# COVID関連クラスの統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# HCとCOVID19のデータのみを選択
data_filtered <- data %>%
  filter(CLASS %in% c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")) %>%
  select(CLASS, "BCORP1", "ZADH2", "TOP1", "CLIP4", "SRSF7", "MVK", "QDPR", "SNRPA", "KAT2A", "PECR",
         "CENPP", "CCDC74B", "TRIM21", "SNRPB", "KCNAB1", "CENPB", "BCL7A", "POLR2A", "PDLIM7", "FYTTD1")

# PCAを実行する列（遺伝子発現値）の選択
pca_data <- data_filtered[, -1]  # 最初の列(CLASS)を除外

# PCA実行
pca_result <- prcomp(pca_data, scale. = TRUE)

# PCA結果の概要
summary(pca_result)

# PCAスコアの取得
pca_scores <- as.data.frame(pca_result$x)

# CLASS情報の追加
pca_scores$CLASS <- data_filtered$CLASS

# Define the class order according to the specified order and their corresponding colors
class_order <- factor(data_filtered$CLASS, levels = c("HC", "COVID_moderate_severe", "COVID_mild", "SLE", "AAV", "SSc", "AD"))
class_colors <- c("SSc" = "orange", "AAV" = "green", "SLE" = "royalblue", "COVID_mild" = "lightcoral", "COVID_moderate_severe" = "magenta4", "HC" = "grey", "AD" = "cyan")

# PCAの第一主成分と第二主成分でプロット
p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = class_order)) +
  scale_color_manual(values = class_colors) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "PCA",
       x = "PC1",
       y = "PC2")

# マージナルヒストグラムを追加
ggExtra::ggMarginal(p,
                    type = "density", 
                    margins = "both", 
                    size = 3,
                    groupColour = TRUE,
                    groupFill = TRUE)


# PCAの負荷量をデータフレームに変換
loadings <- as.data.frame(pca_result$rotation[, 1:2])  # PC1とPC2の負荷量
colnames(loadings) <- c("PC1", "PC2")
loadings$Gene <- rownames(loadings)

# PC1とPC2の負荷量のグラフを表示
ggplot(loadings, aes(x = PC1, y = PC2, label = Gene)) +
  geom_text(vjust = 1.5, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Loadings Plot for PC1 and PC2",
       x = "PC1 Loadings",
       y = "PC2 Loadings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PC1の負荷量を棒グラフで表示
ggplot(loadings, aes(x = reorder(Gene, PC1), y = PC1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Loadings for PC1", x = "Targeted Autoantigens", y = "Loading")

# PC2の負荷量を棒グラフで表示
ggplot(loadings, aes(x = reorder(Gene, PC2), y = PC2)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Loadings for PC2", x = "Targeted Autoantigens", y = "Loading")
