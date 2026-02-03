library(ggplot2)
library(ComplexHeatmap)
library(circlize)
library(dplyr)
library(ggExtra)

# データの読み込み
data <- read.csv("combined_data.csv")

# COVID関連クラスの統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

data_filtered <- data %>%
  filter(CLASS %in% c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")) %>%
  select(CLASS, "BCORP1", "ZADH2", "CLIP4", "TOP1", "SRSF7", "SNRPA", "KCNAB1", "KAT2A", "MVK", "SNRPB",
         "CENPP", "QDPR", "TRIM21", "PECR", "POLR2A", "CCDC74B", "CENPB", "FYTTD1","PDLIM7","BCL7A")

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

loadings$cluster <- "Other"

loadings$cluster[loadings$Gene %in% c("KAT2A", "BCORP1")] <- "Cluster I"

loadings$cluster[loadings$Gene %in% c(
  "MVK", "KCNAB1", "ZADH2", "CLIP4", "QDPR",
  "PECR", "FYTTD1", "CCDC74B"
)] <- "Cluster II"

loadings$cluster[loadings$Gene %in% c(
  "TRIM21", "PDLIM7", "BCL7A", "SRSF7", "SNRPB",
  "SNRPA", "CENPB", "CENPP", "POLR2A", "TOP1"
)] <- "Cluster III"


# PC1とPC2の負荷量のグラフを表示
cluster_colors <- c(
  "Cluster I"   = "black", 
  "Cluster II"  = "sienna",
  "Cluster III" = "orchid"
)

loadings$cluster <- factor(loadings$cluster,
                           levels = c("Cluster I", "Cluster II", "Cluster III", "Other"))

ggplot(loadings, aes(x = PC1, y = PC2, label = Gene, color = cluster)) +
  geom_text(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(name = "Cluster", values = cluster_colors) +
  guides(color = guide_legend(override.aes = list(label = "●", size = 5))) +
  theme_minimal() +
  labs(
    title = "Loadings Plot for PC1 and PC2",
    x = "PC1 Loadings",
    y = "PC2 Loadings",
    color = "Cluster"
  )

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
