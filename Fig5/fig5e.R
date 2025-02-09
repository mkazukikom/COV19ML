# 必要なパッケージをロード
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ComplexHeatmap)
library(circlize)

# データの読み込み
early_covid <- read_csv("early_covid.csv")
late_covid <- read_csv("late_covid.csv")

# 選択された遺伝子およびIgG
selected_genes <- c("ACAA1", "UCKL1", "CLK3", "MPLKIP", "SLC7A11", "FAM107B", "DND1", "FBF1", 
                    "AIFM3", "PTOV1", "BCORP1", "DUSP11", "FAM207A", "ODF3L1", "ZADH2", "CUX1",
                    "KCNAB2", "MYL2", "FOXP3", "DHX9", "KAT2A", "KEAP1", "PABPC3", "SLC9A3R1", 
                    "C1orf86", "SARSCoV2_N", "SARSCoV2_S1", "SARSCoV2_RBD")

# earlyとlateのデータをnameでペアリングし、差分を計算
paired_data <- merge(early_covid, late_covid, by="name", suffixes=c("_early", "_late"))
diff_data <- data.frame(name=paired_data$name)
for(gene in selected_genes) {
  diff_data[[paste0("Δ", gene)]] <- paired_data[[paste0(gene, "_late")]] - paired_data[[paste0(gene, "_early")]]
}

# 欠損値を含む行を削除
diff_data_cleaned <- na.omit(diff_data)

# スピアマン相関とP値の計算
correlation_matrix <- cor(diff_data_cleaned[,-1], method="spearman", use="complete.obs")
testRes = cor.mtest(diff_data_cleaned[,-1], conf.level = 0.95)

# 階層的クラスタリングとヒートマップの描画
corrplot(correlation_matrix, p.mat = testRes$p, sig.level = 0.05, insig='blank', order="hclust", type="lower")
