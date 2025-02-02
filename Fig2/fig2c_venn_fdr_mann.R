# 必要なパッケージをロード
library(VennDiagram)

# 各疾患の有意な遺伝子リストを読み込む
# ファイルパスは実際のパスに置き換えてください
significant_genes_aav <- read.csv("significant_genes_aav_fdr_mann.csv")$Gene
significant_genes_ad <- read.csv("significant_genes_ad_fdr_mann.csv")$Gene
significant_genes_covid <- read.csv("significant_genes_covid_fdr_mann.csv")$Gene
significant_genes_sle <- read.csv("significant_genes_sle_fdr_mann.csv")$Gene
significant_genes_ssc <- read.csv("significant_genes_ssc_fdr_mann.csv")$Gene

# リストに格納
gene_lists <- list(
  COVID = significant_genes_covid,
  AD = significant_genes_ad,
  AAV = significant_genes_aav,
  SSc = significant_genes_ssc,
  SLE = significant_genes_sle
)

# ベン図を作成
venn.plot <- venn.diagram(
  x = gene_lists,
  category.names = c("COVID-19", "AD", "AAV", "SSc", "SLE"),
  filename = NULL,
  output = TRUE,
  fill = c("magenta", "cyan", "green", "orange", "royalblue"),
  cat.col = c("magenta", "cyan", "green", "orange", "royalblue"),
  cat.cex = 0.6,
#  cat.fontface = "bold",
  cat.fontfamily = "Helvetica",
  fontfamily = "Helvetica",
  margin = 0.05
)

# ベン図を表示
grid.draw(venn.plot)
