# 必要なパッケージのロード
library(corrplot)
library(readr)

# データ読み込み（適宜、ファイルパスを変更してください）
data <- read_csv("combined_data.csv")

# 選択した遺伝子
genes <- c("BCORP1", "HNRNPA1L2", "KAT2A", "BICDL1", "FUT2", "PDLIM7", "TRNAU1AP", "SSX2IP", "C6orf223", "BLM")

# 選択した遺伝子に対応する列のみを抽出
selected_data <- data[,genes]

# 相関行列の計算
cor_matrix <- cor(selected_data, method="spearman")
testRes = cor.mtest(selected_data, conf.level = 0.95)

# 相関行列のヒートマップを階層的クラスタリングと共に表示
corrplot(cor_matrix, method="circle", type="lower", order="hclust", insig="blank",
         addrect=2, tl.col="black", tl.srt=45, cl.lim=c(-1, 1),
         cl.ratio=0.1, cl.cex=0.75, tl.cex=0.7)

