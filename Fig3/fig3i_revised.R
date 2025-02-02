# 必要なライブラリをロード
library(dplyr)
library(fmsb)
library(scales)

# データの読み込み
data <- read.csv("combined_data.csv")

# COVID関連クラスの統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# HCとCOVID19のデータのみを選択
data_filtered <- data %>%
  filter(CLASS %in% c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")) %>%
  select(CLASS, "BCORP1", "ZADH2", "TOP1", "CLIP4", "SRSF7", "MVK", "QDPR", "SNRPA", "KAT2A", "PECR",
         "CENPP", "CCDC74B", "TRIM21", "SNRPB", "KCNAB1", "CENPB", "BCL7A", "POLR2A", "PDLIM7", "FYTTD1")

# CLASS列以外のデータを正規化 (0-1の範囲)
data_normalized <- data_filtered %>%
  mutate(across(-CLASS, ~scale(.) %>% as.vector %>% {(. - min(.)) / (max(.) - min(.))}))

# HCとCOVID19の平均値を計算
mean_values <- data_normalized %>%
  group_by(CLASS) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# レーダーチャートのデータ準備
radar_data <- as.data.frame(mean_values[,-1])

# レーダーチャートのプロット範囲を追加
radar_chart_data <- rbind(rep(0.5, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)


# レーダーチャートの描画
radarchart(radar_chart_data,
           axistype = 2,
           plwd=4,
           plty=1,
           pcol=c("green", "cyan", "lightcoral", "magenta4", "grey", "royalblue", "orange"),
)

