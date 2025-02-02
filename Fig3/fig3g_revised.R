# 必要なライブラリをロード
library(dplyr)
library(fmsb)
library(scales)

# データの読み込み
data <- read.csv("combined_data.csv")

# COVID関連クラスの統合
data$CLASS <- ifelse(data$CLASS %in% c("COVID_mild", "COVID_moderate", "COVID_severe"), "COVID19", data$CLASS)

# HCとCOVID19のデータのみを選択
data_filtered <- data %>%
  filter(CLASS %in% c("HC", "COVID19")) %>%
  select(CLASS, LINC00304, BCORP1, KAT2A, FBF1, STON1_GTF2A1L, TRIM21, RPS4X, KLF3_AS1, SP8, BEGAIN)

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
           pcol=c("magenta", "grey"),
           )
