# 必要なライブラリをロード
library(ggplot2)
library(dplyr)

# データ読み込みと前処理関数
read_and_process <- function(file_path, disease_name) {
  data <- read.csv(file_path)
  sum_data <- rowSums(data[, which(colnames(data) == "A4GALT"):ncol(data)])
  return(data.frame(Disease = disease_name, Sum = sum_data))
}

# データセットのパスと疾患名
data_paths <- list(
  HC = "HC_data.csv",
  COVID19 = "COVID_data.csv",
  AD = "AD_data_2.csv",
  AAV = "AAV_data_2.csv",
  SLE = "SLE_data_2.csv",
  SSc = "SSc_data.csv"
)

# 全データを読み込み、結合
all_data <- do.call(rbind, lapply(names(data_paths), function(disease) {
  read_and_process(data_paths[[disease]], disease)
}))

# 疾患の順番を指定
all_data$Group <- factor(all_data$Disease, levels = c("HC", "COVID19", "AD", "AAV", "SSc", "SLE"))

# ノンパラメトリック検定（Mann-Whitney U検定）を実施
hc_sums <- filter(all_data, Disease == "HC")$Sum
diseases <- setdiff(names(data_paths), "HC")
p_values <- sapply(diseases, function(disease) {
  disease_sums <- filter(all_data, Disease == disease)$Sum
  wilcox.test(hc_sums, disease_sums)$p.value
})

# 検定結果をデータフレームに整理
stat_results <- data.frame(
  Group = diseases,
  P_Value = p_values,
  Significance = sapply(p_values, function(p) {
    if (p < 0.001) {
      "***"
    } else if (p < 0.01) {
      "**"
    } else if (p < 0.05) {
      "*"
    } else {
      "ns"
    }
  })
)

# 箱ひげ図を描画
boxplot_plot <- ggplot(all_data, aes(x=Group, y=Sum, fill=Group)) +
  geom_boxplot() +
  scale_fill_manual(values=c("HC"="gray", "COVID19"="magenta", "AD"="cyan", 
                             "AAV"="green", "SSc"="orange", "SLE"="royalblue")) +
  theme_minimal() +
  ylim(c(0,20000)) +
  labs(title="Sum of Autoantibody Levels", x="Group", y="[AU]") +
  geom_text(data = stat_results, aes(x = Group, y = 20000, label = Significance), 
            inherit.aes = FALSE, color = "black", size = 5, vjust = -0.5)

print(boxplot_plot)
