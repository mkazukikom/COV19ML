library(ggplot2)

# データの読み込み
early_covid <- read.csv("early_covid.csv")
late_covid <- read.csv("late_covid.csv")

# 差分の計算
diff_covid <- data.frame(
  KAT2A = late_covid$KAT2A - early_covid$KAT2A,
  SARSCoV2_N = late_covid$SARSCoV2_N - early_covid$SARSCoV2_N,
  SARSCoV2_S1 = late_covid$SARSCoV2_S1 - early_covid$SARSCoV2_S1,
  SARSCoV2_RBD = late_covid$SARSCoV2_RBD - early_covid$SARSCoV2_RBD
)

# 描画関数
draw_plot <- function(data, variable_name, y_variable, title_prefix) {
  ggplot(data, aes_string(x=variable_name, y=y_variable)) +
    geom_point() +
    geom_smooth(method="lm", color="red", fill="red", alpha=0.2, fullrange = TRUE) +
    labs(title=paste(title_prefix, variable_name, "vs", y_variable))
}

# 3x3 パネルの描画
plot_list <- list(
  draw_plot(early_covid, "KAT2A", "SARSCoV2_N", "Early"),
  draw_plot(late_covid, "KAT2A", "SARSCoV2_N", "Late"),
  draw_plot(diff_covid, "KAT2A", "SARSCoV2_N", "Δ"),
  
  draw_plot(early_covid, "KAT2A", "SARSCoV2_S1", "Early"),
  draw_plot(late_covid, "KAT2A", "SARSCoV2_S1", "Late"),
  draw_plot(diff_covid, "KAT2A", "SARSCoV2_S1", "Δ"),
  
  draw_plot(early_covid, "KAT2A", "SARSCoV2_RBD", "Early"),
  draw_plot(late_covid, "KAT2A", "SARSCoV2_RBD", "Late"),
  draw_plot(diff_covid, "KAT2A", "SARSCoV2_RBD", "Δ")
)

# グリッドレイアウトでプロット
gridExtra::grid.arrange(grobs=plot_list, ncol=3)
