library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# データの読み込み
early_covid <- read.csv("early_covid.csv")
late_covid <- read.csv("late_covid.csv")

# Severity情報の抽出と統合 (moderateとsevereを統合)
early_covid$Severity <- sapply(strsplit(early_covid$name, "_"), function(x) paste(head(x, -1), collapse = "_"))
late_covid$Severity <- sapply(strsplit(late_covid$name, "_"), function(x) paste(head(x, -1), collapse = "_"))
early_covid$Severity[early_covid$Severity %in% c("moderate", "severe")] <- "moderate/severe"
late_covid$Severity[late_covid$Severity %in% c("moderate", "severe")] <- "moderate/severe"

# データを結合
combined_data <- merge(early_covid, late_covid, by = "name", suffixes = c("_early", "_late"))

# BCORP1のグラフ
bcrop1_data <- combined_data %>% 
  select(name, Severity_early, BCORP1_early, BCORP1_late) %>%
  pivot_longer(cols = c(BCORP1_early, BCORP1_late), names_to = "Time", values_to = "BCORP1") %>%
  mutate(Time = ifelse(Time == "BCORP1_early", "Early", "Late"))

bcrop1_combined <- bcrop1_data %>% 
  mutate(Severity_combined = "All")

bcrop1_plot <- ggplot(bcrop1_data, aes(x = Time, y = BCORP1, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~Severity_early, scales = "free", ncol = 2) +
  stat_compare_means(aes(group = Time), method = "wilcox.test", paired = TRUE, label = "p.signif") +
  theme_light() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 2.258800045, linetype = 2, color = 'red') +
  ylim(0, 20) +
  labs(title = "", x = "", y = "[AU]")

bcrop1_combined_plot <- ggplot(bcrop1_combined, aes(x = Time, y = BCORP1, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~Severity_combined, scales = "free", ncol = 1) +
  stat_compare_means(aes(group = Time), method = "wilcox.test", paired = TRUE, label = "p.signif") +
  theme_light() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 2.258800045, linetype = 2, color = 'red') +
  ylim(0, 20) +
  labs(title = "Anti-BCORP1", x = "", y = "[AU]")

# KAT2Aのグラフ
kat2a_data <- combined_data %>% 
  select(name, Severity_early, KAT2A_early, KAT2A_late) %>%
  pivot_longer(cols = c(KAT2A_early, KAT2A_late), names_to = "Time", values_to = "KAT2A") %>%
  mutate(Time = ifelse(Time == "KAT2A_early", "Early", "Late"))

kat2a_combined <- kat2a_data %>% 
  mutate(Severity_combined = "All")

kat2a_plot <- ggplot(kat2a_data, aes(x = Time, y = KAT2A, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~Severity_early, scales = "free", ncol = 2) +
  stat_compare_means(aes(group = Time), method = "wilcox.test", paired = TRUE, label = "p.signif") +
  theme_light() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 30.58432629, linetype = 2, color = 'red') +
  ylim(0, 50) +
  labs(title = "", x = "", y = "[AU]")

kat2a_combined_plot <- ggplot(kat2a_combined, aes(x = Time, y = KAT2A, fill = Time)) +
  geom_boxplot() +
  facet_wrap(~Severity_combined, scales = "free", ncol = 1) +
  stat_compare_means(aes(group = Time), method = "wilcox.test", paired = TRUE, label = "p.signif") +
  theme_light() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 30.58432629, linetype = 2, color = 'red') +
  ylim(0, 50) +
  labs(title = "Anti-KAT2A", x = "", y = "[AU]")

# 並べて表示
ggarrange(bcrop1_combined_plot, bcrop1_plot, kat2a_combined_plot, kat2a_plot, 
          ncol = 2, nrow = 2)

