# 必要なライブラリをロード
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("combined_data_age_sex.csv")

# Merge COVID_moderate and COVID_severe into COVID_moderate_severe
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# Filter for relevant classes and select Male data
relevant_classes <- c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")
data_filtered <- data %>%
  filter(CLASS %in% relevant_classes, sex == "M") %>%
  select(CLASS, BCORP1)

# Define the class order according to the specified order and their corresponding colors
class_order <- factor(data_filtered$CLASS, levels = c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "SSc", "AD", "SLE"))
class_colors <- c("SSc" = "orange", "AAV" = "green", "SLE" = "royalblue", "COVID_mild" = "lightcoral", "COVID_moderate_severe" = "magenta4", "HC" = "grey", "AD" = "cyan")

# Perform Mann-Whitney U tests between HC and other groups
hc_data <- data_filtered %>%
  filter(CLASS == "HC") %>%
  pull(BCORP1)

comparison_results <- data_filtered %>%
  filter(CLASS != "HC") %>%
  group_by(CLASS) %>%
  summarise(p_value = wilcox.test(hc_data, BCORP1, exact=FALSE)$p.value)

# Format the p-values for annotation
comparison_results <- comparison_results %>%
  mutate(label = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns"))))

# Plotting for Male data
ggplot(data_filtered, aes(x = class_order, y = BCORP1, fill = class_order)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-BCORP1 (Male)", x = "Class", y = "[AU]") +
  geom_hline(yintercept = 2.258800045, linetype = 2, color = 'red') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 30)) +
  geom_text(data = comparison_results, 
            aes(x = CLASS, y = max(data_filtered$BCORP1) * 1.1, label = label), 
            inherit.aes = FALSE, color = "black")

# Load the data
data <- read.csv("combined_data_age_sex.csv")

# Merge COVID_moderate and COVID_severe into COVID_moderate_severe
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# Filter for relevant classes and select Female data
data_filtered_female <- data %>%
  filter(CLASS %in% relevant_classes, sex == "F") %>%
  select(CLASS, BCORP1)

# Convert CLASS to factor with specified order
data_filtered_female <- data_filtered_female %>%
  mutate(CLASS = factor(CLASS, levels = c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "SSc", "AD", "SLE")))

# Perform Mann-Whitney U tests between HC and other groups for Female
hc_data_female <- data_filtered_female %>%
  filter(CLASS == "HC") %>%
  pull(BCORP1)

comparison_results_female <- data_filtered_female %>%
  filter(CLASS != "HC") %>%
  group_by(CLASS) %>%
  summarise(p_value = wilcox.test(hc_data_female, BCORP1, ecact=FALSE)$p.value)

# Format the p-values for annotation (Female)
comparison_results_female <- comparison_results_female %>%
  mutate(label = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns"))))

# Plotting for Female data
ggplot(data_filtered_female, aes(x = CLASS, y = BCORP1, fill = CLASS)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-BCORP1 (Female)", x = "Class", y = "[AU]") +
  geom_hline(yintercept = 2.258800045, linetype = 2, color = 'red') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 30)) +
  geom_text(data = comparison_results_female, 
            aes(x = CLASS, y = max(data_filtered_female$BCORP1, na.rm = TRUE) * 1.1, label = label), 
            inherit.aes = FALSE, color = "black")
