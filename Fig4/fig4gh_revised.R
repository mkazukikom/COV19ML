# Load necessary libraries
library(ggplot2)
library(dplyr)
library(pROC)

# Load the data
data <- read.csv("combined_data.csv")

# Merge COVID_moderate and COVID_severe into COVID_moderate_severe
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# Filter for relevant classes
relevant_classes <- c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")
data_filtered <- data %>%
  filter(CLASS %in% relevant_classes) %>%
  select(CLASS, KAT2A)

# Define the class order according to the specified order and their corresponding colors
class_order <- factor(data_filtered$CLASS, levels = c("HC", "COVID_moderate_severe", "COVID_mild", "AD", "AAV", "SSc", "SLE"))
class_colors <- c("SSc" = "orange", "AAV" = "green", "SLE" = "royalblue", "COVID_mild" = "lightcoral", "COVID_moderate_severe" = "magenta4", "HC" = "grey", "AD" = "cyan")

# Perform Mann-Whitney U tests between HC and other groups
hc_data <- data_filtered %>%
  filter(CLASS == "HC") %>%
  pull(KAT2A)

comparison_results <- data_filtered %>%
  filter(CLASS != "HC") %>%
  group_by(CLASS) %>%
  summarise(p_value = wilcox.test(hc_data, KAT2A)$p.value)

# Format the p-values for annotation
comparison_results <- comparison_results %>%
  mutate(label = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns"))))

# Plotting
ggplot(data_filtered, aes(x = class_order, y = KAT2A, fill = class_order)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-KAT2A", x = "Class", y = "[AU]") +
  geom_hline(yintercept = 30.58432629, linetype = 2, color = 'red') +
  theme_minimal() +
  geom_text(data = comparison_results, aes(x = CLASS, y = max(data_filtered$KAT2A) * 1.1, label = label), inherit.aes = FALSE)

# Load necessary libraries
library(dplyr)
library(pROC)
library(ggplot2)

# Filter and prepare data for ROC analysis
data_roc <- data_filtered %>%
  filter(CLASS %in% c("HC", "COVID_mild", "COVID_moderate_severe", "SLE", "AAV", "SSc", "AD")) %>%
  mutate(COVID_status = ifelse(CLASS %in% c("COVID_mild", "COVID_moderate_severe"), 1, 0))

# Generate ROC curve
roc_result <- roc(data_roc$COVID_status, data_roc$KAT2A)

# Create ROC plot
roc_plot <- ggroc(roc_result) +
  labs(
    title = "ROC Curve for anti-KAT2A",
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal()

# Calculate sensitivity and specificity at the threshold
threshold = 30.58432629
coords_result <- coords(roc_result, x = threshold, input = "threshold", ret = c("specificity", "sensitivity"))
spec <- as.numeric(coords_result["specificity"])
sens <- as.numeric(coords_result["sensitivity"])

# Ensure the point is on the ROC curve
print(paste("Specificity:", spec, "Sensitivity:", sens))

# Add the red point to the plot
roc_plot <- roc_plot +
  annotate("point", x = spec, y = sens, color = "red", size = 3) +
  annotate(
    "text",
    x = spec - 0.05,  # Adjust the label position for better visibility
    y = sens - 0.05,
    label = paste0(
      "Sensitivity: ", round(sens, 2), 
      "\nSpecificity: ", round(spec, 2)
    ),
    hjust = 0, size = 4
  )

# Display the ROC plot
print(roc_plot)
