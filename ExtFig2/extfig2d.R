# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("combined_data.csv")

# Merge COVID_moderate and COVID_severe into COVID_moderate_severe
data$CLASS <- ifelse(data$CLASS %in% c("COVID_moderate", "COVID_severe"), "COVID_moderate_severe", data$CLASS)

# Filter for relevant classes
relevant_classes <- c("HC", "COVID_mild", "COVID_moderate_severe", "AAV", "AD", "SSc", "SLE")
data_filtered <- data %>%
  filter(CLASS %in% relevant_classes) %>%
  select(CLASS, IFNA4)

# Define the class order according to the specified order and their corresponding colors
class_order <- factor(data_filtered$CLASS, levels = c("HC", "COVID_moderate_severe", "COVID_mild", "AD", "AAV", "SSc", "SLE"))
class_colors <- c("SSc" = "orange", "AAV" = "green", "SLE" = "royalblue", "COVID_mild" = "lightcoral", "COVID_moderate_severe" = "magenta4", "HC" = "grey", "AD" = "cyan")

# Plotting
ggplot(data_filtered, aes(x = class_order, y = log10(IFNA4), fill = class_order)) +
  geom_boxplot() +
  scale_fill_manual(values = class_colors) +
  scale_x_discrete(name = "Class") +
  labs(title = "Anti-IFNA4", x = "Class", y = "[Log10(AU)]") +
  theme_minimal()

