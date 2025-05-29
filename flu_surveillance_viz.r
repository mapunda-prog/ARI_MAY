# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)
library(gridExtra)
library(readr)

# Read the data
flu_data <- read_csv("Influenza_summary_data.csv")
#flu_data <- janitor::clean_names(flu_data)
#glimpse(flu_data)

# Clean and prepare data
flu_data_clean <- flu_data %>%
  filter(WEEK != "Total") %>%  # Remove total row
  mutate(
    week_num = as.numeric(gsub("Week ", "", WEEK)),
    # Calculate positivity rates
    flu_positivity_rate = (`Total Influenza Positive` / `Specimens Processed`) * 100,
    covid_positivity_rate = (`Total SARS-CoV-2 Positive` / `Specimens Processed`) * 100,
    # Handle division by zero
    flu_positivity_rate = ifelse(is.infinite(flu_positivity_rate) | is.nan(flu_positivity_rate), 
                                0, flu_positivity_rate),
    covid_positivity_rate = ifelse(is.infinite(covid_positivity_rate) | is.nan(covid_positivity_rate), 
                                  0, covid_positivity_rate)
  )

# 1. Weekly Specimens Received and Processed
p1 <- ggplot(flu_data_clean, aes(x = week_num)) +
  geom_line(aes(y = `Specimens Received`, color = "Received"), size = 1.2) +
  geom_line(aes(y = `Specimens Processed`, color = "Processed"), size = 1.2) +
  geom_point(aes(y = `Specimens Received`, color = "Received"), size = 2) +
  geom_point(aes(y = `Specimens Processed`, color = "Processed"), size = 2) +
  scale_color_manual(values = c("Received" = "#2E86AB", "Processed" = "#A23B72")) +
  scale_x_continuous(breaks = seq(0, 25, 5), labels = paste0("Week ", seq(0, 25, 5))) +
  labs(
    title = "Weekly Specimen Collection and Processing",
    subtitle = "Influenza Surveillance Data",
    x = "Epidemiological Week",
    y = "Number of Specimens",
    color = "Specimen Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 2. Influenza Strain Distribution Over Time
flu_strains <- flu_data_clean %>%
  select(week_num, `Pandemic A (H1N1)`, `A (H3)`, influenza_B) %>%
  pivot_longer(cols = -week_num, names_to = "strain", values_to = "count") %>%
  mutate(strain = case_when(
    strain == "Pandemic A (H1N1)" ~ "A(H1N1)pdm09",
    strain == "A (H3)" ~ "A(H3N2)",
    strain == "influenza_B" ~ "Influenza B",
    TRUE ~ strain
  ))

p2 <- ggplot(flu_strains, aes(x = week_num, y = count, fill = strain)) +
  geom_area(alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
  scale_x_continuous(breaks = seq(0, 25, 5), labels = paste0("Week ", seq(0, 25, 5))) +
  labs(
    title = "Influenza Strain Distribution by Week",
    subtitle = "Stacked area chart showing positive cases by strain type",
    x = "Epidemiological Week",
    y = "Number of Positive Cases",
    fill = "Influenza Strain"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 3. Influenza vs COVID-19 Comparison
comparison_data <- flu_data_clean %>%
  select(week_num, `Total Influenza Positive`, `Total SARS-CoV-2 Positive`) %>%
  pivot_longer(cols = -week_num, names_to = "pathogen", values_to = "count") %>%
  mutate(pathogen = ifelse(pathogen == "Total Influenza Positive", "Influenza", "SARS-CoV-2"))

p3 <- ggplot(comparison_data, aes(x = week_num, y = count, color = pathogen)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(values = c("Influenza" = "#E74C3C", "SARS-CoV-2" = "#3498DB")) +
  scale_x_continuous(breaks = seq(0, 25, 5), labels = paste0("Week ", seq(0, 25, 5))) +
  labs(
    title = "Influenza vs SARS-CoV-2 Positive Cases",
    subtitle = "Weekly comparison of positive detections",
    x = "Epidemiological Week",
    y = "Number of Positive Cases",
    color = "Pathogen"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 4. Positivity Rates
positivity_data <- flu_data_clean %>%
  select(week_num, flu_positivity_rate, covid_positivity_rate) %>%
  pivot_longer(cols = -week_num, names_to = "pathogen", values_to = "rate") %>%
  mutate(pathogen = case_when(
    pathogen == "flu_positivity_rate" ~ "Influenza",
    pathogen == "covid_positivity_rate" ~ "SARS-CoV-2",
    TRUE ~ pathogen
  ))

p4 <- ggplot(positivity_data, aes(x = week_num, y = rate, fill = pathogen)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Influenza" = "#E74C3C", "SARS-CoV-2" = "#3498DB")) +
  scale_x_continuous(breaks = seq(0, 25, 5), labels = paste0("Week ", seq(0, 25, 5))) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Weekly Positivity Rates",
    subtitle = "Percentage of processed specimens testing positive",
    x = "Epidemiological Week",
    y = "Positivity Rate (%)",
    fill = "Pathogen"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 5. Summary Statistics Table
summary_stats <- flu_data %>%
  filter(WEEK == "Total") %>%
  select(-WEEK) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Count") %>%
  mutate(
    Percentage = case_when(
      Metric %in% c("Pandemic A (H1N1)", "A (H3)", "influenza_B") ~ 
        round((Count / flu_data$`Total Influenza Positive`[flu_data$WEEK == "Total"]) * 100, 1),
      Metric == "Total Influenza Positive" ~ 
        round((Count / flu_data$`Specimens Processed`[flu_data$WEEK == "Total"]) * 100, 1),
      Metric == "Total SARS-CoV-2 Positive" ~ 
        round((Count / flu_data$`Specimens Processed`[flu_data$WEEK == "Total"]) * 100, 1),
      TRUE ~ NA_real_
    )
  )

# 6. Heatmap of Weekly Activity
heatmap_data <- flu_data_clean %>%
  select(week_num, `Pandemic A (H1N1)`, `A (H3)`, influenza_B, `Total SARS-CoV-2 Positive`) %>%
  pivot_longer(cols = -week_num, names_to = "pathogen", values_to = "count") %>%
  mutate(pathogen = case_when(
    pathogen == "Pandemic A (H1N1)" ~ "A(H1N1)pdm09",
    pathogen == "A (H3)" ~ "A(H3N2)",
    pathogen == "influenza_B" ~ "Influenza B",
    pathogen == "Total SARS-CoV-2 Positive" ~ "SARS-CoV-2",
    TRUE ~ pathogen
  ))

p5 <- ggplot(heatmap_data, aes(x = week_num, y = pathogen, fill = count)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "Cases") +
  scale_x_continuous(breaks = seq(1, 21, 2), labels = paste0("W", seq(1, 21, 2))) +
  labs(
    title = "Pathogen Detection Heatmap",
    subtitle = "Weekly positive cases by pathogen type",
    x = "Epidemiological Week",
    y = "Pathogen"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)

# Print summary statistics
print("=== SURVEILLANCE SUMMARY STATISTICS ===")
print(summary_stats)

# Create a combined dashboard layout
dashboard <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Save plots (optional)
# ggsave("specimens_processed.png", p1, width = 10, height = 6, dpi = 300)
# ggsave("strain_distribution.png", p2, width = 10, height = 6, dpi = 300)
# ggsave("flu_vs_covid.png", p3, width = 10, height = 6, dpi = 300)
# ggsave("positivity_rates.png", p4, width = 10, height = 6, dpi = 300)
# ggsave("pathogen_heatmap.png", p5, width = 12, height = 6, dpi = 300)
# ggsave("surveillance_dashboard.png", dashboard, width = 16, height = 12, dpi = 300)