---
title: "FEMA Mitigation Findings"
author: "Mathew Hill"
date: "2025-04-10"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(readxl)
library(tidyverse)
library(caret)
library(broom)
library(gt)
library(ggplot2)
library(scales)
library(segmented)
library(tigris)
library(sf)
library(dplyr)
library(plotly)
options(tigris_use_cache = TRUE)

options(scipen = 999)  # Prevents scientific notation globally
```

```{r}
bcr_dataset <- read.csv("C:/Users/mathe/OneDrive/Documents/final_dataset_joined.csv")
#dataset
```

```{r}
mitigation_pct_dataset <- read_excel("C:/Users/mathe/OneDrive/Documents/APAN Final Project Dataset copy.xlsx")
#final dataset
```

```{r}
mitigation_pct_dataset <- mitigation_pct_dataset %>%
  filter(estimated_total_assistance_cost > 0, 
         hazard_mitigation_assistance > 0, 
         !is.na(total_with_hazard_mitigation)
         )
```

```{r}
mitigation_pct_dataset <- mitigation_pct_dataset %>% filter(!is.na(hazard_mitigation_assistance))

# Create the new variable: percent of disaster costs spent on mitigation
mitigation_pct_dataset <- mitigation_pct_dataset %>%
  mutate(percent_mitigation = (hazard_mitigation_assistance / total_with_hazard_mitigation)*100)
```

```{r}
# Define a function to remove outliers using IQR
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  return(data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ])
}

# Apply outlier removal to both percent_mitigation and total_with_hazard_mitigation
mitigation_pct_dataset_clean <- remove_outliers(mitigation_pct_dataset, "percent_mitigation")
mitigation_pct_dataset_clean <- remove_outliers(mitigation_pct_dataset_clean, "total_with_hazard_mitigation")
mitigation_pct_dataset_clean <- remove_outliers(mitigation_pct_dataset_clean, "total_without_hazard_mitigation")
```

```{r}
# Assuming your dataset is called 'final_dataset_joined'
bcr_dataset_filtered <- bcr_dataset %>%
  mutate(
    costShareProjectAmount = costSharePercentage * projectAmount,
    costShareInitialObligationAmount = costSharePercentage * initialObligationAmount,
    costShareFederalShareObligated = costSharePercentage * federalShareObligated
  )
```

```{r}
# Define disaster type columns
disaster_types <- c("severe_storms", "snow_storms", "winter_storms", "straight_line_winds", 
                    "flooding", "tornadoes", "landslides_or_mudslides", "tsunamis", 
                    "tropical_storm", "hurricane", "earthquake", "typhoons", "wildfires")

# Create disaster_list column
bcr_dataset_filtered <- bcr_dataset_filtered %>%
  mutate(disaster_list = apply(bcr_dataset_filtered[, disaster_types], 1, function(row) {
    present_disasters <- disaster_types[row == 1]  # Get disaster names where value == 1
    if (length(present_disasters) == 0) return(NA)  # If no disasters, return NA
    paste(present_disasters, collapse = ", ")  # Join names with commas
  }))

# View the first few rows
head(bcr_dataset_filtered$disaster_list)
```

```{r}
bcr_dataset_filtered <- bcr_dataset_filtered %>% 
  filter(benefitCostRatio < 50) %>%
  filter(covid == 0)
```

```{r}
# Split disaster_list into separate rows so each disaster type is counted correctly
disaster_bcr_summary <- bcr_dataset_filtered %>%
  filter(!is.na(benefitCostRatio) & !is.na(projectAmount)) %>%  # Remove missing values
  separate_rows(disaster_list, sep = ", ") %>%  # Split multiple disasters into separate rows
  group_by(disaster_list) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),  # Weighted avg
    total_spent = sum(projectAmount),  # Total amount spent for context
    count = n()  # Number of projects in each disaster category
  ) %>%
  arrange(desc(weighted_avg_bcr))  # Sort by highest return

# View the results
print(disaster_bcr_summary)
```

```{r}
# Check if all the disaster types in disaster_list are covered by the labels
unique(disaster_bcr_summary$disaster_list)

# Remove any rows with NA in disaster_list
disaster_bcr_summary_clean <- disaster_bcr_summary %>%
  filter(!is.na(disaster_list))

# Custom labels for disaster types
disaster_labels <- c(
  "severe_storms" = "Severe Storms",
  "snow_storms" = "Snow Storms",
  "winter_storms" = "Winter Storms",
  "straight_line_winds" = "Straight-Line Winds",
  "flooding" = "Flooding",
  "tornadoes" = "Tornadoes",
  "landslides_or_mudslides" = "Landslides/Mudslides",
  "tsunamis" = "Tsunamis",
  "tropical_storm" = "Tropical Storms",
  "hurricane" = "Hurricanes",
  "earthquake" = "Earthquakes",
  "typhoons" = "Typhoons",
  "wildfires" = "Wildfires"
)

# Now plot the cleaned data
ggplot(disaster_bcr_summary_clean, aes(x = reorder(disaster_list, weighted_avg_bcr), y = weighted_avg_bcr)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Mitigation Return on Investment by Disaster Type",
    x = "Disaster Type",
    y = "Weighted Average Benefit-Cost Ratio",
    caption = "Weighted by project cost"
  ) +
  scale_x_discrete(labels = disaster_labels) +  # 🎯 Apply nice labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

```


```{r}
# Split disaster_list into separate rows so each disaster type is counted correctly
region_bcr_summary <- bcr_dataset_filtered %>%
  filter(!is.na(benefitCostRatio) & !is.na(projectAmount)) %>%  # Remove missing values
  separate_rows(disaster_list, sep = ", ") %>%  # Split multiple disasters into separate rows
  group_by(region) %>%  # Group by region, NOT disaster_list
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),  # Weighted avg
    total_spent = sum(projectAmount),  # Total amount spent for context
    count = n()  # Number of projects in each region
  ) %>%
  arrange(desc(weighted_avg_bcr))  # Sort by highest return

# View the results
print(region_bcr_summary)
```

```{r}
# 🚀 Visualization of Weighted Benefit-Cost Ratio by FEMA Region
ggplot(region_bcr_summary, aes(x = reorder(region, weighted_avg_bcr), y = weighted_avg_bcr)) +
  geom_col(fill = "darkorange") +  # Bar chart with orange bars
  coord_flip() +  # Flip for readability
  labs(
    title = "Average Return on Investment for Mitigation by FEMA Region",
    x = "FEMA Region",
    y = "Weighted Average Benefit-Cost Ratio",
    caption = "Weighted by project cost"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
```

```{r}
# Filter and clean data
state_bcr_summary <- bcr_dataset_filtered %>%
  filter(!is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))  # Sort from highest to lowest

# Extract top 10 and bottom 10 states
top_10_states <- state_bcr_summary %>% slice_head(n = 10)
bottom_10_states <- state_bcr_summary %>% slice_tail(n = 10)
```

```{r}
# 🚀 Visualization for Top 10 States
ggplot(top_10_states, aes(x = reorder(state, weighted_avg_bcr), y = weighted_avg_bcr)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 States and Territories: Highest ROI for Mitigation",
    x = "State",
    y = "Weighted Average Benefit-Cost Ratio",
    caption = "Weighted by project cost"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
```

```{r}
# 🚀 Visualization for Bottom 10 States
ggplot(bottom_10_states, aes(x = reorder(state, weighted_avg_bcr), y = weighted_avg_bcr)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Bottom 10 States: Lowest ROI for Mitigation",
    x = "State",
    y = "Weighted Average Benefit-Cost Ratio",
    caption = "Weighted by project cost"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
```

```{r}
# Function to clean up dollar signs and commas
clean_numeric <- function(x) {
  x <- gsub(",", "", x)         # Remove commas
  x <- gsub("\\$", "", x)       # Remove dollar signs
  as.numeric(x)                 # Convert to numeric
}

# Apply to your columns
bcr_dataset_filtered <- bcr_dataset_filtered %>%
  mutate(
    estimated_public_assistance_cost = clean_numeric(estimated_public_assistance_cost),
    hazard_mitigation_assistance = clean_numeric(hazard_mitigation_assistance),
    total_with_hazard_mitigation = clean_numeric(total_with_hazard_mitigation),
    total_without_hazard_mitigation = clean_numeric(total_without_hazard_mitigation),
    estimated_total_assistance_cost = clean_numeric(estimated_total_assistance_cost),
    
    # Recalculate your percentages
    percent_mitigation = (hazard_mitigation_assistance / total_with_hazard_mitigation) * 100,
    percent_mitigation_estimated = (hazard_mitigation_assistance / 
                                     (estimated_total_assistance_cost + hazard_mitigation_assistance)) * 100
  )

```

```{r}
# Filter dataset to only include states with weighted ROI below 1
low_roi_states <- c("DE", "OK", "DC", "IL", "IA", "PA", "RI", "WI", "VT", "IN")

filtered_low_roi_data <- bcr_dataset_filtered %>%
  filter(state %in% low_roi_states)

# Filter dataset to only include states with weighted ROI above 1
high_roi_states <- c("WY", "GU", "AL", "GA", "HI", "MS", "OK", "VI", "CT", "CA")

filtered_high_roi_data <- bcr_dataset_filtered %>%
  filter(state %in% high_roi_states)
```

```{r}
bcr_dataset_filtered <- bcr_dataset_filtered %>%
  mutate(weight = total_with_hazard_mitigation / sum(total_with_hazard_mitigation, na.rm = TRUE))

bcr_dataset_filtered <- bcr_dataset_filtered %>%
  mutate(weighted_percent_mitigation = percent_mitigation * weight)

filtered_low_roi_data <- filtered_low_roi_data %>%
  mutate(weight = total_with_hazard_mitigation / sum(total_with_hazard_mitigation, na.rm = TRUE))

filtered_high_roi_data <- filtered_high_roi_data %>%
  mutate(weight = total_with_hazard_mitigation / sum(total_with_hazard_mitigation, na.rm = TRUE))


weighted_mean_low <- sum(filtered_low_roi_data$percent_mitigation * filtered_low_roi_data$weight, na.rm = TRUE)
weighted_mean_high <- sum(filtered_high_roi_data$percent_mitigation * filtered_high_roi_data$weight, na.rm = TRUE)
```

```{r}
weighted_roi_means <- data.frame(
  ROI_Group = c("Low ROI", "High ROI"),
  Weighted_Mean_Mitigation = c(weighted_mean_low, weighted_mean_high)
)
```

```{r}
# Filter states with low and high ROI
low_roi_states <- c("DE", "OK", "DC", "IL", "IA", "PA", "RI", "WI", "VT", "IN")
high_roi_states <- c("WY", "GU", "AL", "GA", "HI", "MS", "OK", "VI", "CT", "CA")

filtered_low_roi_data <- bcr_dataset_filtered %>%
  filter(state %in% low_roi_states)

filtered_high_roi_data <- bcr_dataset_filtered %>%
  filter(state %in% high_roi_states)

# Now, calculate weighted mean BCR (not percentage mitigation) for low and high ROI states
filtered_low_roi_data <- filtered_low_roi_data %>%
  mutate(weight = total_with_hazard_mitigation / sum(total_with_hazard_mitigation, na.rm = TRUE))

filtered_high_roi_data <- filtered_high_roi_data %>%
  mutate(weight = total_with_hazard_mitigation / sum(total_with_hazard_mitigation, na.rm = TRUE))

# Calculate weighted mean of Benefit-Cost Ratio (BCR) for low and high ROI groups
weighted_mean_low_bcr <- sum(filtered_low_roi_data$benefitCostRatio * filtered_low_roi_data$weight, na.rm = TRUE)
weighted_mean_high_bcr <- sum(filtered_high_roi_data$benefitCostRatio * filtered_high_roi_data$weight, na.rm = TRUE)

# Create data frame for plotting
weighted_roi_means <- data.frame(
  ROI_Group = c("Low ROI", "High ROI"),
  Weighted_Mean_BCR = c(weighted_mean_low_bcr, weighted_mean_high_bcr)
)

# Plot the weighted mean BCR for low and high ROI groups
ggplot(weighted_roi_means, aes(x = ROI_Group, y = Weighted_Mean_BCR, fill = ROI_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Weighted Mean Benefit-Cost Ratio by ROI Group",
       x = "ROI Group",
       y = "Weighted Mean Benefit-Cost Ratio") +
  scale_fill_manual(values = c("Low ROI" = "red", "High ROI" = "blue")) +
  theme_minimal()

```

```{r}
ggplot() +
  geom_violin(data = filtered_low_roi_data, aes(x = "Low ROI", y = percent_mitigation, fill = "Low ROI", weight = weight), alpha = 0.7) +
  geom_violin(data = filtered_high_roi_data, aes(x = "High ROI", y = percent_mitigation, fill = "High ROI", weight = weight), alpha = 0.7) +
  labs(title = "Weighted Distribution of Percent Mitigation by ROI Group",
       x = "ROI Group",
       y = "Percent Mitigation") +
  scale_fill_manual(values = c("Low ROI" = "red", "High ROI" = "blue")) +
  theme_minimal()
```

```{r}
ggplot() +
  geom_histogram(data = filtered_low_roi_data, aes(x = percent_mitigation, fill = "Low ROI", weight = weight), bins = 20, alpha = 0.5) +
  geom_histogram(data = filtered_high_roi_data, aes(x = percent_mitigation, fill = "High ROI", weight = weight), bins = 20, alpha = 0.5) +
  labs(title = "Weighted Distribution of Percent Mitigation by ROI Group",
       x = "Percent Mitigation",
       y = "Weighted Frequency") +
  scale_fill_manual(values = c("Low ROI" = "red", "High ROI" = "blue")) +
  theme_minimal()
```

```{r}
# Calculate the average percent_mitigation by state and count the number of 1's for each disaster type
state_avg_mitigation <- mitigation_pct_dataset_clean %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean(percent_mitigation, na.rm = TRUE),
    severe_storms_count = sum(severe_storms, na.rm = TRUE),
    snow_storms_count = sum(snow_storms, na.rm = TRUE),
    winter_storms_count = sum(winter_storms, na.rm = TRUE),
    straight_line_winds_count = sum(straight_line_winds, na.rm = TRUE),
    flooding_count = sum(flooding, na.rm = TRUE),
    tornadoes_count = sum(tornadoes, na.rm = TRUE),
    landslides_or_mudslides_count = sum(landslides_or_mudslides, na.rm = TRUE),
    tsunamis_count = sum(tsunamis, na.rm = TRUE),
    tropical_storm_count = sum(tropical_storm, na.rm = TRUE),
    hurricane_count = sum(hurricane, na.rm = TRUE),
    earthquake_count = sum(earthquake, na.rm = TRUE),
    typhoons_count = sum(typhoons, na.rm = TRUE),
    wildfires_count = sum(wildfires, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_percent_mitigation))  # Sort from highest to lowest

# View the results
print(state_avg_mitigation)
```

```{r}
# Join state_avg_mitigation with state_bcr_summary
state_summary_joined <- state_avg_mitigation %>%
  inner_join(state_bcr_summary, by = "state") %>%
  mutate(
    roi_category = case_when(
      state %in% high_roi_states ~ "High ROI",
      state %in% low_roi_states ~ "Low ROI",
      TRUE ~ "Other"
    )
  )
# View the merged dataset
print(state_summary_joined)

```

```{r}
ggplot(state_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +
  labs(
    x = "Average % of Disaster Cost Spent on Mitigation",
    y = "Weighted Average Benefit-Cost Ratio (BCR)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State"
  ) +
  theme_minimal()
```

```{r}
# Tornadoes BCR Summary
state_bcr_tornadoes <- bcr_dataset_filtered %>%
  filter(tornadoes == 1 & !is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))

# Straight-Line Winds BCR Summary
state_bcr_winds <- bcr_dataset_filtered %>%
  filter(straight_line_winds == 1 & !is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))

# Winter Storms BCR Summary
state_bcr_winter_storms <- bcr_dataset_filtered %>%
  filter(winter_storms == 1 & !is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))

```

```{r}
# Tornadoes Summary: Calculate percentage of cost allocated to mitigation
tornado_summary <- mitigation_pct_dataset %>%
  filter(tornadoes == 1) %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean((hazard_mitigation_assistance / total_with_hazard_mitigation) * 100, na.rm = TRUE)
  ) %>%
  mutate(disaster_type = "tornadoes")

# Straight-Line Winds Summary: Calculate percentage of cost allocated to mitigation
straight_line_winds_summary <- mitigation_pct_dataset %>%
  filter(straight_line_winds == 1) %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean((hazard_mitigation_assistance / total_with_hazard_mitigation) * 100, na.rm = TRUE)
  ) %>%
  mutate(disaster_type = "straight_line_winds")

# Winter Storms Summary: Calculate percentage of cost allocated to mitigation
winter_storms_summary <- mitigation_pct_dataset %>%
  filter(winter_storms == 1) %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean((hazard_mitigation_assistance / total_with_hazard_mitigation) * 100, na.rm = TRUE)
  ) %>%
  mutate(disaster_type = "winter_storms")

```

```{r}
# Join tornado summary with BCR for tornadoes
state_tornado_summary_joined <- left_join(
  tornado_summary %>% select(state, avg_percent_mitigation),
  state_bcr_tornadoes %>% select(state, weighted_avg_bcr),
  by = "state"
) %>%
  mutate(roi_category = case_when(
    state %in% high_roi_states ~ "High ROI",
    state %in% low_roi_states ~ "Low ROI",
    TRUE ~ "Other"
  ))

# Join straight line winds summary with BCR for straight-line winds
state_winds_summary_joined <- left_join(
  straight_line_winds_summary %>% select(state, avg_percent_mitigation),
  state_bcr_winds %>% select(state, weighted_avg_bcr),
  by = "state"
) %>%
  mutate(roi_category = case_when(
    state %in% high_roi_states ~ "High ROI",
    state %in% low_roi_states ~ "Low ROI",
    TRUE ~ "Other"
  ))

# Join winter storm summary with BCR for winter storms
state_winter_storm_summary_joined <- left_join(
  winter_storms_summary %>% select(state, avg_percent_mitigation),
  state_bcr_winter_storms %>% select(state, weighted_avg_bcr),
  by = "state"
) %>%
  mutate(roi_category = case_when(
    state %in% high_roi_states ~ "High ROI",
    state %in% low_roi_states ~ "Low ROI",
    TRUE ~ "Other"
  ))
```

```{r}
ggplot(state_tornado_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +

  labs(
    x = "Average % of Disaster Cost Spent on Mitigation (Tornadoes Only)",
    y = "Weighted Average Benefit-Cost Ratio (Tornadoes Only)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State (Tornadoes Only)"
  ) +
  theme_minimal()

```

```{r}
ggplot(state_winds_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +
  
  labs(
    x = "Average % of Disaster Cost Spent on Mitigation (Straight-Line Winds Only)",
    y = "Weighted Average Benefit-Cost Ratio (Straight-Line Winds Only)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State (Straight-Line Winds Only)"
  ) +
  theme_minimal()

```

```{r}
ggplot(state_winter_storm_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +
  
  labs(
    x = "Average % of Disaster Cost Spent on Mitigation (Winter Storms Only)",
    y = "Weighted Average Benefit-Cost Ratio (Winter Storms Only)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State (Winter Storms Only)"
  ) +
  theme_minimal()

```

```{r}
# Flooding BCR Summary
state_bcr_flooding <- bcr_dataset_filtered %>%
  filter(flooding == 1 & !is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))
```

```{r}
# Tropical Storms BCR Summary
state_bcr_tropical_storm <- bcr_dataset_filtered %>%
  filter(tropical_storm == 1 & !is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))
```

```{r}
# Severe Storms BCR Summary
state_bcr_severe_storms <- bcr_dataset_filtered %>%
  filter(severe_storms == 1 & !is.na(benefitCostRatio) & !is.na(projectAmount)) %>%
  separate_rows(disaster_list, sep = ", ") %>%
  group_by(state) %>%
  summarize(
    weighted_avg_bcr = sum(benefitCostRatio * projectAmount) / sum(projectAmount),
    total_spent = sum(projectAmount),
    count = n()
  ) %>%
  arrange(desc(weighted_avg_bcr))
```

```{r}
# Flooding Summary: Calculate percentage of cost allocated to mitigation
flooding_summary <- mitigation_pct_dataset %>%
  filter(flooding == 1) %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean((hazard_mitigation_assistance / total_with_hazard_mitigation) * 100, na.rm = TRUE)
  ) %>%
  mutate(disaster_type = "flooding")
```

```{r}
# Tropical Storm Summary: Calculate percentage of cost allocated to mitigation
tropical_storm_summary <- mitigation_pct_dataset %>%
  filter(tropical_storm == 1) %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean((hazard_mitigation_assistance / total_with_hazard_mitigation) * 100, na.rm = TRUE)
  ) %>%
  mutate(disaster_type = "tropical_storm")
```

```{r}
# Severe Storms Summary: Calculate percentage of cost allocated to mitigation
severe_storm_summary <- mitigation_pct_dataset %>%
  filter(severe_storms == 1) %>%
  group_by(state) %>%
  summarize(
    avg_percent_mitigation = mean((hazard_mitigation_assistance / total_with_hazard_mitigation) * 100, na.rm = TRUE)
  ) %>%
  mutate(disaster_type = "severe_storms")
```

```{r}
# Join flooding summary with BCR for flooding
state_flooding_summary_joined <- left_join(
  flooding_summary %>% select(state, avg_percent_mitigation),
  state_bcr_flooding %>% select(state, weighted_avg_bcr),
  by = "state"
) %>%
  mutate(roi_category = case_when(
    state %in% high_roi_states ~ "High ROI",
    state %in% low_roi_states ~ "Low ROI",
    TRUE ~ "Other"
  ))
```

```{r}
# Join tropical storm summary with BCR for tropical storms
state_tropical_storm_summary_joined <- left_join(
  tropical_storm_summary %>% select(state, avg_percent_mitigation),
  state_bcr_tropical_storm %>% select(state, weighted_avg_bcr),
  by = "state"
) %>%
  mutate(roi_category = case_when(
    state %in% high_roi_states ~ "High ROI",
    state %in% low_roi_states ~ "Low ROI",
    TRUE ~ "Other"
  ))
```

```{r}
# Join severe storms summary with BCR for severe storms
state_severe_storm_summary_joined <- left_join(
  severe_storm_summary %>% select(state, avg_percent_mitigation),
  state_bcr_severe_storms %>% select(state, weighted_avg_bcr),
  by = "state"
) %>%
  mutate(roi_category = case_when(
    state %in% high_roi_states ~ "High ROI",
    state %in% low_roi_states ~ "Low ROI",
    TRUE ~ "Other"
  ))
```

```{r}
ggplot(state_flooding_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +
  
  labs(
    x = "Average % of Disaster Cost Spent on Mitigation (Flooding Only)",
    y = "Weighted Average Benefit-Cost Ratio (Flooding Only)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State (Flooding Only)"
  ) +
  theme_minimal()
```

```{r}
ggplot(state_tropical_storm_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +
  labs(
    x = "Average % of Disaster Cost Spent on Mitigation (Tropical Storms Only)",
    y = "Weighted Average Benefit-Cost Ratio (Tropical Storms Only)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State (Tropical Storms Only)"
  ) +
  theme_minimal()
```

```{r}
ggplot(state_severe_storm_summary_joined, aes(x = avg_percent_mitigation, y = weighted_avg_bcr, label = state)) +
  geom_point(aes(color = roi_category), size = 4, alpha = 0.8) +
  geom_text(vjust = -1, size = 3.5) +
  scale_color_manual(values = c("High ROI" = "darkgreen", "Low ROI" = "red", "Other" = "gray")) +
  
  labs(
    x = "Average % of Disaster Cost Spent on Mitigation (Severe Storms Only)",
    y = "Weighted Average Benefit-Cost Ratio (Severe Storms Only)",
    color = "ROI Category",
    title = "Avg % Mitigation vs Weighted Avg BCR by State (Severe Storms Only)"
  ) +
  theme_minimal()
```

```{r}
# Relevant disasters
disaster_types <- c("landslides_or_mudslides", "straight_line_winds", "tornadoes", "winter_storms")

# Initialize a dataframe to store the results
bcr_percentages_by_state <- data.frame(state = character(), disaster_type = character(), percent_bcr_over_1 = numeric())

# Loop through each disaster type and calculate the percentage of BCR > 1 by state
for (disaster in disaster_types) {
  disaster_data <- bcr_dataset_filtered %>%
    filter(get(disaster) == 1) %>%  # Filter for the current disaster type
    group_by(state) %>%  # Group by state
    mutate(bcr_over_1 = ifelse(benefitCostRatio > 1, 1, 0)) %>%  # Create column for BCR > 1
    summarize(percent_bcr_over_1 = mean(bcr_over_1) * 100)  # Calculate the percentage
  
  # Add the disaster type column to the results
  disaster_data$disaster_type <- disaster
  
  # Store the result
  bcr_percentages_by_state <- rbind(bcr_percentages_by_state, disaster_data)
}

# View the results
print(bcr_percentages_by_state)

```

```{r}
landslide_or_mudslide_bcr_pct <- bcr_percentages_by_state %>%
  filter(disaster_type == "landslides_or_mudslides")

straight_line_winds_bcr_pct <- bcr_percentages_by_state %>%
  filter(disaster_type == "straight_line_winds")

tornadoes_bcr_pct <- bcr_percentages_by_state %>%
  filter(disaster_type == "tornadoes")

winter_storms_bcr_pct <- bcr_percentages_by_state %>%
  filter(disaster_type == "winter_storms")
```

```{r}
# Find the bottom 10 states for each disaster type based on percent_bcr_over_1

# For landslides or mudslides
bottom_10_landslide_or_mudslide <- landslide_or_mudslide_bcr_pct %>%
  arrange(percent_bcr_over_1) %>%  # Sort by percent_bcr_over_1
  head(10)  # Get the bottom 10

# For straight line winds
bottom_10_straight_line_winds <- straight_line_winds_bcr_pct %>%
  arrange(percent_bcr_over_1) %>%  # Sort by percent_bcr_over_1
  head(10)  # Get the bottom 10

# For tornadoes
bottom_10_tornadoes <- tornadoes_bcr_pct %>%
  arrange(percent_bcr_over_1) %>%  # Sort by percent_bcr_over_1
  head(10)  # Get the bottom 10

# For Winter Storms
bottom_10_winter_storms <- winter_storms_bcr_pct %>%
  arrange(percent_bcr_over_1) %>%  # Sort by percent_bcr_over_1
  head(10)  # Get the bottom 10
```

```{r}
# Add a new column to mark if the state is a low ROI state
bottom_10_landslide_or_mudslide$roi_status <- ifelse(bottom_10_landslide_or_mudslide$state %in% low_roi_states, "Low ROI", "Other")

# Visualize the bottom 10 states for Landslides or Mudslides with low ROI states highlighted
ggplot(bottom_10_landslide_or_mudslide, aes(x = reorder(state, percent_bcr_over_1), y = percent_bcr_over_1, fill = roi_status)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +  # Flip the axes for better readability
  labs(
    x = "State",
    y = "Percent BCR Over 1",
    title = "Bottom 10 States with Lowest Percent BCR Over 1 - Landslides or Mudslides"
  ) +
  scale_fill_manual(values = c("Low ROI" = "red", "Other" = "skyblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10)
  )
```

```{r}
# Add a new column to mark if the state is a low ROI state
bottom_10_winter_storms$roi_status <- ifelse(bottom_10_winter_storms$state %in% low_roi_states, "Low ROI", "Other")

# Visualize the bottom 10 states for Landslides or Mudslides with low ROI states highlighted
ggplot(bottom_10_winter_storms, aes(x = reorder(state, percent_bcr_over_1), y = percent_bcr_over_1, fill = roi_status)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +  # Flip the axes for better readability
  labs(
    x = "State",
    y = "Percent BCR Over 1",
    title = "Bottom 10 States with Lowest Percent BCR Over 1 - Winter Storms"
  ) +
  scale_fill_manual(values = c("Low ROI" = "red", "Other" = "skyblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10)
  )
```

```{r}
# Add a new column to mark if the state is a low ROI state for Straight-line Winds
bottom_10_straight_line_winds$roi_status <- ifelse(bottom_10_straight_line_winds$state %in% low_roi_states, "Low ROI", "Other")

# Visualize the bottom 10 states for Straight-line Winds with low ROI states highlighted
ggplot(bottom_10_straight_line_winds, aes(x = reorder(state, percent_bcr_over_1), y = percent_bcr_over_1, fill = roi_status)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +  # Flip the axes for better readability
  labs(
    x = "State",
    y = "Percent BCR Over 1",
    title = "Bottom 10 States with Lowest Percent BCR Over 1 - Straight-line Winds"
  ) +
  scale_fill_manual(values = c("Low ROI" = "red", "Other" = "skyblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10)
  )
```

```{r}
# Add a new column to mark if the state is a low ROI state for Tornadoes
bottom_10_tornadoes$roi_status <- ifelse(bottom_10_tornadoes$state %in% low_roi_states, "Low ROI", "Other")

# Visualize the bottom 10 states for Tornadoes with low ROI states highlighted
ggplot(bottom_10_tornadoes, aes(x = reorder(state, percent_bcr_over_1), y = percent_bcr_over_1, fill = roi_status)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +  # Flip the axes for better readability
  labs(
    x = "State",
    y = "Percent BCR Over 1",
    title = "Bottom 10 States with Lowest Percent BCR Over 1 - Tornadoes"
  ) +
  scale_fill_manual(values = c("Low ROI" = "red", "Other" = "skyblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10)
  )

```

```{r}
mod1 <- lm(total_with_hazard_mitigation ~ estimated_total_assistance_cost - 1, data = mitigation_pct_dataset)

summary(mod1)

```

```{r}
mod2 <- lm(benefitCostRatio ~ projectAmount + number_of_disasters + incident_duration_days + severe_storms + snow_storms + winter_storms + straight_line_winds + flooding + tornadoes + landslides_or_mudslides + tsunamis + tropical_storm + hurricane + earthquake, data = bcr_dataset_filtered)

summary(mod2)
```

```{r}
mod2_tidy <- broom::tidy(mod2) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
    "projectAmount" = "Project Amount",
    "number_of_disasters" = "Number of Disasters",
    "incident_duration_days" = "Incident Duration (Days)",
    "severe_storms" = "Severe Storms",
    "snow_storms" = "Snow Storms",
    "winter_storms" = "Winter Storms",
    "straight_line_winds" = "Straight-Line Winds",
    "flooding" = "Flooding",
    "tornadoes" = "Tornadoes",
    "landslides_or_mudslides" = "Landslides/Mudslides",
    "tsunamis" = "Tsunamis",
    "tropical_storm" = "Tropical Storm",
    "hurricane" = "Hurricane",
    "earthquake" = "Earthquake"
  ))
```

```{r}
ggplot(mod2_tidy, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  coord_flip() +
  labs(
    title = "Effect of Different Factors on FEMA's Benefit-Cost Ratio",
    x = "Predictor",
    y = "Estimated Change in BCR"
  ) +
  theme_minimal()
```

