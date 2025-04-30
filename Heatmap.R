# Set up stuff
packages <- c("dplyr", "lubridate", "ggplot2", "scales", "tidyr", "readxl", "stats")
lapply(packages, library, character.only = TRUE)

file_path <- file.choose()
raw_data <- read_excel(file_path)

clean_data <- raw_data %>% 
  select(Regimen_Medications, Allergy, IncisionTime)

# Compliance flag
df_clean <- clean_data %>%
  mutate(Compliance = ifelse(Regimen_Medications == "Other", "Noncompliant", "Compliant"))

# Define the list of Penicillin-related drugs
penicillin_drugs <- c("AMOXICILLIN", "AMOXICILLIN (BULK)", "AMOXICILLIN TRIHYDRATE",
                       "AMOXICILLIN-POT CLAVULANATE", "AMPICILLIN", "AMPICILLIN-SULBACTAM",
                       "CARBENICILLIN INDANYL SODIUM", "DICLOXACILLIN", "DICLOXACILLIN SODIUM",
                       "FLOXACILLIN", "NAFCILLIN", "OXACILLIN", "PENICILLIN", "PENICILLIN G",
                       "PENICILLIN G BENZATHIN,PROCAIN", "PENICILLIN G BENZATHINE", "PENICILLIN G POTASSIUM",
                       "PENICILLIN G PROCAINE", "PENICILLIN G SODIUM", "PENICILLIN V", "PENICILLIN V POTASSIUM",
                       "PENICILLINS", "PIPERACILLIN", "PIPERACILLIN-TAZOBACTAM", "PIPERACILLIN-TAZOBACTAM-DEXTRS",
                       "POLYCILLIN", "TALAMPICILLIN", "TICARCILLIN-CLAVULANATE")

# AllergyFlag column
df_clean <- df_clean %>%
  mutate(AllergyFlag = ifelse(Allergy %in% penicillin_drugs, "Penicillin Allergy", "No Penicillin Allergy"))

# Step 3: Create Time Period based on IncisionTime (Pre-Intervention, Intervention 1, 2, 3)
df_clean <- df_clean %>%
  mutate(TimePeriod = case_when(
    IncisionTime >= as.Date("2023-01-01") & IncisionTime <= as.Date("2023-06-30") ~ "Pre-intervention",          # Pre-intervention (Jan-Jun 2023)
    IncisionTime >= as.Date("2023-07-01") & IncisionTime <= as.Date("2024-06-30") ~ "Intervention 1", # Intervention 1 (Jul 2023 - Jun 2024)
    IncisionTime >= as.Date("2024-07-01") & IncisionTime <= as.Date("2024-09-30") ~ "Intervention 2", # Intervention 2 (Jul 2024 - Sep 2024)
    IncisionTime >= as.Date("2024-10-01") & IncisionTime <= as.Date("2025-03-31") ~ "Intervention 3",          # Intervention 3 (Oct 2024 - Mar 2025)
    TRUE ~ "Other"
  ))

# Remove "Other" (N/A) time period
df_clean <- df_clean %>%
  filter(TimePeriod != "Other")

# Summary for compliance rates per TimePeriod and AllergyFlag
df_summary <- df_clean %>%
  group_by(TimePeriod, AllergyFlag, Compliance) %>%
  summarise(Count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = c(Compliance, AllergyFlag), values_from = Count, values_fill = 0)

# Total and Compliance Rate columns
df_summary <- df_summary %>%
  mutate(
    Total = `Compliant_No Penicillin Allergy` + `Noncompliant_No Penicillin Allergy` + 
            `Compliant_Penicillin Allergy` + `Noncompliant_Penicillin Allergy`,
    ComplianceRate = (`Compliant_No Penicillin Allergy` + `Compliant_Penicillin Allergy`) / Total,
    PenicillinComplianceRate = `Compliant_Penicillin Allergy` / (`Compliant_Penicillin Allergy` + `Noncompliant_Penicillin Allergy`)
  )

# Ratios
df_summary <- df_summary %>%
  mutate(
    ComplianceNumerator = `Compliant_No Penicillin Allergy` + `Compliant_Penicillin Allergy`,
    ComplianceDenominator = ComplianceNumerator + `Noncompliant_No Penicillin Allergy` + `Noncompliant_Penicillin Allergy`,
    ComplianceRatio = paste0(ComplianceNumerator, "/", ComplianceDenominator),
    PenicillinComplianceRatio = paste0(`Compliant_Penicillin Allergy`, "/", 
                                       `Compliant_Penicillin Allergy` + `Noncompliant_Penicillin Allergy`)
  )

# Final summary of Total Compliance and Penicillin Allergy Compliance
df_summary_compliance <- df_summary %>%
  select(TimePeriod, ComplianceRate, PenicillinComplianceRate, ComplianceRatio, PenicillinComplianceRatio, Total)
df_summary_compliance$TimePeriod <- factor(df_summary_compliance$TimePeriod,
                                           levels = c("Pre-intervention", "Intervention 1", "Intervention 2", "Intervention 3"))

# Function to calculate p-value for proportions compared to pre-intervention
calculate_p_value <- function(df_clean, period, allergy_type) {
  # Filter data
  pre_intervention <- df_clean %>% 
    filter(TimePeriod == "Pre-intervention")
  
  if (allergy_type == "All") {
    # All patients compliance
    pre_compliant <- sum(pre_intervention$Compliance == "Compliant")
    pre_total <- nrow(pre_intervention)
    
    period_data <- df_clean %>% filter(TimePeriod == period)
    period_compliant <- sum(period_data$Compliance == "Compliant")
    period_total <- nrow(period_data)
  } else {
    # Penicillin allergy patients compliance
    pre_intervention <- pre_intervention %>% filter(AllergyFlag == "Penicillin Allergy")
    pre_compliant <- sum(pre_intervention$Compliance == "Compliant")
    pre_total <- nrow(pre_intervention)
    
    period_data <- df_clean %>% 
      filter(TimePeriod == period, AllergyFlag == "Penicillin Allergy")
    period_compliant <- sum(period_data$Compliance == "Compliant")
    period_total <- nrow(period_data)
  }
  
  # Double checking data
  if (pre_total > 0 && period_total > 0) {
    # Create a 2x2 contingency table
    cont_table <- matrix(c(
      pre_compliant, pre_total - pre_compliant,
      period_compliant, period_total - period_compliant
    ), nrow = 2, byrow = TRUE)
    
    # Perform Fisher's exact test if needed
    test_result <- fisher.test(cont_table)
    return(test_result$p.value)
  } else {
    return(NA)  # Return NA if we don't have sufficient data
  }
}

# Calculate p-values for all combinations
periods <- c("Intervention 1", "Intervention 2", "Intervention 3")
p_values_all <- sapply(periods, function(p) calculate_p_value(df_clean, p, "All"))
p_values_pen <- sapply(periods, function(p) calculate_p_value(df_clean, p, "Penicillin"))

# Create p-value dataframes - simplified to just mark if p < 0.05, p < 0.01, and p < 0.001
p_values_df_all <- data.frame(
  TimePeriod = periods,
  p_value = p_values_all,
  sig_symbol = ifelse(p_values_all < 0.001, "**", 
                      ifelse(p_values_all < 0.01, "*", ""))
)

p_values_df_pen <- data.frame(
  TimePeriod = periods,
  p_value = p_values_pen,
  sig_symbol = ifelse(p_values_pen < 0.001, "**", 
                      ifelse(p_values_pen < 0.01, "*", ""))
)

# Add Pre-intervention 
p_values_df_all <- rbind(data.frame(TimePeriod = "Pre-intervention", p_value = NA, sig_symbol = ""), p_values_df_all)
p_values_df_pen <- rbind(data.frame(TimePeriod = "Pre-intervention", p_value = NA, sig_symbol = ""), p_values_df_pen)

# Convert to factors with the same levels
p_values_df_all$TimePeriod <- factor(p_values_df_all$TimePeriod, levels = levels(df_summary_compliance$TimePeriod))
p_values_df_pen$TimePeriod <- factor(p_values_df_pen$TimePeriod, levels = levels(df_summary_compliance$TimePeriod))

# Join p-values with the summary data
df_summary_compliance <- df_summary_compliance %>%
  left_join(p_values_df_all %>% select(TimePeriod, sig_symbol_all = sig_symbol), by = "TimePeriod") %>%
  left_join(p_values_df_pen %>% select(TimePeriod, sig_symbol_pen = sig_symbol), by = "TimePeriod")

# Create a heatmap visualization
ggplot(df_summary_compliance, aes(x = TimePeriod)) +
  geom_tile(aes(y = 2, fill = ComplianceRate), color = "white") +  
  geom_tile(aes(y = 1, fill = PenicillinComplianceRate), color = "white") + 
  scale_fill_gradientn(
    colours = c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
    limits = c(0.7, 1.0),
    labels = scales::percent_format(accuracy = 1),
    name = "Compliance Rate"
  ) +
  labs(
    title = "Surgical Prophylaxis Antibiotic Compliance Over Time",
    x = " ",
    y = " ",
    caption = "* p<0.001, ** p<0.01 (compared to pre-intervention)"  
  ) +
  scale_y_continuous(
    breaks = c(2, 1),  
    labels = c("All Patients", "Patients with Penicillin Allergy") 
  ) +
  # Centered percentage and ratio text
  geom_text(aes(
    y = 2,  # Flipped y-axis values
    label = paste0(scales::percent(ComplianceRate, accuracy = 1), "\n(", ComplianceRatio, ")")
  ),
  color = "black", size = 4.5) +  
  geom_text(aes(
    y = 1,  # Flipped y-axis values
    label = paste0(scales::percent(PenicillinComplianceRate, accuracy = 1), "\n(", PenicillinComplianceRatio, ")")
  ),
  color = "black", size = 4.5) + 
  
  geom_text(aes(
    y = 2,  # Flipped y-axis values
    x = as.numeric(TimePeriod) + 0.35,  
    label = sig_symbol_all
  ),
  color = "black", size = 6, hjust = 0, vjust = 0) +  
  geom_text(aes(
    y = 1,  
    x = as.numeric(TimePeriod) + 0.35,  
    label = sig_symbol_pen
  ),
  color = "black", size = 6, hjust = 0, vjust = 0) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold", margin = margin(t = 0)),  
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 5)),  
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 15))  
  )
