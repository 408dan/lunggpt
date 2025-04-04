library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(broom)

# Load the data

df <- read.csv("~/Desktop/gpt_billing/result.csv")
# Extract "with punishment" accuracy
df_punishment <- df %>%
  select(model_name, ICD_accuracy_1, CPT_accuracy_1)

# Convert "no punishment" accuracy into long format
icd_no_punish <- df %>%
  select(model_name, icdo1, icdo2, icdo3, icdo4, icdo5) %>%
  pivot_longer(cols = starts_with("icdo"), names_to = "trial", values_to = "ICD")

cpt_no_punish <- df %>%
  select(model_name, cpto1, cpto2, cpto3, cpto4, cpto5) %>%
  pivot_longer(cols = starts_with("cpto"), names_to = "trial", values_to = "CPT")

# Function to compute summary statistics
compute_summary <- function(data, metric) {
  data %>%
    group_by(model_name) %>%
    summarise(
      Mean = mean(.data[[metric]], na.rm = TRUE),
      StdDev = sd(.data[[metric]], na.rm = TRUE),
      N = n(),
      CI_Lower = Mean - qt(0.975, df = N - 1) * (StdDev / sqrt(N)),
      CI_Upper = Mean + qt(0.975, df = N - 1) * (StdDev / sqrt(N)),
      CV = StdDev / Mean
    ) %>%
    ungroup()
}

# Compute summary statistics
summary_punishment_icd <- compute_summary(df_punishment, "ICD_accuracy_1")
summary_punishment_cpt <- compute_summary(df_punishment, "CPT_accuracy_1")
summary_no_punishment_icd <- compute_summary(icd_no_punish, "ICD")
summary_no_punishment_cpt <- compute_summary(cpt_no_punish, "CPT")

# Function to compute p-values
compute_p_values <- function(data, metric) {
  gemini_values <- data %>% filter(model_name == "gemini") %>% pull(.data[[metric]])
  
  p_values <- data %>%
    filter(model_name != "gemini") %>%
    group_by(model_name) %>%
    summarise(
      p_value = t.test(.data[[metric]], gemini_values, var.equal = FALSE)$p.value
    ) %>%
    ungroup()
  
  return(p_values)
}

# Compute p-values for both accuracy methods
p_values_punishment_icd <- compute_p_values(df_punishment, "ICD_accuracy_1")
p_values_punishment_cpt <- compute_p_values(df_punishment, "CPT_accuracy_1")
p_values_no_punishment_icd <- compute_p_values(icd_no_punish, "ICD")
p_values_no_punishment_cpt <- compute_p_values(cpt_no_punish, "CPT")

# Print results
print("Summary Statistics - With Punishment (ICD)")
print(summary_punishment_icd)
print("Summary Statistics - With Punishment (CPT)")
print(summary_punishment_cpt)
print("Summary Statistics - No Punishment (ICD)")
print(summary_no_punishment_icd)
print("Summary Statistics - No Punishment (CPT)")
print(summary_no_punishment_cpt)

print("P-Values - With Punishment (ICD)")
print(p_values_punishment_icd)
print("P-Values - With Punishment (CPT)")
print(p_values_punishment_cpt)
print("P-Values - No Punishment (ICD)")
print(p_values_no_punishment_icd)
print("P-Values - No Punishment (CPT)")
print(p_values_no_punishment_cpt)
















################################################################################
################################################################################
#CASE-BASED ANALYSIS#####
############################################################################
################################################################################
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Load the dataset
file_path <- "~/Desktop/gpt_billing/result.csv"  # Update with your actual file path
df <- read_csv(file_path)

# Convert 'model_name' to a factor
df$model_name <- as.factor(df$model_name)

# Reshape the data to long format for ANOVA
df_long <- df %>%
  pivot_longer(cols = starts_with("ICD_accuracy_"), names_to = "trial", values_to = "ICD_accuracy") %>%
  pivot_longer(cols = starts_with("CPT_accuracy_"), names_to = "trial_cpt", values_to = "CPT_accuracy")

# Remove unnecessary 'trial' columns (to avoid duplication)
df_long <- df_long %>% select(-trial_cpt)

# Loop through each case (Case 1 to Case 8) and run ANOVA
for (case_id in unique(df_long$case_scenario)) {
  
  # Filter data for the current case
  case_data <- df_long %>% filter(case_scenario == case_id)
  
  # Run ANOVA for ICD Accuracy using individual trials
  icd_anova <- aov(ICD_accuracy ~ model_name, data = case_data)
  icd_summary <- summary(icd_anova)
  
  # Run ANOVA for CPT Accuracy using individual trials
  cpt_anova <- aov(CPT_accuracy ~ model_name, data = case_data)
  cpt_summary <- summary(cpt_anova)
  
  # Print results
  cat("\n====================\n")
  cat("Case:", case_id, "\n")
  cat("====================\n")
  
  cat("\nANOVA for ICD Accuracy:\n")
  print(icd_summary)  # Now includes F-statistic and p-value
  
  cat("\nANOVA for CPT Accuracy:\n")
  print(cpt_summary)  # Now includes F-statistic and p-value
}


################################################################################
################################################################################
# Load the dataset
file_path <- "~/Desktop/gpt_billing/result.csv"  # Update with your actual file path
df <- read_csv(file_path)

# Convert 'model_name' to a factor
df$model_name <- as.factor(df$model_name)

# Reshape the data to long format for ICD and CPT accuracy (only for individual accuracy columns)
df_long <- df %>%
  pivot_longer(cols = starts_with("icdo"), names_to = "trial", values_to = "ICD_accuracy") %>%
  pivot_longer(cols = starts_with("cpto"), names_to = "trial_cpt", values_to = "CPT_accuracy")

# Remove unnecessary 'trial_cpt' column (to avoid duplication)
df_long <- df_long %>% select(-trial_cpt)

# Loop through each case (Case 1 to Case 8) and run ANOVA
for (case_id in unique(df_long$case_scenario)) {
  
  # Filter data for the current case
  case_data <- df_long %>% filter(case_scenario == case_id)
  
  # Run ANOVA for ICD Accuracy using individual trials
  icd_anova <- aov(ICD_accuracy ~ model_name, data = case_data)
  icd_summary <- summary(icd_anova)
  
  # Run Tukey post-hoc test for ICD Accuracy if ANOVA is significant, excluding Case 3
  if (case_id != 3 && icd_summary[[1]]$`Pr(>F)`[1] < 0.05) {
    icd_tukey <- TukeyHSD(icd_anova)
  } else {
    icd_tukey <- "No significant differences found or excluded Case 3, Tukey test skipped"
  }
  
  # Run ANOVA for CPT Accuracy using individual trials
  cpt_anova <- aov(CPT_accuracy ~ model_name, data = case_data)
  cpt_summary <- summary(cpt_anova)
  
  # Run Tukey post-hoc test for CPT Accuracy if ANOVA is significant, excluding Case 3
  if (case_id != 3 && cpt_summary[[1]]$`Pr(>F)`[1] < 0.05) {
    cpt_tukey <- TukeyHSD(cpt_anova)
  } else {
    cpt_tukey <- "No significant differences found or excluded Case 3, Tukey test skipped"
  }
  
  # Print results
  cat("\n====================\n")
  cat("Case:", case_id, "\n")
  cat("====================\n")
  
  cat("\nANOVA for ICD Accuracy:\n")
  print(icd_summary)  # Now includes F-statistic and p-value
  
  cat("\nTukey Post-Hoc for ICD Accuracy:\n")
  print(icd_tukey)  # Directly print the Tukey results
  
  cat("\nANOVA for CPT Accuracy:\n")
  print(cpt_summary)  # Now includes F-statistic and p-value
  
  cat("\nTukey Post-Hoc for CPT Accuracy:\n")
  print(cpt_tukey)  # Directly print the Tukey results
}

