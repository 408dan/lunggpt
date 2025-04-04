# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(boot)

# Load the dataset
file_path <- "~/Desktop/lung_cancer/results/dataset_updated.csv"  # Change this to the correct file path
df <- read.csv(file_path)

# Define LLM columns
llm_columns <- list(
  "ChatGPT-o3" = c("gpt_grade_ba", "gpt_grade_hc", "gpt_grade_ch"),
  "DeepSeek R1" = c("deepseek_grade_ba", "deepseek_grade_hc", "deepseek_grade_ch"),
  "Perplexity" = c("perplexity_grade_ba", "perplexity_grade_hc", "perplexity_grade_ch"),
  "Claude" = c("claude_grade_ba", "claude_grade_hc", "claude_grade_ch"),
  "Gemini" = c("gemini_grade_ba", "gemini_grade_hc", "gemini_grade_ch")
)

# Response categories
response_labels <- c("Correct", "Partially Correct", "Incorrect", "No Answer")
response_values <- c(1, 0.5, 0, 3)

# Function to calculate bootstrapped confidence intervals
bootstrap_ci <- function(data, value, n_boot = 1000, conf = 0.95) {
  boot_fn <- function(data, indices) {
    sampled_data <- data[indices]
    mean(sampled_data == value)
  }
  
  boot_res <- boot(data, statistic = boot_fn, R = n_boot)
  ci <- boot.ci(boot_res, type = "perc", conf = conf)$percent[4:5]
  
  return(ci * 100)  # Convert to percentage
}

# Calculate percentage distributions with confidence intervals
results <- data.frame()

for (llm in names(llm_columns)) {
  data <- df %>% select(all_of(llm_columns[[llm]])) %>% unlist()
  total <- length(data)
  
  for (i in seq_along(response_values)) {
    percentage <- sum(data == response_values[i]) / total * 100
    ci <- bootstrap_ci(data, response_values[i])  # Compute CI
    results <- rbind(results, data.frame(
      LLM = llm, 
      Response = response_labels[i], 
      Percentage = percentage, 
      CI_Lower = ci[1], 
      CI_Upper = ci[2]
    ))
  }
}

# Convert Response to a factor with correct order
results$Response <- factor(results$Response, levels = response_labels)

# Plot the grouped bar chart with confidence intervals
ggplot(results, aes(x = LLM, y = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.8), width = 0.2, size = 0.7) +
  labs(title = "Percent Distribution of LLM Outputs with Confidence Intervals",
       x = "LLM Model",
       y = "Percentage (%)",
       fill = "Response Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_fill_manual(values = c("Correct" = "#131799", 
                               "Partially Correct" = "#2CB5AC", 
                               "Incorrect" = "#9180F4", 
                               "No Answer" = "#e7298a"))





##PRINTING VALUES
# Load necessary libraries
library(dplyr)
library(boot)

# Load your dataset
file_path <- "~/Desktop/lung_cancer/results/dataset.csv"
df <- read.csv(file_path)

# Define LLM columns
llm_columns <- list(
  "ChatGPT-o3" = c("gpt_grade_ba", "gpt_grade_hc", "gpt_grade_ch"),
  "DeepSeek R3" = c("deepseek_grade_ba", "deepseek_grade_hc", "deepseek_grade_ch"),
  "Perplexity" = c("perplexity_grade_ba", "perplexity_grade_hc", "perplexity_grade_ch"),
  "Claude" = c("claude_grade_ba", "claude_grade_hc", "claude_grade_ch"),
  "Gemini" = c("gemini_grade_ba", "gemini_grade_hc", "gemini_grade_ch")
)

# Response categories
response_labels <- c("Correct", "Partially Correct", "Incorrect", "No Answer")
response_values <- c(1, 0.5, 0, 3)

# Bootstrap CI function
bootstrap_ci <- function(data, value, n_boot = 1000, conf = 0.95) {
  boot_fn <- function(d, i) mean(d[i] == value)
  boot_res <- boot(data, boot_fn, R = n_boot)
  ci <- boot.ci(boot_res, type = "perc", conf = conf)$percent[4:5]
  return(ci * 100)
}

# Calculate and print results
for (llm in names(llm_columns)) {
  cat("\n---", llm, "---\n")
  
  data <- df %>% select(all_of(llm_columns[[llm]])) %>% unlist()
  total <- length(data)
  
  for (i in seq_along(response_values)) {
    value <- response_values[i]
    label <- response_labels[i]
    
    percent <- mean(data == value) * 100
    ci <- bootstrap_ci(data, value)
    
    cat(sprintf("%-18s: %5.2f%% (95%% CI: %.2f%% - %.2f%%)\n",
                label, percent, ci[1], ci[2]))
  }
}








#Table 4 and Table X 
# Load necessary libraries
library(dplyr)
library(DescTools)

# Load dataset
df <- read.csv("~/Desktop/lung_cancer/results/dataset_updated.csv", stringsAsFactors = FALSE) # Change this to the correct file path
# Print column names to verify structure
print(names(df))

# Identify actual columns that contain grades
grade_columns <- grep("grade", names(df), value = TRUE)

# Convert relevant columns to numeric
df[grade_columns] <- lapply(df[grade_columns], as.numeric)

# Function to process each model
process_model <- function(df, model) {
  # Identify columns that belong to the given model
  grade_cols <- grep(paste0(model, "_grade"), names(df), value = TRUE)
  
  # Ensure the columns exist
  if (length(grade_cols) == 0) {
    cat("No grade columns found for", model, "\n")
    return(NULL)
  }
  
  # Remove rows where any of the model’s grade columns contain 3
  df_filtered <- df %>% filter(!apply(select(., all_of(grade_cols)), 1, function(x) any(x == 3, na.rm = TRUE)))
  
  # Count removed rows
  removed_rows <- nrow(df) - nrow(df_filtered)
  cat(model, "- Rows removed:", removed_rows, "\n")
  
  # Create majority vote column
  df_filtered[[paste0(model, "_majority")]] <- apply(df_filtered[, grade_cols], 1, function(x) {
    x <- na.omit(x)  # Remove NA values if present
    unique_vals <- unique(x)  # Get unique values in the row
    
    # If all values are the same, return that value
    if (length(unique_vals) == 1) return(unique_vals[1])
    
    # If two values match, return the majority
    if (sum(x == 1) > sum(x == 0)) return(1)
    else if (sum(x == 0) > sum(x == 1)) return(0)
    
    # If all values are different (0, 0.5, and 1), return 0.5 as ground truth
    return(0.5)
  })
  
  # Calculate proportions
  proportions <- df_filtered %>% 
    summarise(
      correct = mean(get(paste0(model, "_majority")) == 1, na.rm = TRUE),
      partial = mean(get(paste0(model, "_majority")) == 0.5, na.rm = TRUE),
      incorrect = mean(get(paste0(model, "_majority")) == 0, na.rm = TRUE)
    )
  
  # Standard error function
  se <- function(p, n) sqrt((p * (1 - p)) / n)
  
  # Compute standard errors and confidence intervals
  n <- nrow(df_filtered)
  conf_int <- function(p, n) BinomCI(p * n, n, conf.level = 0.95)[, 2:3]
  
  results <- proportions %>%
    mutate(
      se_correct = se(correct, n),
      ci_correct = list(conf_int(correct, n)),
      se_partial = se(partial, n),
      ci_partial = list(conf_int(partial, n)),
      se_incorrect = se(incorrect, n),
      ci_incorrect = list(conf_int(incorrect, n))
    )
  
  return(results)
}

# Run the function for each model
models <- c("gpt", "deepseek", "perplexity", "claude", "gemini")
results_list <- lapply(models, function(model) process_model(df, model))

# Combine results
final_results <- bind_rows(results_list, .id = "model")

# Print final results
print(final_results)



#TABLE X WHERE REFUSAL IS GRADED AS INCORRECT
# Load necessary libraries
library(dplyr)
library(DescTools)

# Load dataset
df <- read.csv("~/Desktop/lung_cancer/results/dataset_updated.csv", 
               stringsAsFactors = FALSE) # Change to the correct file path

# Print column names to verify structure
print(names(df))

# Identify actual columns that contain grades
grade_columns <- grep("grade", names(df), value = TRUE)

# Convert relevant columns to numeric
df[grade_columns] <- lapply(df[grade_columns], as.numeric)

# Function to process each model
process_model <- function(df, model) {
  # Identify columns that belong to the given model
  grade_cols <- grep(paste0(model, "_grade"), names(df), value = TRUE)
  
  # Ensure the columns exist
  if (length(grade_cols) == 0) {
    cat("No grade columns found for", model, "\n")
    return(NULL)
  }
  
  # Make a copy of the dataframe so we don't alter the original
  df_model <- df
  
  # Replace any '3' in the model's grade columns with '0' (incorrect)
  for (col in grade_cols) {
    df_model[[col]][df_model[[col]] == 3] <- 0
  }
  
  # Create majority vote column
  df_model[[paste0(model, "_majority")]] <- apply(df_model[, grade_cols], 1, function(x) {
    x <- na.omit(x)  # Remove NA values if present
    unique_vals <- unique(x)  # Get unique values in the row
    
    # If all values are the same, return that value
    if (length(unique_vals) == 1) return(unique_vals[1])
    
    # If at least two values match, return the "majority"
    if (sum(x == 1) > sum(x == 0)) {
      return(1)  # majority are correct
    } else if (sum(x == 0) > sum(x == 1)) {
      return(0)  # majority are incorrect
    }
    
    # If it's a perfect tie (e.g., one 1 and one 0 and maybe one 0.5),
    # return 0.5 as a "partial credit"
    return(0.5)
  })
  
  # Calculate proportions
  proportions <- df_model %>% 
    summarise(
      correct   = mean(get(paste0(model, "_majority")) == 1,   na.rm = TRUE),
      partial   = mean(get(paste0(model, "_majority")) == 0.5, na.rm = TRUE),
      incorrect = mean(get(paste0(model, "_majority")) == 0,   na.rm = TRUE)
    )
  
  # Standard error function
  se <- function(p, n) sqrt((p * (1 - p)) / n)
  
  # Compute standard errors and confidence intervals
  n <- nrow(df_model)
  
  # Helper for binomial CI
  conf_int <- function(p, n) {
    BinomCI(p * n, n, conf.level = 0.95)[, 2:3]  # returns lower/upper
  }
  
  results <- proportions %>%
    mutate(
      se_correct      = se(correct, n),
      ci_correct      = list(conf_int(correct, n)),
      se_partial      = se(partial, n),
      ci_partial      = list(conf_int(partial, n)),
      se_incorrect    = se(incorrect, n),
      ci_incorrect    = list(conf_int(incorrect, n))
    )
  
  return(results)
}

# List of models
models <- c("gpt", "deepseek", "perplexity", "claude", "gemini")

# Run analysis for each model
results_list <- lapply(models, function(model) process_model(df, model))

# Combine results
final_results <- bind_rows(results_list, .id = "model")

# Print final results
print(final_results)





##THIS IS JUST TO EXPOERT THE MAJORITY VOTES FOR Logistic Regression##
# Load necessary libraries
library(dplyr)
library(DescTools)

# Load dataset
df <- read.csv("~/Desktop/lung_cancer/results/dataset_updated.csv", stringsAsFactors = FALSE) # Change this to the correct file path

# Identify actual columns that contain grades
grade_columns <- grep("grade", names(df), value = TRUE)

# Convert relevant columns to numeric
df[grade_columns] <- lapply(df[grade_columns], as.numeric)

# List of models
models <- c("gpt", "deepseek", "perplexity", "claude", "gemini")

# Create a new data frame for majority votes. You can start from `df`
# or from a subset if you only want certain columns carried forward.
df_majority <- df

# For each model, compute a majority column and attach to df_majority
for (model in models) {
  
  # Identify columns that belong to this model
  grade_cols <- grep(paste0(model, "_grade"), names(df_majority), value = TRUE)
  
  # Compute the majority vote for each row
  df_majority[[paste0(model, "_majority")]] <- apply(df_majority[, grade_cols], 1, function(x) {
    # If any grade == 3, we will return NA (exclude it)
    if (any(x == 3, na.rm = TRUE)) {
      return(NA)
    }
    
    # Remove any NA values among grades
    x <- na.omit(x)
    
    # If all values are the same, return that value
    if (length(unique(x)) == 1) {
      return(unique(x))
    }
    
    # Check which value is majority between 0 and 1 (with possible 0.5's)
    # Count occurrences of 1, 0, 0.5
    num1   <- sum(x == 1)
    num0   <- sum(x == 0)
    num05  <- sum(x == 0.5)
    
    # If two or more are 1, return 1
    if (num1 > num0) {
      return(1)
    } else if (num0 > num1) {
      return(0)
    }
    
    # If they're evenly split or all different, return 0.5
    return(0.5)
  })
}

# Now df_majority includes columns like "gpt_majority", "deepseek_majority", etc.
# Export to CSV
write.csv(df_majority, "majority_votes_table.csv", row.names = FALSE)














# Load necessary libraries
library(dplyr)
library(lme4)
library(broom.mixed)
library(tidyr)

# Load dataset
df <- read.csv("~/Desktop/lung_cancer/results/dataset_updated.csv", stringsAsFactors = FALSE)

# Identify only the LLM `_grade_` columns (ignore `_consistency_` columns, if any)
score_columns <- grep("_grade_", names(df), value = TRUE)

# Convert grades to numeric
df[score_columns] <- lapply(df[score_columns], as.numeric)

# Remove rows where any LLM score is `3` (LLM refused to answer)
df <- df %>%
  filter(
    !apply(select(., all_of(score_columns)), 1, function(x) any(x == 3, na.rm = TRUE))
  )

# Reshape dataset into long format
df_long <- df %>%
  pivot_longer(
    cols = all_of(score_columns),
    names_to = "model_rater",
    values_to = "score"
  ) %>%
  # Remove the "_grade_" substring so we have something like "claude_ba"
  mutate(model_rater = gsub("_grade_", "_", model_rater, fixed = TRUE))

# Extract rater from the piece after the last underscore
# e.g. "claude_ba" -> rater = "ba", model = "claude"
df_long <- df_long %>%
  mutate(
    rater = sub("^.*_([^_]+)$", "\\1", model_rater),
    model = sub("_([^_]+)$", "", model_rater)  # remove the rater part from the end
  )

# Convert `rater` to factor
df_long$rater <- factor(df_long$rater)

# Create binary outcome variables
df_long <- df_long %>%
  mutate(
    binary_correct = ifelse(score == 1, 1, 0),  # 1 vs. 0 & 0.5
    binary_correct_partial = ifelse(score %in% c(1, 0.5), 1, 0)
  )

# Convert `model` column to factor
# Make sure each of these levels actually appears in your data!
model_levels <- c("claude", "gpt", "deepseek", "perplexity", "gemini")
df_long$model <- factor(df_long$model, levels = model_levels)

# Inspect how many rows exist per model and rater
# (This is just to confirm everything looks as expected)
print(table(df_long$model, df_long$rater))

# Define function to run mixed-effects logistic regression
run_logistic_regression <- function(outcome) {
  # We use as.formula() to build "binary_correct ~ model + (1|rater)" dynamically
  f <- as.formula(paste0(outcome, " ~ model + (1 | rater)"))
  
  model_fit <- glmer(
    f,
    data = df_long,
    family = binomial
  )
  
  # Extract results with exponentiated estimates (Odds Ratios),
  # confidence intervals, and p-values:
  results <- tidy(model_fit, exponentiate = TRUE, conf.int = TRUE)
  return(results)
}

# Run models for each outcome
results_correct <- run_logistic_regression("binary_correct")
results_correct_partial <- run_logistic_regression("binary_correct_partial")

# Print results
cat("\nOdds Ratios for Correct vs. (Partially Correct + Incorrect):\n")
print(results_correct)

cat("\nOdds Ratios for (Correct + Partial) vs. Incorrect:\n")
print(results_correct_partial)








#Table 7 LR with alternative grading scheme
# Load necessary libraries
library(dplyr)
library(lme4)
library(broom.mixed)
library(tidyr)

# Load dataset
df <- read.csv("~/Desktop/lung_cancer/results/dataset_updated.csv", stringsAsFactors = FALSE)

# Identify only the LLM `_grade_` columns (ignore `_consistency_` columns, if any)
score_columns <- grep("_grade_", names(df), value = TRUE)

# Convert grades to numeric
df[score_columns] <- lapply(df[score_columns], as.numeric)


# Reshape dataset into long format
df_long <- df %>%
  pivot_longer(
    cols      = all_of(score_columns),
    names_to  = "model_rater",
    values_to = "score"
  ) %>%
  # Remove the "_grade_" substring so we have something like "claude_ba"
  mutate(model_rater = gsub("_grade_", "_", model_rater, fixed = TRUE))

# Extract rater from the piece after the last underscore
# e.g. "claude_ba" -> rater = "ba", model = "claude"
df_long <- df_long %>%
  mutate(
    rater = sub("^.*_([^_]+)$", "\\1", model_rater),
    model = sub("_([^_]+)$", "", model_rater)  # remove the rater part
  )

# Convert `rater` to factor
df_long$rater <- factor(df_long$rater)


df_long <- df_long %>%
  mutate(
    binary_correct         = ifelse(score == 1, 1, 0),
    binary_correct_partial = ifelse(score %in% c(1, 0.5), 1, 0)
  )

# Convert `model` column to factor
# Make sure these levels are actually in your data!
model_levels <- c("claude", "gpt", "deepseek", "perplexity", "gemini")
df_long$model <- factor(df_long$model, levels = model_levels)

# Inspect how many rows exist per model and rater
print(table(df_long$model, df_long$rater))

# Define function to run mixed-effects logistic regression
run_logistic_regression <- function(outcome) {
  # Build formula, e.g. "binary_correct ~ model + (1 | rater)"
  f <- as.formula(paste0(outcome, " ~ model + (1 | rater)"))
  
  model_fit <- glmer(f, data = df_long, family = binomial)
  
  # Extract results with exponentiated estimates (Odds Ratios), CIs, and p-values
  results <- tidy(model_fit, exponentiate = TRUE, conf.int = TRUE)
  return(results)
}

# Run models for each outcome
results_correct <- run_logistic_regression("binary_correct")
results_correct_partial <- run_logistic_regression("binary_correct_partial")

# Print results
cat("\nOdds Ratios for Correct vs. (Partial + Incorrect + Refusal):\n")
print(results_correct)

cat("\nOdds Ratios for (Correct + Partial) vs. (Incorrect + Refusal):\n")
print(results_correct_partial)





##COHEN KAPPA

# Load necessary libraries
library(psych)

# Load your dataset
file_path <- "~/Desktop/lung_cancer/results/dataset.csv"
df <- read.csv(file_path)
# List of models and their rating columns
models <- list(
  GPT = c("gpt_grade_ba", "gpt_grade_hc", "gpt_grade_ch"),
  Claude = c("claude_grade_ba", "claude_grade_hc", "claude_grade_ch"),
  DeepSeek = c("deepseek_grade_ba", "deepseek_grade_hc", "deepseek_grade_ch"),
  Perplexity = c("perplexity_grade_ba", "perplexity_grade_hc", "perplexity_grade_ch"),
  Gemini = c("gemini_grade_ba", "gemini_grade_hc", "gemini_grade_ch")
)

# Calculate and print Cohen's Kappa for each model
for (model_name in names(models)) {
  cat("\n--- Cohen's Kappa for", model_name, "---\n")
  ratings <- df[, models[[model_name]]]
  ratings_factor <- as.data.frame(lapply(ratings, factor))
  kappa_res <- cohen.kappa(ratings_factor)
  print(kappa_res)
}













# Load necessary libraries
library(ggplot2)
library(reshape2)

# Manually input Cohen's and Weighted Kappa values from provided results
kappa_data <- data.frame(
  Model = rep(c("ChatGPT-o3", "Claude", "DeepSeek R1", "Perplexity", "Gemini"), each = 6),
  Rater_Pair = rep(c("Rater 1 - Rater 2", "Rater 1 - Rater 3", "Rater 2 - Rater 3"), times = 10),
  Type = rep(c("Cohen", "Weighted"), each = 3, times = 5),
  Kappa = c(
    # GPT
    -0.16, 0.44, -0.073, 0.017, 0.46, 0.14,
    # Claude
    0.14, 0.29, 0.22, 0.39, 0.62, 0.36,
    # DeepSeek
    0.34, 0.21, 0.35, 0.62, 0.60, 0.54,
    # Perplexity
    0.21, 0.30, 0.26, 0.38, 0.62, 0.50,
    # Gemini
    0.26, 0.38, 0.27, 0.51, 0.64, 0.40
  )
)

# Enhanced heatmap for publication
heatmap_plot <- ggplot(kappa_data, aes(Rater_Pair, Type, fill = Kappa)) +
  geom_tile(color = "grey90", size = 0.5) +
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", midpoint = 0.4,
                       limits = c(-0.2, 1), name = "Kappa") +
  facet_wrap(~Model, ncol = 2, scales = "free") +
  geom_text(aes(label = round(Kappa, 2)), size = 4) +
  labs(title = "Inter-Rater Agreement: Cohen's and Weighted Kappa",
       x = "Rater Pair",
       y = "Type of Kappa") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold")
  )

# Display the heatmap
heatmap_plot


