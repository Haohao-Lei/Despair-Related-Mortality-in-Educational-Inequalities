# Combine datasets by hhidpn and cohort (excluding behaviour)
combined <- pain %>%
  inner_join(cesd, by = c("hhidpn", "cohort"))

# Create a function to combine states (excluding behaviour_state)
combine_states <- function(pain_state, cesd_state) {
  ifelse(is.na(pain_state) | is.na(cesd_state), 
         NA, 
         paste(pain_state, cesd_state, sep = ", "))
}

# Extract all the 'age_' column names from 50 to 100 (adjusted for two datasets)
age_columns_pain <- paste0("age_", 50:100, ".x")
age_columns_cesd <- paste0("age_", 50:100, ".y")

# Vectorized operation to combine states for all age columns (excluding behaviour)
combined_states <- mapply(combine_states, 
                          combined[age_columns_pain], 
                          combined[age_columns_cesd])

# Convert the result back to a data frame and rename the columns
combined_states_df <- as.data.frame(combined_states)
colnames(combined_states_df) <- paste0("age_", 50:100)

# Bind the ID and cohort columns with the combined states
final_combined <- cbind(combined[c("hhidpn", "cohort")], combined_states_df)
final_combined$hhidpn <- as.numeric(final_combined$hhidpn)

# Merge datasets by 'hhidpn'
merged_data <- final_combined %>%
  left_join(mortality %>% dplyr::select(hhidpn, radage_y), by = "hhidpn")

# Ensure `radage_y` is numeric
merged_data <- merged_data %>%
  mutate(radage_y = as.numeric(as.character(radage_y)))

# Identify the `age_` columns
age_columns <- grep("^age_", names(merged_data), value = TRUE)

# Extract the ages from the column names (e.g., 'age_50' -> 50)
age_values <- as.numeric(sub("age_", "", age_columns))

# Ensure `radage_y` is numeric
merged_data <- merged_data %>%
  mutate(radage_y = as.numeric(as.character(radage_y)))

# Apply the mortality coding
for (i in seq_along(age_columns)) {
  col_name <- age_columns[i]
  col_age <- age_values[i]
  
  merged_data[[col_name]] <- ifelse(
    !is.na(merged_data$radage_y) & merged_data$radage_y <= col_age,
    "M",
    merged_data[[col_name]]
  )
}

# Rename the updated dataset to 'seq'
seq <- merged_data

#### Identify the age of death and 10 years before age of death
# Exclude those who die before age 50
mortality <- filter(mortality, radage_y >= 50)
table(mortality$radage_y, useNA = "always")
mortality$radage_st <- mortality$radage_y - 10
table(mortality$radage_st, useNA = "always")

# Create a template for the new dataset 'seq_10y' with columns y10 to y0
seq_10y <- data.frame(hhidpn = seq$hhidpn, 
                      y10 = NA, y9 = NA, y8 = NA, y7 = NA, y6 = NA, 
                      y5 = NA, y4 = NA, y3 = NA, y2 = NA, y1 = NA, y0 = NA)

# Iterate over each row to assign values for the last 11 years before death
for(i in 1:nrow(seq)) {
  # Age at death
  age_at_death <- as.numeric(seq$radage_y[i])
  
  # Identify the starting and ending ages for the last 11 years before death
  start_age <- age_at_death - 10
  end_age <- age_at_death
  
  # Get column names for the relevant ages
  relevant_cols <- paste0("age_", start_age:end_age)
  
  # Check if these columns exist in the dataset
  existing_cols <- relevant_cols[relevant_cols %in% colnames(seq)]
  
  # Fill the new dataset with the existing values or 'M' if not available
  for(j in 1:11) {
    age_col_name <- paste0("age_", start_age + (j - 1))
    y_col_name <- paste0("y", 11 - j)
    if(age_col_name %in% existing_cols) {
      seq_10y[i, y_col_name] <- seq[i, age_col_name]
    } else {
      seq_10y[i, y_col_name] <- "M"
    }
  }
}

# Define the alphabet, labels, and state codes for the sequence data
alphabet <- c('M', 'N_N', 'N_D', 'P_N', 'P_D')
labels <- c('Missing', 'No Pain, Not Depressed', 'No Pain, Depressed', 
            'Pain, Not Depressed', 'Pain, Depressed')
scodes <- c('M', 'N_N', 'N_D', 'P_N', 'P_D')

# Create a mapping from the combined state strings to scodes
state_mapping <- list(
  'N, N' = 'N_N',
  'N, D' = 'N_D',
  'P, N' = 'P_N',
  'P, D' = 'P_D',
  'M' = 'M',
  'D' = 'D'
)

# Apply this mapping to the seq_10y data
state_cols <- paste0('y', 10:0)
for(col in state_cols) {
  seq_10y[[col]] <- sapply(seq_10y[[col]], function(x) {
    if (x %in% names(state_mapping)) {
      return(state_mapping[[x]])
    } else {
      return('M')
    }
  })
}

# Create the sequence object
sequence <- seqdef(dplyr::select(seq_10y,-hhidpn, -y0), alphabet = alphabet, states = scodes, labels = labels)


# Plotting
ggseqmtplot(sequence)
ggseqdplot(sequence)
ggseqiplot(sequence)
