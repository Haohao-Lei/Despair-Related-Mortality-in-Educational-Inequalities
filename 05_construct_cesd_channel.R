#######################     construct the emotional channel   ########################
cesd <- dplyr::select(mortality, hhidpn,cohort, cesd_w2:cesd_w15, agey_b_w2:agey_b_w15)

# Define the columns to recode
cesd_columns <- c("cesd_w2", "cesd_w3", "cesd_w4", "cesd_w5", "cesd_w6", "cesd_w7", 
                  "cesd_w8", "cesd_w9", "cesd_w10", "cesd_w11", "cesd_w12", 
                  "cesd_w13", "cesd_w14", "cesd_w15")

# Recode CESD scores and replace original variables
for (column in cesd_columns) {
  cesd[[column]] <- ifelse(cesd[[column]] >= 4, "Y", "N")
}

#recode into NA 
cesd <- cesd %>%
  mutate(across(3:16, ~case_when(
    . %in% c(".d:DK", ".m:Missing", ".r:Refuse") ~ NA_character_,
    TRUE ~ .
  )))

cesd <- cesd %>%
  mutate(across(3:16, ~case_when(
    . == "N" ~ "N",
    . == "Y" ~ "D",
    TRUE ~ .
  )))

# Step 1: Pivot the dataset to long format
cesd_long <- cesd %>%
  pivot_longer(
    cols = starts_with("cesd") | starts_with("agey"),
    names_to = c(".value", "wave"),
    names_pattern = "(.*)(_.*)"
  )
colnames(cesd_long) <- c("hhidpn", "cohort", "wave",   "ces_d",   "agey_b") 

# Step 2: Pivot the dataset back to wide format with age as columns
# Pivoting to wide format with ages as column names
cesd <- cesd_long %>%
  dplyr::select(hhidpn,cohort, agey_b, ces_d) %>%
  mutate(agey_b = as.numeric(agey_b)) %>%
  filter(agey_b >= 50 & agey_b <= 105) %>%
  group_by(hhidpn) %>%
  pivot_wider(
    names_from = agey_b,
    values_from = ces_d,
    names_prefix = "age_"
  ) %>%
  ungroup()

# Custom function to replace NULL with a placeholder and unlist
process_cell <- function(cell) {
  if (is.null(cell)) {
    return("M")  # or NA, or "NULL", depending on your preference
  } else if (is.list(cell)) {
    return(paste(unlist(cell), collapse=", "))  # Flatten list to string
  } else {
    return(cell)
  }
}

# Apply the function to every cell in the dataframe
cesd <- cesd %>%
  mutate(across(everything(), ~sapply(.x, process_cell)))

cesd <- cesd %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(everything(), ~ifelse(is.na(.), "M", .)))

cesd <- cesd %>%
  mutate(across(where(is.list), ~sapply(.x, function(x) paste(x, collapse=", "))))

cesd <- cesd %>%
  mutate(across(3:53, ~case_when(
    . == "D, D" ~ "D",
    . == "D, N" ~ "D",
    . == "N, D" ~ "N",
    . == "N, N" ~ "N",
    TRUE ~ .
  )))

# Exclude the first column from the reordering process
df_excluding_first <- cesd[, 3:53]

# Extracting numerical part of the age from column names, excluding the first column
ages <- as.numeric(gsub("age_", "", names(df_excluding_first)))

# Create a named vector where names are the original column names (excluding the first) and values are the ages
age_order <- setNames(ages, names(df_excluding_first))

# Order the age_order vector by ages to get the correct order of column names, excluding the first
ordered_names <- names(sort(age_order))

# Reorder the data frame columns based on the ordered column names, excluding the first
df_ordered_excluding_first <- df_excluding_first[, ordered_names]

# Combine the first column with the reordered columns
cesd <- cbind(cesd[1:2], df_ordered_excluding_first)

# Function to replace "M" with NA for all columns except 'hhidpn'
replace_M_with_NA <- function(data) {
  data <- data %>%
    mutate(across(.cols = -hhidpn, ~ifelse(is.character(.x) & .x == "M", NA, .x)))
  return(data)
}

# Apply the function
cesd <- replace_M_with_NA(cesd)

# Convert all 'age_*' columns to character type to handle 'Y', 'N', and NaN consistently
cesd[ , 3:ncol(cesd)] <- lapply(cesd[ , 3:ncol(cesd)], as.character)

# Function to impute missing values
impute_missing <- function(row) {
  # Find the last non-missing index
  last_non_na_index <- max(which(!is.na(row)))
  
  # Impute missing values from the last non-NA value backward
  for (i in (last_non_na_index-1):3) {
    if (is.na(row[i])) {
      row[i] <- row[i + 1]
    }
  }
  
  return(row)
}

# Apply the function to each row (individual)
cesd_imputed <- t(apply(cesd, 1, impute_missing))

# Convert back to data frame and maintain original column names
cesd <- as.data.frame(cesd_imputed, stringsAsFactors = FALSE)

cesd <- cesd %>%
  mutate(across(3:53, ~case_when(
    . == "NA, N" ~ "N",
    . == "NA, NA" ~ NA,
    TRUE ~ .
  )))
