#######################     construct the pain channel   ########################
pain <- dplyr::select(mortality, hhidpn,cohort, painfr_w1:painfr_w15, agey_b_w1:agey_b_w15)

#recode into NA 
pain <- pain %>%
  mutate(across(3:17, ~case_when(
    . %in% c(".d:DK", ".m:Missing", ".r:Refuse") ~ NA_character_,
    TRUE ~ .
  )))

pain <- pain %>%
  mutate(across(3:17, ~case_when(
    . == "0.no" ~ "N",
    . == "1.yes" ~ "P",
    TRUE ~ .
  )))

# Step 1: Pivot the dataset to long format
pain_long <- pain %>%
  pivot_longer(
    cols = starts_with("painfr") | starts_with("agey"),
    names_to = c(".value", "wave"),
    names_pattern = "(.*)(_.*)"
  )

# Step 2: Pivot the dataset back to wide format with age as columns
# Pivoting to wide format with ages as column names
pain <- pain_long %>%
  dplyr::select(hhidpn,cohort, agey_b, painfr) %>%
  mutate(agey_b = as.numeric(agey_b)) %>%
  filter(agey_b >= 50 & agey_b <= 105) %>%
  group_by(hhidpn) %>%
  pivot_wider(
    names_from = agey_b,
    values_from = painfr,
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
pain <- pain %>%
  mutate(across(everything(), ~sapply(.x, process_cell)))

pain <- pain %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(everything(), ~ifelse(is.na(.), "M", .)))

pain <- pain %>%
  mutate(across(where(is.list), ~sapply(.x, function(x) paste(x, collapse=", "))))

pain <- pain %>%
  mutate(across(3:53, ~case_when(
    . == "N, N" ~ "N",
    . == "P, P" ~ "P",
    . == "P, N" ~ "P",
    . == "N, P" ~ "N",
    TRUE ~ .
  )))

# Exclude the first column from the reordering process
df_excluding_first <- pain[, 3:53]

# Extracting numerical part of the age from column names, excluding the first column
ages <- as.numeric(gsub("age_", "", names(df_excluding_first)))

# Create a named vector where names are the original column names (excluding the first) and values are the ages
age_order <- setNames(ages, names(df_excluding_first))

# Order the age_order vector by ages to get the correct order of column names, excluding the first
ordered_names <- names(sort(age_order))

# Reorder the data frame columns based on the ordered column names, excluding the first
df_ordered_excluding_first <- df_excluding_first[, ordered_names]

# Combine the first column with the reordered columns
pain <- cbind(pain[1:2], df_ordered_excluding_first)

# Function to replace "M" with NA for all columns except 'hhidpn'
replace_M_with_NA <- function(data) {
  data <- data %>%
    mutate(across(.cols = -hhidpn, ~ifelse(is.character(.x) & .x == "M", NA, .x)))
  return(data)
}

# Apply the function
pain <- replace_M_with_NA(pain)

# Convert all 'age_*' columns to character type to handle 'Y', 'N', and NaN consistently
pain[ , 3:ncol(pain)] <- lapply(pain[ , 3:ncol(pain)], as.character)

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
pain_imputed <- t(apply(pain, 1, impute_missing))
# Convert back to data frame and maintain original column names
pain <- as.data.frame(pain_imputed, stringsAsFactors = FALSE)