##########change the data from the wide to long format
###at the first step, change the name of each variable
# Function to rename variables
rename_variables <- function(df) {
  cols <- names(df)
  
  # Handling two-digit wave numbers first (10 to 15)
  for (i in 10:15) {
    pattern <- paste0("^(.*)r", i, "(.*)$")
    replacement <- paste0("\\1", "\\2_w", i)
    cols <- gsub(pattern, replacement, cols)
  }
  
  # Then handling single-digit wave numbers (1 to 9)
  for (i in 1:9) {
    pattern <- paste0("^(.*)r", i, "(.*)$")
    replacement <- paste0("\\1", "\\2_w", i)
    cols <- gsub(pattern, replacement, cols)
  }
  
  names(df) <- cols
  return(df)
}

# Apply the function to the data
data <- rename_variables(data)

# View the first few columns of the modified dataset
head(data)

#relabeled each cohort
data$cohort <- "0"
table(data$rabyear)
data$cohort[data$rabyear >= 1910 & data$rabyear <= 1919] <- "1910-1919"
data$cohort[data$rabyear >= 1920 & data$rabyear <= 1929] <- "1920-1929"
data$cohort[data$rabyear >= 1930 & data$rabyear <= 1939] <- "1930-1939"
data$cohort[data$rabyear >= 1940 & data$rabyear <= 1949] <- "1940-1949"
data$cohort[data$rabyear >= 1950 & data$rabyear <= 1959] <- "1950-1959"
data$cohort[data$rabyear >= 1960 & data$rabyear <= 1969] <- "1960-1969"
data <- dplyr::filter(data, cohort>0)
table(data$cohort)

data <- as_factor(data)

#only focused on thoes who die
table(data$radage_y)
mortality <- filter(data, !is.na(radage_y))

#create the age_entry variable
# ensure the age columns are in chronological order
age_vars <- paste0("agey_b_w", 1:15)

data <- data %>%
  mutate(
    age_entry = coalesce(!!!syms(age_vars))   # first non-missing age
  )

table(data$age_entry)