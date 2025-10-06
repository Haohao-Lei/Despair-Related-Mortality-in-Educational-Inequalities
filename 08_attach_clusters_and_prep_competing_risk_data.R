##########################################################################################################
##################################  start to conduct the competing risk model    #########################
##########################################################################################################
#combine the type and clutser to the original dataset
seq$type_lcs <- "0"
seq$type_lcs <- cl2.4fac
seq <- dplyr::select(seq,hhidpn, type_lcs)
seq$hhidpn <- as.numeric(seq$hhidpn)

# Use left_join to keep all rows from mortality
mortality <- left_join(mortality, seq, by = "hhidpn")
table(mortality$type_lcs, useNA = "always")
mortality$radage_y
table(mortality %>%
        filter(is.na(type_lcs)) %>%
        dplyr::select(radage_y))

#code the missing mortality characteristics
mortality$type_lcs <- as.character(mortality$type_lcs)
mortality$type_lcs[is.na(mortality$type_lcs)] <- "1"
table(mortality$type_lcs, useNA = "always")

#clean the data in the correct format
#combine the mortality data with the main dataset
mort <- dplyr::select(mortality, hhidpn, type_lcs)
data <- left_join(data, mort, by = "hhidpn")

#recode the new variable of status
# I split the person-year in three categories:
# 1) Death of despair
# 2) No despair death
# 3) Death of pain
# 4) Uncertain death trajectory
# 5) Censored

###dealing with the Censored issue
table(data$radage_y)# look at the older than 100, younger than 50 death
data <- filter(data, radage_y >= 50 | is.na(radage_y))
data$state <- 999
data$state[data$type_lcs=="Type 1"] <- 1
data$state[data$type_lcs=="Type 2"] <- 2
data$state[data$type_lcs=="Type 3"] <- 3
data$state[data$type_lcs=="Type 4"] <- 4
data$state[data$state==999] <- 0
table(data$state, useNA = "always")

#create the follow up years
data$ftime <- data$radage_y
table(data$ftime, useNA = "always")

# Identify the columns related to age (agey_b_w1 to agey_b_w15)
age_columns <- paste0("agey_b_w", 1:15)

# Apply a function to find the last non-NA value for each individual across age columns
data <- data %>%
  rowwise() %>%
  mutate(last_age = last(na.omit(c_across(all_of(age_columns)))))
table(data$last_age)
data <- filter(data, last_age >= 50 | is.na(last_age))#exclude those who are younger than age 50

#look at the results, compare the last age and ftime
head(dplyr::select(data,ftime, last_age), 10)
# Create a table of the `state` variable where `ftime` is not NA
state_table <- data %>%
  filter(!is.na(ftime)) %>%
  count(state)

# Create a table of the `state` variable where `ftime` is NA
n_state_table <- data %>%
  filter(is.na(ftime)) %>%
  count(state)

# Display the table
print(state_table)
print(n_state_table)

#Fill in ftime with last_age where ftime is NA
data <- data %>%
  mutate(ftime = if_else(is.na(ftime), last_age, ftime))
table(data$ftime)

#recode the education variable
data$education <- as.numeric(data$raeduc)
data$education[data$education<5|data$education==6]<- 0
data$education[data$education==5]<- 1
data$education <- as.character(data$education)
data$education[data$education=="0"] <- "Non BA degree holder"
data$education[data$education=="1"] <- "BA degree holder"
data$education <- as.factor(data$education) 
table(data$education)
# Convert 'ragender' to numeric
data$gender <- as.factor(data$ragender)
# Convert 'raracem' to numeric
data$race <- as.numeric(data$raracem)
data$race[data$race>1]<- 0
data$race[data$race=="0"] <- "Non White"
data$race[data$race=="1"] <- "White"
data$race <- as.factor(data$race) 
table(data$race)

##recode the childhood health
# Recode the variable, keeping 1, 2, 3, 4, 5 and setting the rest to NA
data$childhealth <- ifelse(data$rachshlt %in% c("1.excellent", "2.very good", "3.good", "4.fair", "5.poor"), 
                           data$rachshlt, 
                           6)
# Recode the variable, giving names to 1-6 and setting the rest to "Missing"
data$childhealth <- dplyr::recode(data$childhealth,
                                  `1` = "1.excellent",
                                  `2` = "2.very good",
                                  `3` = "3.good",
                                  `4` = "4.fair",
                                  `5` = "5.poor",
                                  `6` = "6.Missing",
                                  .default = "Missing")

# Check the recoded variable
table(data$childhealth)