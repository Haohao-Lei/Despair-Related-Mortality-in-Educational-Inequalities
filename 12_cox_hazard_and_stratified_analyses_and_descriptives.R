########################################################################################################################
###################################      estimate hazard rate with weights              ##############################
########################################################################################################################
# Load the survival package
library(survival)

# Display the current levels of the gender factor
levels(data$gender)
data$gender <- droplevels(data$gender)
levels(data$gender) <- c("Male","Female")

table(data$childhealth)
# Remove the numbers and periods
data$childhealth <- gsub("^[0-9]+\\.", "", data$childhealth)
data$childhealth <- tools::toTitleCase(data$childhealth)

colnames(x = data)[colnames(x = data)== "education"] <- 'Education'
colnames(x = data)[colnames(x = data) == "cohort"] <- 'Cohort'
colnames(x = data)[colnames(x = data) == "gender"] <- 'Gender'
colnames(x = data)[colnames(x = data) == "race"] <- 'Race'
colnames(x = data)[colnames(x = data) == "childhealth"] <- 'Subjective Childhood Health'

# Example variable names:
# entry_time: Time when the individual becomes at risk (left-truncation time)
# ftime: Follow-up time until event or censoring
# state: Event type indicator (1, 2, 3, 4)

# Cause-specific Cox model for Event 1
model3.1 <- coxph(
  Surv(ftime, state == 1) ~ Education + Cohort + Gender + Race + `Subjective Childhood Health`,
  data = data,
  weights = ipw_LT
)

summary(model3.1)

# Cause-specific Cox model for Event 2
model3.2 <- coxph(
  Surv(ftime, state == 2) ~ Education + Cohort + Gender + Race + `Subjective Childhood Health`,
  data = data,
  weights = ipw_LT
)

summary(model3.2)

# Cause-specific Cox model for Event 3
model3.3 <- coxph(
  Surv(ftime, state == 3) ~ Education + Cohort + Gender + Race + `Subjective Childhood Health`,
  data = data,
  weights = ipw_LT
)

summary(model3.3)

# Cause-specific Cox model for Event 4
model3.4 <- coxph(
  Surv(ftime, state == 4) ~ Education + Cohort + Gender + Race + `Subjective Childhood Health`,
  data = data,
  weights = ipw_LT
)

summary(model3.4)

#visualize the hazard ratio
library(forestmodel)

# Create the forest plot
p4.1 <- forest_model(model3.1) + ggtitle("Unobserved despair development death")
p4.2 <- forest_model(model3.2) + ggtitle("Despair free death")
p4.3 <- forest_model(model3.3) + ggtitle("Chronic pain related death")
p4.4 <- forest_model(model3.4) + ggtitle("Dual despair death")

# Arrange the plots into a 2x2 grid
combined_plot <- (p4.1 | p4.2) / (p4.3 | p4.4)

# Display the combined plot
print(combined_plot)

########################################################################################################################
###################################      look at across the cohort              ##############################
########################################################################################################################
###Look at the all cause mortality and educational difference
# Step 1: Create a combined grouping variable for Education and Cohort
data$Edu_Cohort <- interaction(data$Education, data$Cohort, sep = "_")

# Step 2: Compute CIF for all-cause mortality stratified by the combined variable
cif_all_cause <- tidycmprsk::cuminc(Surv(ftime, all_cause_status) ~ Edu_Cohort,
                                    data = data, weights = ipw_LT)

# Step 3: Convert CIF object to a tidy data frame
df_cif <- tidycmprsk::tidy(cif_all_cause)

# Step 4: Split the 'strata' column back into 'Education' and 'Cohort'
df_cif <- df_cif %>%
  separate(strata, into = c("Education", "Cohort"), sep = "_")

# Step 5: Plot the CIF with faceting by Cohort and coloring by Education
ggplot(df_cif, aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1) +
  facet_wrap(~ Cohort) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12)
  ) +
  scale_x_continuous(
    limits = c(50, 106),
    breaks = seq(50, 106, by = 5),
    labels = seq(50, 106, by = 5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "All-Cause Mortality Cumulative Incidence Function by Education Status and Cohort",
    x = "Age",
    y = "Cumulative Incidence"
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")

##### Look at the unobserved death trajectory
# Step 1: Create a combined grouping variable for Education and Cohort
data$Edu_Cohort <- interaction(data$Education, data$Cohort, sep = "_")

# Step 2: Compute CIF for the unobserved death trajectory stratified by the combined variable
cuminc <- tidycmprsk::cuminc(Surv(ftime, state) ~ Edu_Cohort,
                             data = data, weights = ipw_LT)
# Step 3: Convert the CIF object to a tidy data frame
df_cif <- tidycmprsk::tidy(cuminc)

# Step 4: Filter the data to include only state == 1
df_cif_state1 <- df_cif %>%
  filter(outcome == 1)  # Ensure 'event' corresponds to state == 1

# Step 5: Split the 'strata' column into 'Education' and 'Cohort'
df_cif_state1 <- df_cif_state1 %>%
  separate(strata, into = c("Education", "Cohort"), sep = "_")

# Step 6: Plot the CIF with faceting by Cohort and coloring by Education, without confidence intervals
ggplot(df_cif_state1, aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Cohort) +
  theme_classic() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Unobserved despair development death",  # Custom title indicating state == 1
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

####look at Death without Chronic Pain or Depression
# Step 4: Filter the data to include only state == 2
df_cif_state2 <- df_cif %>%
  filter(outcome == 2)  # Ensure 'event' corresponds to state == 2

# Step 5: Split the 'strata' column into 'Education' and 'Cohort'
df_cif_state2 <- df_cif_state2 %>%
  separate(strata, into = c("Education", "Cohort"), sep = "_")

# Step 6: Plot the CIF with faceting by Cohort and coloring by Education, without confidence intervals
ggplot(df_cif_state2, aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Cohort) +
  theme_classic() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Despair free death",  # Custom title indicating state == 1
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

####look at Chronic Pain Death Trajectory
# Step 4: Filter the data to include only state == 3
df_cif_state3 <- df_cif %>%
  filter(outcome == 3)  # Ensure 'event' corresponds to state == 3

# Step 5: Split the 'strata' column into 'Education' and 'Cohort'
df_cif_state3 <- df_cif_state3 %>%
  separate(strata, into = c("Education", "Cohort"), sep = "_")

# Step 6: Plot the CIF with faceting by Cohort and coloring by Education, without confidence intervals
ggplot(df_cif_state3, aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Cohort) +
  theme_classic() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Chronic pain related death",  # Custom title indicating state == 1
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

####look at Chronic Pain and Depression Death Trajectory
# Step 4: Filter the data to include only state == 4
df_cif_state4 <- df_cif %>%
  filter(outcome == 4)  # Ensure 'event' corresponds to state == 4

# Step 5: Split the 'strata' column into 'Education' and 'Cohort'
df_cif_state4 <- df_cif_state4 %>%
  separate(strata, into = c("Education", "Cohort"), sep = "_")

# Step 6: Plot the CIF with faceting by Cohort and coloring by Education, without confidence intervals
ggplot(df_cif_state4, aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Cohort) +
  theme_classic() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Dual despair death",  # Custom title indicating state == 1
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

#########Let's only look at age 80
df_cif_80 <- dplyr::filter(df_cif, time==80)
df_cif_80 <- df_cif_80 %>%
  separate(strata, into = c("Education", "Cohort"), sep = "_")
df_cif_80 <- dplyr::filter(df_cif_80, !Cohort %in% c("1950-1959", "1960-1969"))
df_cif_80 <- df_cif_80 %>%
  group_by(time, outcome, Cohort) %>%
  summarise(edu_dif = estimate[Education == "Non BA degree holder"] -
              estimate[Education == "BA degree holder"]) %>%
  ungroup()

df_cif_80$outcome <- dplyr::recode(df_cif_80$outcome,
                                   "1" = "Unobserved despair development death",
                                   "2" = "Despair free death",
                                   "3" = "Chronic pain related death",
                                   "4" = "Dual despair death")

# Improved Plot with Adjusted X-axis Interval and Larger Y-axis Text
ggplot(df_cif_80, aes(x = outcome, y = edu_dif, fill = Cohort)) +
  geom_col(position = "dodge", width = 0.7) +  # position "dodge" for side-by-side bars
  theme_light() +
  scale_fill_viridis_d(name = "") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1),  # Formatting y-axis as percentage
    breaks = seq(-0.5, max(df_cif_80$edu_dif, na.rm = TRUE), by = 0.005)  # Interval of 0.5%
  ) +
  scale_x_discrete(
    expand = expansion(add = c(0.5, 0.5))  # Adds some space on the x-axis
  ) +
  labs(
    x = "",
    y = "Absolute Educational Difference in Cumulative Incidence"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10)  # Makes the y-axis text larger
  ) +
  coord_flip()  # Flips the axes

########################################################################################################################
###################################      look at the gender difference              ##############################
########################################################################################################################
# Step 1: Create a combined grouping variable for Education and Cohort
data$Edu_Gender <- interaction(data$Education, data$Gender, sep = "_")

# Step 2: Compute CIF for the unobserved death trajectory stratified by the combined variable
cuminc <- tidycmprsk::cuminc(
  Surv(ftime, state) ~ Edu_Gender,
  data = data,
  weights = ipw_LT  # inverse probability weights
)

# Step 3: Convert the CIF object to a tidy data frame
df_cif <- tidycmprsk::tidy(cuminc)

# Step 5: Split the 'strata' column into 'Education' and 'Gender'
df_cif <- df_cif %>%
  separate(strata, into = c("Education", "Gender"), sep = "_")

# Step 6: Plot the CIF with faceting by Cohort and coloring by Education, without confidence intervals
ggplot(filter(df_cif, outcome == 1), aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Gender) +
  theme_light() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Unobserved despair development death",  # Custom title indicating state == 1
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

#Death without Chronic Pain or Depression
ggplot(filter(df_cif, outcome == 2), aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Gender) +
  theme_light() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Death without Chronic Pain or Depression", 
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

#Chronic Pain Death Trajectory
ggplot(filter(df_cif, outcome == 3), aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Gender) +
  theme_light() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Chronic Pain Death Trajectory",
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels

#Chronic Pain and Depression Death Trajectory 
ggplot(filter(df_cif, outcome == 4), aes(x = time, y = estimate, color = Education)) +
  geom_step(size = 1.5) +
  facet_wrap(~ Gender) +
  theme_light() +
  scale_x_continuous(
    limits = c(50, 106),            # Set x-axis range from 50 to 106
    breaks = seq(50, 106, by = 5),  # Define x-axis ticks at intervals of 5
    labels = seq(50, 106, by = 5)   # Label x-axis ticks
  ) +
  scale_y_continuous(
    limits = c(0, 0.20),                 # Set y-axis range from 0 to 0.20
    breaks = seq(0, 0.20, by = 0.01),    # Define y-axis ticks at intervals of 0.01
    labels = scales::percent_format(accuracy = 1)  # Label y-axis ticks as percentages
  ) +
  labs(
    title = "Chronic Pain and Depression Death Trajectory ",
    x = "Age",                              # Custom x-axis label
    y = "Cumulative Incidence"              # Custom y-axis label
  ) +
  scale_color_brewer(palette = "Set1", name = "Education")  # Color palette for Education levels


#######look at the descriptive statistics
#choose the first non-missing weight for each individual
# Create the 'weight' variable
data <- data %>% 
  mutate(weight = coalesce(wtresp_w1, wtresp_w2, wtresp_w3, wtresp_w4, wtresp_w5, wtresp_w6, wtresp_w7, 
                           wtresp_w8, wtresp_w9, wtresp_w10, wtresp_w11, wtresp_w12, wtresp_w13, wtresp_w14))
wpct(data$Cohort, weight = data$weight, na.rm = TRUE)
wpct(data$Gender, weight = data$weight, na.rm = TRUE)
wpct(data$Race, weight = data$weight, na.rm = TRUE)
wpct(data$Education, weight = data$weight, na.rm = TRUE)
wpct(data$`Subjective Childhood Health`, weight = data$weight, na.rm = TRUE)
mean(data$radage_y, na.rm = TRUE)
sd(data$radage_y, na.rm = TRUE)

ggplot(data, aes(x = radage_y)) +
  geom_density(aes(y = ..density..), fill = "blue", alpha = 0.45) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(min(data$radage_y, na.rm = TRUE), max(data$radage_y, na.rm = TRUE), by = 5)) +
  labs(title = "", x = "Age at death", y = "") +
  theme_linedraw()

prop.table(table(filter(data, state != 0)$state))
sum(data$ftime)
mean(data$ftime)
summary(data$ftime)

# Assuming your data is already loaded and processed
total_follow_up_time <- "2,883,253"  # Total follow-up time in person-years

ggplot(data, aes(x = ftime)) +
  geom_density(aes(y = ..density..), fill = "blue", alpha = 0.45) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = seq(min(data$ftime, na.rm = TRUE), max(data$ftime, na.rm = TRUE), by = 5)) +
  labs(title = "", x = "Follow-up time (years)", y = "") +
  theme_linedraw() +
  # Add text annotation for total follow-up time under the curve
  annotate("text", 
           x = 88,   # Centered on the median follow-up time
           y = 0.001,                               # Adjust y value based on the density plot height
           label = paste("Total Follow-up Time:", total_follow_up_time, "person-years"),
           hjust = 0.5, size = 4, color = "black")

ggseqiplot(sequence[c(4, 6), ], 
           with.legend = TRUE, 
           space = 0, 
           border = TRUE) +
  ylab("") +  # Remove existing y-axis label if any
  scale_x_discrete(              # Modify x-axis labels
    breaks = 1:10,                # Assuming x-axis has 10 points corresponding to years
    labels = paste("Year", 10:1)  # Labels from "Year 10" to "Year 1"
  ) +
  scale_y_continuous(             # Set custom y-axis labels for continuous scale
    breaks = c(1, 2),             # Specify the numeric positions
    labels = c("A", "B")          # Assign "A" and "B" to these positions
  ) +
  theme(
    # Enlarging axis texts
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    
    # Enlarging axis titles
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    
    # Enlarging legend text and removing legend title
    legend.text = element_text(size = 12),
    legend.title = element_blank()  # Removes the legend title
  )

#######look at cause of death at different age for the total population
cuminc <- tidycmprsk::cuminc(Surv(ftime, state) ~ 1, data = data, weights = ipw_LT)
Total <- cuminc$tidy
Total <- Total %>%
  mutate(cause = case_when(
    outcome == "1" ~ "Unobserved despair development death",
    outcome == "2" ~ "Despair free death",
    outcome == "3" ~  "Chronic pain related death",
    outcome == "4" ~  "Dual despair death",
    TRUE ~ NA_character_  # Assign NA for any other unexpected values
  ))
Total$cause <- as.factor(Total$cause)

Total <- Total %>%
  mutate(cause = factor(cause, levels = desired_order))

# Assuming your data is stored in a dataframe called df
Total$single <- c(Total$estimate[1], diff(Total$estimate))

ggplot(filter(Total,cause != 'All cause'), aes(x = time, y = single, fill = cause)) +
  geom_area(position = 'stack', alpha = 0.7) +  # Reduced transparency
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.10)) +  # Y-axis in percentage with 10% intervals
  scale_x_continuous(
    limits = c(50, NA), 
    breaks = seq(50, max(filter(decompose, strata == 'BA degree holder' & cause != 'All cause')$time), by = 5)
  ) +  # X-axis starts at 50 with 5-year intervals
  scale_fill_viridis_d(name = "") +  # Change color legend label to "Cause of Death"
  labs(
    title = 'Incidence of different types of death',
    x = 'Age',
    y = "Cumulative Incidence",
    fill = 'Cause'
  ) +
  theme_light() +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  guides(fill = guide_legend(nrow = 2))  # Arrange legend into two rows

###look at the percentage at each age
# Assuming the dataframe is called df
# First, calculate the total count of outcomes for each age
Total$total_outcomes <- ave(Total$n.event, Total$time, FUN = sum)

# Calculate the percentage for each outcome
Total$percentage <- (Total$n.event / Total$total_outcomes) * 100

# Create a new dataframe with the required information
Total <- Total[, c("time", "outcome", "percentage")]
Total$outcome <- dplyr::case_when(
  Total$outcome == "1" ~ "Unobserved despair development death",
  Total$outcome == "2" ~ "Despair free death",
  Total$outcome == "3" ~ "Chronic pain related death",
  Total$outcome == "4" ~ "Dual despair death",
  TRUE ~ as.character(Total$outcome)  # Retain original values for any other cases
)

# Create the bar chart with similar formatting and color scheme
ggplot(Total, aes(x = factor(time), y = percentage, fill = factor(outcome))) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.7) +  # Reduced transparency
  scale_y_continuous(labels = percent_format(), breaks = seq(0, 100, by = 10)) +  # Y-axis in percentage
  scale_x_discrete() +  # X-axis as discrete factors
  scale_fill_viridis_d(name = "") +  # Use Viridis color scale
  labs(
    title = "Percentage of Each Outcome by Age",
    x = "Age (Time)",
    y = "Percentage (%)",
    fill = "Outcome"
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  guides(fill = guide_legend(nrow = 2))  # Arrange legend into two rows

image(apply(expand.grid(x=seq(-2,1,length.out=500),y=seq(-1.5,1.5,length.out=500)),1,function(c){z=0+0i;for(i in 1:100){z=z^2+(c[1]+1i*c[2]);if(Mod(z)>2)break};return(i)}),col=rainbow(100))