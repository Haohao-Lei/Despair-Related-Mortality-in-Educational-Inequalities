########################################################################################################################
###################################      Look at the all cause mortality      ##############################
########################################################################################################################
# Create a new variable 'all_cause_status' where all death types are coded as 1
data$all_cause_status <- as.factor(ifelse(data$state != "0", 1, 0))  #0 = censored, >0 = death

# Compute CIF for all-cause mortality stratified by education
cif_all_cause <- tidycmprsk::cuminc(
  formula = Surv(ftime, all_cause_status) ~ education,
  data    = data,
  weights  = ipw_LT
)
# Proceed to plot as shown earlier
ggcuminc(cif_all_cause) +
  scale_ggsurvfit() +
  theme_classic() +
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
    title = "All-Cause Mortality Cumulative Incidence Function by Education Status",
    x = "Age",
    y = "Cumulative Incidence"
  )

########################################################################################################################
###################################      output the visualization              ##############################
########################################################################################################################
library(patchwork)
## Combine the four plots into a 2x2 grid and collect the legends
combined_plot <- (p2.1 + p2.2) / (p2.3 + p2.4) +
  plot_layout(guides = "collect") &    # Collect legends from all plots
  theme(
    legend.position = "bottom",        # Position the combined legend at the bottom
    legend.box = "horizontal"           # Arrange the legend items horizontally
  )

#look at the plots
combined_plot
p1.1
p1.2
p1.3

#edit the p1.3
p1.3[[1]] <- p1.3[[1]] + 
  ggtitle("Unobserved despair development death") +
  scale_x_discrete(
    limits = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"),  
    labels = c("Year 10", "Year 9", "Year 8", "Year 7", "Year 6", "Year 5", "Year 4", "Year 3", "Year 2", "Year 1")
  )
  
p1.3[[2]] <- p1.3[[2]] + 
  ggtitle("Despair free death")+
  scale_x_discrete(
    limits = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"),  
    labels = c("Year 10", "Year 9", "Year 8", "Year 7", "Year 6", "Year 5", "Year 4", "Year 3", "Year 2", "Year 1")
  )

p1.3[[3]] <- p1.3[[3]] + 
  ggtitle("Chronic pain related death")+
  scale_x_discrete(
    limits = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"),  
    labels = c("Year 10", "Year 9", "Year 8", "Year 7", "Year 6", "Year 5", "Year 4", "Year 3", "Year 2", "Year 1")
  )

p1.3[[4]] <- p1.3[[4]] + 
  ggtitle("Dual despair death")+
  scale_x_discrete(
    limits = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"),  
    labels = c("Year 10", "Year 9", "Year 8", "Year 7", "Year 6", "Year 5", "Year 4", "Year 3", "Year 2", "Year 1")
  )

p1.3[[1]][["labels"]]$subtitle <- "Percentage: 12.78% of deaths"
p1.3[[2]][["labels"]]$subtitle <- "Percentage: 46.67% of deaths"
p1.3[[3]][["labels"]]$subtitle <- "Percentage: 21.71% of deaths"
p1.3[[4]][["labels"]]$subtitle <- "Percentage: 18.84% of deaths"

#output the plots
# Save the plot to a file
ggsave("Cumulative Incidence.png", plot = combined_plot, width = 10, height = 6, dpi = 3000)
ggsave("Representative Sequence.png", plot = p1.3, width = 10, height = 5, dpi = 2000)

p1.3

#Try to present a decomposition plot
#extract the data
all_cause <- cif_all_cause$tidy
all_cause$cause <- "All cause"

competing <- cuminc$tidy
competing <- competing %>%
  mutate(cause = case_when(
    outcome == "1" ~ "Unobserved despair development death",
    outcome == "2" ~ "Despair free death",
    outcome == "3" ~  "Chronic pain related death",
    outcome == "4" ~  "Dual despair death",
    TRUE ~ NA_character_  # Assign NA for any other unexpected values
  ))

decompose <- rbind(all_cause, competing)
decompose$cause <- as.factor(decompose$cause)

# Create the area plot with customized axes
# Define the desired order of causes
desired_order <- c("Dual despair death", "Chronic pain related death", "Despair free death","Unobserved despair development death")

# Convert 'cause' to a factor with the specified order
decompose <- decompose %>%
  mutate(cause = factor(cause, levels = desired_order))

p3.1 <- ggplot(filter(decompose, strata == 'BA degree holder' & cause != 'All cause'), aes(x = time, y = estimate, fill = cause)) +
  geom_area(position = 'stack', alpha = 0.7) +  # Reduced transparency
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.10)) +  # Y-axis in percentage with 10% intervals
  scale_x_continuous(
    limits = c(50, NA), 
    breaks = seq(50, max(filter(decompose, strata == 'BA degree holder' & cause != 'All cause')$time), by = 5)
  ) +  # X-axis starts at 50 with 5-year intervals
  scale_fill_viridis_d(name = "") +  # Change color legend label to "Cause of Death"
  labs(
    title = 'Cumulative Incidence Function for BA degree holders',
    x = 'Age',
    y = "Cumulative Incidence",
    fill = 'Cause'
  ) +
  theme_light() +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  guides(fill = guide_legend(nrow = 2))  # Arrange legend into two rows

# Create the area plot with customized axes
p3.2 <- ggplot(filter(decompose, strata == 'Non BA degree holder' & cause != 'All cause'), aes(x = time, y = estimate, fill = cause)) +
  geom_area(position = 'stack', alpha = 0.7) +  # Reduced transparency
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.10)) +  # Y-axis in percentage with 10% intervals
  scale_x_continuous(
    limits = c(50, NA), 
    breaks = seq(50, max(filter(decompose, strata == 'BA degree holder' & cause != 'All cause')$time), by = 5)
  ) +  # X-axis starts at 50 with 5-year intervals
  scale_fill_viridis_d(name = "") +  # Change color legend label to "Cause of Death"
  labs(title = 'Cumulative Incidence Function for Non BA degree holders',
       x = 'Age',
       y = "Cumulative Incidence",
       fill = 'Cause') +
  theme_light() +
  theme(legend.position = "bottom")+  # Move legend to the bottom
  guides(fill = guide_legend(nrow = 2))  # Arrange legend into two rows

# Create the line plot for all-cause mortality with the legend inside the plot and no legend title
#Non_BA
p3.3 <- ggplot(filter(decompose, cause == 'All cause'), aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "black", size = 1.2) +  # Same color for both lines
  scale_alpha_manual(values = c(0.2, 1), name = NULL) +  # No transparency legend title
  
  # Decrease the number of breaks for the y-axis (20% intervals) and x-axis (10-year intervals)
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.20)) +  # Y-axis in percentage with 20% intervals
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time), by = 10)) +  # X-axis starts at 50 with 10-year intervals
  
  labs(x = '',
       y = "") +  # No axis titles
  
  theme_void() +  # Makes the plot void
  
  # Add axis lines and tick marks
  theme(
    axis.line = element_line(color = "black"),  # Keep axis lines
    axis.ticks = element_line(color = "black"),  # Keep axis ticks
    axis.text.x = element_text(size = 20),  # Increase x-axis text size
    axis.text.y = element_text(size = 20),  # Increase y-axis text size
    legend.position = "none"  # Remove legend
  )

#BA
p3.4 <- ggplot(filter(decompose, cause == 'All cause'), aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "black", size = 1.2) +  # Same color for both lines
  scale_alpha_manual(values = c(1, 0.2), name = NULL) +  # No transparency legend title
  
  # Decrease the number of breaks for the y-axis (20% intervals) and x-axis (10-year intervals)
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.20)) +  # Y-axis in percentage with 20% intervals
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time), by = 10)) +  # X-axis starts at 50 with 10-year intervals
  
  labs(x = '',
       y = "") +  # No axis titles
  
  theme_void() +  # Makes the plot void
  
  # Add axis lines and tick marks
  theme(
    axis.line = element_line(color = "black"),  # Keep axis lines
    axis.ticks = element_line(color = "black"),  # Keep axis ticks
    axis.text.x = element_text(size = 20),  # Increase x-axis text size
    axis.text.y = element_text(size = 20),  # Increase y-axis text size
    legend.position = "none"  # Remove legend
  )

# Save the plot to a file
ggsave("Decomposition_BA.png", plot = p3.1, width = 10, height = 6, dpi = 1000)
ggsave("Decomposition_non_BA.png", plot = p3.2, width = 10, height = 6, dpi = 3000)
ggsave("line_BA.png", plot = p3.4, width = 10, height = 6, dpi = 2000)
ggsave("line_non_BA.png", plot = p3.3, width = 10, height = 6, dpi = 2000)
######################################################################################################################
#########################         Non BA                   ###########################################################
######################################################################################################################
# Chronic Pain and Depression Death Trajectory
p3.5 <- ggplot(filter(decompose, cause == 'Chronic Pain and Depression Death Trajectory'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#440154", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(0.3, 1)) +  # Assign different transparency to strata
  scale_y_continuous(labels = percent, breaks = seq(0, 0.14, by = 0.04)) +  # Y-axis in percentage with 4% intervals
  scale_x_continuous(limits = c(50, 105), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +  # X-axis starts at 50 with 10-year intervals
  labs(x = NULL, y = "") +  # Remove x-axis label for top plots
  theme_light() +
  theme(
    legend.position = "none",  # Exclude the legend
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.line.x = element_blank(),    # Remove x-axis line
    axis.text.y = element_text(size = 20),  # Increase y-axis text size
    axis.title.y = element_text(size = 20)  # Increase y-axis title size
  )

# Chronic Pain Death Trajectory
p3.6 <- ggplot(filter(decompose, cause == 'Chronic Pain Death Trajectory'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#31688EFF", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.20, by = 0.04)) +
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +
  labs(x = NULL, y = "") +  # Remove x-axis label for top plots
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),    # Remove x-axis line
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

# Death without Chronic Pain or Depression
p3.7 <- ggplot(filter(decompose, cause == 'Death without Chronic Pain or Depression'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#35B779FF", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.8, by = 0.2)) +
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +
  labs(x = NULL, y = "") +  # Remove x-axis label for top plots
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),    # Remove x-axis line
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

# Unobserved Death Trajectory
p3.8 <- ggplot(filter(decompose, cause == 'Unobserved Death Trajectory'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#FDE725FF", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(0.35, 1)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.18, by = 0.04)) +
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +
  labs(x = '', y = "") +  # Retain x-axis label only for the last plot
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),   # Retain x-axis text with increased size
    axis.ticks.x = element_line(),           # Retain x-axis ticks
    axis.title.x = element_text(size = 20),  # Retain x-axis title with increased size
    axis.line.x = element_line(),            # Retain x-axis line
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

# Stack the plots vertically
# Use arrangeGrob to create a grob object for ggsave
combined_plot <- arrangeGrob(p3.5, p3.6, p3.7, p3.8, ncol = 1)

# Save the combined plot
ggsave("sub_non_BA.png", plot = combined_plot, width = 6, height = 12, dpi = 2000, units = "in")

######################################################################################################################
#########################         BA Degree Holder                   ###########################################################
######################################################################################################################
# Chronic Pain and Depression Death Trajectory
p3.9 <- ggplot(filter(decompose, cause == 'Chronic Pain and Depression Death Trajectory'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#440154", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(1, 0.3)) +  # Assign different transparency to strata
  scale_y_continuous(labels = percent, breaks = seq(0, 0.14, by = 0.04)) +  # Y-axis in percentage with 4% intervals
  scale_x_continuous(limits = c(50, 105), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +  # X-axis starts at 50 with 10-year intervals
  labs(x = NULL, y = "") +  # Remove x-axis label for top plots
  theme_light() +
  theme(
    legend.position = "none",  # Exclude the legend
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.line.x = element_blank(),    # Remove x-axis line
    axis.text.y = element_text(size = 20),  # Increase y-axis text size
    axis.title.y = element_text(size = 20)  # Increase y-axis title size
  )

# Chronic Pain Death Trajectory
p3.10 <- ggplot(filter(decompose, cause == 'Chronic Pain Death Trajectory'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#31688EFF", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(1, 0.3)) +  # Assign different transparency to strata
  scale_y_continuous(labels = percent, breaks = seq(0, 0.20, by = 0.04)) +
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +
  labs(x = NULL, y = "") +  # Remove x-axis label for top plots
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),    # Remove x-axis line
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

# Death without Chronic Pain or Depression
p3.11 <- ggplot(filter(decompose, cause == 'Death without Chronic Pain or Depression'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#35B779FF", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(1, 0.3)) +  # Assign different transparency to strata
  scale_y_continuous(labels = percent, breaks = seq(0, 0.8, by = 0.2)) +
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +
  labs(x = NULL, y = "") +  # Remove x-axis label for top plots
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),    # Remove x-axis line
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

# Unobserved Death Trajectory
p3.12 <- ggplot(filter(decompose, cause == 'Unobserved Death Trajectory'), 
               aes(x = time, y = estimate, group = strata)) +
  geom_line(aes(alpha = strata), color = "#FDE725FF", size = 2) +  # Increased line thickness from 1.2 to 2
  scale_alpha_manual(values = c(1, 0.35)) +  # Assign different transparency to strata
  scale_y_continuous(labels = percent, breaks = seq(0, 0.18, by = 0.04)) +
  scale_x_continuous(limits = c(50, NA), breaks = seq(50, max(decompose$time, na.rm = TRUE), by = 10)) +
  labs(x = '', y = "") +  # Retain x-axis label only for the last plot
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),   # Retain x-axis text with increased size
    axis.ticks.x = element_line(),           # Retain x-axis ticks
    axis.title.x = element_text(size = 20),  # Retain x-axis title with increased size
    axis.line.x = element_line(),            # Retain x-axis line
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )

# Stack the plots vertically
# Use arrangeGrob to create a grob object for ggsave
combined_plot2 <- arrangeGrob(p3.9, p3.10, p3.11, p3.12, ncol = 1)

# Save the combined plot
ggsave("sub_BA.png", plot = combined_plot2, width = 6, height = 12, dpi = 2000, units = "in")

#########################         Bar chart                   ###########################################################
decompose_bar <- filter(decompose, time %in% c(80, 90, 100) & cause != "All cause")
decompose_bar$time <- as.factor(decompose_bar$time)
# BA group
# Define your custom alpha values for each strata
custom_alphas <- c(
  "BA degree holder" = 1,
  "Non BA degree holder" = 0.3
)
# First, reorder the 'time' variable in your data frame
decompose_bar$time <- factor(decompose_bar$time, levels = c(100, 90, 80))

# Now, create the plot with rotated y-axis labels, custom y-axis breaks, and the new time order
p4.1 <- ggplot(data = decompose_bar, aes(x = strata, y = estimate, fill = cause, alpha = strata)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # Set smaller bar width
  scale_fill_viridis_d() +  # Use the viridis color palette
  scale_alpha_manual(values = custom_alphas) +  # Set custom alpha values for each strata
  scale_y_continuous(labels = percent, breaks = c(0.25, 0.75)) +  # Show only 50% and 75%
  theme_void() +
  theme(
    legend.position = "none",               # Exclude the legend
    plot.title = element_blank(),           # Exclude the title
    axis.text.y = element_text(size = 25, angle = 90, hjust = 0.5),  # Rotate Y axis text by 90 degrees
    axis.ticks.y = element_line(),          # Show Y axis ticks
    axis.line.y = element_line(),           # Show Y axis line
    axis.title.y = element_blank(),         # Remove Y axis title
    strip.background = element_blank(),     # Remove strip background
    strip.text = element_blank()            # Remove facet labels
  ) +
  labs(
    x = NULL,  # Remove X label
    y = NULL   # Remove Y label
  ) +
  facet_wrap(~ time, nrow = 1, scales = "fixed", switch = "y")  # Use fixed scales for all facets

# Define your custom alpha values for each strata
custom_alphas <- c(
  "BA degree holder" = 0.3,
  "Non BA degree holder" = 1
)
# First, reorder the 'time' variable in your data frame
decompose_bar$time <- factor(decompose_bar$time, levels = c(100, 90, 80))

# Now, create the plot with rotated y-axis labels, custom y-axis breaks, and the new time order
p4.2 <- ggplot(data = decompose_bar, aes(x = strata, y = estimate, fill = cause, alpha = strata)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # Set smaller bar width
  scale_fill_viridis_d() +  # Use the viridis color palette
  scale_alpha_manual(values = custom_alphas) +  # Set custom alpha values for each strata
  scale_y_continuous(labels = percent, breaks = c(0.25, 0.75)) +  # Show only 50% and 75%
  theme_void() +
  theme(
    legend.position = "none",               # Exclude the legend
    plot.title = element_blank(),           # Exclude the title
    axis.text.y = element_text(size = 25, angle = 90, hjust = 0.5),  # Rotate Y axis text by 90 degrees
    axis.ticks.y = element_line(),          # Show Y axis ticks
    axis.line.y = element_line(),           # Show Y axis line
    axis.title.y = element_blank(),         # Remove Y axis title
    strip.background = element_blank(),     # Remove strip background
    strip.text = element_blank()            # Remove facet labels
  ) +
  labs(
    x = NULL,  # Remove X label
    y = NULL   # Remove Y label
  ) +
  facet_wrap(~ time, nrow = 1, scales = "fixed", switch = "y")  # Use fixed scales for all facets


ggsave("bar_BA.png", plot = p4.1, width = 10, height = 4, dpi = 2000, units = "in")
ggsave("non_bar_BA.png", plot = p4.2, width = 10, height = 4, dpi = 2000, units = "in")