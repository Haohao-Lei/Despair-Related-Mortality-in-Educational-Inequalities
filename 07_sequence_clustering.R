#####start the clustering analysis
#these steps are very time consuming
dist <- seqdist(sequence, method = "OM", indel = 1, sm = "TRATE")

#justify the number of clusters
#(agnesRange <- wcKMedRange(dist, 2:10))

#plot(agnesRange, stat = c("ASW", "HG", "PBC"), lwd = 10)

#clusterward2 <- agnes(dist, diss = TRUE, method = "ward")

#cl2.4 <- cutree(clusterward2, k = 4)
#cl2.4fac <- factor(cl2.4, labels = paste("Type", 1:4))

# Save an R object (e.g., a data frame or any other object)
#save(clusterward2, file = "clusterward2.RData")
#save(cl2.4fac, file = "cl2.4fac.RData")
load("cl2.4fac.RData")

cb_palette <- c("#0072B2", "#D55E00", "#009E73", 
                "#F0E442", "#000000")

# Modify ggseqdplot with custom colors
p1.1 <- ggseqdplot(sequence, group = cl2.4fac, facet_ncol=2) +
  scale_fill_manual(values = cb_palette) +
  theme_minimal()

# Modify ggseqiplot with custom colors
p1.2 <- ggseqiplot(sequence, group = cl2.4fac, facet_ncol=2) +
  scale_fill_manual(values = cb_palette) +
  theme_minimal()+
  theme(
    legend.title = element_blank(),          # Removes the legend title
  )

p1.2$data$grouplab <- as.character(p1.2$data$grouplab)
p1.2$data$grouplab[p1.2$data$grouplab=="Type 1 (n=2090)"] <- "Unobserved despair development death (n=2090)"
p1.2$data$grouplab[p1.2$data$grouplab=="Type 2 (n=7625)"] <- "Despair free death (n=7625)"
p1.2$data$grouplab[p1.2$data$grouplab=="Type 3 (n=3547)"] <- "Chronic pain related death (n=3547)"
p1.2$data$grouplab[p1.2$data$grouplab=="Type 4 (n=3079)"] <- "Dual despair death (n=3079)"


# seqrplot with the specified dissimilarity matrix and group
p1.3 <- ggseqrplot(sequence, 
                   diss = dist,
                   nrep = 4,
                   group = cl2.4fac,
                   facet_ncol = 2,
                   stats = F,
                   criterion = "density")

#combine the type and clutser to the original dataset
#seq$type <- "0"
#seq$type <- cl2.4fac
#seq <- dplyr::select(seq,hhidpn, type)
#seq$hhidpn <- as.numeric(seq$hhidpn)

# Use left_join to keep all rows from mortality
#mortality <- left_join(mortality, seq, by = "hhidpn")
#table(mortality$type, useNA = "always")
#mortality$radage_y
#table(mortality %>%
#filter(is.na(type)) %>%
#dplyr::select(radage_y))

#code the missing mortality characteristics
#mortality$type
#mortality$type[is.na(mortality$type)] <- "Type 1"
#table(mortality$type, useNA = "always")

#clean the data in the correct format
#combine the mortality data with the main dataset
#mort <- dplyr::select(mortality, hhidpn, type)
#data <- left_join(data, mort, by = "hhidpn")

#recode the new variable of status
# I split the person-year in three categories:
# 1) Death of despair
# 2) No despair death
# 3) Death of pain
# 4) Uncertain death trajectory
# 5) Censored

###dealing with the Censored issue
#table(data$radage_y)# look at the older than 100, younger than 50 death
#data <- filter(data, radage_y >= 50 | is.na(radage_y))
#data$state <- 999
#data$state[data$type=="Type 1"] <- 1
#data$state[data$type=="Type 2"] <- 2
#data$state[data$type=="Type 3"] <- 3
#data$state[data$type=="Type 4"] <- 4
#data$state[data$state==999] <- 0
#table(data$state, useNA = "always")

#create the follow up years
#data$ftime <- data$radage_y
#table(data$ftime, useNA = "always")

# Identify the columns related to age (agey_b_w1 to agey_b_w15)
#age_columns <- paste0("agey_b_w", 1:15)

# Apply a function to find the last non-NA value for each individual across age columns
#data <- data %>%
#rowwise() %>%
#mutate(last_age = last(na.omit(c_across(all_of(age_columns)))))
#table(data$last_age)
#data <- filter(data, last_age >= 50 | is.na(last_age))#exclude those who are younger than age 50

#look at the results, compare the last age and ftime
#head(dplyr::select(data,ftime, last_age), 10)
# Create a table of the `state` variable where `ftime` is not NA
#state_table <- data %>%
#filter(!is.na(ftime)) %>%
#count(state)

# Create a table of the `state` variable where `ftime` is NA
#n_state_table <- data %>%
#filter(is.na(ftime)) %>%
#count(state)

# Display the table
#print(state_table)
#print(n_state_table)

#Fill in ftime with last_age where ftime is NA
#data <- data %>%
#mutate(ftime = if_else(is.na(ftime), last_age, ftime))
#table(data$ftime)

##########################       try to use the LCS instead of OM     #################################
#distance with LCS
#sequence.lcs=seqdist(sequence, method="LCS")
########################################### cluster analysis  #########################################
#sequence.lcs.cluster = agnes(sequence.lcs, diss = T, method = "ward") 
#save(sequence.lcs.cluster, file="sequence.lcs.cluster.Rdata" )
#load("sequence.lcs.cluster.Rdata")
#(agnesRange <- wcKMedRange(sequence.lcs, 2:10))

# Define a color-blind-friendly palette
#cb_palette <- c("#0072B2", "#D55E00", "#56B4E9", "#009E73", 
#"#F0E442",  "#AA4499", "#000000")

#look at 4 clusters
#dist.4cl = cutree(sequence.lcs.cluster, k = 4)
# Modify ggseqdplot with custom colors
#p1.1 <- ggseqdplot(sequence, group = dist.4cl, facet_ncol=2) +
#scale_fill_manual(values = cb_palette) +
#theme_minimal()

# Modify ggseqiplot with custom colors
#p1.2 <- ggseqiplot(sequence, group = dist.4cl, facet_ncol=2) +
#scale_fill_manual(values = cb_palette) +
#theme_minimal()

# seqrplot with the specified dissimilarity matrix and group
#p1.3 <- ggseqrplot(sequence, 
#diss = sequence.lcs,
#nrep = 4,
#group = as.factor(dist.4cl),
#facet_ncol = 2,
#stats = F,
#criterion = "density")