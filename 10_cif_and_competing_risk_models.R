##########################################################################################################
# visualise the results, compute CIF with IPW and fit tidycmprsk::crr models (minimal safe changes)
##########################################################################################################

# Ensure 'state' is a factor with 0 = censored and positive codes for events
data$state <- as.factor(data$state)

# --- Safety: ensure ipw_LT exists, is numeric, finite, and of correct length ---------------------------
if (!("ipw_LT" %in% names(data))) {
  warning("ipw_LT not found in data: creating neutral weights = 1 for all rows.")
  data$ipw_LT <- rep(1, nrow(data))
}

# Coerce to numeric (in case it was a tibble-column of length 0 or weird type)
data$ipw_LT <- as.numeric(data$ipw_LT)

# Replace NA/Inf/non-positive weights:
bad_idx <- which(!is.finite(data$ipw_LT) | is.na(data$ipw_LT) | data$ipw_LT <= 0)
if (length(bad_idx) > 0) {
  finite_pos <- data$ipw_LT[is.finite(data$ipw_LT) & !is.na(data$ipw_LT) & data$ipw_LT > 0]
  if (length(finite_pos) == 0) {
    warning("No finite positive ipw_LT values found. Setting all weights to 1 (neutral).")
    data$ipw_LT <- rep(1, nrow(data))
  } else {
    repval <- median(finite_pos, na.rm = TRUE)
    data$ipw_LT[bad_idx] <- repval
    warning(length(bad_idx), " ipw_LT entries were NA/Inf/non-positive and replaced with median finite weight = ", round(repval,4))
  }
}

# Final sanity: ensure correct length and finiteness
stopifnot(length(data$ipw_LT) == nrow(data))
if (any(!is.finite(data$ipw_LT))) stop("Non-finite values remain in ipw_LT after repair.")

# Optional quick diagnostic print (comment out if too verbose)
cat("ipw_LT: min, 1stQ, median, mean, 3rdQ, max =\n")
print(as.numeric(round(summary(data$ipw_LT), 6)))
cat("Effective sample size (ESS) ≈ ", round((sum(data$ipw_LT)^2) / sum(data$ipw_LT^2)), "\n")

# ---------------------------------------------------------------------------------------
# Create cumulative incidence function object (weighted). Keep formula unchanged.
# ---------------------------------------------------------------------------------------
cuminc <- tidycmprsk::cuminc(
  formula = Surv(ftime, state) ~ education,
  data    = data,
  weights = data$ipw_LT   # explicitly pass the vector to avoid scope issues
)

# Calculate & print the average follow-up time (unchanged)
average_follow_up <- mean(data$ftime, na.rm = TRUE)
print(paste("Average Follow-Up Time:", round(average_follow_up, 2)))

# -------------------------
# Plotting (unchanged except we reference 'cuminc' we just built)
# -------------------------
p2.1 <- cuminc %>%
  ggcuminc(outcome = 1, size = 1.5) +
  scale_ggsurvfit() +
  theme_classic() +
  scale_x_continuous(limits = c(50, 106), breaks = seq(50, 106, by = 5), labels = seq(50, 106, by = 5)) +
  scale_y_continuous(breaks = seq(0, 0.20, by = 0.01), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Unobserved despair development death", x = "Age", y = "Cumulative Incidence")

p2.2 <- cuminc %>%
  ggcuminc(outcome = 2, size = 1.5) +
  scale_ggsurvfit() +
  theme_classic() +
  scale_x_continuous(limits = c(50, 106), breaks = seq(50, 106, by = 5), labels = seq(50, 106, by = 5)) +
  scale_y_continuous(breaks = seq(0, 0.80, by = 0.05), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Death without Chronic Pain or Depression", x = "Age", y = "Cumulative Incidence")

p2.3 <- cuminc %>%
  ggcuminc(outcome = 3, size = 1.5) +
  scale_ggsurvfit() +
  theme_classic() +
  scale_x_continuous(limits = c(50, 106), breaks = seq(50, 106, by = 5), labels = seq(50, 106, by = 5)) +
  scale_y_continuous(breaks = seq(0, 0.20, by = 0.01), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Chronic Pain Death Trajectory", x = "Age", y = "Cumulative Incidence")

p2.4 <- cuminc %>%
  ggcuminc(outcome = 4, size = 1.5) +
  scale_ggsurvfit() +
  theme_classic() +
  scale_x_continuous(limits = c(50, 106), breaks = seq(50, 106, by = 5), labels = seq(50, 106, by = 5)) +
  scale_y_continuous(breaks = seq(0, 0.20, by = 0.01), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Chronic Pain and Depression Death Trajectory", x = "Age", y = "Cumulative Incidence")

# -------------------------
# Fit Fine-Gray (tidycmprsk::crr) models using the same weights
# -------------------------
# Ensure 'state' still factor; tidycmprsk::crr expects numeric status codes but it is okay to pass data frame with factor as long as levels correspond.
data$state <- as.factor(data$state)  # harmless redundancy

# Use explicit weights = data$ipw_LT to avoid scoping problems
model2.1 <- tidycmprsk::crr(
  formula  = Surv(ftime, state) ~ education + cohort + gender + race + childhealth,
  data     = data,
  failcode = 1,
  cencode  = 0,
  weights  = data$ipw_LT
)
print(model2.1$tidy)

model2.2 <- tidycmprsk::crr(
  formula  = Surv(ftime, state) ~ education + cohort + gender + race + childhealth,
  data     = data,
  failcode = 2,
  cencode  = 0,
  weights  = data$ipw_LT
)
print(model2.2$tidy)

model2.3 <- tidycmprsk::crr(
  formula  = Surv(ftime, state) ~ education + cohort + gender + race + childhealth,
  data     = data,
  failcode = 3,
  cencode  = 0,
  weights  = data$ipw_LT
)
print(model2.3$tidy)

model2.4 <- tidycmprsk::crr(
  formula  = Surv(ftime, state) ~ education + cohort + gender + race + childhealth,
  data     = data,
  failcode = 4,
  cencode  = 0,
  weights  = data$ipw_LT
)
print(model2.4$tidy)

