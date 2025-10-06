##########################################################################################################
########################  BUILD INVERSE-PROBABILITY WEIGHTS (IPW) FOR LEFT-TRUNCATION  ##################
##########################################################################################################

# Keep coherent intervals on the age time-scale
data <- data %>%
  filter(!is.na(age_entry), !is.na(ftime)) %>%
  filter(age_entry <= ftime) %>%
  mutate(
    # Avoid evaluating ECDF exactly at the jump point
    ftime_adj = ifelse(ftime == age_entry, age_entry + 1e-4, ftime),
    cohort    = as.factor(cohort)
  )

# Quick checks
pct_violate <- mean(data$age_entry >= data$ftime_adj, na.rm = TRUE) * 100
print(round(pct_violate, 7))                      # should be ~0
stopifnot(!any(data$ftime_adj < data$age_entry))  # must be TRUE

## ----- Estimate H(t) = Pr(entered by age t) ---------------------------------------------------------
## Stabilized weights: numerator = overall ECDF; denominator = cohort-specific ECDF (REPLACED BY MODEL-BASED ESTIMATE)

# Overall ECDF for age_entry (numerator: unchanged)
H_overall_fun <- ecdf(data$age_entry)
data$H_overall_hat <- H_overall_fun(data$ftime_adj)

# ------------------ REPLACEMENT: model-based denominator (conditional CDF) ------------------
# Fit a Cox model for entry age (treating observed age_entry as 'event' times).
# Keep covariates consistent with downstream models (you can adjust formula if desired).

# Fit Cox on the observed entry ages (everyone "has an event" at their entry)
# we set event indicator to 1 for all rows (everyone has an observed entry time)
entry_cox <- coxph(Surv(age_entry, rep(1L, nrow(data))) ~ education + cohort + gender + race + childhealth,
                   data = data, x = TRUE)

# Get baseline cumulative hazard (un-centered)
basehaz_df <- basehaz(entry_cox, centered = FALSE)

# Build an approximating function for baseline cumulative hazard
# rule = 2 => linear extrapolation outside range (safer than NA)
basehaz_fun <- approxfun(basehaz_df$time, basehaz_df$hazard, rule = 2)

# Compute linear predictor and predicted cumulative hazard at each subject's ftime_adj
lp <- predict(entry_cox, newdata = data, type = "lp")             # linear predictor
cumhaz_at_ftime <- basehaz_fun(data$ftime_adj)                   # baseline cumhaz at ftime_adj

# Estimated conditional CDF of entry by ftime_adj: F_hat = 1 - exp(-H0(t) * exp(lp))
# Guard numeric stability for extreme values via pmin/pmax if needed below
F_hat <- 1 - exp(-cumhaz_at_ftime * exp(lp))

# Put the model-based denominator into the data frame
data$H_cohort_hat <- F_hat   # reuse the same name so downstream code stays identical
# small numeric safeguards: clamp model-based CDF to (eps, 1-eps) to avoid divide-by-zero / Inf later
eps <- .Machine$double.eps
data$H_cohort_hat <- pmin(pmax(data$H_cohort_hat, eps), 1 - eps)
# small numeric safeguard for numerator too
eps <- .Machine$double.eps
data$H_overall_hat <- pmin(pmax(data$H_overall_hat, eps), 1 - eps)

# raw stabilized LT weights
num <- pmax(data$H_overall_hat, eps)
den <- pmax(data$H_cohort_hat, eps)
data$ipw_LT_raw <- num / den

# Guard: if ipw_LT_raw not finite or missing everywhere, fall back to 1
finite_raw <- is.finite(data$ipw_LT_raw) & !is.na(data$ipw_LT_raw)
if (!any(finite_raw)) {
  warning("ipw_LT_raw is all NA/Inf. Setting ipw_LT = 1 for all observations.")
  data$ipw_LT <- rep(1, nrow(data))
} else {
  # Trim to 1st - 99th percentiles
  lo <- quantile(data$ipw_LT_raw[finite_raw], 0.01, na.rm = TRUE)
  hi <- quantile(data$ipw_LT_raw[finite_raw], 0.99, na.rm = TRUE)
  data$ipw_LT_trim <- pmin(pmax(data$ipw_LT_raw, lo), hi)
  
  # Normalize to mean = 1 (on the finite entries)
  data$ipw_LT <- data$ipw_LT_trim / mean(data$ipw_LT_trim[finite_raw], na.rm = TRUE)
  
  # Replace any lingering NA/Inf/non-positive with median positive weight
  bad_idx <- which(!is.finite(data$ipw_LT) | is.na(data$ipw_LT) | data$ipw_LT <= 0)
  if (length(bad_idx) > 0) {
    finite_pos <- data$ipw_LT[is.finite(data$ipw_LT) & !is.na(data$ipw_LT) & data$ipw_LT > 0]
    if (length(finite_pos) == 0) {
      data$ipw_LT <- rep(1, nrow(data))
    } else {
      repval <- median(finite_pos, na.rm = TRUE)
      data$ipw_LT[bad_idx] <- repval
    }
  }
}
# Quick diagnostics
cat("IPW (post-trim/normalize) summary:\n")
print(round(summary(data$ipw_LT), 6))
cat("ESS ≈", round((sum(data$ipw_LT)^2) / sum(data$ipw_LT^2)), "\n")
# ----------------------------------------------------------------------------------------------
