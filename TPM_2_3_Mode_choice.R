# =============================================================================
# ETH Zurich | Transport Planning Methods | Fall 2026 | Group E
# Assignment 2.3: Mode choice - multinomial logit (MNL) with Apollo
# -----------------------------------------------------------------------------
# Structure follows the course demo script for 2.3 (Xuan He, IVT-ETH):
#   0) Define paths and settings
#   1) Read data and prepare (incl. 80% / 20% split)
#   2) Define and estimate the models (a. controls, b. parameters,
#      c. validate inputs, d. model definition, e. estimation, f. output)
#   3) Prediction performance on the training and test data
#   4) Value of travel time (VTT)
#   5) Elasticities
#   6) Apply the model to the trip distribution from 2.2
#
# Model development: M1 basic -> M2 + PT attributes -> M3 + socio-demographics
#                    -> M4 + interactions with supply attributes.
# Final model: M2 (behavioural choice; M4 has the lowest BIC, see Section 3).
#
# Confidentiality: no individual MZMV trip, person or household records are
# written to disk. Section 6 reads the protected MZMV ZIP into memory only.
# =============================================================================

# load libraries
library(apollo)
library(tidyverse)   # dplyr, readr
library(sf)
# also needed in Section 6: archive (protected ZIP), askpass (password prompt)


##### ----0) Define paths and settings  #####

# Weighting switch: TRUE = WP-weighted pseudo-likelihood (used in the report),
# FALSE = unweighted. Rerun the whole script after changing it.
USE_WEIGHTS <- TRUE
weight_col  <- "WP"

# Course-provided mode-choice dataset (Moodle)
choice_file <- "~/Downloads/ModeChoice_mzmv_tripsWAlternatives.Rda"

# Project folder with the outputs of 2.1 / 2.2 and the protected MZMV ZIP.
# The gravity OD matrix MUST be the current 2.2 output (weighted M4 productions,
# 493,402 trips); it is checked in Section 6.
project_dir  <- "/Users/albanbruchig/Documents/School/ETHZ/Fall/Transport Planning Methods"
gravity_file <- file.path(project_dir, "output", "assignment_2_2", "modelled_OD_trips.csv")
zone_file    <- file.path(project_dir, "Materials_for_Trip Generation", "output_2_1",
                          "trip_generation_home_work.gpkg")
zip_file     <- file.path(project_dir, "Materials for descriptive_analysis-20260930",
                          "MZMV_Data_protect.zip")
EXPECTED_GRAVITY_TOTAL <- 493402.26   # total of the 2.2 matrix (weighted M4)

out_dir <- file.path(project_dir, "output", "assignment_2_3",
                     if (USE_WEIGHTS) "weighted" else "unweighted")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("Mode-choice analysis: ", if (USE_WEIGHTS) "WEIGHTED" else "UNWEIGHTED")

# Households are kept together in the train/test split (HHNR = household number)
respondent_id_col <- "HHNR"

# Section 6: minimum number of observed commuter trips for an OD-pair /
# origin-zone / destination-zone mean probability (otherwise next fallback)
OD_MIN_PAIR_N   <- 5L
OD_MIN_ORIGIN_N <- 10L
OD_MIN_DEST_N   <- 10L

# Alternatives as coded in the dataset: car = 1, PT = 2, bike = 3, walk = 4
alternatives <- c("car", "pt", "bike", "walk")
avail_cols   <- paste0("avail_", alternatives)
car_vars  <- c("totalTravelTime_car", "cost_car")
pt_vars   <- c("totalTravelTime_pt", "cost_pt", "access_time", "wait_time",
               "transfers_pt", "transferWaitingTime_pt", "frequency_pt")
bike_vars <- "totalTravelTime_bike"
walk_vars <- "totalTravelTime_walk"
numeric_vars <- c(car_vars, pt_vars, bike_vars, walk_vars)
mode_attributes <- list(car = car_vars, pt = pt_vars, bike = bike_vars, walk = walk_vars)


##### ----1) Read data and prepare  #####

choice_file <- path.expand(choice_file)
if (!file.exists(choice_file)) {
  message("Select the course-provided .Rda file in the file chooser.")
  choice_file <- file.choose()
}
read_choice <- function(path) {
  if (grepl("\\.rds$", path, ignore.case = TRUE)) return(readRDS(path))
  if (grepl("\\.csv$", path, ignore.case = TRUE))
    return(as.data.frame(read_csv(path, show_col_types = FALSE)))
  if (grepl("\\.(rda|rdata)$", path, ignore.case = TRUE)) {
    local_env <- new.env(parent = baseenv())
    objects <- load(path, envir = local_env)
    target <- if ("dat" %in% objects) "dat" else {
      possible <- objects[vapply(objects, function(nm)
        is.data.frame(get(nm, envir = local_env)), logical(1))]
      if (length(possible) != 1L)
        stop(".Rda must contain 'dat' or a single data.frame. Found: ",
             paste(objects, collapse = ", "))
      possible[1L]
    }
    return(as.data.frame(get(target, envir = local_env)))
  }
  stop("Supported formats: .Rda, .RData, .csv or .rds.")
}
raw_data <- as.data.frame(read_choice(choice_file))
message("Loaded prepared choice data: ", nrow(raw_data), " rows, ",
        ncol(raw_data), " columns; WP available: ", weight_col %in% names(raw_data))

missing_cols <- setdiff(c("CHOICE", avail_cols, numeric_vars), names(raw_data))
if (length(missing_cols)) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))

data <- raw_data
data$CHOICE <- suppressWarnings(as.integer(as.character(data$CHOICE)))
if (anyNA(data$CHOICE) || any(!data$CHOICE %in% 1:4))
  stop("CHOICE must contain only 1=car, 2=PT, 3=bike, 4=walk.")
# convert CHOSEN codes to verbal (as in the demo)
data$chosen_mode <- factor(data$CHOICE, levels = 1:4, labels = alternatives)

for (x in c(avail_cols, numeric_vars)) data[[x]] <- suppressWarnings(as.numeric(as.character(data[[x]])))
if (any(!is.finite(as.matrix(data[avail_cols]))) ||
    any(!as.matrix(data[avail_cols]) %in% c(0, 1)))
  stop("Availability variables must be complete and contain only 0 or 1.")

# Attributes of an UNAVAILABLE alternative do not enter the likelihood: missing
# values there are set to 0 (avoids NA utilities). For AVAILABLE alternatives,
# missing attributes are not imputed: those observations are dropped.
bad_rows <- rep(FALSE, nrow(data))
for (m in alternatives) {
  available <- data[[paste0("avail_", m)]] == 1
  for (x in mode_attributes[[m]]) {
    bad_rows <- bad_rows | (available & !is.finite(data[[x]]))
    data[[x]][!available & !is.finite(data[[x]])] <- 0
  }
}
if (any(bad_rows)) message("Discarding ", sum(bad_rows), " observations with missing active attributes.")
data <- data[!bad_rows, , drop = FALSE]
if (nrow(data) == 0) stop("No usable observations remain.")

# Survey weights: WP for the weighted run (must be finite and positive),
# 1 for the unweighted run. reference_wp is kept for common-scale test scores.
if (USE_WEIGHTS) {
  if (!weight_col %in% names(data))
    stop("Weighted run requested, but column '", weight_col, "' is missing.")
  data$survey_weight <- suppressWarnings(as.numeric(as.character(data[[weight_col]])))
  if (any(!is.finite(data$survey_weight) | data$survey_weight <= 0))
    stop("Column '", weight_col, "' contains missing, non-finite or non-positive weights.")
} else {
  data$survey_weight <- rep(1, nrow(data))
}
data$reference_wp <- rep(NA_real_, nrow(data))
if (weight_col %in% names(data)) {
  candidate_wp <- suppressWarnings(as.numeric(as.character(data[[weight_col]])))
  if (all(is.finite(candidate_wp) & candidate_wp > 0)) {
    data$reference_wp <- candidate_wp
  } else if (!USE_WEIGHTS) {
    warning("WP is present but invalid: only unweighted validation scores are available.")
  }
}

# Socio-demographics for M3 / M4. Two-category fields coded 0/1 or 1/2 become
# an indicator for raw code 1 (raw code 2 / 0 = reference). The meaning of the
# raw codes must be checked in the course codebook before interpretation.
# Missing values get a separate "unknown" indicator.
prepare_binary <- function(x, field) {
  if (is.logical(x)) x <- as.integer(x)
  raw <- suppressWarnings(as.numeric(as.character(x)))
  observed <- sort(unique(raw[is.finite(raw)]))
  if (!length(observed)) stop("M3/M4: ", field, " has no observed finite values.")
  if (all(observed %in% c(0, 1))) {
    indicator <- raw; coding <- "0/1: raw 1 -> model 1, raw 0 -> model 0"
  } else if (all(observed %in% c(1, 2))) {
    indicator <- ifelse(raw == 1, 1, ifelse(raw == 2, 0, NA_real_))
    coding <- "1/2: raw 1 -> model 1, raw 2 -> model 0"
  } else {
    stop("M3/M4 cannot automatically encode ", field, ". Observed codes: ",
         paste(head(observed, 20), collapse = ", "), ". Recode explicitly.")
  }
  unknown <- !is.finite(indicator)
  indicator[unknown] <- 0
  message("Variable ", field, ": ", coding, "; unknown values = ", sum(unknown))
  list(value = as.numeric(indicator), missing = as.numeric(unknown),
       n_missing = sum(unknown), n_yes = sum(indicator == 1), coding = coding)
}
for (field in c("male", "pt_pass_ga"))
  if (!field %in% names(data)) stop("Required socio-demographic covariate missing: ", field)
male_info <- prepare_binary(data$male, "male")
ga_info   <- prepare_binary(data$pt_pass_ga, "pt_pass_ga")
data$male_model   <- male_info$value
data$ga_model     <- ga_info$value
data$male_missing <- male_info$missing
data$ga_missing   <- ga_info$missing

# Consistency checks (aggregated output only)
message("Mode counts (CHOICE: car=1, PT=2, bike=3, walk=4):")
print(table(data$chosen_mode))
if (weight_col %in% names(data)) print(summary(data[[weight_col]]))
A_mat <- as.matrix(data[avail_cols])
if (any(rowSums(A_mat) == 0)) stop("Some trips have no available alternative.")
if (any(A_mat[cbind(seq_len(nrow(data)), data$CHOICE)] != 1))
  stop("Some respondents chose an alternative marked unavailable.")
for (m in alternatives) {
  available <- data[[paste0("avail_", m)]] == 1
  for (x in mode_attributes[[m]])
    if (any(data[[x]][available] < 0)) stop("Negative values detected in ", x, " for available trips.")
}
data$trip_id <- seq_len(nrow(data))   # one choice observation per row (Apollo ID)
message("Usable choice observations: ", nrow(data))

# divide data in 80% training (estimate_set) and 20% test set (test_set);
# all trips of one household are kept in the same set
set.seed(2026)
if (!respondent_id_col %in% names(data)) stop("Respondent ID column not found: ", respondent_id_col)
if (anyNA(data[[respondent_id_col]])) stop("Respondent IDs contain missing values.")
households <- unique(data[[respondent_id_col]])
train_households <- sample(households, floor(0.8 * length(households)))
in_train <- data[[respondent_id_col]] %in% train_households
estimate_set <- data[in_train, , drop = FALSE]
test_set     <- data[!in_train, , drop = FALSE]
if (!nrow(estimate_set) || !nrow(test_set)) stop("Training or test set is empty.")
if (!all(1:4 %in% estimate_set$CHOICE)) warning("Not all four modes are chosen in the training set.")

# Normalise weights by the TRAINING mean (common constant: does not change the
# estimates, keeps the pseudo-log-likelihood on the scale of the sample size)
weight_scale <- mean(estimate_set$survey_weight)
if (!is.finite(weight_scale) || weight_scale <= 0) stop("Cannot normalise weights.")
estimate_set$model_weight <- estimate_set$survey_weight / weight_scale
test_set$model_weight     <- test_set$survey_weight / weight_scale
data$model_weight         <- data$survey_weight / weight_scale

weighted_n_eff <- function(w) sum(w)^2 / sum(w^2)
write_csv(data.frame(
  sample = c("train", "test"),
  n = c(nrow(estimate_set), nrow(test_set)),
  sum_normalised_weights = c(sum(estimate_set$model_weight), sum(test_set$model_weight)),
  effective_sample_size = c(weighted_n_eff(estimate_set$model_weight),
                            weighted_n_eff(test_set$model_weight)),
  weighting = if (USE_WEIGHTS) "WP" else "equal observations"
), file.path(out_dir, "sample_sizes.csv"))


##### ----2) Defining and estimating the models  #####

### a. Set core controls
database <- as.data.frame(estimate_set)   # Apollo requires the name "database"
database$chosen_mode <- NULL   # factor column, not used in the model (Apollo warns about factors)
apollo_initialise()
apollo_control <- list(modelName  = "M1_basic",
                       modelDescr = "Mode choice: 4 alternatives",
                       indivID    = "trip_id",
                       panelData  = FALSE,
                       nCores     = 1,
                       weights    = "model_weight",
                       outputDirectory = out_dir)

### b. Parameters (starting values) and fixed parameters per model
# Reference alternative: walk (asc_walk fixed at 0). One parameter vector for
# all four models; parameters not used by a model are kept fixed at 0.
apollo_beta <- c(
  asc_car = 0, asc_pt = 0, asc_bike = 0, asc_walk = 0,
  b_tt_car = -0.03, b_tt_pt = -0.03, b_tt_bike = -0.03, b_tt_walk = -0.03,
  b_cost = -0.10,
  b_access = 0, b_wait = 0, b_transfers = 0, b_transferwait = 0, b_headway = 0,
  b_male_car = 0, b_ga_pt = 0, b_male_unknown_car = 0, b_ga_unknown_pt = 0,
  b_tt_car_male = 0, b_tt_pt_ga = 0
)
pt_extra_params  <- c("b_access", "b_wait", "b_transfers", "b_transferwait", "b_headway")
socio_pars       <- c("b_male_car", "b_ga_pt", "b_male_unknown_car", "b_ga_unknown_pt")
interaction_pars <- c("b_tt_car_male", "b_tt_pt_ga")
# missing-category intercepts are estimated only if missing values occur
missing_fixed <- c(if (!any(estimate_set$male_missing == 1)) "b_male_unknown_car",
                   if (!any(estimate_set$ga_missing == 1)) "b_ga_unknown_pt")
if (length(unique(estimate_set$male_model)) < 2L || length(unique(estimate_set$ga_model)) < 2L)
  stop("M3/M4: male or pt_pass_ga has no 0/1 variation in the training sample.")

model_specs <- list(
  M1 = list(name = "M1_basic", descr = "ASCs + alternative travel times + generic cost",
            fixed = c("asc_walk", pt_extra_params, socio_pars, interaction_pars)),
  M2 = list(name = "M2_PT", descr = "M1 plus 5 PT supply parameters",
            fixed = c("asc_walk", socio_pars, interaction_pars)),
  M3 = list(name = "M3_socio", descr = "M2 plus gender and GA main effects",
            fixed = c("asc_walk", interaction_pars, missing_fixed)),
  M4 = list(name = "M4_interactions", descr = "M3 plus gender x car time and GA x PT time",
            fixed = c("asc_walk", missing_fixed))
)

### d. Define model (utility functions of the most general model, M4)
# M1-M3 are obtained by fixing the corresponding parameters at 0 (see b.).
# Total PT time already contains access/wait/transfer time, so the extra PT
# coefficients are incremental effects conditional on total PT time.
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  V <- list()
  V[["car"]]  <- asc_car + b_tt_car * totalTravelTime_car + b_cost * cost_car +
    b_male_car * male_model + b_male_unknown_car * male_missing +
    b_tt_car_male * totalTravelTime_car * male_model
  V[["pt"]]   <- asc_pt + b_tt_pt * totalTravelTime_pt + b_cost * cost_pt +
    b_access * access_time + b_wait * wait_time + b_transfers * transfers_pt +
    b_transferwait * transferWaitingTime_pt + b_headway * frequency_pt +
    b_ga_pt * ga_model + b_ga_unknown_pt * ga_missing +
    b_tt_pt_ga * totalTravelTime_pt * ga_model
  V[["bike"]] <- asc_bike + b_tt_bike * totalTravelTime_bike
  V[["walk"]] <- asc_walk + b_tt_walk * totalTravelTime_walk
  mnl_settings <- list(alternatives = c(car = 1, pt = 2, bike = 3, walk = 4),
                       avail = list(car = avail_car, pt = avail_pt,
                                    bike = avail_bike, walk = avail_walk),
                       choiceVar = CHOICE, utilities = V)
  P <- list(model = apollo_mnl(mnl_settings, functionality))
  # weights in apollo_control are applied only through apollo_weighting:
  # log-likelihood contribution = model_weight * log(P_chosen)
  P <- apollo_weighting(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### c. validate inputs, e. estimate model, f. analyse model -- for M1 to M4
models <- list()
for (k in names(model_specs)) {
  spec <- model_specs[[k]]
  apollo_control$modelName  <- spec$name
  apollo_control$modelDescr <- spec$descr
  apollo_fixed <- spec$fixed
  # c. validate inputs
  apollo_inputs <- apollo_validateInputs(apollo_beta = apollo_beta, apollo_fixed = apollo_fixed,
                                         database = database, apollo_control = apollo_control,
                                         recycle = FALSE)
  # e. estimate model
  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                           estimate_settings = list(writeIter = FALSE))
  # f. analyse model (no model objects with restricted data are saved)
  apollo_modelOutput(model)
  apollo_saveOutput(model, list(saveOld = FALSE, saveModelObject = FALSE,
                                printFunctions = FALSE, printOutliers = FALSE))
  b_full <- apollo_beta
  b_full[names(model$estimate)] <- model$estimate   # fixed parameters stay at 0
  models[[k]] <- list(name = spec$name, model = model, fixed = apollo_fixed, b = b_full)
}

# Coefficient development table for the report (robust SEs; these are not a
# substitute for full clustered/stratified survey-design inference)
coefficient_development <- bind_rows(lapply(models, function(fit) {
  est <- fit$model$estimate
  se_all <- fit$model$robse
  if (is.null(se_all)) se_all <- fit$model$se
  if (is.null(se_all)) se_all <- numeric(0)
  se <- unname(se_all[names(est)])
  z  <- unname(est) / se
  data.frame(model = fit$name, parameter = names(est), estimate = unname(est),
             std_error = se, z_value = z, p_value = 2 * pnorm(-abs(z)))
}))
write_csv(coefficient_development, file.path(out_dir, "model_development_coefficients.csv"))


##### ----3) Prediction performance (training and test data)  #####

# Choice probabilities with the same utilities as in apollo_probabilities;
# works on any dataset without re-estimation; respects availability.
predict_mnl <- function(df, b) {
  U <- cbind(
    car = b["asc_car"] + b["b_tt_car"] * df$totalTravelTime_car + b["b_cost"] * df$cost_car +
      b["b_male_car"] * df$male_model + b["b_male_unknown_car"] * df$male_missing +
      b["b_tt_car_male"] * df$totalTravelTime_car * df$male_model,
    pt = b["asc_pt"] + b["b_tt_pt"] * df$totalTravelTime_pt + b["b_cost"] * df$cost_pt +
      b["b_access"] * df$access_time + b["b_wait"] * df$wait_time +
      b["b_transfers"] * df$transfers_pt + b["b_transferwait"] * df$transferWaitingTime_pt +
      b["b_headway"] * df$frequency_pt + b["b_ga_pt"] * df$ga_model +
      b["b_ga_unknown_pt"] * df$ga_missing + b["b_tt_pt_ga"] * df$totalTravelTime_pt * df$ga_model,
    bike = b["asc_bike"] + b["b_tt_bike"] * df$totalTravelTime_bike,
    walk = b["asc_walk"] + b["b_tt_walk"] * df$totalTravelTime_walk)
  avail <- as.matrix(df[avail_cols])
  U[avail == 0] <- -Inf
  row_max <- apply(U, 1, max)
  expo <- exp(sweep(U, 1, row_max, "-"))
  expo[avail == 0] <- 0
  P <- expo / rowSums(expo)
  colnames(P) <- alternatives
  P
}

# postprocess predictions (as in the demo): predicted mode = highest probability,
# confusion matrix observed (rows) x predicted (columns), weighted with model_weight
proc_preds <- function(P, base_df) {
  estimated_mode <- factor(max.col(P, ties.method = "first"), levels = 1:4, labels = alternatives)
  conf <- xtabs(w ~ observed + estimated,
                data = data.frame(estimated = estimated_mode, observed = base_df$chosen_mode,
                                  w = base_df$model_weight))
  print(round(conf))
  list(estimated_mode = estimated_mode, confusion = conf)
}

# accuracy and log-loss: weighted as in estimation, plus common-scale
# unweighted and WP versions (for comparing weighted and unweighted runs)
assess <- function(df, b) {
  P <- predict_mnl(df, b)
  p_chosen <- P[cbind(seq_len(nrow(df)), df$CHOICE)]
  log_p <- log(pmax(p_chosen, .Machine$double.xmin))
  correct <- as.numeric(max.col(P, ties.method = "first") == df$CHOICE)
  w <- df$model_weight
  has_wp <- all(is.finite(df$reference_wp) & df$reference_wp > 0)
  data.frame(
    accuracy = sum(w * correct) / sum(w),
    log_loss = -sum(w * log_p) / sum(w),
    log_likelihood = sum(w * log_p),
    accuracy_unweighted = mean(correct),
    log_loss_unweighted = -mean(log_p),
    accuracy_WP = if (has_wp) sum(df$reference_wp * correct) / sum(df$reference_wp) else NA_real_,
    log_loss_WP = if (has_wp) -sum(df$reference_wp * log_p) / sum(df$reference_wp) else NA_real_)
}

# 1) training data and 2) test data, for every model
comparison <- bind_rows(lapply(models, function(fit) {
  train_perf <- assess(estimate_set, fit$b)
  test_perf  <- assess(test_set, fit$b)
  n_par <- length(apollo_beta) - length(fit$fixed)
  # weighted runs: information criteria use the weighted pseudo-log-likelihood
  data.frame(model = fit$name, weighting = if (USE_WEIGHTS) "WP" else "unweighted",
             parameters = n_par,
             AIC_train = -2 * train_perf$log_likelihood + 2 * n_par,
             BIC_train = -2 * train_perf$log_likelihood + log(sum(estimate_set$model_weight)) * n_par,
             accuracy_train = train_perf$accuracy, accuracy_test = test_perf$accuracy,
             logloss_train = train_perf$log_loss, logloss_test = test_perf$log_loss,
             accuracy_train_unweighted = train_perf$accuracy_unweighted,
             accuracy_test_unweighted = test_perf$accuracy_unweighted,
             logloss_train_unweighted = train_perf$log_loss_unweighted,
             logloss_test_unweighted = test_perf$log_loss_unweighted,
             accuracy_train_WP = train_perf$accuracy_WP, accuracy_test_WP = test_perf$accuracy_WP,
             logloss_train_WP = train_perf$log_loss_WP, logloss_test_WP = test_perf$log_loss_WP)
}))
print(comparison)
write_csv(comparison, file.path(out_dir, "model_comparison.csv"))

# Final model: M2, a deliberate behavioural choice. M4 has the lowest training
# BIC and test log-loss, but implausible baseline VTTs and GA waiting effects.
chosen_name <- "M2_PT"
selected <- models[[which(vapply(models, function(x) x$name == chosen_name, logical(1)))]]
final_model <- selected$model
b <- selected$b
message("Final specification: ", chosen_name, " (NOT the BIC winner; M4 has a lower BIC).")
write_csv(data.frame(choice_data = choice_file, gravity_data = gravity_file,
                     USE_WEIGHTS = USE_WEIGHTS,
                     raw_weight_column = if (USE_WEIGHTS) weight_col else "not used",
                     weight_normalisation = weight_scale,
                     train_n = nrow(estimate_set), test_n = nrow(test_set),
                     selected_model = chosen_name,
                     male_model_coding = male_info$coding, ga_model_coding = ga_info$coding),
          file.path(out_dir, "run_settings.csv"))

# plausibility: marginal time and cost effects must be negative
time_effects <- c(car_baseline = b["b_tt_car"], car_male_1 = b["b_tt_car"] + b["b_tt_car_male"],
                  pt_baseline = b["b_tt_pt"], pt_ga_1 = b["b_tt_pt"] + b["b_tt_pt_ga"],
                  bike = b["b_tt_bike"], walk = b["b_tt_walk"])
if (b["b_cost"] >= 0 || any(time_effects >= 0))
  warning("Selected model has non-negative cost/time marginal effect(s).")

# confusion matrices of the final model
# 1) training dataset
pred_train <- proc_preds(predict_mnl(estimate_set, b), estimate_set)
write.csv(as.data.frame.matrix(pred_train$confusion), file.path(out_dir, "train_confusion_matrix.csv"))
# 2) test dataset
P_test <- predict_mnl(test_set, b)
pred_test <- proc_preds(P_test, test_set)
write.csv(as.data.frame.matrix(pred_test$confusion), file.path(out_dir, "test_confusion_matrix.csv"))

# observed vs predicted mode shares in the test set
w_test <- test_set$model_weight
shares <- data.frame(
  mode = alternatives,
  observed_share  = vapply(1:4, function(k) sum(w_test[test_set$CHOICE == k]) / sum(w_test), numeric(1)),
  predicted_share = colSums(P_test * w_test) / sum(w_test))
write_csv(shares, file.path(out_dir, "test_mode_shares.csv"))


##### ---- 4) Calculate the value of travel time (VTT)  #####
# time in min, cost in CHF -> VTT [CHF/h] = 60 * b_time / b_cost
if (abs(b["b_cost"]) < 1e-8) {
  warning("Generic cost coefficient close to zero: VTT unstable.")
} else {
  vtt <- data.frame(group = c("Car: male=0", "Car: male=1", "PT: GA=0", "PT: GA=1"),
                    CHF_per_hour = 60 * c(b["b_tt_car"], b["b_tt_car"] + b["b_tt_car_male"],
                                          b["b_tt_pt"],  b["b_tt_pt"]  + b["b_tt_pt_ga"]) /
                      unname(b["b_cost"]))
  print(vtt)
  write_csv(vtt, file.path(out_dir, "value_of_travel_time.csv"))
  
  # delta method (standard errors); interaction VTTs only if estimated
  expr <- c(car_baseline = "60*b_tt_car/b_cost", pt_baseline = "60*b_tt_pt/b_cost")
  if (!"b_tt_car_male" %in% selected$fixed) expr <- c(expr, car_male_1 = "60*(b_tt_car+b_tt_car_male)/b_cost")
  if (!"b_tt_pt_ga" %in% selected$fixed)    expr <- c(expr, pt_ga_1 = "60*(b_tt_pt+b_tt_pt_ga)/b_cost")
  valueTravelTime <- tryCatch(apollo_deltaMethod(final_model, list(expression = expr)),
                              error = function(e) {
                                warning("Apollo delta method unsuccessful: ", conditionMessage(e)); NULL })
  if (!is.null(valueTravelTime))
    write.csv(as.data.frame(valueTravelTime), file.path(out_dir, "value_of_travel_time_delta_method.csv"))
}


##### ---- 5) Calculate elasticities  #####
# Aggregate elasticity from a 1% increase of one attribute:
#   E = [(weighted predicted demand new / old) - 1] / 0.01
# Increasing a PT component (access, wait, transfer wait) also increases total
# PT time by the same amount. Unavailable alternatives stay unavailable.
elasticity_step <- 0.01
base_P <- predict_mnl(data, b)                        # base predictions
elasticity_vars <- data.frame(
  variable = c("cost_car", "cost_pt", "totalTravelTime_car", "totalTravelTime_pt",
               "totalTravelTime_bike", "totalTravelTime_walk", "access_time", "wait_time",
               "transferWaitingTime_pt", "frequency_pt"),
  own_mode = c("car", "pt", "car", "pt", "bike", "walk", "pt", "pt", "pt", "pt"),
  stringsAsFactors = FALSE)

# modify attribute by 1%
scenario <- function(df, variable, pct = elasticity_step) {
  x <- df
  dx <- pct * x[[variable]]
  x[[variable]] <- x[[variable]] + dx
  if (variable %in% c("access_time", "wait_time", "transferWaitingTime_pt"))
    x$totalTravelTime_pt <- x$totalTravelTime_pt + dx
  x
}
# predict with the modified attribute and compare with the base
elasticities <- bind_rows(lapply(seq_len(nrow(elasticity_vars)), function(i) {
  variable <- elasticity_vars$variable[i]; own_mode <- elasticity_vars$own_mode[i]
  changed_P  <- predict_mnl(scenario(data, variable), b)
  old_totals <- colSums(base_P * data$model_weight)
  new_totals <- colSums(changed_P * data$model_weight)
  e <- (new_totals / old_totals - 1) / elasticity_step
  data.frame(attribute = variable, response_mode = alternatives, attribute_mode = own_mode,
             elasticity = unname(e),
             type = ifelse(alternatives == own_mode, "direct", "cross"))
}))
write_csv(elasticities, file.path(out_dir, "all_elasticities.csv"))
print(filter(elasticities, type == "direct"))

# two cross elasticities: car demand w.r.t. PT time; PT demand w.r.t. car cost
cross_examples <- elasticities |>
  filter((attribute == "totalTravelTime_pt" & response_mode == "car") |
           (attribute == "cost_car" & response_mode == "pt"))
print(cross_examples)
write_csv(cross_examples, file.path(out_dir, "two_cross_elasticities.csv"))

# transfers are a count: report the effect of one additional transfer instead
plus_transfer <- data
plus_transfer$transfers_pt <- plus_transfer$transfers_pt + 1
new_total <- colSums(predict_mnl(plus_transfer, b) * data$model_weight)
old_total <- colSums(base_P * data$model_weight)
write_csv(data.frame(mode = alternatives, change_percent = 100 * (new_total / old_total - 1)),
          file.path(out_dir, "one_additional_pt_transfer.csv"))
rm(plus_transfer)


##### ---- 6) Apply to trip distribution from 2.2  #####
# Mode probabilities of the observed commuter trips (same sample definition as
# in 2.2: employed, weekday, Home -> Work) are averaged per OD pair and applied
# to the gravity-model flows. WP is used for the averages but is NOT applied
# to the gravity trips a second time.
if (!isTRUE(USE_WEIGHTS) || !identical(chosen_name, "M2_PT"))
  stop("Section 6 is configured for the WP-weighted M2 model.")
for (p in c("archive", "askpass"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Install the R package '", p, "' first.")
od_out_dir <- file.path(out_dir, "OD_integration")
dir.create(od_out_dir, recursive = TRUE, showWarnings = FALSE)

# -- load calculated OD flows from 2.2 and the zone polygons --------------------
if (!file.exists(gravity_file)) stop("Missing Section 2.2 OD CSV: ", gravity_file)
if (!file.exists(zone_file)) stop("Missing NPVM zone GeoPackage: ", zone_file)
od <- as.data.frame(read_csv(gravity_file,
                             col_types = cols(from_zone = col_character(), to_zone = col_character(),
                                              trips = col_double()),
                             show_col_types = FALSE))
if (!all(c("from_zone", "to_zone", "trips") %in% names(od)))
  stop("Gravity OD file needs columns from_zone, to_zone, trips.")
od$from_zone <- trimws(od$from_zone)
od$to_zone   <- trimws(od$to_zone)
if (anyNA(od) || any(od$trips < 0)) stop("Invalid OD matrix: missing IDs/trips or negative trips.")
if (anyDuplicated(od[c("from_zone", "to_zone")])) stop("Duplicate OD pairs in the gravity matrix.")
if (nrow(od) != 143L^2) warning("Gravity matrix dimensions differ from 143 x 143.")
cat("Gravity OD: ", nrow(od), " pairs; ", format(round(sum(od$trips), 2), big.mark = ","),
    " trips.\n", sep = "")
if (abs(sum(od$trips) - EXPECTED_GRAVITY_TOTAL) > 1)
  stop("The gravity matrix totals ", round(sum(od$trips)), " trips instead of ",
       round(EXPECTED_GRAVITY_TOTAL), ": this is not the current 2.2 output (weighted M4).")

zones <- st_read(zone_file, quiet = TRUE)
if (!"ID_NPVM" %in% names(zones)) stop("GeoPackage lacks ID_NPVM.")
orig_crs <- st_crs(zones)
if (is.na(orig_crs) || (is.na(orig_crs$epsg) &&
                        grepl("undefined|unknown", orig_crs$input, ignore.case = TRUE))) {
  # the 2.1 GeoPackage holds LV95 coordinates without CRS metadata
  bb <- st_bbox(zones)
  if (!(bb["xmin"] > 2e6 && bb["xmin"] < 3e6 && bb["ymin"] > 1e6 && bb["ymin"] < 1.5e6))
    stop("Geometry CRS undefined and coordinates are not LV95-like.")
  st_crs(zones) <- 2056   # metadata only, no transformation
}
zones <- st_transform(st_make_valid(zones), 2056)
zones$zone_id <- trimws(as.character(zones$ID_NPVM))
if (anyNA(zones$zone_id) || any(zones$zone_id == "")) stop("Missing or blank zone IDs.")
zones <- summarise(group_by(select(zones, zone_id), zone_id), .groups = "drop")
if (!setequal(unique(c(od$from_zone, od$to_zone)), zones$zone_id))
  stop("Gravity OD IDs do not match the ID_NPVM of the zone polygons.")

# -- reproduce the 2.2 commuter sample from the protected MZMV ZIP ----------------
# 2.2 sample: employed (AMSTAT = 1), weekday (Tag 1-5), purpose work (wzweck1 = 2),
# trip starts at home (first trip of a complete outing OR start <= 200 m from home).
# The choice data lack these flags, so trips are matched by (HHNR, AUSNR, WEGNR).
to_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  s <- trimws(as.character(x)); s[s == ""] <- NA_character_
  v <- suppressWarnings(parse_double(s, locale = locale(decimal_mark = ".")))
  bad <- !is.na(s) & is.na(v)
  if (any(bad)) v[bad] <- suppressWarnings(parse_double(s[bad], locale = locale(decimal_mark = ",")))
  v
}
read_protected_mzmv <- function(zfile) {
  if (!file.exists(zfile)) {
    message("Choose the ORIGINAL password-protected MZMV_Data_protect.zip.")
    if (!interactive()) stop("Protected ZIP not found: ", zfile)
    zfile <- file.choose()
  }
  password <- Sys.getenv("MZMV_ZIP_PASSWORD", unset = "")
  if (!nzchar(password)) password <- askpass::askpass("Password for the protected MZMV ZIP")
  if (!nzchar(password)) stop("No ZIP password entered.")
  members <- c(trips = "MZMV_Data_protect/wegeinland.csv",
               persons = "MZMV_Data_protect/zielpersonen.csv",
               households = "MZMV_Data_protect/haushalte.csv")
  if (!all(members %in% archive::archive(zfile, password = password)$path))
    stop("Expected ZIP members not found.")
  read_member <- function(name) {
    con <- archive::archive_read(zfile, file = name, mode = "r", password = password)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    read_delim(con, delim = ";", col_types = cols(.default = col_character()),
               locale = locale(encoding = "UTF-8"), show_col_types = FALSE, progress = FALSE)
  }
  lapply(members, read_member)   # read into memory only
}
mzmv <- read_protected_mzmv(zip_file)
original_trips <- mzmv$trips; people <- mzmv$persons; homes <- mzmv$households
rm(mzmv)
if (anyDuplicated(people$HHNR) || anyDuplicated(homes$HHNR))
  stop("Unexpected non-unique HHNR in the person/household table.")

eligible_people <- people[people$AMSTAT %in% "1" & people$Tag %in% as.character(1:5), "HHNR", drop = FALSE]
original_trips <- original_trips |>
  left_join(select(homes, HHNR, W_X, W_Y), by = "HHNR") |>
  group_by(HHNR, AUSNR) |>
  mutate(.first_outing = to_numeric(AUSNR) > 0 & to_numeric(WEGNR) == min(to_numeric(WEGNR))) |>
  ungroup()
sx <- to_numeric(original_trips$S_X); sy <- to_numeric(original_trips$S_Y)
hx <- to_numeric(original_trips$W_X); hy <- to_numeric(original_trips$W_Y)
valid_home <- is.finite(sx) & is.finite(sy) & is.finite(hx) & is.finite(hy) &
  sx > -990 & hx > -990 & abs(sx) <= 180 & abs(hx) <= 180 & abs(sy) <= 90 & abs(hy) <= 90
dist_home <- 6371000 * sqrt(((hx - sx) * pi / 180 * cos((sy + hy) / 2 * pi / 180))^2 +
                              ((hy - sy) * pi / 180)^2)
at_home <- valid_home & dist_home <= 200
original_trips$.home_start <- (!is.na(original_trips$.first_outing) & original_trips$.first_outing) |
  (!is.na(at_home) & at_home)
eligible_trips <- original_trips |>
  inner_join(eligible_people, by = "HHNR") |>
  filter(.home_start, wzweck1 == "2") |>
  select(HHNR, AUSNR, WEGNR)
rm(original_trips, people, homes, eligible_people, sx, sy, hx, hy, valid_home, dist_home, at_home)

# match the commuter trips to the choice observations (identifiers in canonical form)
norm_code <- function(x) {
  s <- trimws(as.character(x))
  s[is.na(x) | s == ""] <- NA_character_
  num <- !is.na(s) & grepl("^[0-9]+$", s)
  s[num] <- sub("^0+(?=[0-9])", "", s[num], perl = TRUE)
  s
}
keycols <- c("HHNR", "AUSNR", "WEGNR")
if (!all(c(keycols, "wzweck1", "start_x", "start_y", "end_x", "end_y") %in% names(data)))
  stop("Choice data lack trip identifiers or coordinates.")
choice_keys <- data
for (nm in keycols) {
  eligible_trips[[nm]] <- norm_code(eligible_trips[[nm]])
  choice_keys[[nm]]    <- norm_code(choice_keys[[nm]])
}
if (anyNA(eligible_trips[keycols]) || anyNA(choice_keys[keycols]))
  stop("Missing trip identifiers prevent an exact match.")
if (anyDuplicated(choice_keys[keycols])) stop("Duplicate (HHNR, AUSNR, WEGNR) keys in the choice data.")
eligible_trips <- distinct(eligible_trips)
commuter_choice <- semi_join(choice_keys, eligible_trips, by = keycols)
commuter_choice <- commuter_choice[as.character(commuter_choice$wzweck1) == "2", , drop = FALSE]
cat("Eligible Home -> Work weekday trips (2.2 definition): ", nrow(eligible_trips), "\n",
    "  of which in the choice data (> 200 m): ", nrow(commuter_choice), "\n", sep = "")
if (!nrow(commuter_choice)) stop("No commuter trips match the choice observations.")
rm(eligible_trips, choice_keys)

# add from-to zone to the commuter trips (spatial intersection)
for (x in c("start_x", "start_y", "end_x", "end_y", "model_weight"))
  commuter_choice[[x]] <- to_numeric(commuter_choice[[x]])
good <- with(commuter_choice, is.finite(start_x) & is.finite(start_y) & is.finite(end_x) &
               is.finite(end_y) & is.finite(model_weight) & model_weight > 0 &
               abs(start_x) <= 180 & abs(end_x) <= 180 & abs(start_y) <= 90 & abs(end_y) <= 90)
if (any(!good)) message("Excluded commuter trips with invalid coordinates/weights: ", sum(!good))
commuter_choice <- commuter_choice[good, , drop = FALSE]
zone_of <- function(hits) vapply(hits, function(h) if (length(h)) zones$zone_id[h[1]] else NA_character_,
                                 character(1))
commuter_choice$from_zone <- zone_of(st_intersects(st_transform(st_as_sf(
  commuter_choice, coords = c("start_x", "start_y"), crs = 4326, remove = FALSE), 2056), zones))
commuter_choice$to_zone <- zone_of(st_intersects(st_transform(st_as_sf(
  commuter_choice, coords = c("end_x", "end_y"), crs = 4326, remove = FALSE), 2056), zones))
in_area <- !is.na(commuter_choice$from_zone) & !is.na(commuter_choice$to_zone)
cat("  of which with both ends in the 143 zones: ", sum(in_area), "\n", sep = "")
commuter_choice <- commuter_choice[in_area, , drop = FALSE]
if (!nrow(commuter_choice)) stop("No commuter trips in the 143 zones.")

# M2 probabilities per trip (not raised to the power of WP)
P_comm <- predict_mnl(commuter_choice, b)
if (any(!is.finite(P_comm)) || max(abs(rowSums(P_comm) - 1)) > 1e-7) stop("Invalid M2 probabilities.")
trip_probs <- data.frame(from_zone = commuter_choice$from_zone, to_zone = commuter_choice$to_zone,
                         w = commuter_choice$model_weight)
for (m in alternatives) trip_probs[[paste0("num_", m)]] <- P_comm[, m] * trip_probs$w
rm(commuter_choice, P_comm)

# summarize over OD relations (several trips per OD pair): WP-weighted mean
# probability per OD pair, per origin zone, per destination zone and regionally
mean_probs <- function(tbl, cols, prefix) {
  x <- tbl |>
    group_by(across(all_of(cols))) |>
    summarise(n = n(), wsum = sum(w), across(all_of(paste0("num_", alternatives)), sum),
              .groups = "drop")
  for (m in alternatives) x[[paste0("p_", prefix, "_", m)]] <- x[[paste0("num_", m)]] / x$wsum
  names(x)[names(x) == "n"] <- paste0("n_", prefix)
  select(x, all_of(cols), all_of(paste0("n_", prefix)), all_of(paste0("p_", prefix, "_", alternatives)))
}
by_pair   <- mean_probs(trip_probs, c("from_zone", "to_zone"), "pair")
by_origin <- mean_probs(trip_probs, "from_zone", "origin")
by_dest   <- mean_probs(trip_probs, "to_zone", "dest")
regional  <- sapply(alternatives, function(m) sum(trip_probs[[paste0("num_", m)]]) / sum(trip_probs$w))
rm(trip_probs)

# apply the mean probabilities to every gravity OD pair: OD-pair mean if at least
# pair_threshold trips, else origin-zone mean, else destination-zone mean
# (at least 10 trips each), else the regional mean
apply_mode_shares <- function(pair_threshold, suffix) {
  merged <- od |>
    left_join(by_pair, by = c("from_zone", "to_zone")) |>
    left_join(by_origin, by = "from_zone") |>
    left_join(by_dest, by = "to_zone")
  use_pair   <- !is.na(merged$n_pair) & merged$n_pair >= pair_threshold
  use_origin <- !use_pair & !is.na(merged$n_origin) & merged$n_origin >= OD_MIN_ORIGIN_N
  use_dest   <- !use_pair & !use_origin & !is.na(merged$n_dest) & merged$n_dest >= OD_MIN_DEST_N
  merged$probability_source <- ifelse(use_pair, "OD_pair", ifelse(use_origin, "origin",
                                                                  ifelse(use_dest, "destination", "regional")))
  for (m in alternatives) {
    merged[[paste0("p_", m)]] <- ifelse(use_pair, merged[[paste0("p_pair_", m)]],
                                        ifelse(use_origin, merged[[paste0("p_origin_", m)]],
                                               ifelse(use_dest, merged[[paste0("p_dest_", m)]], regional[[m]])))
    merged[[paste0("trips_", m)]] <- merged$trips * merged[[paste0("p_", m)]]
  }
  trcols <- paste0("trips_", alternatives)
  probs <- as.matrix(merged[paste0("p_", alternatives)])
  if (any(!is.finite(probs)) || max(abs(rowSums(probs) - 1)) > 1e-7 ||
      max(abs(rowSums(merged[trcols]) - merged$trips)) > 1e-6)
    stop("OD probability or trip-total conservation failed.")
  regional_split <- data.frame(mode = alternatives, trips = unname(colSums(merged[trcols])))
  regional_split$share <- regional_split$trips / sum(regional_split$trips)
  coverage <- merged |>
    group_by(probability_source) |>
    summarise(OD_pairs = n(), gravity_trips = sum(trips), .groups = "drop")
  coverage$share_gravity_trips <- coverage$gravity_trips / sum(od$trips)
  # modelled OD volumes are exported only for the main scenario (>= 5 trips per
  # direct OD mean); the sensitivity scenario exports regional summaries only
  if (pair_threshold >= 5L)
    write_csv(select(merged, from_zone, to_zone, trips, probability_source, all_of(trcols)),
              file.path(od_out_dir, paste0("OD_modal_trips_", suffix, ".csv")))
  write_csv(regional_split, file.path(od_out_dir, paste0("regional_modal_split_", suffix, ".csv")))
  write_csv(coverage, file.path(od_out_dir, paste0("probability_coverage_", suffix, ".csv")))
  cat("\n=== Modal split of commuter trips, ", suffix, " (min. ", pair_threshold,
      " trips per OD pair) ===\n", sep = "")
  print(as.data.frame(coverage), row.names = FALSE)
  print(regional_split, row.names = FALSE)
  cat("Conservation: PASS; total modelled trips = ", round(sum(regional_split$trips), 2), "\n", sep = "")
  invisible(regional_split)
}
modal_split_main <- apply_mode_shares(OD_MIN_PAIR_N, "min5")             # main result
modal_split_sens <- apply_mode_shares(1L, "min1_sensitivity")           # sensitivity

message("Finished (", if (USE_WEIGHTS) "WP-weighted" else "unweighted", "). Outputs in: ",
        normalizePath(out_dir))