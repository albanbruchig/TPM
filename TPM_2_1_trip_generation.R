# ==============================================================================
#  Transport Planning Methods (HS 2026) - Assignment 2.1: Trip generation
#  Home -> Work trips:
#    (1) person-level count regression estimated on the MZMV 2021
#    (2) applied to the NPVM 2010 zones of the Zurich region via Strukturdaten
#
#  Structure follows the exercise template (Steps 0-6).
#  All MZMV variable names and codes are set in `cfg` (Step 0).
#  Exploratory plots: see TPM_2_1_EDA.R (run after Step 3 of this script).
# ==============================================================================


### Step 0: Packages, paths, settings ------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(arrow)
  library(sf)
  library(MASS)
  library(scales)
  library(patchwork)
})
select <- dplyr::select   # MASS masks dplyr::select -> restore it

# paths (as in the template)
path_data <- "/Users/albanbruchig/Documents/School/ETHZ/Fall/Transport Planning Methods/MZMV_Data_protect"
path2     <- "/Users/albanbruchig/Documents/School/ETHZ/Fall/Transport Planning Methods/Materials_for_Trip Generation"
path_out  <- file.path(path2, "output_2_1")
dir.create(path_out, showWarnings = FALSE, recursive = TRUE)

# ---- MZMV 2021 variable names and codes (MZMV 2021 Datenbankstruktur) -----------
cfg <- list(
  trip_purpose = "wzweck1",    purpose_work = 2,       # ZWECK: 2 = Arbeiten (purpose at the destination)
  outing       = "AUSNR",      trip_no = "WEGNR",      # Ausgang = chain of trips leaving and returning home
  age          = "alter",                              # age in years
  weekday      = "Tag",        weekdays     = 1:5,     # TAG: 1 = Montag ... 7 = Sonntag (capital T!)
  car_avail    = "f42100e",    car_yes = c(1, 2), car_no = c(3, -99),  # 1 immer, 2 nach Absprache, 3 nicht, -99 <18 / no licence
  abo_ga       = "f41600_01a",                         # GA (JANEIN: 1 = Ja, 2 = Nein, -99 nicht gestellt)
  abo_verbund  = "f41600_01c",                         # Verbund-Abo (JANEIN)
  employment   = "AMSTAT",     employed = 1,           # Arbeitsmarktstatus: 1 = Erwerbstaetige/r (incl. apprentices)
  hh_urban     = "W_stadt_land_2012",                  # Haushalte: 1 staedtisch, 2 intermediaer, 3 laendlich
  weight       = "WP"                                  # person weight
)
# How to decide that a work trip STARTS AT HOME (wzweck2 cannot do this: 1 = "Hinweg"
# only means "not heading home", e.g. a trip lunch -> work is also a Hinweg):
#   "outing" = first trip of a complete Ausgang (AUSNR > 0)
#   "coords" = trip start lies within HOME_RADIUS_M of the home address (S_X/S_Y vs W_X/W_Y)
#   "either" = one of the two (default; Step 2 prints how well they agree)
#   "hinweg" = simple version: every work trip with wzweck2 == 1 (also counts e.g. lunch -> work)
HOME_START    <- "either"
HOME_RADIUS_M <- 200

ONLY_WEEKDAYS  <- TRUE    # Strukturdaten / NPVM describe an average working day
ONLY_CANTON_ZH <- FALSE   # TRUE = estimate on residents of canton ZH only (W_KANTON == 1)
FINAL_MODEL    <- NULL    # NULL = pick the applicable model with the lowest AIC; or e.g. "M3"

# Age classes as in the Strukturdaten (RE_<age>_<car><abo>)
AGE_CLASSES <- c("0017", "1824", "2544", "4564", "6574", "75XX")
# Pooled age classes (employed persons < 18 and > 74 are rare in the MZMV).
# Step 5 estimates every key model both with these pooled classes and with the
# 6 separate Strukturdaten classes and tests which fits better.
AGE_MAP <- c("0017" = "0024", "1824" = "0024", "2544" = "2544",
             "4564" = "4564", "6574" = "65XX", "75XX" = "65XX")

# Plot styling (one look for every figure in the report)
col_1 <- "#2a78d6"; col_2 <- "#eb6834"; col_3 <- "#1baf7a"; col_4 <- "#4a3aa7"
col_seq_lo <- "#e3edf9"; col_seq_hi <- "#0b3d7a"; col_mid <- "#f2f2f0"
cols_cartc <- c("NOC_NOTC" = col_1, "NOC_TC" = col_2, "CAR_NOTC" = col_3, "CAR_TC" = col_4)
theme_set(theme_minimal(base_size = 10) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"),
                  legend.position = "bottom"))
save_fig <- function(p, name, w = 16, h = 10) {
  if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)
  if (file.access(path_out, 2) != 0) stop("No write permission in ", path_out)
  for (ext in c("png", "pdf")) {
    f <- file.path(path_out, paste0(name, ".", ext))
    if (file.exists(f)) unlink(f)                # remove an old (possibly locked) version first
    ok <- tryCatch({
      ggsave(f, p, width = w, height = h, units = "cm", dpi = 300)
      TRUE
    }, error = function(e) { while (dev.cur() > 1) dev.off(); FALSE })   # close a half-opened device
    if (!ok) warning("Could not write ", f, " (file open elsewhere, or folder synced by iCloud?)",
                     " - continuing without it.", call. = FALSE)
  }
}


### Step 1: Hypotheses ----------------------------------------------------------
# (in the report) Origin side = person attributes in the Strukturdaten,
#                 destination side = zonal attractors in the Strukturdaten
#
# | Origin | Production (Strukturdaten)                       | Destination | Attraction (Strukturdaten)                          |
# |--------|--------------------------------------------------|-------------|-----------------------------------------------------|
# | Home   | Employees RE_<age>_<car>_<abo> (RE, RE_E/RE_M)   | Work        | FTE / Arbeitsplaetze SG_BES (FTE_E, FTE_M, EMPL_x)  |
# | Home   | Pupils R_PUPILS_* , students R_STUDENTS_*        | Education   | SG_PUPILS_I, SG_PUPILS_II, SG_STUDENTS, SCL_E_A     |
# | Home   | Population R_<age>_<car>_<abo>, HH types         | Shopping    | SG_VISITORS_SALES(_ST/_LT), EMPL_4 (retail)         |
# | Home   | Population R_<age>_<car>_<abo>, HH types         | Leisure     | SG_VISITORS_LEISURE/_CULTURE/_TOTAL, EMPL_5 (gastro)|


### Step 2: Load and combine data -----------------------------------------------

df_tripsCH    <- as_tibble(fread(file.path(path_data, "wegeinland.csv")))
df_hh         <- as_tibble(fread(file.path(path_data, "haushalte.csv")))
df_persons    <- as_tibble(fread(file.path(path_data, "zielpersonen.csv")))
Strukturdaten <- as.data.frame(read_parquet(file.path(path2, "Strukturgroessen.parquet")))
npvm_zones_ZH <- read_sf(file.path(path2, "NPVM2010MetroZH.shp"))

stopifnot("one target person per household expected" = !anyDuplicated(df_persons$HHNR))

# -- which trips start at home? ---------------------------------------------------
# (a) first trip of each complete Ausgang (AUSNR = -98 marks incomplete outings)
# (b) start coordinates within HOME_RADIUS_M of the home coordinates (WGS 84: X = lon, Y = lat)
dist_m <- function(lon1, lat1, lon2, lat2) {          # equirectangular approximation, fine < 1 km
  rad <- pi / 180
  6371000 * sqrt(((lon2 - lon1) * rad * cos((lat1 + lat2) / 2 * rad))^2 + ((lat2 - lat1) * rad)^2)
}
df_tripsCH <- df_tripsCH %>%
  left_join(df_hh %>% select(HHNR, W_X, W_Y), by = "HHNR") %>%
  group_by(HHNR, .data[[cfg$outing]]) %>%
  mutate(start_home_outing = .data[[cfg$outing]] > 0 & .data[[cfg$trip_no]] == min(.data[[cfg$trip_no]])) %>%
  ungroup() %>%
  mutate(coords_ok        = S_X > -990 & W_X > -990,
         start_home_coord = coords_ok & dist_m(S_X, S_Y, W_X, W_Y) <= HOME_RADIUS_M,
         start_home = case_when(HOME_START == "outing" ~ start_home_outing,
                                HOME_START == "coords" ~ start_home_coord,
                                HOME_START == "hinweg" ~ wzweck2 == 1,
                                TRUE                   ~ start_home_outing | start_home_coord),
         is_work = .data[[cfg$trip_purpose]] == cfg$purpose_work)

cat("\n--- Work trips: does the trip start at home? (outing rule vs coordinates) ---\n")
print(with(filter(df_tripsCH, is_work), table(outing = start_home_outing, coords = start_home_coord)))

# -- number of Home -> Work trips per person -------------------------------------
df_n_trips <- df_tripsCH %>%
  filter(is_work, start_home) %>%
  count(HHNR, name = "n_work")

# -- combine: persons + household + trip counts ----------------------------------
# Start from the PERSONS (left join), not from the trips: people who made no
# Home->Work trip on their reference day must stay in the data with n_work = 0,
# otherwise the zeros are lost and every rate is biased upwards.
df_trips <- df_persons %>%
  left_join(df_hh, by = "HHNR", suffix = c("", "_hh")) %>%
  left_join(df_n_trips, by = "HHNR") %>%
  mutate(n_work = replace_na(n_work, 0L))

cat("\nPersons:", nrow(df_trips), " | Home->Work trips:", sum(df_trips$n_work), "\n")


### Step 3: Construct (dummy) variables ------------------------------------------
# R builds the dummies itself from factors; the first level is the reference.

neg_to_na <- function(x) replace(x, !is.na(x) & x < 0, NA)
abo_flag  <- function(x) case_when(x == 1 ~ TRUE, x %in% c(2, -99) ~ FALSE, TRUE ~ NA)

df_trips <- df_trips %>%
  mutate(
    age       = neg_to_na(.data[[cfg$age]]),
    age_class = cut(age, breaks = c(-Inf, 17, 24, 44, 64, 74, Inf), labels = AGE_CLASSES),
    age_est   = factor(unname(AGE_MAP[as.character(age_class)]), levels = unique(AGE_MAP)),
    car = factor(case_when(.data[[cfg$car_avail]] %in% cfg$car_yes ~ "CAR",
                           .data[[cfg$car_avail]] %in% cfg$car_no  ~ "NOC",
                           TRUE ~ NA_character_), levels = c("NOC", "CAR")),
    ga      = abo_flag(.data[[cfg$abo_ga]]),
    verbund = abo_flag(.data[[cfg$abo_verbund]]),
    tc = factor(case_when(ga | verbund     ~ "TC",     # GA or Verbund = "TC" in the Strukturdaten
                          !ga & !verbund   ~ "NOTC",   # (Halbtax does NOT count as TC there)
                          TRUE ~ NA_character_), levels = c("NOTC", "TC")),
    urban = factor(neg_to_na(.data[[cfg$hh_urban]]), levels = 1:3,
                   labels = c("urban", "intermediate", "rural")),
    is_employed = .data[[cfg$employment]] %in% cfg$employed,
    is_weekday  = .data[[cfg$weekday]] %in% cfg$weekdays,
    # same key as in the Strukturdaten columns, e.g. "2544_CARTC"
    group_name  = paste0(age_class, "_", car, tc),
    group_est   = interaction(age_est, car, tc, sep = "_", drop = TRUE),     # pooled ages (16 cells)
    age_full    = age_class,                                                 # all 6 Strukturdaten ages
    group_full  = interaction(age_class, car, tc, sep = "_", drop = TRUE)    # the 24 Strukturdaten groups
  ) %>%
  mutate(age_est  = fct_relevel(age_est, "2544"),   # reference = largest age class
         age_full = fct_relevel(age_full, "2544"))

# -- estimation sample: employed persons (the Strukturdaten population RE_*) ----
df_sample <- df_trips %>%
  filter(is_employed,
         !ONLY_WEEKDAYS | is_weekday,
         !ONLY_CANTON_ZH | W_KANTON == 1)

#### Testing for NAs
model_vars <- c("n_work", "age_est", "car", "tc", "urban")
cat("\nNAs per model variable (employed sample):\n")
print(colSums(is.na(df_sample[, model_vars])))

# Delete NAs (complete cases on the model variables only)
df_non_na <- df_sample %>% drop_na(all_of(model_vars))
cat("Dropped", nrow(df_sample) - nrow(df_non_na), "of", nrow(df_sample), "persons with NAs\n")
print(colSums(is.na(df_non_na[, model_vars])))   # check again -> all 0

#### Groups matching the Strukturdaten
group_stats <- df_non_na %>%
  group_by(age_class, car, tc, group_name) %>%
  summarise(n = n(), mean = mean(n_work), var = var(n_work), .groups = "drop")
cat("\nObservations per Strukturdaten group (", nrow(group_stats), "of 24 groups present):\n")
print(group_stats, n = 30)
cat("\nObservations and trips per age class (separate vs pooled):\n")
print(df_non_na %>% group_by(age_full) %>% summarise(n = n(), trips = sum(n_work), mean = mean(n_work)))
print(df_non_na %>% group_by(age_est)  %>% summarise(n = n(), trips = sum(n_work), mean = mean(n_work)))


#### Sample flow: how many persons / trips survive each filter
steps <- list("All target persons (Zielpersonen)" = rep(TRUE, nrow(df_trips)),
              "employed (AMSTAT = 1)"             = df_trips$is_employed)
if (ONLY_WEEKDAYS)  steps[["reference day Monday-Friday"]] <- steps[[length(steps)]] & df_trips$is_weekday
if (ONLY_CANTON_ZH) steps[["living in canton ZH"]]        <- steps[[length(steps)]] & df_trips$W_KANTON %in% 1
steps[["no missing model variables (estimation sample)"]] <- df_trips$HHNR %in% df_non_na$HHNR
flow <- imap_dfr(steps, ~ tibble(step = .y, persons = sum(.x), home_work_trips = sum(df_trips$n_work[.x]),
                                 trips_per_person = sum(df_trips$n_work[.x]) / sum(.x)))
cat("\n--- Sample flow ---\n"); print(flow, width = Inf)
write_csv(flow, file.path(path_out, "sample_flow.csv"))


### Step 4: Model proof -----------------------------------------------------------
# Counts (0, 1, 2, ...) -> OLS is not appropriate (discrete, non-negative,
# heteroscedastic, skewed). Candidates: Poisson vs negative binomial.
# Decide with the data: compare mean and variance, Pearson dispersion of the
# Poisson model, and a likelihood-ratio test Poisson vs NB.

y <- df_non_na$n_work
proof <- tibble(n = length(y), median = median(y), mean = mean(y), var = var(y),
                var_to_mean = var(y) / mean(y))
cat("\n--- Dispersion check ---\n"); print(proof)
print(table(n_work = y))

f_test  <- n_work ~ age_est + car + tc
m_pois0 <- glm(f_test, family = poisson, data = df_non_na)
pearson_disp <- sum(residuals(m_pois0, type = "pearson")^2) / df.residual(m_pois0)

# glm.nb warns (theta -> Inf) when there is no overdispersion; that IS the result
m_nb0 <- withCallingHandlers(glm.nb(f_test, data = df_non_na),
                             warning = function(w) { message("glm.nb: ", conditionMessage(w)); invokeRestart("muffleWarning") })
lr_stat <- max(0, 2 * (as.numeric(logLik(m_nb0)) - as.numeric(logLik(m_pois0))))
lr_p    <- pchisq(lr_stat, df = 1, lower.tail = FALSE) / 2    # theta on boundary -> halve p

USE_NB <- pearson_disp > 1 && lr_p < 0.05
cat(sprintf("\nPearson dispersion (Poisson) = %.3f | NB theta = %.2f | LR Poisson vs NB = %.2f (p = %.3g)\n",
            pearson_disp, m_nb0$theta, lr_stat, lr_p))
cat(if (USE_NB) "=> Overdispersion: use NEGATIVE BINOMIAL (glm.nb)\n"
    else        "=> No overdispersion: NB collapses to Poisson -> use POISSON (glm)\n")

# ---- Fig 1: observed distribution vs Poisson / NB expectation ----------------
k_max <- max(3, max(y))
mu_p  <- fitted(m_pois0); mu_nb <- fitted(m_nb0)
dist_df <- tibble(k = 0:k_max) %>%
  mutate(Observed = map_dbl(k, ~ mean(y == .x)),
         Poisson  = map_dbl(k, ~ mean(dpois(.x, mu_p))),
         `Negative binomial` = map_dbl(k, ~ mean(dnbinom(.x, size = m_nb0$theta, mu = mu_nb))))
p_dist <- ggplot(dist_df, aes(k)) +
  geom_col(aes(y = Observed), fill = "grey70", width = 0.7) +
  geom_line(aes(y = Poisson, colour = "Poisson"), linewidth = 0.6) +
  geom_point(aes(y = Poisson, colour = "Poisson"), size = 2.5) +
  geom_line(aes(y = `Negative binomial`, colour = "Negative binomial"), linewidth = 0.6, linetype = "22") +
  geom_point(aes(y = `Negative binomial`, colour = "Negative binomial"), size = 2.5, shape = 17) +
  scale_colour_manual(values = c("Poisson" = col_1, "Negative binomial" = col_2), name = "Expected under") +
  scale_x_continuous(breaks = 0:k_max) +
  scale_y_continuous(labels = percent) +
  labs(title = "Home-to-Work trips per employed person and day",
       subtitle = sprintf("bars = observed (n = %s); mean = %.2f, variance = %.2f",
                          comma(length(y)), mean(y), var(y)),
       x = "Number of Home-to-Work trips", y = "Share of persons")
print(p_dist); save_fig(p_dist, "fig1_distribution_vs_poisson_nb")

# ---- Fig 2: mean-variance relation per Strukturdaten group --------------------
mv <- group_stats %>% filter(n >= 20)
p_mv <- ggplot(mv, aes(mean, var)) +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "Poisson: Var = E"), colour = "grey40") +
  stat_function(aes(linetype = "NB: Var = E + E^2/theta"), fun = function(m) m + m^2 / m_nb0$theta,
                colour = "grey40") +
  geom_point(aes(size = n), colour = col_1, alpha = 0.8) +
  scale_linetype_manual(values = c("Poisson: Var = E" = "solid", "NB: Var = E + E^2/theta" = "22"), name = NULL) +
  scale_size_area(max_size = 6, name = "persons in group") +
  labs(title = "Mean vs variance of Home-to-Work trips per person group",
       subtitle = "groups = age x car availability x PT subscription (n >= 20)",
       x = "Mean trips per person", y = "Variance") +
  theme(legend.box = "vertical")
print(p_mv); save_fig(p_mv, "fig2_mean_variance")


### Step 5: Estimate models --------------------------------------------------------

fit_count <- function(f) {
  if (USE_NB) suppressWarnings(glm.nb(f, data = df_non_na))
  else        glm(f, family = poisson, data = df_non_na)
}
forms <- list(
  M0 = n_work ~ 1,
  M1 = n_work ~ age_est,
  M2 = n_work ~ age_est + car,
  M3 = n_work ~ age_est + car + tc,
  M4 = n_work ~ age_est + car * tc,
  M5 = n_work ~ group_est,                        # one rate per cell (= cross-classification)
  M6 = n_work ~ age_est + car + tc + urban,       # + urban/rural type of the home municipality
  # same as M3 / M5 but with the 6 Strukturdaten age classes kept SEPARATE (no pooling)
  M3f = n_work ~ age_full + car + tc,
  M4f = n_work ~ age_full + car * tc,             # M4 with the 6 Strukturdaten age classes
  M5f = n_work ~ group_full                       # one rate per Strukturdaten group (template / TA approach)
)
models <- map(forms, fit_count)

# significance
for (m in names(models)[-1]) { cat("\n=================", m, "=================\n"); print(summary(models[[m]])) }

# comparison table: log-likelihood (higher = better), AIC/BIC (lower = better), McFadden R2
ll0 <- as.numeric(logLik(models$M0))
cmp <- imap_dfr(models, function(m, nm) tibble(
  model = nm, formula = deparse1(formula(m)),
  k = attr(logLik(m), "df"), logLik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m),
  McFadden_R2 = 1 - as.numeric(logLik(m)) / ll0,
  age_classes = if (any(c("age_full", "group_full") %in% all.vars(formula(m)))) "6 separate" else "4 pooled",
  inestimable_coefs = sum(is.na(coef(m)))))
cat("\n--- Model comparison ---\n"); print(cmp, width = Inf)
write_csv(cmp, file.path(path_out, "model_comparison.csv"))

# Compare nested models (likelihood-ratio tests). Order matters!
cat("\n--- LR tests: M0 < M1 < M2 < M3 < M4 ---\n")
print(do.call(anova, c(unname(models[c("M0", "M1", "M2", "M3", "M4")]), list(test = "Chisq"))))
cat("\n--- LR test: M4 vs M5 (full cell model) ---\n"); print(anova(models$M4, models$M5, test = "Chisq"))
cat("\n--- LR test: M3 vs M6 (+ urban type) ---\n");     print(anova(models$M3, models$M6, test = "Chisq"))

# Pooled vs separate age classes: the pooled model is the separate one with the
# constraints "rate(<18) = rate(18-24)" and "rate(75+) = rate(65-74)", so the two
# are nested -> LR test. H0: pooling loses nothing.
cat("\n--- Pooled vs separate age classes ---\n")
cat("Additive models  M3 (pooled) vs M3f (separate):\n"); print(anova(models$M3, models$M3f, test = "Chisq"))
cat("Interaction      M4 (pooled) vs M4f (separate):\n"); print(anova(models$M4, models$M4f, test = "Chisq"))
cat("Group models     M5 (pooled) vs M5f (separate, 24 groups):\n"); print(anova(models$M5, models$M5f, test = "Chisq"))
print(cmp %>% filter(model %in% c("M3", "M3f", "M4", "M4f", "M5", "M5f")) %>% select(model, age_classes, k, logLik, AIC, BIC))

# A model can only be applied if it yields a sensible rate for EVERY Strukturdaten group:
#  - cell models (M5, M5f) need every cell observed and with at least one trip
#    (a cell with 0 trips gets rate 0 = coefficient -> -Inf)
#  - additive models need at least one trip in every level of every factor
has_trips <- function(v) all(tapply(df_non_na$n_work, df_non_na[[v]], sum) > 0)
cell_ok   <- function(v, n_cells) nlevels(df_non_na[[v]]) == n_cells && has_trips(v)
ok <- c(M5  = cell_ok("group_est", length(unique(AGE_MAP)) * 4),
        M5f = cell_ok("group_full", 24),
        M3f = has_trips("age_full"),
        M4f = has_trips("age_full"))
# M6 needs a valid urban/rural type (1, 2 or 3) in EVERY zone. The Strukturdaten were
# aggregated from NPVM 2017 to 2010 zones, so a categorical column can be non-integer or NA.
sl3 <- suppressWarnings(as.numeric(as.character(Strukturdaten$ID_SL3_2020)))
ok["M6"] <- length(sl3) > 0 && all(sl3 %in% 1:3)
if (!ok["M6"]) cat("M6 not applicable: ID_SL3_2020 is not 1/2/3 in", sum(!(sl3 %in% 1:3)), "zones\n")
cat("\nApplicable to all 24 groups?\n"); print(ok)
cat("Groups with < 30 persons:", paste(group_stats$group_name[group_stats$n < 30], collapse = ", "), "\n")

# choose the final model: lowest AIC among models that can be applied to every zone/group
# (M5 is only applicable if every age x car x PT cell occurs in the sample)
applicable <- cmp %>% filter(model != "M0", inestimable_coefs == 0,
                             !(model %in% names(ok)) | ok[model] %in% TRUE)
final_name <- if (is.null(FINAL_MODEL)) applicable$model[which.min(applicable$AIC)] else FINAL_MODEL
final      <- models[[final_name]]
cat("\n>>> Final model:", final_name, ":", deparse1(formula(final)), "\n")

# -- Final model: re-estimate the selected specification WITH the MZMV person
#    weights (WP) so that the rates represent the population. Model selection
#    above stays unweighted (LR tests / AIC / BIC require a true likelihood).
if (!requireNamespace("survey", quietly = TRUE)) install.packages("survey")
final_unw <- final
if (inherits(final_unw, "negbin")) stop("Final model is NB: weighted NB not implemented here.")
des_w <- survey::svydesign(ids = ~1, weights = as.formula(paste0("~", cfg$weight)),
                           data = df_non_na)
final <- survey::svyglm(formula(final_unw), design = des_w, family = quasipoisson())
cat("\n>>> Final model", final_name, "re-estimated with person weights", cfg$weight, "\n")

r_u <- fitted(final_unw); r_w <- fitted(final)
cat(sprintf("Weighted (final) vs unweighted fit: predicted rates differ by %.1f%% on average (max %.1f%%)\n",
            100 * mean(abs(r_w / r_u - 1)), 100 * max(abs(r_w / r_u - 1))))
cat("\n--- Final model: unweighted vs weighted ---\n")
print(round(data.frame(IRR_unw = exp(coef(final_unw)), p_unw = summary(final_unw)$coefficients[, 4],
                       IRR_w   = exp(coef(final)),     p_w   = summary(final)$coefficients[, 4]), 3))

# -- LaTeX tables for the report (booktabs) --------------------------------------
if (requireNamespace("knitr", quietly = TRUE)) {
  # each file is a complete table environment with \label{tab:<name>}, e.g. \ref{tab:sample_flow}
  tex <- function(df, file, digits, caption)
    writeLines(knitr::kable(df, format = "latex", booktabs = TRUE, digits = digits, caption = caption,
                            label = sub("^tab_|\\.tex$", "", sub("\\.tex$", "", file)), linesep = ""),
               file.path(path_out, file))
  tex(cmp %>% select(model, formula, age_classes, k, logLik, AIC, BIC, McFadden_R2),
      "tab_model_comparison.tex", c(0, 0, 0, 0, 1, 1, 1, 4), "Model comparison (Home-to-Work trips per person)")
  sc <- summary(final)$coefficients
  ci <- confint.default(final)
  tex(tibble(Term = rownames(sc), Estimate = sc[, 1], `Std. error` = sc[, 2],
             IRR = exp(sc[, 1]), `IRR 2.5%` = exp(ci[, 1]), `IRR 97.5%` = exp(ci[, 2]), `p-value` = sc[, 4]),
      "tab_final_model.tex", c(0, 3, 3, 3, 3, 3, 4), paste0("Final model ", final_name))
  tex(flow, "tab_sample_flow.tex", c(0, 0, 0, 3), "Sample flow")
  tex(proof, "tab_dispersion.tex", c(0, 0, 3, 3, 3), "Dispersion of Home-to-Work trips per person")
}

# ---- the 24 Strukturdaten groups as model input (used here and in Step 6) ------
lev <- function(v) levels(df_non_na[[v]])
grid24 <- expand_grid(age_class = AGE_CLASSES, car = c("NOC", "CAR"), tc = c("NOTC", "TC")) %>%
  mutate(age_est    = factor(unname(AGE_MAP[age_class]), levels = lev("age_est")),
         age_full   = factor(age_class, levels = lev("age_full")),
         car        = factor(car, levels = lev("car")),
         tc         = factor(tc,  levels = lev("tc")),
         group_est  = factor(paste(age_est, car, tc, sep = "_"), levels = lev("group_est")),
         group_full = factor(paste(age_class, car, tc, sep = "_"), levels = lev("group_full")),
         group_name = paste0(age_class, "_", car, tc))
rate_of <- function(m, nd = grid24) {
  out <- rep(NA_real_, nrow(nd))
  okrow <- complete.cases(nd[, intersect(all.vars(formula(m))[-1], names(nd))])
  out[okrow] <- predict(m, newdata = nd[okrow, ], type = "response")
  out
}

# ---- Fig 3b: group rates, pooled vs separate age classes ------------------------
# The separate 24-group model M5f reproduces each group's sample mean exactly,
# so "observed mean +- 95% CI" IS model M5f. The question is whether the
# pooled / additive models stay inside those intervals.
obs24 <- df_non_na %>% group_by(group_name) %>%
  summarise(n = n(), mean = mean(n_work), se = sqrt(var(n_work) / n()), .groups = "drop")
cmp24 <- grid24 %>% select(age_class, car, tc, group_name) %>%
  left_join(obs24, by = "group_name") %>%
  mutate(`M5 (pooled groups)` = rate_of(models$M5),
         `M3 (additive, pooled ages)` = rate_of(models$M3),
         `M3f (additive, separate ages)` = rate_of(models$M3f),
         panel = factor(paste(ifelse(car == "CAR", "Car", "No car"), ifelse(tc == "TC", "PT subscr.", "no PT subscr."), sep = ", "),
                        levels = c("No car, no PT subscr.", "No car, PT subscr.", "Car, no PT subscr.", "Car, PT subscr.")))
cmp24_long <- cmp24 %>%
  pivot_longer(c(`M5 (pooled groups)`, `M3 (additive, pooled ages)`, `M3f (additive, separate ages)`),
               names_to = "model", values_to = "rate")
p_grp <- ggplot(cmp24, aes(age_class)) +
  geom_errorbar(aes(ymin = pmax(mean - 1.96 * se, 0), ymax = mean + 1.96 * se), width = 0.25, colour = "grey55") +
  geom_point(aes(y = mean), colour = "grey25", size = 2) +
  geom_text(aes(y = 0, label = paste0("n=", n)), size = 2.3, colour = "grey45", vjust = 0) +
  geom_point(data = cmp24_long, aes(y = rate, colour = model, shape = model), size = 2.3,
             position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c(col_1, col_2, col_3), name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  facet_wrap(~ panel, nrow = 2) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = "Trip rate per Strukturdaten group: separate vs pooled age classes",
       subtitle = "grey = sample mean +- 95% CI (identical to the 24-group model M5f); n = persons in group",
       x = "Age class", y = "Home-to-Work trips per person and day")
print(p_grp); save_fig(p_grp, "fig3b_group_rates_pooled_vs_separate", w = 18, h = 13)

# ---- Fig 3: incidence-rate ratios of the final model ---------------------------
irr <- tibble(term = names(coef(final)), est = coef(final),
              lo = confint.default(final)[, 1], hi = confint.default(final)[, 2]) %>%
  filter(term != "(Intercept)") %>%
  mutate(across(c(est, lo, hi), exp),
         term = recode(term, age_est0024 = "Age < 25 (vs 25-44)", age_est4564 = "Age 45-64 (vs 25-44)",
                       age_est65XX = "Age 65+ (vs 25-44)", carCAR = "Car available",
                       tcTC = "PT subscription (GA/Verbund)", `carCAR:tcTC` = "Car x PT subscription",
                       urbanintermediate = "Intermediate municipality (vs urban)",
                       urbanrural = "Rural municipality (vs urban)",
                       age_full0017 = "Age < 18 (vs 25-44)", age_full1824 = "Age 18-24 (vs 25-44)",
                       age_full4564 = "Age 45-64 (vs 25-44)", age_full6574 = "Age 65-74 (vs 25-44)",
                       age_full75XX = "Age 75+ (vs 25-44)"),
         term = fct_reorder(term, est))
p_irr <- ggplot(irr, aes(est, term)) +
  geom_vline(xintercept = 1, colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, colour = col_1) +
  geom_point(size = 2.5, colour = col_1) +
  scale_x_log10() +
  labs(title = paste0("Final model ", final_name, ": incidence-rate ratios"),
       subtitle = "exp(coefficient) with 95% CI; > 1 = more Home-to-Work trips",
       x = "Incidence-rate ratio (log scale)", y = NULL)
print(p_irr); save_fig(p_irr, "fig3_irr_final_model")

# ---- Fig 4: residual check - observed vs predicted mean per group ---------------
calib <- df_non_na %>% mutate(pred = fitted(final)) %>%
  group_by(group_name, car, tc) %>%
  summarise(n = n(), obs = weighted.mean(n_work, .data[[cfg$weight]]), pred = mean(pred), .groups = "drop") %>%
  filter(n >= 20) %>% mutate(cartc = factor(paste(car, tc, sep = "_"), levels = names(cols_cartc)))
p_cal <- ggplot(calib, aes(pred, obs)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50") +
  geom_point(aes(size = n, colour = cartc), alpha = 0.85) +
  scale_colour_manual(values = cols_cartc, name = NULL,
                      labels = c("No car, no PT subscr.", "No car, PT subscr.",
                                 "Car, no PT subscr.", "Car, PT subscr.")) +
  scale_size_area(max_size = 6, name = "persons") +
  guides(colour = guide_legend(nrow = 2), size = guide_legend(nrow = 2)) +
  labs(title = "(a) Calibration",
       subtitle = "on the diagonal = well calibrated", x = "Predicted mean", y = "Observed mean")
p_res <- tibble(fit = fitted(final), res = residuals(final, type = "pearson")) %>%
  mutate(bin = ntile(fit, 20)) %>% group_by(bin) %>%
  summarise(fit = mean(fit), res = mean(res), .groups = "drop") %>%
  ggplot(aes(fit, res)) + geom_hline(yintercept = 0, colour = "grey50") +
  geom_point(colour = col_1, size = 2) +
  labs(title = "(b) Binned Pearson residuals", subtitle = "mean over 20 bins of the fitted value",
       x = "Fitted value", y = "Mean Pearson residual")
p_check <- (p_cal + p_res) +
  plot_layout(guides = "collect") +
  plot_annotation(title = paste0("Residual checks, final model ", final_name)) &
  theme(legend.position = "bottom", legend.box = "horizontal")
print(p_check); save_fig(p_check, "fig4_residual_checks", w = 22, h = 11)


### Step 6: Apply the model to the region ----------------------------------------

#### Prepare: trip rate for each of the 24 Strukturdaten groups
grid <- grid24
uses_urban <- "urban" %in% all.vars(formula(final))
if (uses_urban) grid <- expand_grid(grid, urban = factor(lev("urban"), levels = lev("urban")))
grid$rate <- as.numeric(predict(final, newdata = grid, type = "response"))   # trips per employee and day

#### Plausibility check: rates per group
p_rates <- grid %>%
  { if (uses_urban) filter(., urban == "urban") else . } %>%
  mutate(cartc = factor(paste(car, tc, sep = "_"), levels = names(cols_cartc))) %>%
  ggplot(aes(age_class, rate, fill = cartc)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = cols_cartc, name = NULL,
                    labels = c("No car, no PT subscr.", "No car, PT subscr.",
                               "Car, no PT subscr.", "Car, PT subscr.")) +
  labs(title = "Predicted Home-to-Work trip rates per Strukturdaten group",
       subtitle = paste0("final model ", final_name, if (uses_urban) " (urban municipalities)" else ""),
       x = "Age class", y = "Trips per employed person and day")
print(p_rates); save_fig(p_rates, "fig5_trip_rates_per_group")
write_csv(grid %>% mutate(across(where(is.factor), as.character)), file.path(path_out, "trip_rates_per_group.csv"))

#### Apply
zone_id <- "ID_NPVM"
pat     <- "RE_(0017|1824|2544|4564|6574|75XX)_(CAR|NOC)(NOTC|TC)"   # excludes RE_E_* / RE_M_*
grp_cols <- grep(pat, names(Strukturdaten), value = TRUE)
stopifnot("expected 24 employee group columns" = length(grp_cols) == 24)
fte_col  <- grep("^FTE$", names(Strukturdaten), value = TRUE)
sl3_col  <- grep("^ID_SL3_2020$", names(Strukturdaten), value = TRUE)

emp_long <- Strukturdaten %>%
  select(all_of(c(zone_id, sl3_col, grp_cols))) %>%
  pivot_longer(all_of(grp_cols), names_to = "col", values_to = "n_employees") %>%
  mutate(m = str_match(col, pat), group_name = paste0(m[, 2], "_", m[, 3], m[, 4])) %>%
  select(-m)
if (uses_urban) {
  emp_long <- emp_long %>%
    mutate(urban = factor(.data[[sl3_col]], levels = 1:3, labels = c("urban", "intermediate", "rural")))
  emp_long <- left_join(emp_long, grid %>% select(group_name, urban, rate), by = c("group_name", "urban"))
} else {
  emp_long <- left_join(emp_long, grid %>% select(group_name, rate), by = "group_name")
}
if (anyNA(emp_long$rate)) {
  cat("\nGroups without a predicted rate:\n")
  print(emp_long %>% filter(is.na(rate)) %>% count(group_name, across(any_of("urban")), name = "zones"))
  cat("\nRates per group from the final model:\n")
  print(grid %>% select(any_of(c("group_name", "urban", "rate"))), n = 100)
  stop("Some groups have no trip rate (see above). Final model: ", final_name)
}

zones <- emp_long %>%
  group_by(.data[[zone_id]]) %>%
  summarise(employees = sum(n_employees), trips_work = sum(n_employees * rate), .groups = "drop") %>%
  left_join(Strukturdaten %>% select(all_of(c(zone_id, fte_col))), by = zone_id) %>%
  rename(attractor_work = all_of(fte_col)) %>%
  # balance attraction to production (closed region: "there is only Zurich")
  mutate(attraction_work = attractor_work * sum(trips_work) / sum(attractor_work),
         rate_per_employee = trips_work / employees)

cat("\n--- Plausibility ---\n")
cat("Employees in the 24 groups:", comma(sum(zones$employees)),
    if ("RE" %in% names(Strukturdaten)) paste0(" | column RE: ", comma(sum(Strukturdaten$RE))) else "", "\n")
cat(sprintf("Produced H->W trips: %s | per employee: %.3f (sample mean: %.3f)\n",
            comma(round(sum(zones$trips_work))), sum(zones$trips_work) / sum(zones$employees), mean(y)))
cat(sprintf("FTE (raw attractor): %s | balancing factor trips/FTE = %.3f\n",
            comma(round(sum(zones$attractor_work))), sum(zones$trips_work) / sum(zones$attractor_work)))

# -- Sensitivity: does pooling the age classes change the zone results? ----------
edge_share <- with(emp_long, sum(n_employees[str_detect(group_name, "^(0017|75XX)")]) / sum(n_employees))
cat(sprintf("\nShare of employed residents aged < 18 or 75+ in the region: %.1f%%\n", 100 * edge_share))
sens_models <- intersect(c("M3", "M3f", "M5", "M5f"), applicable$model)
sens <- map_dfc(set_names(sens_models), function(nm) {
  emp_long %>% select(all_of(zone_id), group_name, n_employees) %>%
    left_join(tibble(group_name = grid24$group_name, r = rate_of(models[[nm]])), by = "group_name") %>%
    group_by(.data[[zone_id]]) %>% summarise(p = sum(n_employees * r), .groups = "drop") %>% pull(p)
})
cat("Total produced trips per model:\n"); print(round(colSums(sens)))
for (pr in list(c("M3", "M3f"), c("M5", "M5f"))) if (all(pr %in% names(sens))) {
  d <- sens[[pr[2]]] / sens[[pr[1]]] - 1
  cat(sprintf("%s (pooled) vs %s (separate): zone production differs by %.2f%% on average, at most %.2f%%\n",
              pr[1], pr[2], 100 * mean(abs(d), na.rm = TRUE), 100 * max(abs(d), na.rm = TRUE)))
}

# add to the NPVM zones: the zone ID in NPVM2010MetroZH.shp is ID_Zo (= ID_NPVM in the Strukturdaten)
shp_id <- "ID_Zo"
match_share <- if (shp_id %in% names(npvm_zones_ZH))
  mean(as.character(npvm_zones_ZH[[shp_id]]) %in% as.character(zones[[zone_id]])) else 0
cat(sprintf("Shapefile zones matched by ID (%s = %s): %.1f%%\n", shp_id, zone_id, 100 * match_share))
if (match_share > 0.95) {
  zones_j <- zones %>% mutate(.key = as.character(.data[[zone_id]]))
  npvm_zones_ZH <- npvm_zones_ZH %>% mutate(.key = as.character(.data[[shp_id]])) %>%
    left_join(zones_j %>% select(-all_of(zone_id)), by = ".key") %>% select(-.key)
  if (anyNA(npvm_zones_ZH$trips_work)) warning("Some zones did not match by ID.")
} else {
  # fallback as in the template: same row order in Strukturdaten and shapefile
  warning("Zone IDs do not match: binding by row order (as in the template).")
  stopifnot(nrow(npvm_zones_ZH) == nrow(zones))
  zones_ordered <- zones[match(Strukturdaten[[zone_id]], zones[[zone_id]]), ]
  npvm_zones_ZH <- st_sf(bind_cols(st_drop_geometry(npvm_zones_ZH), zones_ordered),
                         geometry = st_geometry(npvm_zones_ZH))
}
# alignment check: FTE (Strukturdaten) and ZoneVZA (shapefile) both count full-time
# equivalents per zone, so they must be strongly correlated if the zones line up
if ("ZoneVZA" %in% names(npvm_zones_ZH))
  cat(sprintf("Correlation FTE (Strukturdaten) vs ZoneVZA (shapefile): %.3f\n",
              cor(npvm_zones_ZH$attractor_work, npvm_zones_ZH$ZoneVZA, use = "complete.obs")))
npvm_zones_ZH$area_km2 <- as.numeric(st_area(npvm_zones_ZH)) / 1e6
npvm_zones_ZH <- npvm_zones_ZH %>%
  mutate(
    prod_density = trips_work / area_km2,
    attr_density = attraction_work / area_km2,
    balance = trips_work - attraction_work)

#### Visualize
map_seq <- function(var, title, fill_lab) {
  ggplot(npvm_zones_ZH) +
    geom_sf(aes(fill = .data[[var]]), colour = "white", linewidth = 0.05) +
    scale_fill_gradient(low = col_seq_lo, high = col_seq_hi, trans = "sqrt", labels = comma,
                        name = fill_lab) +
    labs(title = title) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
          legend.key.width = unit(1.4, "cm"))
}
# Trip production per zone
p_prod <- map_seq("trips_work", "Production: Home-to-Work trips", "trips / day")
# Trip attraction per zone (FTE, balanced to the total production)
p_attr <- map_seq("attraction_work", "Attraction: Home-to-Work trips (FTE, balanced)", "trips / day")
p_maps <- p_prod + p_attr
print(p_maps); save_fig(p_maps, "fig6_maps_production_attraction", w = 20, h = 11)

# Same per km2 (zones differ a lot in size; densities are easier to compare)
p_dens <- map_seq("prod_density", "Production density", "trips / km2") +
  map_seq("attr_density", "Attraction density", "trips / km2")
print(p_dens); save_fig(p_dens, "fig7_maps_densities", w = 20, h = 11)

# Net balance: residential zones (P > A) vs employment centres (A > P)
lim <- max(abs(npvm_zones_ZH$balance), na.rm = TRUE)
p_bal <- ggplot(npvm_zones_ZH) +
  geom_sf(aes(fill = balance), colour = "white", linewidth = 0.05) +
  scale_fill_gradient2(low = col_2, mid = col_mid, high = col_1, midpoint = 0,
                       limits = c(-lim, lim), labels = comma, name = "production - attraction") +
  labs(title = "Net balance of Home-to-Work trips",
       subtitle = "blue = net producer (residential), orange = net attractor (jobs)") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        legend.key.width = unit(1.6, "cm"))
print(p_bal); save_fig(p_bal, "fig8_map_balance", w = 12, h = 12)

#### Save results
write_sf(npvm_zones_ZH, file.path(path_out, "trip_generation_home_work.gpkg"), delete_dsn = TRUE)
write_csv(st_drop_geometry(npvm_zones_ZH), file.path(path_out, "trip_generation_home_work_zones.csv"))
cat("\nResults written to", path_out, "\n")





### Step 7: Weighted vs unweighted M4 (Table tab:weighted, Discussion) -----------
# Compares the final (weighted) M4 with its unweighted version and checks the
# group calibration. Prints aggregates only; run after Step 6.

# ---- 0. Objects from the Part 2.1 script ------------------------------
dat   <- df_non_na            # estimation sample (20'892 persons)
m4    <- models$M4            # unweighted M4: n_work ~ age_est + car * tc
y_col <- "n_work"
dat$w <- as.numeric(dat[[cfg$weight]])   # WP

# employed residents per pooled model group (age_est x car x tc), whole study area
strd_groups <- emp_long %>%
  group_by(group_name) %>%
  summarise(N = sum(n_employees), .groups = "drop") %>%
  left_join(grid24 %>% select(group_name, age_est, car, tc), by = "group_name") %>%
  group_by(age_est, car, tc) %>%
  summarise(N = sum(N), .groups = "drop")

# ---- 1. What does WP look like? ------------------------------------------
cat("\n==== WP ====\n")
print(summary(dat$w))
cat("sum WP:", round(sum(dat$w)), "  n:", nrow(dat), "\n")
cat("mean trips/person  unweighted:", round(mean(dat[[y_col]]), 3),
    "  weighted:", round(weighted.mean(dat[[y_col]], dat$w), 3), "\n")

# ---- 2. Weighted M4 (design-based standard errors) --------------------------
des_w <- survey::svydesign(ids = ~1, weights = ~w, data = dat)
m4w   <- survey::svyglm(formula(m4), design = des_w, family = quasipoisson())

cmp_w <- data.frame(
  est_unw = coef(m4),  se_unw = sqrt(diag(vcov(m4))),
  est_w   = coef(m4w), se_w   = sqrt(diag(vcov(m4w))))
cmp_w$IRR_unw <- exp(cmp_w$est_unw)
cmp_w$IRR_w   <- exp(cmp_w$est_w)
cmp_w$p_unw   <- summary(m4)$coefficients[, 4]
cmp_w$p_w     <- summary(m4w)$coefficients[, 4]
cat("\n==== M4 coefficients: unweighted vs weighted ====\n")
print(round(cmp_w, 3))

# ---- 3. Fitted rates per pooled group ---------------------------------------
vars_w <- all.vars(formula(m4))[-1]
grid_w <- unique(as.data.frame(dat[, vars_w]))
grid_w <- grid_w[do.call(order, grid_w), , drop = FALSE]
grid_w$rate_unw   <- as.numeric(predict(m4,  newdata = grid_w, type = "response"))
grid_w$rate_w     <- as.numeric(predict(m4w, newdata = grid_w, type = "response"))
grid_w$change_pct <- 100 * (grid_w$rate_w / grid_w$rate_unw - 1)
cat("\n==== Fitted trips/person per group ====\n")
print(grid_w, digits = 3, row.names = FALSE)

# ---- 4. Calibration: observed vs predicted group means (unweighted M4) --------
key_w <- interaction(as.data.frame(dat[, vars_w]), drop = TRUE, sep = " | ")
fit_w <- fitted(m4)
cal_w <- data.frame(
  n            = as.vector(tapply(dat[[y_col]], key_w, length)),
  observed     = as.vector(tapply(dat[[y_col]], key_w, mean)),
  predicted    = as.vector(tapply(fit_w, key_w, mean)),
  obs_weighted = as.vector(tapply(dat[[y_col]] * dat$w, key_w, sum) /
                             tapply(dat$w, key_w, sum)),
  row.names = levels(key_w))
cal_w$obs_minus_pred <- cal_w$observed - cal_w$predicted
cal_w$pearson_z <- with(cal_w, (observed - predicted) / sqrt(predicted / n))
cat("\n==== Calibration by group (unweighted M4) ====\n")
print(round(cal_w, 3))

# ---- 5. Regional totals with both rate sets -----------------------------------
g_w <- merge(as.data.frame(strd_groups), grid_w, by = vars_w)
cat("\n==== Regional totals ====\n")
cat("groups matched:", nrow(g_w), "of", nrow(strd_groups), "\n")
cat("sum N (employed residents):", round(sum(g_w$N)), "\n")
cat("sum P unweighted:", round(sum(g_w$N * g_w$rate_unw)),
    "  rate per employed resident:", round(sum(g_w$N * g_w$rate_unw) / sum(g_w$N), 3), "\n")
cat("sum P weighted:  ", round(sum(g_w$N * g_w$rate_w)),
    "  rate per employed resident:", round(sum(g_w$N * g_w$rate_w) / sum(g_w$N), 3), "\n")

cat("\n==== Weighted final model ====\n")
sc <- summary(final)$coefficients; ci <- confint.default(final)
print(round(data.frame(est = sc[, 1], se = sc[, 2], IRR = exp(sc[, 1]),
                       lo = exp(ci[, 1]), hi = exp(ci[, 2]), p = sc[, 4]), 3))
cat("\n==== Calibration, 16 groups (weighted observed vs weighted model) ====\n")
df_non_na %>% mutate(pred = fitted(final), w = .data[[cfg$weight]]) %>%
  group_by(age_est, car, tc) %>%
  summarise(n = n(), obs_w = weighted.mean(n_work, w), pred = mean(pred), .groups = "drop") %>%
  mutate(diff = obs_w - pred) %>% print(n = 16)
cat("\n==== Totals ====\n")
cat("weighted sample mean:", round(weighted.mean(df_non_na$n_work, df_non_na[[cfg$weight]]), 3), "\n")
cat("sum P:", round(sum(zones$trips_work)), " | employed:", round(sum(zones$employees)),
    " | rate:", round(sum(zones$trips_work) / sum(zones$employees), 3), "\n")