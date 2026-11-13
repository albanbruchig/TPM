# ==============================================================================
#  2.1 Trip Generation Exercise -- template-aligned version
#
#  Based on: TPM_2_1_trip_generation-2.R
#  Organized according to: 2_1_Trip Generation_STUDENTS(1).R
#
#  Task: Home -> Work trip generation (MZMV 2021 -> Zurich NPVM zones).
#  Reproducible revision, 13 November 2026:
#  - uses the verified local paths and original TPM analytical workflow;
#  - AUTOMATICALLY selects the lowest-AIC model that is estimable AND
#    predictable from the available regional covariates; no hard-coded M4;
#  - refits that selected model with MZMV person weights (WP);
#  - handles geometry-only shapefiles and records unverified row-order joins.
#  No input MZMV microdata are written to the output directory.
#
#  Steps: 0. Packages and paths; 1. Hypotheses; 2. Load and combine data;
#         3. Dummies and groups; 4. Model proof; 5. Estimate models;
#         6. Apply the model to the region.
#
#  Check the local paths and MZMV coding before running.
# ==============================================================================


### Step 0: Load packages and define base paths ----------------------------------

#### Libraries

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(arrow)
  library(sf)
  library(MASS)
  library(scales)
  library(patchwork)
  library(survey)
})
# Always use dplyr::select explicitly; MASS also exports dplyr::select().

#### Paths

# Verified local folder structure (macOS); edit path_base if moving the project.
path_base <- "/Users/aliozdemir/Documents/ETH BAUG/Bauingenieurwissenschaften/Master/3. Semester/Transport Planning Methods"
path_data <- file.path(path_base, "Materials for descriptive_analysis-20260930", "MZMV_Data_protect")
path2 <- file.path(path_base, "Materials for Trip Generation")
path_zones <- path_base
path_out <- file.path(path2, "output_2_1")
dir.create(path_out, showWarnings = FALSE, recursive = TRUE)

# Data-driven model selection: always AIC among regionally applicable models.
# This is NOT a preferred model name: M4, M6, etc. can win depending on data.
MODEL_CRITERION <- "AIC"
# The course-provided shapefile has only .shp and .shx (no zone-ID .dbf).
# Allow the original template's row-order binding, but flag it as UNVERIFIED.
ALLOW_UNVERIFIED_ROW_JOIN <- TRUE
# MZMV uses urban typology 2012 while this zonal column is from 2020;
# using it is a documented cross-year approximation only when all zone
# values really are the three valid classes 1, 2 and 3.
URBAN_REGION_COLUMN <- "ID_SL3_2020"

#### Settings and configuration (retained from TPM)

# ---- CONFIG: MZMV 2021 variable names / codes --------------------------------
# !!! Verify each line against the codebooks of wegeinland / zielpersonen.
#     The script prints frequency tables of every coded variable in Step 2 so
#     you can see immediately whether a code is off.
# (checked against the MZMV 2021 codebooks of Wegeinland, Zielpersonen, Haushalte)
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
  # only used for exploratory plots (not in the Strukturdaten -> not in the applied model)
  workload     = "ERWERB",                             # 1 Vollzeit, 2 Teilzeit, 9 erwerbstaetig (unbekannt)
  home_office  = "f81300",                             # Arbeit zu Hause: 1 Ja, 2 Manchmal, 3 Nein
  weight       = "WP",                                 # person weight
  dep_time     = "f51100"                              # Wegeinland: departure time, minutes after midnight
)
# How to decide that a work trip STARTS AT HOME (wzweck2 cannot do this: 1 = "Hinweg"
# only means "not heading home", e.g. a trip lunch -> work is also a Hinweg):
#   "outing" = first trip of a complete Ausgang (AUSNR > 0)
#   "coords" = trip start lies within HOME_RADIUS_M of the home address (S_X/S_Y vs W_X/W_Y)
#   "either" = one of the two (default; the script prints how well they agree)
#   "hinweg" = simple version: every work trip with wzweck2 == 1 (also counts e.g. lunch -> work)
HOME_START    <- "either"
HOME_RADIUS_M <- 200

ONLY_WEEKDAYS  <- TRUE    # Strukturdaten / NPVM describe an average working day
ONLY_CANTON_ZH <- FALSE   # TRUE = estimate on residents of canton ZH only (W_KANTON == 1)
# No FINAL_MODEL override: selection is always determined by the data.

# Age classes as in the Strukturdaten (RE_<age>_<car><abo>)
AGE_CLASSES <- c("0017", "1824", "2544", "4564", "6574", "75XX")
# Age classes used for ESTIMATION. Employed persons < 18 and > 74 are rare in
# the MZMV, so they are pooled with their neighbour (check the counts printed in
# Step 3 and change the mapping if you have enough observations).
AGE_MAP <- c("0017" = "0024", "1824" = "0024", "2544" = "2544",
             "4564" = "4564", "6574" = "65XX", "75XX" = "65XX")

#### Plot settings and figure-export helper

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
  ggsave(file.path(path_out, paste0(name, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(path_out, paste0(name, ".pdf")), p, width = w, height = h, units = "cm")
}


### Step 1: Hypothesis ------------------------------------------
# (in the report) Origin side = person attributes in the Strukturdaten,
#                 destination side = zonal attractors in the Strukturdaten
#
# | Origin | Production (Strukturdaten)                       | Destination | Attraction (Strukturdaten)                          |
# |--------|--------------------------------------------------|-------------|-----------------------------------------------------|
# | Home   | Employees RE_<age>_<car>_<abo> (RE, RE_E/RE_M)   | Work        | FTE / Arbeitsplaetze SG_BES (FTE_E, FTE_M, EMPL_x)  |
# | Home   | Pupils R_PUPILS_* , students R_STUDENTS_*        | Education   | SG_PUPILS_I, SG_PUPILS_II, SG_STUDENTS, SCL_E_A     |
# | Home   | Population R_<age>_<car>_<abo>, HH types         | Shopping    | SG_VISITORS_SALES(_ST/_LT), EMPL_4 (retail)         |
# | Home   | Population R_<age>_<car>_<abo>, HH types         | Leisure     | SG_VISITORS_LEISURE/_CULTURE/_TOTAL, EMPL_5 (gastro)|


### Step 2: Load and Combine Data ------------------------------------------------

#### Load

input_files <- c(file.path(path_data, c("wegeinland.csv", "haushalte.csv", "zielpersonen.csv")),
                 file.path(path2, "Strukturgroessen.parquet"),
                 file.path(path_zones, "NPVM2010MetroZH.shp"))
if (any(!file.exists(input_files))) {
  stop("Missing input files:\n", paste(input_files[!file.exists(input_files)], collapse = "\n"))
}
# IMPORTANT: follow ETH data-protection requirements concerning the protected
# MZMV archive; this code retains the existing direct-CSV workflow.
df_tripsCH <- tibble::as_tibble(data.table::fread(file.path(path_data, "wegeinland.csv")))
df_hh <- tibble::as_tibble(data.table::fread(file.path(path_data, "haushalte.csv")))
df_persons <- tibble::as_tibble(data.table::fread(file.path(path_data, "zielpersonen.csv")))
Strukturdaten <- as.data.frame(arrow::read_parquet(file.path(path2, "Strukturgroessen.parquet")))
npvm_zones_ZH <- sf::read_sf(file.path(path_zones, "NPVM2010MetroZH.shp"))

# The supplied shapefile has no .prj, although its metre-valued coordinates
# are consistent with Swiss LV95. Assign CRS only after an explicit bounds check.
if (is.na(sf::st_crs(npvm_zones_ZH))) {
  bb <- sf::st_bbox(npvm_zones_ZH)
  lv95_bounds <- bb[["xmin"]] > 2400000 && bb[["xmax"]] < 2900000 &&
                 bb[["ymin"]] > 1000000 && bb[["ymax"]] < 1400000
  if (!lv95_bounds) stop("Missing CRS; coordinates do not match expected LV95 range. Verify manually.")
  sf::st_crs(npvm_zones_ZH) <- 2056
  warning("NPVM shapefile lacks .prj; assigned EPSG:2056 after LV95 bounds check.")
}
if (!all(sf::st_is_valid(npvm_zones_ZH))) stop("NPVM polygon geometry invalid. Review geometries first.")
stopifnot(!anyDuplicated(Strukturdaten$ID_NPVM))
stopifnot(nrow(Strukturdaten) == nrow(npvm_zones_ZH))

#### Validate input variables

# -- sanity checks on the config ------------------------------------------------
check_cols <- function(df, cols, df_name) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop("Not found in ", df_name, ": ", paste(miss, collapse = ", "),
                         "\n  -> look up the MZMV 2021 name in the codebook and edit `cfg`.")
}
check_cols(df_tripsCH, c("HHNR", cfg$trip_purpose, cfg$outing, cfg$trip_no, "S_X", "S_Y"), "wegeinland")
check_cols(df_persons, c("HHNR", cfg$age, cfg$weekday, cfg$car_avail, cfg$abo_ga,
                         cfg$abo_verbund, cfg$employment), "zielpersonen")
check_cols(df_hh, c("HHNR", cfg$hh_urban, "W_X", "W_Y"), "haushalte")
stopifnot("one target person per household expected" = !anyDuplicated(df_persons$HHNR))

cat("\n--- Code check (compare with codebook) ---\n")
for (v in c(cfg$trip_purpose, "wzweck2")) { cat("\nwegeinland$", v, "\n"); print(table(df_tripsCH[[v]], useNA = "ifany")) }
for (v in c(cfg$weekday, cfg$car_avail, cfg$abo_ga, cfg$abo_verbund, cfg$employment)) {
  cat("\nzielpersonen$", v, "\n"); print(table(df_persons[[v]], useNA = "ifany"))
}

#### Combine

# -- which trips start at home? ---------------------------------------------------
# (a) first trip of each complete Ausgang (AUSNR = -98 marks incomplete outings)
# (b) start coordinates within HOME_RADIUS_M of the home coordinates (WGS 84: X = lon, Y = lat)
dist_m <- function(lon1, lat1, lon2, lat2) {          # equirectangular approximation, fine < 1 km
  rad <- pi / 180
  6371000 * sqrt(((lon2 - lon1) * rad * cos((lat1 + lat2) / 2 * rad))^2 + ((lat2 - lat1) * rad)^2)
}
df_tripsCH <- df_tripsCH %>%
  left_join(df_hh %>% dplyr::select(HHNR, W_X, W_Y), by = "HHNR") %>%
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


### Step 3: Construct Dummy Variables -------------------------------------------

#### Dummy variables
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
         !ONLY_CANTON_ZH | W_KANTON == 1) %>%
  tibble::as_tibble()  # avoid data.table's different [ , j] semantics

#### Testing for NAs
model_vars <- c("n_work", "age_est", "car", "tc", "urban")
cat("\nNAs per model variable (employed sample):\n")
print(colSums(is.na(df_sample[, model_vars])))

# Delete NAs (complete cases on the model variables only)
df_non_na <- df_sample %>% drop_na(all_of(model_vars))
cat("Dropped", nrow(df_sample) - nrow(df_non_na), "of", nrow(df_sample), "persons with NAs\n")
print(colSums(is.na(df_non_na[, model_vars])))   # check again -> all 0

#### Prepare groups to match *Strukturdaten*
group_stats <- df_non_na %>%
  group_by(age_class, car, tc, group_name) %>%
  summarise(n = n(), mean = mean(n_work), var = var(n_work), .groups = "drop")
cat("\nObservations per Strukturdaten group (", nrow(group_stats), "of 24 groups present):\n")
print(group_stats, n = 30)
cat("\nObservations and trips per age class (separate vs pooled):\n")
print(df_non_na %>% group_by(age_full) %>% summarise(n = n(), trips = sum(n_work), mean = mean(n_work)))
print(df_non_na %>% group_by(age_est)  %>% summarise(n = n(), trips = sum(n_work), mean = mean(n_work)))


#### Additional exploratory analyses (from the TPM script)
# These analyses are retained here within Step 3 to preserve the original
# calculations, outputs and order of execution.

ci_mean <- function(d, ...) d %>% group_by(...) %>%
  summarise(n = n(), mean = mean(n_work), se = sd(n_work) / sqrt(n()), .groups = "drop") %>%
  mutate(lo = pmax(mean - 1.96 * se, 0), hi = mean + 1.96 * se)
has <- function(df, v) v %in% names(df)

# -- sample flow: how many persons / trips survive each filter -------------------
steps <- list("All target persons (Zielpersonen)" = rep(TRUE, nrow(df_trips)),
              "employed (AMSTAT = 1)"             = df_trips$is_employed)
if (ONLY_WEEKDAYS)  steps[["reference day Monday-Friday"]] <- steps[[length(steps)]] & df_trips$is_weekday
if (ONLY_CANTON_ZH) steps[["living in canton ZH"]]        <- steps[[length(steps)]] & df_trips$W_KANTON %in% 1
steps[["no missing model variables (estimation sample)"]] <- df_trips$HHNR %in% df_non_na$HHNR
flow <- imap_dfr(steps, ~ tibble(step = .y, persons = sum(.x), home_work_trips = sum(df_trips$n_work[.x]),
                                 trips_per_person = sum(df_trips$n_work[.x]) / sum(.x)))
cat("\n--- Sample flow ---\n"); print(flow, width = Inf)
write_csv(flow, file.path(path_out, "sample_flow.csv"))

# -- E1: trips by weekday of the reference day (justifies the weekday filter) ---
wd <- df_trips %>% filter(is_employed) %>%
  mutate(day = factor(.data[[cfg$weekday]], levels = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  filter(!is.na(day)) %>% ci_mean(day)
p_e1 <- ggplot(wd, aes(day, mean)) +
  geom_col(fill = col_1, width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, colour = "grey30") +
  labs(title = "Home-to-Work trips by day of the week",
       subtitle = "employed persons, mean +- 95% CI per reference day",
       x = NULL, y = "Trips per person and day")
print(p_e1); save_fig(p_e1, "figE1_trips_by_weekday", h = 8)

# -- E2: descriptive trip rates by each explanatory variable ----------------------
desc <- bind_rows(
  ci_mean(df_non_na, level = age_full)                                       %>% mutate(variable = "Age class"),
  ci_mean(df_non_na, level = fct_recode(car, "No car" = "NOC", "Car" = "CAR")) %>% mutate(variable = "Car availability"),
  ci_mean(df_non_na, level = fct_recode(tc, "No pass" = "NOTC", "GA / Verbund" = "TC")) %>% mutate(variable = "PT pass"),
  ci_mean(df_non_na, level = urban)                                          %>% mutate(variable = "Home municipality")
) %>% mutate(level = fct_relevel(as.character(level), "0017", "1824", "2544", "4564", "6574", "75XX"),
             variable = factor(variable, levels = c("Age class", "Car availability", "PT pass", "Home municipality")))
p_e2 <- ggplot(desc, aes(level, mean)) +
  geom_hline(yintercept = mean(df_non_na$n_work), colour = "grey60", linetype = "22") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, colour = "grey40") +
  geom_point(colour = col_1, size = 2.5) +
  facet_wrap(~ variable, scales = "free_x", nrow = 1) +
  labs(title = "Home-to-Work trip rate by explanatory variable",
       subtitle = "estimation sample, mean +- 95% CI; dashed = overall mean",
       x = NULL, y = "Trips per person and day") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p_e2); save_fig(p_e2, "figE2_rates_by_variable", w = 20, h = 9)

# -- E3: departure time of Home-to-Work trips ----------------------------------
if (has(df_tripsCH, cfg$dep_time)) {
  dep <- df_tripsCH %>%
    filter(is_work, start_home, HHNR %in% df_non_na$HHNR, .data[[cfg$dep_time]] >= 0) %>%
    mutate(hour = (.data[[cfg$dep_time]] %/% 60) %% 24) %>% count(hour) %>% mutate(share = n / sum(n))
  p_e3 <- ggplot(dep, aes(hour, share)) +
    geom_col(fill = col_1, width = 0.85) +
    scale_x_continuous(breaks = seq(0, 23, 2)) + scale_y_continuous(labels = percent) +
    labs(title = "Departure time of Home-to-Work trips",
         subtitle = "estimation sample, share of trips per hour of departure", x = "Hour of day", y = "Share of trips")
  print(p_e3); save_fig(p_e3, "figE3_departure_time", h = 8)
}

# -- E4: part-time work and home office (2021!) ----------------------------------
if (has(df_non_na, cfg$workload) && has(df_non_na, cfg$home_office)) {
  ho <- df_non_na %>%
    mutate(workload = factor(.data[[cfg$workload]], levels = c(1, 2), labels = c("Full-time", "Part-time")),
           home_office = factor(.data[[cfg$home_office]], levels = c(3, 2, 1),
                                labels = c("Never works from home", "Sometimes", "Works from home"))) %>%
    filter(!is.na(workload), !is.na(home_office)) %>% ci_mean(workload, home_office)
  p_e4 <- ggplot(ho, aes(home_office, mean, fill = workload)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.8), width = 0.2, colour = "grey30") +
    scale_fill_manual(values = c("Full-time" = col_1, "Part-time" = col_2), name = NULL) +
    labs(title = "Home-to-Work trips by workload and home office",
         subtitle = "mean +- 95% CI; not in the Strukturdaten, so not part of the applied model",
         x = NULL, y = "Trips per person and day")
  print(p_e4); save_fig(p_e4, "figE4_workload_home_office", h = 9)
}

# -- E5: is the MZMV sample composed like the Zurich region? ----------------------
pat_grp <- "RE_(0017|1824|2544|4564|6574|75XX)_(CAR|NOC)(NOTC|TC)"
sd_cols <- grep(pat_grp, names(Strukturdaten), value = TRUE)
comp <- bind_rows(
  tibble(col = sd_cols, n = colSums(Strukturdaten[, sd_cols])) %>%
    mutate(m = str_match(col, pat_grp), group_name = paste0(m[, 2], "_", m[, 3], m[, 4]), source = "Strukturdaten (Zurich region)") %>%
    dplyr::select(group_name, n, source),
  df_non_na %>% group_by(group_name) %>%
    summarise(n = if (has(df_non_na, cfg$weight)) sum(.data[[cfg$weight]]) else n(), .groups = "drop") %>%
    mutate(source = "MZMV estimation sample (weighted)")) %>%
  group_by(source) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
  mutate(age_class = substr(group_name, 1, 4),
         panel = factor(recode(substring(group_name, 6), CARNOTC = "Car, no PT pass", CARTC = "Car, PT pass",
                               NOCNOTC = "No car, no PT pass", NOCTC = "No car, PT pass"),
                        levels = c("No car, no PT pass", "No car, PT pass", "Car, no PT pass", "Car, PT pass")))
p_e5 <- ggplot(comp, aes(age_class, share, fill = source)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  facet_wrap(~ panel, nrow = 2) +
  scale_fill_manual(values = c(col_1, col_2), name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(title = "Composition of employed persons: MZMV sample vs Strukturdaten",
       subtitle = "share of all employed persons in each of the 24 groups",
       x = "Age class", y = "Share")
print(p_e5); save_fig(p_e5, "figE5_composition_mzmv_vs_strukturdaten", w = 18, h = 12)


### Step 4: Model proof ----------------------------------------------------------

#### Distribution, mean/variance and overdispersion
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

#### Model proof: visual checks

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


### Step 5: Estimate Model --------------------------------------------------------

#### Model

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
  M5f = n_work ~ group_full                       # one rate per Strukturdaten group (template / TA approach)
)
models <- map(forms, fit_count)

#### Significance

# significance
for (m in names(models)[-1]) { cat("\n=================", m, "=================\n"); print(summary(models[[m]])) }

#### Compare models using ANOVA and goodness-of-fit statistics

# comparison table: log-likelihood (higher = better), AIC/BIC (lower = better), McFadden R2
ll0 <- as.numeric(logLik(models$M0))
cmp <- imap_dfr(models, function(m, nm) tibble(
  model = nm, formula = deparse1(formula(m)),
  k = attr(logLik(m), "df"), logLik = as.numeric(logLik(m)), AIC = AIC(m), BIC = BIC(m),
  McFadden_R2 = 1 - as.numeric(logLik(m)) / ll0,
  age_classes = if (any(c("age_full", "group_full") %in% all.vars(formula(m)))) "6 separate" else "4 pooled",
  inestimable_coefs = sum(is.na(coef(m)))))
cat("\n--- Model comparison ---\n"); print(cmp, width = Inf)
# Model-comparison CSV is saved below after regional-applicability checks.

# Compare nested models (likelihood-ratio tests). Order matters!
cat("\n--- LR tests: M0 < M1 < M2 < M3 < M4 ---\n")
print(do.call(anova, c(unname(models[c("M0", "M1", "M2", "M3", "M4")]), list(test = "Chisq"))))
cat("\n--- LR test: M3 vs M5 (full cell model) ---\n"); print(anova(models$M3, models$M5, test = "Chisq"))
cat("\n--- LR test: M3 vs M6 (+ urban type) ---\n");     print(anova(models$M3, models$M6, test = "Chisq"))

# Pooled vs separate age classes: the pooled model is the separate one with the
# constraints "rate(<18) = rate(18-24)" and "rate(75+) = rate(65-74)", so the two
# are nested -> LR test. H0: pooling loses nothing.
cat("\n--- Pooled vs separate age classes ---\n")
cat("Additive models  M3 (pooled) vs M3f (separate):\n"); print(anova(models$M3, models$M3f, test = "Chisq"))
cat("Group models     M5 (pooled) vs M5f (separate, 24 groups):\n"); print(anova(models$M5, models$M5f, test = "Chisq"))
print(cmp %>% filter(model %in% c("M3", "M3f", "M5", "M5f")) %>% dplyr::select(model, age_classes, k, logLik, AIC, BIC))

#### Final model selection

# ---- Dynamic regional applicability, then model selection ----------------------
# The 24 joint age x car x PT groups are supplied in Strukturdaten; unlike
# municipality type, these groups can be mapped directly to the survey model.
AGE_PATTERN <- "RE_(0017|1824|2544|4564|6574|75XX)_(CAR|NOC)(NOTC|TC)"
region_group_cols <- grep(AGE_PATTERN, names(Strukturdaten), value = TRUE)
if (length(region_group_cols) != 24L) {
  stop("Expected exactly 24 RE employee-group columns in Strukturdaten, found ",
       length(region_group_cols), ". Inspect names(Strukturdaten).")
}
if (!all(c("ID_NPVM", "FTE") %in% names(Strukturdaten))) {
  stop("Regional data must contain ID_NPVM and FTE.")
}
if (anyNA(Strukturdaten[, region_group_cols]) ||
    any(as.matrix(Strukturdaten[, region_group_cols]) < 0)) {
  stop("Regional employee-group counts contain missing or negative values.")
}

lev <- function(v) levels(df_non_na[[v]])
grid24 <- tidyr::expand_grid(
  age_class = AGE_CLASSES, car = c("NOC", "CAR"), tc = c("NOTC", "TC")
) %>%
  mutate(age_est = factor(unname(AGE_MAP[age_class]), levels = lev("age_est")),
         age_full = factor(age_class, levels = lev("age_full")),
         car = factor(car, levels = lev("car")),
         tc = factor(tc, levels = lev("tc")),
         group_est = factor(paste(age_est, car, tc, sep = "_"), levels = lev("group_est")),
         group_full = factor(paste(age_class, car, tc, sep = "_"), levels = lev("group_full")),
         group_name = paste0(age_class, "_", car, tc))
stopifnot(nrow(grid24) == 24L)

# M6 is usable only if the ENTIRE regional column represents real three-class
# urbanisation codes. The current aggregated dataset contains values > 3, so
# this condition rejects M6 without hardcoding a model name as the winner.
urban_codes <- if (URBAN_REGION_COLUMN %in% names(Strukturdaten))
  Strukturdaten[[URBAN_REGION_COLUMN]] else NULL
urban_region_ok <- !is.null(urban_codes) &&
  all(!is.na(urban_codes)) &&
  all(as.character(urban_codes) %in% as.character(1:3))
cat("\nRegional three-class urbanisation usable:", urban_region_ok, "\n")
if (!urban_region_ok) {
  cat("Reason: missing/NA/mixed codes in", URBAN_REGION_COLUMN,
      "(urban 1, intermediate 2, rural 3 required for all zones).\n")
}

# Build precisely the prediction rows needed by each model. For an urban model,
# the three municipality categories are crossed with all 24 groups.
regional_newdata <- function(model) {
  pred_vars <- all.vars(stats::formula(model))[-1]
  if ("urban" %in% pred_vars) {
    if (!urban_region_ok) stop("Regional three-class urbanisation is unavailable.")
    out <- tidyr::expand_grid(grid24, urban = factor(
      c("urban", "intermediate", "rural"), levels = lev("urban")))
  } else {
    out <- grid24
  }
  out
}

model_applicability <- function(model) {
  if (any(!is.finite(stats::coef(model)))) return(FALSE)
  tryCatch({
    nd <- regional_newdata(model)
    pred <- as.numeric(stats::predict(model, newdata = nd, type = "response"))
    length(pred) == nrow(nd) && all(is.finite(pred)) && all(pred >= 0)
  }, error = function(e) FALSE)
}

cmp <- cmp %>%
  mutate(applicable_to_region = vapply(models[model], model_applicability, logical(1)))
cat("\n--- Candidate models including regional applicability ---\n")
print(cmp %>% dplyr::select(model, AIC, BIC, inestimable_coefs,
                             applicable_to_region), width = Inf)
write_csv(cmp, file.path(path_out, "model_comparison.csv"))

applicable <- cmp %>% filter(applicable_to_region, is.finite(.data[[MODEL_CRITERION]]))
if (nrow(applicable) == 0) stop("No estimable model is compatible with the regional variables.")
final_name <- applicable$model[which.min(applicable[[MODEL_CRITERION]])]
final_unweighted <- models[[final_name]]
cat("\n>>> AUTOMATIC MODEL SELECTION: ", final_name, " (minimum ", MODEL_CRITERION,
    " among applicable models)\n", sep = "")
cat("Selected unweighted formula:", deparse1(stats::formula(final_unweighted)), "\n")

# ---- Survey-weighted re-estimation of the AUTO-SELECTED specification ---------
# Selection compares standard (unweighted) likelihood AICs on a common sample.
# The selected mean model is re-estimated with respondent weights WP for zonal
# prediction, as in the submitted report. No fixed formula/model name here.
if (!cfg$weight %in% names(df_non_na)) stop("Missing person weight column: ", cfg$weight)
wt <- df_non_na[[cfg$weight]]
if (anyNA(wt) || any(!is.finite(wt)) || any(wt <= 0)) {
  stop("Person weights must be finite, positive and nonmissing.")
}
if (USE_NB) {
  # For an automatically chosen NB family, retain NB with normalised weights.
  # NB weighted standard errors here are NOT design-based survey SEs.
  df_w <- df_non_na %>% mutate(.w = wt / mean(wt))
  final_w <- MASS::glm.nb(stats::formula(final_unweighted), data = df_w, weights = .w)
  warning("Weighted negative binomial estimation does not provide design-based survey standard errors.")
} else {
  design_mzmv <- survey::svydesign(
    ids = ~1, weights = stats::as.formula(paste0("~", cfg$weight)),
    data = as.data.frame(df_non_na))
  final_w <- survey::svyglm(
    stats::formula(final_unweighted), design = design_mzmv,
    family = quasipoisson(link = "log"))
}
final <- final_w
cat("Weighted final model:", final_name, "\n")
print(summary(final))

r_u <- as.numeric(predict(final_unweighted, newdata = df_non_na, type = "response"))
r_w <- as.numeric(predict(final, newdata = df_non_na, type = "response"))
cat(sprintf("Weighted vs unweighted fit: predicted rates differ by %.1f%% on average (max %.1f%%)\n",
            100 * mean(abs(r_w / r_u - 1)), 100 * max(abs(r_w / r_u - 1))))

# For figures and sensitivity comparisons below, retain the original unweighted
# candidate fits. Only the selected regional prediction uses the weighted fit.
rate_of <- function(m, nd = grid24) {
  pred <- tryCatch(as.numeric(predict(m, newdata = nd, type = "response")),
                   error = function(e) rep(NA_real_, nrow(nd)))
  pred
}

# -- LaTeX tables for the report (booktabs) --------------------------------------
if (requireNamespace("knitr", quietly = TRUE)) {
  tex <- function(df, file, digits, caption)
    writeLines(knitr::kable(df, format = "latex", booktabs = TRUE, digits = digits, caption = caption,
                            linesep = ""), file.path(path_out, file))
  tex(cmp %>% dplyr::select(model, formula, age_classes, k, logLik, AIC, BIC, McFadden_R2),
      "tab_model_comparison.tex", c(0, 0, 0, 0, 1, 1, 1, 4), "Model comparison (Home-to-Work trips per person)")
  sc <- summary(final)$coefficients
  ci <- confint.default(final)
  tex(tibble(Term = rownames(sc), Estimate = sc[, 1], `Std. error` = sc[, 2],
             IRR = exp(sc[, 1]), `IRR 2.5%` = exp(ci[, 1]), `IRR 97.5%` = exp(ci[, 2]), `p-value` = sc[, 4]),
      "tab_final_model.tex", c(0, 3, 3, 3, 3, 3, 4), paste0("Final model ", final_name))
  tex(flow, "tab_sample_flow.tex", c(0, 0, 0, 3), "Sample flow")
  tex(proof, "tab_dispersion.tex", c(0, 0, 3, 3, 3), "Dispersion of Home-to-Work trips per person")
}

#### Prepare grouped predictions for model comparison

# The 24-group prediction grid and rate_of() were prepared above for the
# region-aware model selection. They are reused in the diagnostic plots below.

# ---- Fig 3b: group rates, pooled vs separate age classes ------------------------
# The separate 24-group model M5f reproduces each group's sample mean exactly,
# so "observed mean +- 95% CI" IS model M5f. The question is whether the
# pooled / additive models stay inside those intervals.
obs24 <- df_non_na %>% group_by(group_name) %>%
  summarise(n = n(), mean = mean(n_work), se = sqrt(var(n_work) / n()), .groups = "drop")
cmp24 <- grid24 %>% dplyr::select(age_class, car, tc, group_name) %>%
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

# ---- Fig 3: incidence-rate ratios of the survey-weighted final model -----------
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

# ---- Fig 4: residual check - weighted observed vs predicted group means --------
calib <- df_non_na %>% mutate(pred = as.numeric(fitted(final)), .survey_w = .data[[cfg$weight]]) %>%
  group_by(group_name, car, tc) %>%
  summarise(n = n(), obs = weighted.mean(n_work, .survey_w),
            pred = weighted.mean(pred, .survey_w), .groups = "drop") %>%
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


### Step 6: Apply your model to the Region ---------------------------------------

#### Prepare

# Trip rates for the 24 Strukturdaten groups
uses_urban <- "urban" %in% all.vars(formula(final))
grid <- regional_newdata(final)
grid$rate <- as.numeric(predict(final, newdata = grid, type = "response"))   # trips per employee and day

#### Plausibility Check

# Trip rates per group
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
pat <- AGE_PATTERN  # the 24 group columns already validated during model selection
grp_cols <- region_group_cols
fte_col <- "FTE"
sl3_col <- if (uses_urban) URBAN_REGION_COLUMN else character(0)

emp_long <- Strukturdaten %>%
  dplyr::select(all_of(c(zone_id, sl3_col, grp_cols))) %>%
  pivot_longer(all_of(grp_cols), names_to = "col", values_to = "n_employees") %>%
  mutate(m = str_match(col, pat), group_name = paste0(m[, 2], "_", m[, 3], m[, 4])) %>%
  dplyr::select(-m)
if (uses_urban) {
  emp_long <- emp_long %>%
    mutate(urban = factor(.data[[sl3_col]], levels = 1:3,
                          labels = c("urban", "intermediate", "rural")))
  emp_long <- left_join(emp_long, grid %>% dplyr::select(group_name, urban, rate), by = c("group_name", "urban"))
} else {
  emp_long <- left_join(emp_long, grid %>% dplyr::select(group_name, rate), by = "group_name")
}
stopifnot(!anyNA(emp_long$rate))

zones <- emp_long %>%
  group_by(.data[[zone_id]]) %>%
  summarise(employees = sum(n_employees), trips_work = sum(n_employees * rate), .groups = "drop") %>%
  left_join(Strukturdaten %>% dplyr::select(all_of(c(zone_id, fte_col))), by = zone_id) %>%
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
  emp_long %>% dplyr::select(all_of(zone_id), group_name, n_employees) %>%
    left_join(tibble(group_name = grid24$group_name, r = rate_of(models[[nm]])), by = "group_name") %>%
    group_by(.data[[zone_id]]) %>% summarise(p = sum(n_employees * r), .groups = "drop") %>% pull(p)
})
cat("Total produced trips per model:\n"); print(round(colSums(sens)))
for (pr in list(c("M3", "M3f"), c("M5", "M5f"))) if (all(pr %in% names(sens))) {
  d <- sens[[pr[2]]] / sens[[pr[1]]] - 1
  cat(sprintf("%s (pooled) vs %s (separate): zone production differs by %.2f%% on average, at most %.2f%%\n",
              pr[1], pr[2], 100 * mean(abs(d), na.rm = TRUE), 100 * max(abs(d), na.rm = TRUE)))
}

# ---- Attach zone results to geometries ----------------------------------------
# Always put zone results back into the original Strukturdaten row order.
# group_by() can sort ID_NPVM, so using raw `zones` order is not safe.
zones_ordered <- zones[match(Strukturdaten[[zone_id]], zones[[zone_id]]), , drop = FALSE]
stopifnot(!anyNA(zones_ordered$trips_work),
          identical(as.character(zones_ordered[[zone_id]]), as.character(Strukturdaten[[zone_id]])))

# If reliable polygon IDs exist, join on ID rather than relying on row order.
shp_id <- intersect(c("ID_NPVM", "ID", "NO", "N_ID", "ZONE_ID", "id", "ID_Zone"),
                    names(npvm_zones_ZH))[1]
if (!is.na(shp_id)) {
  if (anyDuplicated(npvm_zones_ZH[[shp_id]]) ||
      !setequal(as.character(npvm_zones_ZH[[shp_id]]), as.character(zones[[zone_id]]))) {
    stop("Shapefile contains a candidate zone ID, but it does not match the model IDs. Check manually.")
  }
  zones_j <- zones %>% mutate(.key = as.character(.data[[zone_id]]))
  npvm_zones_ZH <- npvm_zones_ZH %>%
    mutate(.key = as.character(.data[[shp_id]])) %>%
    left_join(zones_j %>% dplyr::select(-all_of(zone_id)), by = ".key") %>%
    dplyr::select(-.key)
  spatial_match_status <- "verified_zone_id_join"
} else {
  if (!ALLOW_UNVERIFIED_ROW_JOIN) {
    stop("Geometry has no zone IDs. Obtain complete .dbf or explicitly enable provisional row-order join.")
  }
  warning("PROVISIONAL SPATIAL JOIN: shapefile lacks zone IDs. Row order is NOT independently verified.")
  stopifnot(nrow(npvm_zones_ZH) == nrow(zones_ordered))
  npvm_zones_ZH <- sf::st_sf(
    dplyr::bind_cols(sf::st_drop_geometry(npvm_zones_ZH), zones_ordered),
    geometry = sf::st_geometry(npvm_zones_ZH))
  spatial_match_status <- "PROVISIONAL_unverified_row_order"
}
npvm_zones_ZH$spatial_join_status <- spatial_match_status
if (anyNA(npvm_zones_ZH$trips_work)) stop("Unmatched zone trip production after spatial join.")
writeLines(paste0("Spatial correspondence status: ", spatial_match_status,
                  "\nDo not regard zone-level maps as spatially validated unless polygon IDs are verified."),
           file.path(path_out, "spatial_join_status.txt"))
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
# File names match the exercise workflow. When the source shapefile has no
# identifiers, spatial_join_status explicitly marks outputs as provisional.
readr::write_csv(
  tibble::tibble(selected_model = final_name,
                 selection_criterion = MODEL_CRITERION,
                 family = if (USE_NB) "negative_binomial" else "quasipoisson_weighted",
                 urban_covariate_usable = urban_region_ok,
                 spatial_join_status = spatial_match_status,
                 produced_trips = sum(zones$trips_work),
                 balanced_attractions = sum(zones$attraction_work)),
  file.path(path_out, "model_selection_and_totals.csv"))
write_sf(npvm_zones_ZH, file.path(path_out, "trip_generation_home_work.gpkg"), delete_dsn = TRUE)
write_csv(st_drop_geometry(npvm_zones_ZH), file.path(path_out, "trip_generation_home_work_zones.csv"))
cat("\nResults written to", path_out, "\n")
cat("Selected", final_name, "automatically using", MODEL_CRITERION,
    "from compatible candidates; weighted regional total:",
    round(sum(zones$trips_work), 1), "trips/day.\n")
if (grepl("PROVISIONAL", spatial_match_status))
  cat("WARNING: polygon assignment uses UNVERIFIED row order. See spatial_join_status.txt.\n")
