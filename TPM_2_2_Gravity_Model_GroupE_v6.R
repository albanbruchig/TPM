# =============================================================================
# ETH Zürich | Transport Planning Methods | Fall 2026
# Assignment 2.2: Trip distribution - gravity model (deadline: 02 Nov 2026)
# -----------------------------------------------------------------------------
# PART A: Observed MZMV home -> work OD flows, weighted with WP.
# PART B: Motorcar travel times (Geofabrik OSM + dodgr), gravity model,
#         iterative proportional fitting (Furness), maps and export.
#
# IMPORTANT BEFORE RUNNING:
#   1. Paths, columns and survey filters are now tailored to Group E Part 2.1.
#      Confirm the existing project directory before running.
#   2. The Part 2.1 productions and attractions are linked in CONFIG.
#   3. Choose/justify beta: the handout does not supply an impedance coefficient.
#   4. Use the *same* home->work and weekday definition as in Assignment 2.1.
#   5. MZMV is confidential: read data directly from its password-protected ZIP,
#      Never export individual trips.
#
# This script is intentionally strict: it stops with a helpful error when its
# required schema or a critical model assumption has not been configured.
# =============================================================================

# =========================== 0. CONFIG =======================================
CONFIG <- list(
  # Confirmed locally from your directory listing. You may move the entire project
  # as long as you update this one path. No confidential source data are exported.
  project_dir = "/Users/albanbruchig/Documents/School/ETHZ/Fall/Transport Planning Methods",
  zone_shapefile = "Materials_for_Trip Generation/output_2_1/trip_generation_home_work.gpkg",
  zone_id_col = "ID_NPVM",
  part21_csv = "Materials_for_Trip Generation/output_2_1/trip_generation_home_work_zones.csv",
  part21_zone_col = "ID_NPVM",
  part21_production_col = "trips_work",
  part21_attraction_col = "attraction_work",
  
  # Original protected ZIP. All three CSV members are read into R memory only.
  trips_zip = "Materials for descriptive_analysis-20260930/MZMV_Data_protect.zip",
  trips_member = "MZMV_Data_protect/wegeinland.csv",
  persons_member = "MZMV_Data_protect/zielpersonen.csv",
  households_member = "MZMV_Data_protect/haushalte.csv",
  zip_password_env = "MZMV_ZIP_PASSWORD",
  trip_separator = ";",             # original Part 2.1 uses fread() for CSV import
  trip_decimal = ".",               # if CSV has decimal commas, conversion handles them
  trip_encoding = "UTF-8",          # change to "Latin1" only if headings are garbled
  trip_origin_lon = "S_X",
  trip_origin_lat = "S_Y",
  trip_dest_lon = "Z_X",
  trip_dest_lat = "Z_Y",
  trip_weight_col = "WP",
  trip_purpose_col = "wzweck1",    # destination purpose variable
  work_purpose_code = "2",
  
  # Exact Group E Part 2.1 classification (HOME_START = "either"):
  # Home if first trip of a complete outing OR departure <= 200m from home.
  trip_person_id_col = "HHNR",
  trip_outing_col = "AUSNR",
  trip_sequence_col = "WEGNR",
  home_distance_threshold_m = 200,
  # Exact estimation sample restriction from Group E 2.1:
  weekday_col = "Tag",             # from zielpersonen.csv, NOT wegeinland.csv
  weekday_values = c("1", "2", "3", "4", "5"),
  employment_col = "AMSTAT",      # from zielpersonen.csv
  employed_values = c("1"),
  canton_restriction = FALSE,     # ONLY_CANTON_ZH = FALSE in Part 2.1
  
  output_dir = "output/assignment_2_2",
  osm_cache = "cache/osm_motorcar_graph.rds", # v3 cache still compatible
  osm_margin_m = 10000,          # road-network buffer around study-area bbox
  fallback_speed_kmh = 40,       # documented assumption ONLY for missing times
  fallback_circuity = 1.3,       # road-distance / straight-line-distance
  min_interzonal_min = 1,
  intrazonal_min = 10,           # specified by assignment
  
  beta_mode = "match_observed_mean_time",          # 'manual' or 'match_observed_mean_time'
  beta_per_minute = 0.05,        # ILLUSTRATIVE, NOT an assignment-given value!
  beta_search_interval = c(0.001, 0.20),
  epsilon = 1e-6,               # specified by assignment
  max_iterations = 10000,
  max_map_lines = 350,          # plotting only; ALL OD pairs are exported
  run_part_b = TRUE,           # FIRST check 2.2(a); then switch to TRUE for gravity
  run_unit_tests = TRUE
)

# =========================== 1. PACKAGES =====================================
required_packages <- c("sf", "dplyr", "ggplot2", "readr", "osmdata",
                       "dodgr", "archive")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop("Install packages before running: install.packages(c('",
       paste(missing_packages, collapse = "', '"), "'))")
}
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(osmdata)
  library(dodgr)
})

resolve <- function(path) {
  if (grepl("^(/|~|[A-Za-z]:[/\\\\])", path)) {
    path.expand(path)
  } else {
    file.path(path.expand(CONFIG$project_dir), path)
  }
}
assert_file <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}
assert_cols <- function(df, cols, label) {
  needed <- setdiff(cols, names(df))
  if (length(needed)) {
    stop(label, " is missing columns: ", paste(needed, collapse = ", "),
         "\nAvailable: ", paste(names(df), collapse = ", "), call. = FALSE)
  }
}
as_id <- function(x) trimws(as.character(x))
as_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  # MZMV exports may represent decimals with "," or "." depending on locale.
  z <- trimws(as.character(x))
  z[z == ""] <- NA_character_
  a <- suppressWarnings(readr::parse_double(
    z, locale = readr::locale(decimal_mark = CONFIG$trip_decimal)))
  bad <- !is.na(z) & is.na(a)
  if (any(bad)) {
    alternate <- if (CONFIG$trip_decimal == ".") "," else "."
    a[bad] <- suppressWarnings(readr::parse_double(
      z[bad], locale = readr::locale(decimal_mark = alternate)))
  }
  a
}
announce <- function(x) cat("\n", strrep("=", 65), "\n", x, "\n", sep = "")

OUT <- resolve(CONFIG$output_dir)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
CACHE <- resolve(CONFIG$osm_cache)
dir.create(dirname(CACHE), recursive = TRUE, showWarnings = FALSE)

# ===================== 2. CORE IPF / FURNESS ROUTINE ==========================
# T_ij = P_i * A_j * exp(-beta * t_ij) * r_i * c_j
# r_i and c_j are updated row/column balancing factors, respectively.
# All zero-production and zero-attraction zones are allowed.
furness <- function(P, A, time_min, beta, epsilon = 1e-6,
                    max_iterations = 10000) {
  stopifnot(is.matrix(time_min), length(P) == nrow(time_min),
            length(A) == ncol(time_min), is.finite(beta), beta >= 0)
  if (anyNA(P) || anyNA(A) || any(P < 0) || any(A < 0))
    stop("Productions and attractions must be nonnegative, without NAs.")
  if (any(!is.finite(time_min)) || any(time_min <= 0))
    stop("Travel-time matrix must contain strictly positive, finite minutes.")
  if (abs(sum(P) - sum(A)) > 1e-8 * max(1, sum(P)))
    stop("Furness requires equal total productions and attractions.")
  if (sum(P) <= 0) stop("Total production must be positive.")
  
  # Exp(-beta*t) is a deterrence function; its coefficient must be justified.
  deterrence <- exp(-beta * time_min)
  mat <- outer(P, A) * deterrence
  if (any(rowSums(mat)[P > 0] <= 0) || any(colSums(mat)[A > 0] <= 0))
    stop("Gravity seed has an unsupported row/column (zero mass).")
  
  rf_total <- rep(1, length(P))
  cf_total <- rep(1, length(A))
  error_history <- numeric(max_iterations)
  converged <- FALSE
  
  for (it in seq_len(max_iterations)) {
    # Row adjustment: impose trip productions.
    rows <- rowSums(mat)
    if (any(rows[P > 0] <= 0)) stop("Numerical underflow during row balancing.")
    rf <- rep(1, length(P))
    rf[P > 0] <- P[P > 0] / rows[P > 0]
    mat <- sweep(mat, 1, rf, `*`)
    rf_total <- rf_total * rf
    
    # Column adjustment: impose trip attractions.
    cols <- colSums(mat)
    if (any(cols[A > 0] <= 0)) stop("Numerical underflow during column balancing.")
    cf <- rep(1, length(A))
    cf[A > 0] <- A[A > 0] / cols[A > 0]
    mat <- sweep(mat, 2, cf, `*`)
    cf_total <- cf_total * cf
    
    # Maximum *relative* deviation, with a safe denominator for small totals.
    error_history[it] <- max(
      abs(rowSums(mat) - P) / pmax(P, 1),
      abs(colSums(mat) - A) / pmax(A, 1)
    )
    if (error_history[it] <= epsilon) {
      converged <- TRUE
      break
    }
  }
  if (!converged) {
    stop("Furness did not converge within ", max_iterations,
         " iterations. Last relative error = ", signif(error_history[it], 4))
  }
  list(matrix = mat, iterations = it, error = error_history[it],
       history = error_history[seq_len(it)],
       row_factor = rf_total, col_factor = cf_total)
}

# Self-tests are independent of confidential data and run before any file I/O.
if (isTRUE(CONFIG$run_unit_tests)) {
  t0 <- matrix(c(10, 20, 30, 10), nrow = 2)
  p0 <- c(120, 80); a0 <- c(70, 130)
  test <- furness(p0, a0, t0, beta = 0.05)
  stopifnot(max(abs(rowSums(test$matrix) - p0)) < 1e-3,
            max(abs(colSums(test$matrix) - a0)) < 1e-3)
  test_zero <- furness(c(100, 0), c(40, 60), t0, beta = 0.05)
  stopifnot(all(test_zero$matrix[2, ] == 0),
            max(abs(colSums(test_zero$matrix) - c(40, 60))) < 1e-3)
  message("Furness unit tests passed.")
}

# ===================== 3. STUDY AREA + PART 2.1 ===============================
announce("3. Read zone geometry and Part 2.1 zone totals")
zones_path <- resolve(CONFIG$zone_shapefile)
part21_path <- resolve(CONFIG$part21_csv)
assert_file(zones_path, "Zone shapefile")
assert_file(part21_path, "Part 2.1 CSV")

zones_raw <- sf::read_sf(zones_path, quiet = TRUE)
assert_cols(zones_raw, CONFIG$zone_id_col, "Zoning shapefile")
# Verified Part 2.1 GeoPackage: 143 polygons, Swiss LV95 bounding box
# (Easting 2664530..2705843, Northing 1222918..1276522), but its stored
# CRS is labelled "Undefined Cartesian SRS with unknown unit".
# Assign metadata only for this exact verified file; do NOT transform before it.
orig_crs <- sf::st_crs(zones_raw)
undefined_crs <- is.na(orig_crs) ||
  (is.na(orig_crs$epsg) &&
     grepl("undefined|unknown", orig_crs$input, ignore.case = TRUE))
if (undefined_crs) {
  if (basename(zones_path) != "trip_generation_home_work.gpkg") {
    stop("Zone geometry has undefined CRS; determine original CRS before proceeding.")
  }
  suppressWarnings(sf::st_crs(zones_raw) <- 2056)
  message("Assigned EPSG:2056 to the verified 2.1 GeoPackage (metadata only).")
}
zones_raw$zone_id <- as_id(zones_raw[[CONFIG$zone_id_col]])
if (anyNA(zones_raw$zone_id) || any(zones_raw$zone_id == ""))
  stop("Zoning shapefile contains missing/blank zone identifiers.")

# Set geometry to Swiss LV95 for distances, centroids and maps.
zones_raw <- sf::st_transform(sf::st_make_valid(zones_raw), 2056)
zones <- zones_raw |>
  dplyr::select(zone_id) |>
  dplyr::group_by(zone_id) |>
  dplyr::summarise(.groups = "drop")

p21 <- readr::read_csv(part21_path, show_col_types = FALSE)
assert_cols(p21, c(CONFIG$part21_zone_col, CONFIG$part21_production_col,
                   CONFIG$part21_attraction_col), "Part 2.1 results")
zone_totals <- data.frame(
  zone_id = as_id(p21[[CONFIG$part21_zone_col]]),
  production = as.numeric(p21[[CONFIG$part21_production_col]]),
  attraction = as.numeric(p21[[CONFIG$part21_attraction_col]]),
  stringsAsFactors = FALSE
)
if (anyDuplicated(zone_totals$zone_id)) stop("Duplicate zone IDs in Part 2.1 output.")
if (anyNA(zone_totals) || any(zone_totals$production < 0) ||
    any(zone_totals$attraction < 0))
  stop("Part 2.1 totals have missing or negative values.")
if (any(zone_totals$zone_id == "")) stop("Blank zone IDs in Part 2.1 results.")
missing_zones <- setdiff(zone_totals$zone_id, zones$zone_id)
if (length(missing_zones))
  stop("The following Part 2.1 zones are absent from the shapefile: ",
       paste(missing_zones, collapse = ", "))

# The study area is defined by the 2.1 zones, rather than all of Switzerland.
zones <- zones[match(zone_totals$zone_id, zones$zone_id), ]
stopifnot(identical(zones$zone_id, zone_totals$zone_id))
if (nrow(zones) != 143)
  warning("Expected around 143 Zürich zones based on your Part 2.1 workflow; found ",
          nrow(zones), ". Verify study-area selection.")

initial_total_P <- sum(zone_totals$production)
initial_total_A <- sum(zone_totals$attraction)
if (initial_total_A <= 0 || initial_total_P <= 0)
  stop("Region must have positive total production and attraction.")
scale_A <- initial_total_P / initial_total_A
zone_totals$attraction_original <- zone_totals$attraction
zone_totals$attraction <- zone_totals$attraction * scale_A
message("Sum production: ", round(initial_total_P, 2),
        "; attraction before scaling: ", round(initial_total_A, 2),
        "; attraction scale factor: ", signif(scale_A, 7))
readr::write_csv(zone_totals,
                 file.path(OUT, "zone_totals_balanced.csv"))

# Calculate zone centroids as specified in 2.2(a). They can fall outside a
# concave polygon; for routing, dodgr snaps each to the closest road vertex.
centroids <- suppressWarnings(sf::st_centroid(zones))
xy <- sf::st_coordinates(centroids)
names_x <- zones$zone_id
centroids_wgs <- sf::st_transform(centroids, 4326)
ll <- sf::st_coordinates(centroids_wgs)

# ===================== 4. OBSERVED MZMV TRIPS ================================
announce("4. Read MZMV + reproduce Group E Part 2.1 filtering")

# Read encrypted members via a connection. No individual trip/person/household
# records are extracted to disk; keep password out of the R code and chat.
read_private_mzmv <- function() {
  zfile <- resolve(CONFIG$trips_zip)
  assert_file(zfile, "Protected MZMV ZIP")
  password <- Sys.getenv(CONFIG$zip_password_env, unset = "")
  if (!nzchar(password)) {
    if (!requireNamespace("askpass", quietly = TRUE)) {
      stop("Install 'askpass' (install.packages('askpass')) or set the ",
           "MZMV_ZIP_PASSWORD environment variable securely.")
    }
    password <- askpass::askpass("Password for protected MZMV ZIP")
  }
  if (!nzchar(password)) stop("No password provided for the MZMV ZIP.")
  members <- archive::archive(zfile, password = password)$path
  requested <- c(CONFIG$trips_member, CONFIG$persons_member,
                 CONFIG$households_member)
  if (!all(requested %in% members)) {
    stop("Missing ZIP member(s): ",
         paste(setdiff(requested, members), collapse = ", "))
  }
  read_member <- function(member) {
    con <- archive::archive_read(zfile, file = member, mode = "r",
                                 password = password)
    # readr may close its input connection; avoid a second close masking read errors.
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    # Character import prevents premature conversion of Swiss CSV2 values,
    # preserves HHNR identifiers, and permits explicit numeric checks below.
    readr::read_delim(
      con, delim = CONFIG$trip_separator,
      col_types = readr::cols(.default = readr::col_character()),
      locale = readr::locale(encoding = CONFIG$trip_encoding),
      show_col_types = FALSE, progress = FALSE)
  }
  ans <- list(
    trips = read_member(CONFIG$trips_member),
    persons = read_member(CONFIG$persons_member),
    households = read_member(CONFIG$households_member))
  rm(password)
  ans
}
private_data <- read_private_mzmv()
trips <- private_data$trips
persons <- private_data$persons
households <- private_data$households
rm(private_data)

assert_cols(trips, c("HHNR", "AUSNR", "WEGNR", "wzweck1",
                     "S_X", "S_Y", "Z_X", "Z_Y"), "MZMV wegeinland")
assert_cols(persons, c("HHNR", CONFIG$weekday_col, CONFIG$employment_col),
            "MZMV zielpersonen")
assert_cols(households, c("HHNR", "W_X", "W_Y"), "MZMV haushalte")
if (anyDuplicated(persons$HHNR)) stop("More than one target person per HHNR.")
if (anyDuplicated(households$HHNR)) stop("Duplicate household HHNR values.")

# Correct universe from Group E's estimation sample:
# AMSTAT = 1 (employed), Tag = 1..5 (Mon..Fri); NO canton filter.
p_eligible <- persons[
  as.character(persons[[CONFIG$employment_col]]) %in% CONFIG$employed_values &
    as.character(persons[[CONFIG$weekday_col]]) %in% CONFIG$weekday_values,
  , drop = FALSE]
message("Eligible employed weekday respondents: ", nrow(p_eligible))
if (!nrow(p_eligible)) stop("No employed weekday respondents. Check codes.")

person_join <- data.frame(HHNR = p_eligible$HHNR,
                          stringsAsFactors = FALSE)
if (CONFIG$trip_weight_col %in% names(p_eligible)) {
  person_join$.person_wp <- p_eligible[[CONFIG$trip_weight_col]]
}
home_join <- households[, c("HHNR", "W_X", "W_Y"), drop = FALSE]
# IMPORTANT: classify outgoing trips on the full trip table before limiting to
# work purpose, mirroring the Group E 2.1 code.
trips <- trips |>
  dplyr::left_join(home_join, by = "HHNR") |>
  dplyr::group_by(HHNR, AUSNR) |>
  dplyr::mutate(
    .start_outing = as_number(AUSNR) > 0 &
      as_number(WEGNR) == min(as_number(WEGNR))) |>
  dplyr::ungroup()

# Group E 2.1 used an equirectangular approx. to evaluate <=200m from home.
# Use the same approximation here, with explicit invalid-coordinate checks.
dist_m <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  6371000 * sqrt(((lon2 - lon1) * rad * cos((lat1 + lat2) / 2 * rad))^2 +
                   ((lat2 - lat1) * rad)^2)
}
for (var in c("S_X", "S_Y", "Z_X", "Z_Y", "W_X", "W_Y")) {
  trips[[paste0(".num_", var)]] <- as_number(trips[[var]])
}
coords_ok <- with(trips,
                  is.finite(.num_S_X) & is.finite(.num_S_Y) &
                    is.finite(.num_W_X) & is.finite(.num_W_Y) &
                    .num_S_X > -990 & .num_W_X > -990 &
                    abs(.num_S_X) <= 180 & abs(.num_W_X) <= 180 &
                    abs(.num_S_Y) <= 90 & abs(.num_W_Y) <= 90)
trips$.start_coords <- coords_ok &
  dist_m(trips$.num_S_X, trips$.num_S_Y,
         trips$.num_W_X, trips$.num_W_Y) <= CONFIG$home_distance_threshold_m
trips$.start_home <- (!is.na(trips$.start_outing) & trips$.start_outing) |
  (!is.na(trips$.start_coords) & trips$.start_coords)
trips$.is_work <- as.character(trips[[CONFIG$trip_purpose_col]]) ==
  CONFIG$work_purpose_code

# Note: WP can reside in the trip table or in the person table, depending on
# the delivered files. Join eligible persons and choose an available WP field.
trips <- trips |>
  dplyr::inner_join(person_join, by = "HHNR") |>
  dplyr::filter(.start_home, .is_work)
if (!nrow(trips)) stop("No home-to-work trips among employed weekday respondents.")
if (CONFIG$trip_weight_col %in% names(trips)) {
  trips$.weight <- as_number(trips[[CONFIG$trip_weight_col]])
  message("Using trip-table WP for observed OD weighting.")
} else if (".person_wp" %in% names(trips)) {
  trips$.weight <- as_number(trips$.person_wp)
  message("Using target-person WP for observed OD weighting.")
} else {
  stop("WP is absent from both trip and person data.")
}
trips$.s_lon <- trips$.num_S_X
trips$.s_lat <- trips$.num_S_Y
trips$.z_lon <- trips$.num_Z_X
trips$.z_lat <- trips$.num_Z_Y

# Keep only valid WGS84 coordinates and nonnegative survey expansion weights.
valid <- is.finite(trips$.weight) & trips$.weight >= 0 &
  is.finite(trips$.s_lon) & is.finite(trips$.s_lat) &
  is.finite(trips$.z_lon) & is.finite(trips$.z_lat) &
  abs(trips$.s_lon) <= 180 & abs(trips$.z_lon) <= 180 &
  abs(trips$.s_lat) <= 90 & abs(trips$.z_lat) <= 90
message("Home-to-work records before coordinate check: ", nrow(trips),
        "; invalid records: ", sum(!valid))
trips <- trips[valid, , drop = FALSE]
if (!nrow(trips)) stop("No spatially valid home-to-work trips.")
rm(persons, households, p_eligible, person_join, home_join)

# WGS84 start/end points are transformed into Swiss LV95 for polygon matching.
origins_sf <- sf::st_transform(sf::st_as_sf(
  trips, coords = c(".s_lon", ".s_lat"), crs = 4326, remove = FALSE), 2056)
destinations_sf <- sf::st_transform(sf::st_as_sf(
  trips, coords = c(".z_lon", ".z_lat"), crs = 4326, remove = FALSE), 2056)
start_hits <- sf::st_intersects(origins_sf, zones)
end_hits <- sf::st_intersects(destinations_sf, zones)
choose_zone <- function(hits, zone_id) {
  vapply(hits, function(ids) {
    if (!length(ids)) NA_character_ else as.character(zone_id[ids[1]])
  }, FUN.VALUE = character(1))
}
trips$from_zone <- choose_zone(start_hits, zones$zone_id)
trips$to_zone <- choose_zone(end_hits, zones$zone_id)
message("Origin zones unassigned: ", sum(is.na(trips$from_zone)),
        "; destination zones unassigned: ", sum(is.na(trips$to_zone)),
        "; multiple origin hits: ", sum(lengths(start_hits) > 1),
        "; multiple destination hits: ", sum(lengths(end_hits) > 1))

# Internal Zurich flows only. Count both survey WP and sample trips in memory.
trips_internal <- trips[!is.na(trips$from_zone) & !is.na(trips$to_zone),
                        c("from_zone", "to_zone", ".weight"), drop = FALSE]
if (!nrow(trips_internal)) stop("No within-study-area OD trips.")
observed_od <- trips_internal |>
  dplyr::group_by(from_zone, to_zone) |>
  dplyr::summarise(trips = sum(.weight), n_samp = dplyr::n(),
                   .groups = "drop") |>
  dplyr::arrange(from_zone, to_zone)
# Only aggregate weighted OD totals are exported; no individual trajectories.
readr::write_csv(dplyr::select(observed_od, from_zone, to_zone, trips),
                 file.path(OUT, "observed_OD_weighted.csv"))
tbd_zone_counts <- table(start_in = !is.na(trips$from_zone),
                         end_in   = !is.na(trips$to_zone))
rm(trips, origins_sf, destinations_sf, start_hits, end_hits, trips_internal)

# ================ 5. FLOW GEOMETRY + OBSERVED MAP =============================
announce("5. Make centroid-to-centroid OD lines and observed map")
make_interzonal_lines <- function(od, zone_ids, centroid_xy, crs) {
  od <- od[!is.na(od$trips) & od$trips > 0 &
             od$from_zone != od$to_zone, , drop = FALSE]
  fr <- match(od$from_zone, zone_ids)
  to <- match(od$to_zone, zone_ids)
  if (anyNA(fr) || anyNA(to)) stop("OD pair references unknown zone.")
  geoms <- lapply(seq_len(nrow(od)), function(i) {
    sf::st_linestring(rbind(centroid_xy[fr[i], 1:2], centroid_xy[to[i], 1:2]))
  })
  sf::st_sf(od, geometry = sf::st_sfc(geoms, crs = crs))
}

plot_od_map <- function(od, title, file_name, max_lines) {
  inter <- od[od$from_zone != od$to_zone & od$trips > 0, , drop = FALSE]
  inter <- inter[order(inter$trips, decreasing = TRUE), , drop = FALSE]
  if (nrow(inter) > max_lines) inter <- utils::head(inter, max_lines)
  inter_sf <- make_interzonal_lines(inter, names_x, xy, sf::st_crs(zones))
  
  intra <- od[od$from_zone == od$to_zone & od$trips > 0, , drop = FALSE]
  size_df <- data.frame(zone_id = intra$from_zone, intra_trips = intra$trips)
  intra_sf <- dplyr::left_join(centroids, size_df, by = "zone_id") |>
    dplyr::filter(!is.na(intra_trips) & intra_trips > 0)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = zones, fill = "grey96", colour = "grey75",
                     linewidth = 0.15)
  if (nrow(inter_sf)) {
    p <- p +
      ggplot2::geom_sf(data = inter_sf,
                       ggplot2::aes(linewidth = trips), color = "#2166ac",
                       alpha = 0.55) +
      ggplot2::scale_linewidth_continuous(range = c(0.2, 2.5),
                                          name = "Interzonal trips")
  }
  if (nrow(intra_sf)) {
    p <- p +
      ggplot2::geom_sf(data = intra_sf,
                       ggplot2::aes(size = intra_trips),
                       shape = 21, fill = "#d6604d", colour = "white",
                       alpha = 0.8, stroke = 0.25) +
      ggplot2::scale_size_area(max_size = 9, name = "Intrazonal trips")
  }
  p <- p + ggplot2::labs(
    title = title,
    subtitle = paste0("Top ", nrow(inter), " interzonal pairs; ",
                      "all intrazonal flows shown. LV95 / EPSG:2056."),
    caption = "Lines join zone centroids (schematic connections, not road routes).") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   legend.position = "right")
  ggplot2::ggsave(file.path(OUT, file_name), p, width = 12,
                  height = 9, dpi = 300, bg = "white")
  invisible(p)
}

observed_lines <- make_interzonal_lines(
  dplyr::select(observed_od, from_zone, to_zone, trips), names_x, xy,
  sf::st_crs(zones))
# As requested: export an actual ESRI shapefile (several companion files).
sf::write_sf(observed_lines,
             dsn = file.path(OUT, "observed_OD_lines.shp"),
             delete_dsn = TRUE, quiet = TRUE)
plot_od_map(observed_od, "Observed MZMV home-to-work OD flows (weighted WP)",
            "map_observed_OD.png", CONFIG$max_map_lines)

# Stop after observed OD maps until sample/coverage checks are reviewed.
# The gravity section also runs 2.2(a) first when enabled.
if (isTRUE(CONFIG$run_part_b)) {
  # ================= 6. MOTORCAR NETWORK + TRAVEL TIME ==========================
  announce("6. Obtain driving-time matrix with Geofabrik OSM + dodgr")
  if (file.exists(CACHE)) {
    graph <- readRDS(CACHE)
    message("Using cached OSM street graph: ", CACHE)
  } else {
    # 2026-10-20: large Overpass download returned HTTP 502. Instead, use a
    # published Switzerland OSM extract from Geofabrik via osmextract, clip its
    # *road network* to the study area, then build dodgr motorcar travel times.
    # This still uses OpenStreetMap streets and the same motorcar weighting.
    # Unlike the original Overpass query, it downloads the national .osm.pbf
    # once (currently approximately 0.5 GB), so allow disk space and time.
    if (!requireNamespace("osmextract", quietly = TRUE)) {
      stop("Install osmextract first: install.packages('osmextract')")
    }
    bb_lv95 <- sf::st_as_sfc(sf::st_bbox(zones))
    bb_expanded <- sf::st_buffer(bb_lv95, CONFIG$osm_margin_m)
    bb_wgs84 <- sf::st_transform(bb_expanded, 4326)
    osm_extract_dir <- resolve("cache/osmextract")
    dir.create(osm_extract_dir, recursive = TRUE, showWarnings = FALSE)
    message("Downloading / reading Geofabrik Switzerland OSM extract. ",
            "The extract and translated layers are cached under: ",
            osm_extract_dir)
    # Compatibility: older CRAN binaries of osmextract may not export
    # oe_get_dodgrnetwork(), despite its presence in the newer online docs.
    # oe_get_network() provides an sf road network; weight_streetnet() builds
    # the equivalent dodgr motorcar graph with right-hand traffic.
    roads_args <- list(
      place = "Switzerland",
      mode = "driving",
      boundary = bb_wgs84,
      boundary_type = "spat",       # keep full crossing segments; do not clip links
      download_directory = osm_extract_dir,
      quiet = FALSE
    )
    # Available in recent versions: normalise OSM oneway/roundabout direction.
    if ("clean_output" %in% names(formals(osmextract::oe_get_network))) {
      roads_args$clean_output <- TRUE
    }
    roads <- do.call(osmextract::oe_get_network, roads_args)
    if (nrow(roads) == 0 ||
        !all(c("highway", "osm_id") %in% names(roads))) {
      stop("No roads, or missing highway/osm_id fields in Geofabrik data.")
    }
    message("OSM driving road features: ", nrow(roads),
            "; building dodgr motorcar graph...")
    graph <- dodgr::weight_streetnet(
      roads, wt_profile = "motorcar", type_col = "highway",
      id_col = "osm_id", left_side = FALSE
    )
    rm(roads, roads_args)
    if (nrow(graph) == 0 || !all(c("time", "time_weighted") %in% names(graph))) {
      stop("The Geofabrik motorcar graph is empty or lacks travel-time weights.")
    }
    saveRDS(graph, CACHE)           # Public OSM road graph; no MZMV person data
    message("Motorcar network cached: ", nrow(graph), " directed edges.")
  }
  
  # Each matrix row/column has exactly the same order as Part 2.1 zones.
  # dodgr_times() returns seconds; shortest=FALSE routes by *fastest* time.
  from_to <- data.frame(lon = as.numeric(ll[, "X"]),
                        lat = as.numeric(ll[, "Y"])) # WGS84 / EPSG:4326
  stopifnot(all(is.finite(as.matrix(from_to))),
            all(abs(from_to$lon) <= 180), all(abs(from_to$lat) <= 90))
  raw_seconds <- dodgr::dodgr_times(graph, from = from_to, to = from_to,
                                    shortest = FALSE)
  if (!all(dim(raw_seconds) == c(nrow(zones), nrow(zones))))
    stop("Routing matrix does not match the number/order of zones.")
  raw_minutes <- raw_seconds / 60
  bad_time <- !is.finite(raw_minutes) | raw_minutes <= 0
  # The zero diagonal is NOT a defect: replace it with the mandated 10 minutes.
  diag(bad_time) <- FALSE
  n_missing <- sum(bad_time)
  message("Non-diagonal OD pairs requiring imputation: ", n_missing,
          " of ", length(raw_minutes) - nrow(raw_minutes))
  
  # Impute disconnected OD pairs by road-adjusted centroid straight-line time.
  # Estimate = straight_line_km * circuity / speed_kmh * 60 minutes.
  # This is transparent but is NOT equivalent to OSM routing, so record its share.
  dx <- outer(xy[, 1], xy[, 1], `-`)
  dy <- outer(xy[, 2], xy[, 2], `-`)
  direct_metres <- sqrt(dx^2 + dy^2)
  fallback_min <- (direct_metres / 1000) * CONFIG$fallback_circuity /
    CONFIG$fallback_speed_kmh * 60
  fallback_min <- pmax(fallback_min, CONFIG$min_interzonal_min)
  time_min <- raw_minutes
  time_min[bad_time] <- fallback_min[bad_time]
  diag(time_min) <- CONFIG$intrazonal_min
  rownames(time_min) <- colnames(time_min) <- names_x
  if (any(!is.finite(time_min)) || any(time_min <= 0))
    stop("Travel-time matrix still contains invalid times after imputation.")
  readr::write_csv(
    data.frame(from_zone = rep(names_x, times = length(names_x)),
               to_zone = rep(names_x, each = length(names_x)),
               travel_time_min = as.vector(time_min),
               imputed = as.vector(bad_time)),
    file.path(OUT, "OD_travel_times_minutes.csv"))
  
  # ===================== 7. GRAVITY MODEL + FURNESS =============================
  announce("7. Doubly constrained gravity model and Furness iterations")
  P <- zone_totals$production
  A <- zone_totals$attraction
  
  # Optionally infer beta from observed mean trip time. This is a model choice,
  # NOT required/calibrated by the handout. Weighted survey coverage matters.
  if (CONFIG$beta_mode == "manual") {
    beta <- CONFIG$beta_per_minute
    warning("Manual beta = ", beta, " per minute. This is illustrative until ",
            "justified via a lecture reference, calibration or sensitivity check.")
  } else if (CONFIG$beta_mode == "match_observed_mean_time") {
    oi <- match(observed_od$from_zone, names_x)
    di <- match(observed_od$to_zone, names_x)
    observed_times <- time_min[cbind(oi, di)]
    observed_mean <- stats::weighted.mean(observed_times, observed_od$trips)
    model_mean <- function(b) {
      sol <- furness(P, A, time_min, b, CONFIG$epsilon,
                     CONFIG$max_iterations)
      sum(sol$matrix * time_min) / sum(sol$matrix)
    }
    fit <- stats::optimize(
      f = function(b) (model_mean(b) - observed_mean)^2,
      interval = CONFIG$beta_search_interval,
      tol = 1e-5)
    beta <- fit$minimum
    message("Observed weighted mean OD time = ", signif(observed_mean, 5),
            " min; fitted beta = ", signif(beta, 5), " per minute; ",
            "model mean = ", signif(model_mean(beta), 5), " min")
    if (min(abs(beta - CONFIG$beta_search_interval)) < 1e-4)
      warning("Beta estimated near search boundary; investigate calibration.")
  } else {
    stop("beta_mode must be 'manual' or 'match_observed_mean_time'.")
  }
  
  result <- furness(P, A, time_min, beta = beta,
                    epsilon = CONFIG$epsilon,
                    max_iterations = CONFIG$max_iterations)
  M <- result$matrix
  rownames(M) <- colnames(M) <- names_x
  stopifnot(max(abs(rowSums(M) - P) / pmax(P, 1)) <= CONFIG$epsilon,
            max(abs(colSums(M) - A) / pmax(A, 1)) <= CONFIG$epsilon)
  message("Converged after ", result$iterations, " iterations; error = ",
          format(result$error, scientific = TRUE))
  
  # Required 3-column export for the following parts of the 4-step model.
  modelled_od <- data.frame(
    from_zone = rep(names_x, times = length(names_x)),
    to_zone = rep(names_x, each = length(names_x)),
    trips = as.vector(M), stringsAsFactors = FALSE)
  readr::write_csv(modelled_od, file.path(OUT, "modelled_OD_trips.csv"))
  # A square OD matrix additionally facilitates inclusion in the report.
  square <- data.frame(from_zone = names_x, M, check.names = FALSE)
  readr::write_csv(square, file.path(OUT, "modelled_OD_matrix.csv"))
  
  balancing <- data.frame(
    zone_id = names_x,
    production_target = P,
    production_model = rowSums(M),
    attraction_target = A,
    attraction_model = colSums(M),
    row_factor = result$row_factor,
    column_factor = result$col_factor)
  readr::write_csv(balancing, file.path(OUT, "furness_zone_balance.csv"))
  readr::write_csv(data.frame(iteration = seq_along(result$history),
                              max_relative_error = result$history),
                   file.path(OUT, "furness_convergence.csv"))
  
  p_conv <- ggplot2::ggplot(
    data.frame(iteration = seq_along(result$history),
               error = pmax(result$history, .Machine$double.eps)),
    ggplot2::aes(iteration, error)) +
    ggplot2::geom_line(colour = "#2166ac") +
    ggplot2::geom_hline(yintercept = CONFIG$epsilon, linetype = 2,
                        colour = "#b2182b") +
    ggplot2::scale_y_log10() +
    ggplot2::labs(title = "Furness convergence",
                  x = "Iteration", y = "Maximum relative marginal error") +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(file.path(OUT, "furness_convergence.png"),
                  p_conv, width = 8, height = 5, dpi = 300, bg = "white")
  
  # ======================== 8. MODELLED MAP =====================================
  announce("8. Export modelled lines and map both intra- and interzonal flows")
  modelled_lines <- make_interzonal_lines(modelled_od, names_x, xy,
                                          sf::st_crs(zones))
  sf::write_sf(modelled_lines,
               dsn = file.path(OUT, "modelled_OD_lines.shp"),
               delete_dsn = TRUE, quiet = TRUE)
  plot_od_map(modelled_od, "Gravity-model home-to-work trip distribution",
              "map_modelled_OD.png", CONFIG$max_map_lines)
  
  # ======================== 9. FINAL QUALITY CHECKS =============================
  announce("9. Summary / quality assurance")
  observed_intra <- sum(observed_od$trips[observed_od$from_zone ==
                                            observed_od$to_zone])
  modelled_intra <- sum(diag(M))
  summary_text <- c(
    "ETH TPM 2026 - Assignment 2.2 quality-assurance summary",
    paste("Zones:", length(names_x)),
    paste("Observed home-work trips (unweighted sample count):",
          sum(observed_od$n_samp)),
    paste("Observed WP-weighted trips within study area:",
          signif(sum(observed_od$trips), 8)),
    paste("Observed weighted intrazonal share (%):",
          signif(100 * observed_intra / sum(observed_od$trips), 5)),
    paste("Part 2.1 production total:", signif(sum(P), 9)),
    paste("Part 2.1 attraction BEFORE scaling:", signif(initial_total_A, 9)),
    paste("Attraction scaling factor:", signif(scale_A, 8)),
    paste("Modelled trip total:", signif(sum(M), 9)),
    paste("Modelled intrazonal share (%):",
          signif(100 * modelled_intra / sum(M), 5)),
    paste("OSM motorcar impedance: dodgr_times(..., shortest=FALSE)"),
    paste("Intrazonal travel time (min):", CONFIG$intrazonal_min),
    paste("Missing off-diagonal OSM OD times imputed:", n_missing),
    paste("Missing OSM OD time share (%):",
          signif(100 * n_missing / (length(time_min) - nrow(time_min)), 5)),
    paste("Fallback speed (km/h):", CONFIG$fallback_speed_kmh),
    paste("Fallback road circuity:", CONFIG$fallback_circuity),
    paste("Gravity deterrence: exp(-beta * t_min); beta =", signif(beta, 8)),
    paste("Furness iterations:", result$iterations),
    paste("Furness final max relative error:", signif(result$error, 8)),
    paste("Maximum absolute production error:",
          signif(max(abs(rowSums(M) - P)), 8)),
    paste("Maximum absolute attraction error:",
          signif(max(abs(colSums(M) - A)), 8))
  )
  writeLines(summary_text, file.path(OUT, "QA_summary.txt"))
  cat(paste(summary_text, collapse = "\n"), "\n")
  message("\nDONE. Aggregated outputs, matrices and figures are in: ", OUT,
          "\nNo individual MZMV trip records were written to disk.")
  
} else {
  message("\nPart 2.2(a) complete: observed flows saved in ", OUT,
          ". Review them before setting CONFIG$run_part_b = TRUE.")
}

# ===================== 10. VALUES REPORTED IN THE TEXT ========================
# Prints the aggregates quoted in the report (no individual MZMV records).
# Requires run_part_b = TRUE.

pct <- function(x) sprintf("%.1f %%", 100 * x)
f0  <- function(x) format(round(x), big.mark = "'")
hdr <- function(s) cat("\n====", s, "====\n")
nz  <- length(names_x)
off <- row(time_min) != col(time_min)

# ---- Assigning trips to zones ----------------------------------------
hdr("Assigning trips to zones (mutually exclusive)")
cat("n   Home->Work trips (before coord check): ", length(valid), "\n")
cat("n1  no valid coordinates / weight:         ", sum(!valid), "\n")
if (exists("tbd_zone_counts")) {
  zc <- tbd_zone_counts
  cat("n2  start outside study area:              ", sum(zc["FALSE", ]), "\n")
  cat("n3  start inside, end outside:             ", zc["TRUE", "FALSE"], "\n")
  cat("n4  both ends inside:                      ", zc["TRUE", "TRUE"], "\n")
  cat("    (end outside, any start):              ", sum(zc[, "FALSE"]), "\n")
} else cat("!! tbd_zone_counts missing: add the extra line to the main script\n")
cat("n4 check (sum of n_samp):                  ", sum(observed_od$n_samp), "\n")

# ---- Aggregation --------------------------------------------------------
intra_o <- observed_od$from_zone == observed_od$to_zone
hdr("Aggregation (observed)")
cat("T_obs weighted trips/day:                  ", f0(sum(observed_od$trips)), "\n")
cat("k OD pairs with >=1 observation:           ", nrow(observed_od), "of", nz^2, "\n")
cat("  interzonal pairs with >=1 obs:           ", sum(!intra_o), "\n")
cat("share of ALL pairs with 0 or 1 obs:        ",
    pct((nz^2 - sum(observed_od$n_samp >= 2)) / nz^2), "\n")
cat("share of observed pairs with exactly 1 obs:", pct(mean(observed_od$n_samp == 1)), "\n")
cat("share of observed pairs with >=5 obs:      ", pct(mean(observed_od$n_samp >= 5)), "\n")
cat("intrazonal share, weighted:                ",
    pct(sum(observed_od$trips[intra_o]) / sum(observed_od$trips)), "\n")
cat("intrazonal share, unweighted:              ",
    pct(sum(observed_od$n_samp[intra_o]) / sum(observed_od$n_samp)), "\n")
cat("interzonal lines on observed map:          ",
    min(CONFIG$max_map_lines, sum(!intra_o)), "\n")

# ---- Balancing --------------------------------------------------------------
hdr("Balancing productions and attractions")
cat("sum P:                                     ", f0(initial_total_P), "\n")
cat("sum A before scaling:                      ", f0(initial_total_A), "\n")
cat("scale factor sum(P)/sum(A):                ", signif(scale_A, 5), "\n")

# ---- Travel times -------------------------------------------------------------
hdr("Travel times")
cat("missing interzonal times:                  ", n_missing, "of", sum(off),
    "(", pct(n_missing / sum(off)), ")\n")
tt <- time_min[off]
cat("interzonal min / median / mean / max (min):",
    round(min(tt), 1), "/", round(median(tt), 1), "/",
    round(mean(tt), 1), "/", round(max(tt), 1), "\n")

# ---- Gamma ---------------------------------------------------------------------
hdr("Deterrence parameter")
c_mod <- sum(M * time_min) / sum(M)
cat("c_obs (weighted mean, observed):           ", round(observed_mean, 2), "min\n")
cat("gamma (= beta in code):                    ", signif(beta, 4), "per min\n")
cat("c_mod (check, should equal c_obs):         ", round(c_mod, 2), "min\n")

# ---- IPF -----------------------------------------------------------------------
hdr("IPF")
cat("iterations:                                ", result$iterations, "\n")
cat("final max relative error:                  ", signif(result$error, 3), "\n")

# ---- Comparison table ------------------------------------------------------------
hdr("Comparison table (observed | modelled)")
cat("Trips per working day:  ", f0(sum(observed_od$trips)), "|", f0(sum(M)), "\n")
cat("Intrazonal share:       ",
    pct(sum(observed_od$trips[intra_o]) / sum(observed_od$trips)), "|",
    pct(sum(diag(M)) / sum(M)), "\n")
cat("Mean car time (min):    ", round(observed_mean, 1), "|", round(c_mod, 1), "\n")
cat("OD pairs with trips:    ", nrow(observed_od), "|", sum(M > 0),
    "  (modelled >= 1 trip/day:", sum(M >= 1), ")\n")

# ---- Top 10 modelled interzonal flows ----------------------------------------------
hdr("Top 10 modelled interzonal flows (LaTeX rows)")
Mi <- M; diag(Mi) <- NA
idx <- order(Mi, decreasing = TRUE, na.last = NA)[1:10]
oz <- names_x[row(Mi)[idx]]; dz <- names_x[col(Mi)[idx]]
cat(sprintf("%s & %s & %s \\\\\n", oz, dz, f0(Mi[idx])), sep = "")
cat("\nAttributes of these zones (to find a name column):\n")
print(head(sf::st_drop_geometry(zones_raw[zones_raw$zone_id %in% c(oz, dz), ]), 20))

# ---- Optional: trip-time distribution, 5-min bands --------------------------------
hdr("Trip-time distribution, % of trips per 5-min band (observed | modelled)")
br <- seq(0, ceiling(max(time_min) / 5) * 5, by = 5)
t_obs <- time_min[cbind(match(observed_od$from_zone, names_x),
                        match(observed_od$to_zone, names_x))]
obs_b <- tapply(observed_od$trips, cut(t_obs, br, include.lowest = TRUE), sum)
mod_b <- tapply(as.vector(M), cut(as.vector(time_min), br, include.lowest = TRUE), sum)
ttd <- data.frame(observed = obs_b / sum(observed_od$trips), modelled = mod_b / sum(M))
ttd[is.na(ttd)] <- 0
print(round(100 * ttd, 1))
# intrazonal trips (c = 10 min) fall into the 5-10 band; split them off
intra_m <- row(M) == col(M)
cat("5-10 band, interzonal only (observed | modelled):",
    pct(sum(observed_od$trips[!intra_o & t_obs > 5 & t_obs <= 10]) / sum(observed_od$trips)), "|",
    pct(sum(M[!intra_m & time_min > 5 & time_min <= 10]) / sum(M)), "\n")
cat("mean interzonal time (observed | modelled):",
    round(weighted.mean(t_obs[!intra_o], observed_od$trips[!intra_o]), 1), "|",
    round(weighted.mean(time_min[!intra_m], M[!intra_m]), 1), "min\n")


library(osmdata)
bb <- sf::st_bbox(sf::st_transform(
  sf::st_buffer(sf::st_as_sfc(sf::st_bbox(zones)), 10000), 4326))
q <- opq(bbox = bb, timeout = 180) |> add_osm_feature(key = "highway")
t0 <- Sys.time()
res <- tryCatch(osmdata_sf(q), error = function(e) e)
Sys.time() - t0
if (inherits(res, "error")) conditionMessage(res) else
  paste("succeeded:", nrow(res$osm_lines), "road lines")