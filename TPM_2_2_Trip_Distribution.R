
# ================================================================
# ETH Zurich - Transport Planning Methods (HS 2026)
# Assignment 2.2 - Trip Distribution (Gravity Model)
# Part A: Observed Home -> Work OD Flows
# ================================================================

# ---------------------------------------------------------------
# Step 0: Load packages and define base paths
# ---------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "data.table",
  "sf",
  "ggplot2"
)

missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(sf)
  library(ggplot2)
})

# Main project directory
project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

# Assignment 2.1 output directory
path_21 <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1"
)

# Assignment 2.2 output directory
path_22 <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2"
)

# Relevant files from Assignment 2.1
zones_file <- file.path(
  path_21, "trip_generation_provisional.gpkg"
)

totals_file <- file.path(
  path_21, "trip_generation_zones.csv"
)

# Check paths before creating new output files
stopifnot(dir.exists(project_dir))
stopifnot(file.exists(zones_file))
stopifnot(file.exists(totals_file))

cat("Project directory verified.\n")
cat("Assignment 2.1 input files found.\n\n")


# ---------------------------------------------------------------
# Step 1: Read and inspect Assignment 2.1 results
# ---------------------------------------------------------------

# Identify layers in the GeoPackage
layers_21 <- sf::st_layers(zones_file)
print(layers_21)

if (length(layers_21$name) != 1L) {
  stop("Please identify the correct GeoPackage layer.")
}

# Load zone geometries
npvm_zones <- sf::read_sf(
  zones_file,
  layer = layers_21$name[1],
  quiet = TRUE
)

# Load zonal production and attraction data
zone_totals_21 <- readr::read_csv(
  totals_file,
  show_col_types = FALSE
)

# Inspect number of zones
cat("\n--- Zone counts ---\n")
cat("GeoPackage:", nrow(npvm_zones), "\n")
cat("CSV:", nrow(zone_totals_21), "\n")

# Inspect column names
cat("\n--- GeoPackage columns ---\n")
print(names(npvm_zones))

cat("\n--- CSV columns ---\n")
print(names(zone_totals_21))

# Inspect spatial reference system
cat("\n--- Coordinate reference system ---\n")
print(sf::st_crs(npvm_zones)$input)
print(sf::st_crs(npvm_zones)$epsg)

# Inspect geographic extent
cat("\n--- Bounding box ---\n")
print(sf::st_bbox(npvm_zones))

# Check geometry
cat("\n--- Geometry checks ---\n")
print(table(sf::st_geometry_type(npvm_zones)))

valid_geometry <- sf::st_is_valid(npvm_zones)

cat(
  "Valid geometries:",
  sum(valid_geometry),
  "of", nrow(npvm_zones), "\n"
)

# Identify potentially relevant ID columns
cat("\n--- Possible zone identifiers ---\n")

id_columns <- grep(
  "id|row",
  names(sf::st_drop_geometry(npvm_zones)),
  ignore.case = TRUE,
  value = TRUE
)

print(id_columns)

# Inspect candidate production/attraction columns
cat("\n--- Production and attraction candidates ---\n")

candidate_columns <- grep(
  "trip|prod|attract|work",
  names(zone_totals_21),
  ignore.case = TRUE,
  value = TRUE
)

numeric_candidates <- candidate_columns[
  vapply(
    zone_totals_21[candidate_columns],
    is.numeric,
    logical(1)
  )
]

if (length(numeric_candidates) > 0) {
  
  totals_check <- data.frame(
    column = numeric_candidates,
    total = vapply(
      zone_totals_21[numeric_candidates],
      sum,
      numeric(1)
    )
  )
  
  print(totals_check, row.names = FALSE)
  
} else {
  cat("No numeric candidate columns identified.\n")
}

cat("\nInitial data inspection completed.\n")


# ---------------------------------------------------------------
# Step 1: Read data
# ---------------------------------------------------------------

library(sf)
library(readr)

# 1.1 Verify the Assignment 2.1 zone identifiers

zone_id_col <- "tg_ID_NPVM"

stopifnot(
  nrow(npvm_zones) == 143,
  nrow(zone_totals_21) == 143,
  zone_id_col %in% names(npvm_zones),
  zone_id_col %in% names(zone_totals_21)
)

zone_ids <- as.character(npvm_zones[[zone_id_col]])
csv_ids <- as.character(zone_totals_21[[zone_id_col]])

if (anyNA(zone_ids) || anyNA(csv_ids) ||
    anyDuplicated(zone_ids) || anyDuplicated(csv_ids)) {
  stop("Missing or duplicate zone identifiers.")
}

if (!setequal(zone_ids, csv_ids)) {
  stop("GeoPackage and CSV zone identifiers differ.")
}

# Compare attributes using the zone IDs, not row order.
check_cols <- c(
  "employees",
  "trips_work",
  "attractor_work",
  "attraction_work"
)

csv_order <- match(zone_ids, csv_ids)

for (nm in check_cols) {
  x <- npvm_zones[[nm]]
  y <- zone_totals_21[[nm]][csv_order]
  
  if (!isTRUE(all.equal(x, y, tolerance = 1e-6))) {
    stop("GeoPackage/CSV mismatch in: ", nm)
  }
}

cat("Zone identifiers and attributes match.\n")

# Production and attraction from Assignment 2.1
P <- npvm_zones$trips_work
A <- npvm_zones$attraction_work

if (anyNA(P) || anyNA(A) ||
    any(P < 0) || any(A < 0)) {
  stop("Invalid production or attraction values.")
}

if (abs(sum(P) - sum(A)) > 0.01) {
  stop("Production and attraction are not balanced.")
}

cat("Production:", sum(P), "\n")
cat("Attraction:", sum(A), "\n\n")


# 1.2 Locate protected MZMV archive

mzmv_dir <- file.path(
  project_dir,
  "Materials for descriptive_analysis-20260930"
)

zip_file <- file.path(
  mzmv_dir,
  "MZMV_Data_protect.zip"
)

if (!file.exists(zip_file)) {
  stop(
    "Protected MZMV ZIP not found at:\n",
    zip_file,
    "\nPlease check the archive location."
  )
}

if (!requireNamespace("archive", quietly = TRUE) ||
    !requireNamespace("askpass", quietly = TRUE)) {
  stop("Install the archive and askpass packages.")
}

# Require a version that supports password-protected ZIPs.
if (!"password" %in%
    names(formals(archive::archive_read))) {
  stop("Please update the archive package.")
}


# 1.3 Read protected files directly into R memory

mzmv_data <- local({
  
  password <- askpass::askpass(
    "Enter the MZMV archive password"
  )
  
  if (!nzchar(password)) {
    stop("No archive password provided.")
  }
  
  members <- archive::archive(
    zip_file,
    password = password
  )$path
  
  read_member <- function(filename) {
    
    matches <- members[
      members == filename |
        endsWith(members, paste0("/", filename))
    ]
    
    if (length(matches) != 1L) {
      stop(
        "Expected exactly one ZIP member named: ",
        filename
      )
    }
    
    connection <- archive::archive_read(
      zip_file,
      file = matches[1],
      password = password
    )
    
    on.exit(
      try(close(connection), silent = TRUE),
      add = TRUE
    )
    
    readr::read_delim(
      file = connection,
      delim = ";",
      col_types = readr::cols(
        .default = readr::col_character()
      ),
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  
  list(
    trips = read_member("wegeinland.csv"),
    persons = read_member("zielpersonen.csv"),
    households = read_member("haushalte.csv")
  )
})

df_tripsCH <- mzmv_data$trips
df_persons <- mzmv_data$persons
df_households <- mzmv_data$households

rm(mzmv_data)


# 1.4 Verify required variables

check_required <- function(data, columns, dataset) {
  
  missing <- setdiff(columns, names(data))
  
  if (length(missing) > 0) {
    stop(
      dataset, " is missing: ",
      paste(missing, collapse = ", ")
    )
  }
}

check_required(
  df_tripsCH,
  c(
    "HHNR", "AUSNR", "WEGNR",
    "S_X", "S_Y", "Z_X", "Z_Y",
    "wzweck1"
  ),
  "Trips"
)

check_required(
  df_persons,
  c("HHNR", "AMSTAT", "Tag"),
  "Persons"
)

check_required(
  df_households,
  c("HHNR", "W_X", "W_Y"),
  "Households"
)

if (!("WP" %in% names(df_tripsCH) ||
      "WP" %in% names(df_persons))) {
  stop("Person weight WP not found.")
}


# 1.5 Display non-identifying quality checks

cat("\n--- MZMV data successfully loaded ---\n")

cat("Trip records:", nrow(df_tripsCH), "\n")
cat("Person records:", nrow(df_persons), "\n")
cat("Household records:", nrow(df_households), "\n")

cat(
  "WP available in trips:",
  "WP" %in% names(df_tripsCH), "\n"
)

cat(
  "WP available in persons:",
  "WP" %in% names(df_persons), "\n"
)

cat(
  "Duplicate person HHNR:",
  anyDuplicated(df_persons$HHNR), "\n"
)

cat(
  "Duplicate household HHNR:",
  anyDuplicated(df_households$HHNR), "\n"
)

cat("\nStep 1 completed.\n")


# ---------------------------------------------------------------
# Step 2: Assign zones to trips
# ---------------------------------------------------------------

library(dplyr)
library(sf)

# Requires npvm_zones, df_tripsCH, df_persons and
# df_households from Steps 0 and 1.

stopifnot(
  exists("npvm_zones"),
  exists("df_tripsCH"),
  exists("df_persons"),
  exists("df_households"),
  sf::st_crs(npvm_zones)$epsg == 2056
)

# Convert MZMV numbers safely (including decimal commas).
as_mzmv_numeric <- function(x) {
  suppressWarnings(
    as.numeric(gsub(",", ".", trimws(as.character(x)),
                    fixed = TRUE))
  )
}

# ---------------------------------------------------------------
# 2.1 Identify eligible respondents
# ---------------------------------------------------------------

# AMSTAT = 1: employed persons
# Tag = 1...5: Monday to Friday
# No restriction to canton Zurich (as in Assignment 2.1).

eligible_persons <- df_persons %>%
  filter(
    AMSTAT == "1",
    Tag %in% as.character(1:5)
  ) %>%
  select(HHNR)

cat("Eligible employed weekday respondents:",
    nrow(eligible_persons), "\n")


# ---------------------------------------------------------------
# 2.2 Identify trips starting at home
# ---------------------------------------------------------------

# Retain all trips during home-origin classification.
# Filtering to work trips beforehand would incorrectly
# identify the first work trip as the first outing trip.

trips_all <- df_tripsCH %>%
  mutate(weg.id = row_number()) %>%
  select(
    weg.id, HHNR, AUSNR, WEGNR, wzweck1, WP,
    S_X, S_Y, Z_X, Z_Y
  ) %>%
  left_join(
    df_households %>%
      select(HHNR, W_X, W_Y),
    by = "HHNR"
  ) %>%
  mutate(
    aus_nr = as_mzmv_numeric(AUSNR),
    weg_nr = as_mzmv_numeric(WEGNR),
    start_lon = as_mzmv_numeric(S_X),
    start_lat = as_mzmv_numeric(S_Y),
    dest_lon = as_mzmv_numeric(Z_X),
    dest_lat = as_mzmv_numeric(Z_Y),
    home_lon = as_mzmv_numeric(W_X),
    home_lat = as_mzmv_numeric(W_Y)
  ) %>%
  group_by(HHNR, AUSNR) %>%
  mutate(
    first_outing = if (all(is.na(weg_nr))) {
      FALSE
    } else {
      !is.na(aus_nr) & aus_nr > 0 &
        !is.na(weg_nr) &
        weg_nr == min(weg_nr, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Approximate great-circle distance for short distances.
# This reproduces the distance approach of Group E 2.1.

distance_m <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  
  6371000 * sqrt(
    ((lon2 - lon1) * rad *
       cos((lat1 + lat2) / 2 * rad))^2 +
      ((lat2 - lat1) * rad)^2
  )
}

valid_home_coords <- with(
  trips_all,
  is.finite(start_lon) & is.finite(start_lat) &
    is.finite(home_lon) & is.finite(home_lat) &
    abs(start_lon) <= 180 & abs(home_lon) <= 180 &
    abs(start_lat) <= 90 & abs(home_lat) <= 90
)

trips_all <- trips_all %>%
  mutate(
    distance_home_m = distance_m(
      start_lon, start_lat, home_lon, home_lat
    ),
    near_home = valid_home_coords &
      distance_home_m <= 200,
    starts_at_home =
      coalesce(first_outing, FALSE) |
      coalesce(near_home, FALSE),
    is_work = coalesce(wzweck1 == "2", FALSE)
  )


# ---------------------------------------------------------------
# 2.3 Retain Home -> Work trips on weekdays
# ---------------------------------------------------------------

trips_work <- trips_all %>%
  semi_join(eligible_persons, by = "HHNR") %>%
  filter(starts_at_home, is_work) %>%
  mutate(weight = as_mzmv_numeric(WP))

n_home_work <- nrow(trips_work)

cat("Home -> Work trip records:", n_home_work, "\n")

if (n_home_work == 0) {
  stop("No Home -> Work trips identified.")
}


# ---------------------------------------------------------------
# 2.4 Check spatial coordinates and weights
# ---------------------------------------------------------------

valid_trip <- with(
  trips_work,
  is.finite(start_lon) & is.finite(start_lat) &
    is.finite(dest_lon) & is.finite(dest_lat) &
    abs(start_lon) <= 180 & abs(dest_lon) <= 180 &
    abs(start_lat) <= 90 & abs(dest_lat) <= 90 &
    is.finite(weight) & weight >= 0
)

cat("Invalid coordinates or WP:", sum(!valid_trip), "\n")

trips_work <- trips_work[valid_trip, ]

if (nrow(trips_work) == 0) {
  stop("No valid Home -> Work trips remain.")
}


# ---------------------------------------------------------------
# 2.5 Convert trip endpoints to spatial data
# ---------------------------------------------------------------

# MZMV coordinates: WGS84 (EPSG:4326)
# NPVM polygons: Swiss LV95 (EPSG:2056)

trips_Start <- st_as_sf(
  trips_work,
  coords = c("start_lon", "start_lat"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(2056)

trips_Ziel <- st_as_sf(
  trips_work,
  coords = c("dest_lon", "dest_lat"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(2056)


# ---------------------------------------------------------------
# 2.6 Assign origin and destination zones
# ---------------------------------------------------------------

start_hits <- st_intersects(
  trips_Start, npvm_zones
)

ziel_hits <- st_intersects(
  trips_Ziel, npvm_zones
)

# Identify ambiguous polygon intersections.
n_multiple_start <- sum(lengths(start_hits) > 1)
n_multiple_ziel <- sum(lengths(ziel_hits) > 1)

cat("Multiple origin-zone matches:",
    n_multiple_start, "\n")
cat("Multiple destination-zone matches:",
    n_multiple_ziel, "\n")

if (n_multiple_start > 0 || n_multiple_ziel > 0) {
  stop(
    "Some points intersect multiple polygons. ",
    "Review these assignments before proceeding."
  )
}

# Preserve actual NPVM identifiers rather than relying
# on the polygon row numbers.

zone_ids <- as.character(npvm_zones$tg_ID_NPVM)

get_zone <- function(hits, ids) {
  vapply(
    hits,
    function(x) {
      if (length(x) == 0) {
        NA_character_
      } else {
        ids[x[1]]
      }
    },
    character(1)
  )
}

trips_work$from_zone <- get_zone(
  start_hits, zone_ids
)

trips_work$to_zone <- get_zone(
  ziel_hits, zone_ids
)


# ---------------------------------------------------------------
# 2.7 Keep internal study-area trips
# ---------------------------------------------------------------

origin_inside <- !is.na(trips_work$from_zone)
destination_inside <- !is.na(trips_work$to_zone)

n_outside_origin <- sum(!origin_inside)

n_inside_outside <- sum(
  origin_inside & !destination_inside
)

n_internal <- sum(
  origin_inside & destination_inside
)

stopifnot(
  n_outside_origin +
    n_inside_outside +
    n_internal == nrow(trips_work)
)

# Keep only anonymised zone-level trip information
# needed for the subsequent maps and aggregation.
# Do NOT export individual survey trip records.

trips_internal <- trips_work %>%
  filter(
    !is.na(from_zone),
    !is.na(to_zone)
  ) %>%
  transmute(
    trip_index = row_number(),
    from_zone,
    to_zone,
    WP = weight
  )


# ---------------------------------------------------------------
# 2.8 Quality assurance
# ---------------------------------------------------------------

cat("\n--- Part A, Step 2: Summary ---\n")
cat("Eligible respondents:",
    nrow(eligible_persons), "\n")
cat("Home -> Work trips before coordinate check:",
    n_home_work, "\n")
cat("Trips with valid coordinates and WP:",
    nrow(trips_work), "\n")
cat("Origin outside study area:",
    n_outside_origin, "\n")
cat("Origin inside, destination outside:",
    n_inside_outside, "\n")
cat("Both endpoints inside study area:",
    n_internal, "\n")
cat("Weighted sum of internal observed trips:",
    round(sum(trips_internal$WP), 2), "\n")
cat("Intrazonal sampled trips:",
    sum(trips_internal$from_zone ==
          trips_internal$to_zone), "\n")
cat("Interzonal sampled trips:",
    sum(trips_internal$from_zone !=
          trips_internal$to_zone), "\n")

cat("\nStep 2 completed.\n")


# ---------------------------------------------------------------
# Step 3: Visualize individual origin-destination flows
# ---------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)

# Requires the verified Step 2 results.
if (!exists("trips_internal") ||
    !exists("npvm_zones") ||
    !exists("project_dir")) {
  stop("Run Steps 0-2 before Step 3.")
}

stopifnot(
  nrow(npvm_zones) == 143,
  sf::st_crs(npvm_zones)$epsg == 2056,
  all(c("from_zone", "to_zone", "WP") %in%
        names(trips_internal))
)

# ---------------------------------------------------------------
# 3.1 Generate geographical zone centroids
# ---------------------------------------------------------------

npvm_centroids <- st_centroid(npvm_zones)

zone_ids <- as.character(
  npvm_centroids$tg_ID_NPVM
)

centroid_xy <- st_coordinates(npvm_centroids)

# ---------------------------------------------------------------
# 3.2 Separate interzonal and intrazonal observations
# ---------------------------------------------------------------

individual_trips <- trips_internal %>%
  mutate(
    from_idx = match(from_zone, zone_ids),
    to_idx = match(to_zone, zone_ids)
  )

if (anyNA(individual_trips$from_idx) ||
    anyNA(individual_trips$to_idx)) {
  stop("Some observed trips reference unknown zones.")
}

trips_inter <- individual_trips %>%
  filter(from_zone != to_zone)

trips_intra <- individual_trips %>%
  filter(from_zone == to_zone)

stopifnot(
  nrow(trips_inter) + nrow(trips_intra) ==
    nrow(individual_trips)
)

# ---------------------------------------------------------------
# 3.3 Create one linestring per interzonal trip
# ---------------------------------------------------------------

create_linestring <- function(point1, point2) {
  st_linestring(rbind(point1, point2))
}

individual_lines <- lapply(
  seq_len(nrow(trips_inter)),
  function(i) {
    create_linestring(
      centroid_xy[trips_inter$from_idx[i], 1:2],
      centroid_xy[trips_inter$to_idx[i], 1:2]
    )
  }
)

individual_lines_sf <- st_sf(
  trip_index = trips_inter$trip_index,
  geometry = st_sfc(
    individual_lines,
    crs = 2056
  )
)

# ---------------------------------------------------------------
# 3.4 Create one centroid point per intrazonal trip
# ---------------------------------------------------------------

individual_intra_sf <- st_sf(
  trip_index = trips_intra$trip_index,
  geometry = st_geometry(npvm_centroids)[
    trips_intra$from_idx
  ]
)

# ---------------------------------------------------------------
# 3.5 Verify the individual geometries
# ---------------------------------------------------------------

cat("\n--- Individual OD geometries ---\n")

cat(
  "Interzonal lines:",
  nrow(individual_lines_sf), "\n"
)

cat(
  "Intrazonal points:",
  nrow(individual_intra_sf), "\n"
)

cat(
  "Total represented observations:",
  nrow(individual_lines_sf) +
    nrow(individual_intra_sf), "\n"
)

stopifnot(
  nrow(individual_lines_sf) == nrow(trips_inter),
  nrow(individual_intra_sf) == nrow(trips_intra),
  nrow(individual_lines_sf) +
    nrow(individual_intra_sf) ==
    nrow(trips_internal)
)

# ---------------------------------------------------------------
# 3.6 Visualise all individual observed trips
# ---------------------------------------------------------------

plot_individual <- ggplot() +
  
  # Background: NPVM 2010 zones
  geom_sf(
    data = npvm_zones,
    fill = "grey96",
    colour = "grey78",
    linewidth = 0.15
  ) +
  
  # One line per interzonal trip (unweighted)
  geom_sf(
    data = individual_lines_sf,
    aes(colour = "Interzonal"),
    linewidth = 0.30,
    alpha = 0.25
  ) +
  
  # One point per intrazonal trip (unweighted)
  geom_sf(
    data = individual_intra_sf,
    aes(colour = "Intrazonal"),
    size = 1.6,
    alpha = 0.5
  ) +
  
  scale_colour_manual(
    name = "Trip type",
    values = c(
      "Interzonal" = "#2166AC",
      "Intrazonal" = "#D6604D"
    ),
    breaks = c("Interzonal", "Intrazonal")
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Individual observed Home-Work trips",
    subtitle = paste0(
      "Zurich region | MZMV 2021 | ",
      nrow(individual_trips),
      " unweighted observations"
    ),
    caption = paste0(
      "Each line or point represents one survey trip. ",
      "Connections use zone centroids, not actual trip ",
      "coordinates. Overlapping observations may coincide."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8,
      colour = "grey40"
    ),
    legend.position = "bottom"
  )

# Display in RStudio
print(plot_individual)

# ---------------------------------------------------------------
# 3.7 Export the report figure
# ---------------------------------------------------------------

output_dir <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = file.path(
    output_dir,
    "PartA_Individual_Home_Work_Flows.png"
  ),
  plot = plot_individual,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\nStep 3 completed.\n")
cat("Individual OD map saved successfully.\n")


# ---------------------------------------------------------------
# Step 4: Aggregated OD flows between zones
# ---------------------------------------------------------------

library(dplyr)
library(tidyr)

# Verify required objects from previous steps
stopifnot(
  exists("trips_internal"),
  exists("npvm_zones"),
  all(c("from_zone", "to_zone", "WP") %in%
        names(trips_internal))
)

# ---------------------------------------------------------------
# 4.1 Aggregate commuting trips by OD relation
# ---------------------------------------------------------------

tripsOD <- trips_internal %>%
  group_by(from_zone, to_zone) %>%
  summarise(
    n_Work = sum(WP),
    n_sample = n(),
    .groups = "drop"
  ) %>%
  arrange(from_zone, to_zone)

# Check aggregation
cat("\n--- Observed OD aggregation ---\n")

cat("Individual observations:",
    nrow(trips_internal), "\n")

cat("Observed OD pairs:",
    nrow(tripsOD), "\n")

cat("Sum of weighted trips:",
    round(sum(tripsOD$n_Work), 2), "\n")

cat("Sum of sample counts:",
    sum(tripsOD$n_sample), "\n")

# Verify totals
stopifnot(
  sum(tripsOD$n_sample) == nrow(trips_internal),
  isTRUE(all.equal(
    sum(tripsOD$n_Work),
    sum(trips_internal$WP),
    tolerance = 1e-8
  ))
)

# ---------------------------------------------------------------
# 4.2 Create a complete 143 x 143 OD matrix
# ---------------------------------------------------------------

zone_ids <- as.character(npvm_zones$tg_ID_NPVM)

npvm_centroids_matr <- tidyr::expand_grid(
  from_zone = zone_ids,
  to_zone = zone_ids
) %>%
  left_join(
    tripsOD,
    by = c("from_zone", "to_zone")
  ) %>%
  mutate(
    n_Work = coalesce(n_Work, 0),
    n_sample = coalesce(n_sample, 0L)
  )

# Verify matrix dimensions
stopifnot(
  nrow(npvm_centroids_matr) == 143^2,
  isTRUE(all.equal(
    sum(npvm_centroids_matr$n_Work),
    sum(trips_internal$WP),
    tolerance = 1e-8
  ))
)

# ---------------------------------------------------------------
# 4.3 Calculate intrazonal and interzonal totals
# ---------------------------------------------------------------

intra_OD <- tripsOD %>%
  filter(from_zone == to_zone)

inter_OD <- tripsOD %>%
  filter(from_zone != to_zone)

total_weighted <- sum(tripsOD$n_Work)
intra_weighted <- sum(intra_OD$n_Work)
inter_weighted <- sum(inter_OD$n_Work)

cat("\n--- Intrazonal / interzonal flows ---\n")

cat("Intrazonal OD pairs:",
    nrow(intra_OD), "\n")

cat("Interzonal OD pairs:",
    nrow(inter_OD), "\n")

cat("Weighted intrazonal trips:",
    round(intra_weighted, 2), "\n")

cat("Weighted interzonal trips:",
    round(inter_weighted, 2), "\n")

cat("Intrazonal share:",
    round(100 * intra_weighted / total_weighted, 2),
    "%\n")

cat("Interzonal share:",
    round(100 * inter_weighted / total_weighted, 2),
    "%\n")

# ---------------------------------------------------------------
# 4.4 Assess sample coverage
# ---------------------------------------------------------------

cat("\n--- Sample coverage ---\n")

cat("Total possible OD pairs:",
    nrow(npvm_centroids_matr), "\n")

cat("OD pairs with observations:",
    nrow(tripsOD), "\n")

cat("OD pairs with exactly one observation:",
    sum(tripsOD$n_sample == 1), "\n")

cat("OD pairs with at least five observations:",
    sum(tripsOD$n_sample >= 5), "\n")

cat("Percentage of OD pairs with observations:",
    round(
      100 * nrow(tripsOD) /
        nrow(npvm_centroids_matr),
      2
    ),
    "%\n")

cat("\nStep 4.1 completed.\n")


# ---------------------------------------------------------------
# Step 4.5: Create spatial lines for aggregated OD flows
# ---------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)

# Requires completed Steps 0-4.4.
stopifnot(
  exists("npvm_zones"),
  exists("npvm_centroids_matr"),
  exists("tripsOD"),
  exists("project_dir"),
  nrow(npvm_centroids_matr) == 143^2
)

# ---------------------------------------------------------------
# 4.5.1 Generate zone centroids
# ---------------------------------------------------------------

zone_ids <- as.character(npvm_zones$tg_ID_NPVM)

# Retain only zone IDs and centroid geometry.
npvm_centroids <- st_sf(
  zone_id = zone_ids,
  geometry = st_centroid(st_geometry(npvm_zones))
)

centroid_xy <- st_coordinates(npvm_centroids)

# ---------------------------------------------------------------
# 4.5.2 Create lines for all interzonal OD relations
# ---------------------------------------------------------------

# The complete OD table contains 143 x 143 relations.
# Intrazonal pairs are represented separately as points.

od_pairs_inter <- npvm_centroids_matr %>%
  filter(from_zone != to_zone) %>%
  mutate(
    from_idx = match(from_zone, zone_ids),
    to_idx = match(to_zone, zone_ids)
  )

if (anyNA(od_pairs_inter$from_idx) ||
    anyNA(od_pairs_inter$to_idx)) {
  stop("Unknown zone identifiers in OD matrix.")
}

# Create centroid-to-centroid linestrings.
create_linestring <- function(point1, point2) {
  st_linestring(rbind(point1, point2))
}

od_geometries <- lapply(
  seq_len(nrow(od_pairs_inter)),
  function(i) {
    create_linestring(
      centroid_xy[od_pairs_inter$from_idx[i], 1:2],
      centroid_xy[od_pairs_inter$to_idx[i], 1:2]
    )
  }
)

od_lines_all_sf <- st_sf(
  od_pairs_inter %>%
    select(from_zone, to_zone, n_Work, n_sample),
  geometry = st_sfc(
    od_geometries,
    crs = st_crs(npvm_zones)
  )
)

# Keep only OD relations containing observed trips.
od_lines_MZMV_sf <- od_lines_all_sf %>%
  filter(n_sample > 0)

# ---------------------------------------------------------------
# 4.5.3 Prepare intrazonal observations
# ---------------------------------------------------------------

wegeIntra_MZMV <- npvm_centroids_matr %>%
  filter(from_zone == to_zone) %>%
  transmute(
    zone_id = from_zone,
    n_Work,
    n_sample
  )

intra_MZMV_sf <- npvm_centroids %>%
  left_join(
    wegeIntra_MZMV,
    by = "zone_id"
  ) %>%
  filter(n_sample > 0)

# ---------------------------------------------------------------
# 4.5.4 Verify spatial aggregation
# ---------------------------------------------------------------

cat("\n--- Aggregated OD geometry checks ---\n")

cat("All possible interzonal relations:",
    nrow(od_lines_all_sf), "\n")

cat("Observed interzonal relations:",
    nrow(od_lines_MZMV_sf), "\n")

cat("Observed intrazonal relations:",
    nrow(intra_MZMV_sf), "\n")

cat("Weighted interzonal total:",
    round(sum(od_lines_MZMV_sf$n_Work), 2), "\n")

cat("Weighted intrazonal total:",
    round(sum(intra_MZMV_sf$n_Work), 2), "\n")

stopifnot(
  nrow(od_lines_all_sf) == 143 * 142,
  nrow(od_lines_MZMV_sf) == nrow(inter_OD),
  nrow(intra_MZMV_sf) == nrow(intra_OD),
  isTRUE(all.equal(
    sum(od_lines_MZMV_sf$n_Work) +
      sum(intra_MZMV_sf$n_Work),
    sum(tripsOD$n_Work),
    tolerance = 1e-8
  ))
)

# ---------------------------------------------------------------
# 4.5.5 Select the strongest interzonal flows for display
# ---------------------------------------------------------------

max_map_lines <- 350L

sorted_indices <- order(
  od_lines_MZMV_sf$n_Work,
  decreasing = TRUE
)

display_indices <- head(
  sorted_indices,
  max_map_lines
)

od_lines_display <- od_lines_MZMV_sf[
  display_indices, 
]

cat("Interzonal lines displayed:",
    nrow(od_lines_display), "\n")

# ---------------------------------------------------------------
# 4.5.6 Visualise aggregated OD flows
# ---------------------------------------------------------------

plot_aggregated <- ggplot() +
  
  # Study area background
  geom_sf(
    data = npvm_zones,
    fill = "grey96",
    colour = "grey78",
    linewidth = 0.15
  ) +
  
  # Weighted interzonal connections
  geom_sf(
    data = od_lines_display,
    aes(linewidth = n_Work),
    colour = "#2166AC",
    alpha = 0.45
  ) +
  
  # Weighted intrazonal trips
  geom_sf(
    data = intra_MZMV_sf,
    aes(size = n_Work),
    shape = 21,
    fill = "#D6604D",
    colour = "white",
    stroke = 0.25,
    alpha = 0.85
  ) +
  
  scale_linewidth_continuous(
    range = c(0.2, 2.5),
    name = "Interzonal trips (WP)"
  ) +
  
  scale_size_area(
    max_size = 9,
    name = "Intrazonal trips (WP)"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Observed Home-Work OD flows",
    subtitle = paste0(
      "Zurich region | MZMV 2021 | ",
      "WP-weighted commuting flows"
    ),
    caption = paste0(
      "Strongest ", nrow(od_lines_display),
      " interzonal OD pairs shown; all observed ",
      "intrazonal flows included. ",
      "Connections represent zone centroids."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8,
      colour = "grey40"
    ),
    legend.position = "right"
  )

# Display figure in RStudio
print(plot_aggregated)

# ---------------------------------------------------------------
# 4.5.7 Export figure and aggregated spatial data
# ---------------------------------------------------------------

output_dir <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = file.path(
    output_dir,
    "PartA_Aggregated_Home_Work_Flows.png"
  ),
  plot = plot_aggregated,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Export observed interzonal relations as a shapefile.
# This is aggregated survey information, not individual
# coordinates. Small cells still require confidentiality.
# Keep this file locally under the course's data safeguards.

st_write(
  od_lines_MZMV_sf,
  dsn = file.path(
    output_dir,
    "PartA_Observed_OD_Lines.shp"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("\nStep 4.5 completed.\n")
cat("Aggregated map and shapefile saved.\n")


# ---------------------------------------------------------------
# Step 4.6: Verify exported files
# ---------------------------------------------------------------

library(sf)

shp_path <- file.path(
  output_dir,
  "PartA_Observed_OD_Lines.shp"
)

png_path <- file.path(
  output_dir,
  "PartA_Aggregated_Home_Work_Flows.png"
)

cat("\n--- Export verification ---\n")

cat("PNG exists:", file.exists(png_path), "\n")
cat("Shapefile exists:", file.exists(shp_path), "\n")

# If shapefile is missing, create it without deleting
# a potentially nonexistent file.
if (!file.exists(shp_path)) {
  
  sf::st_write(
    od_lines_MZMV_sf,
    dsn = shp_path,
    quiet = TRUE
  )
}

# Verify shapefile components
shp_components <- paste0(
  tools::file_path_sans_ext(shp_path),
  c(".shp", ".shx", ".dbf", ".prj")
)

if (!all(file.exists(shp_components))) {
  stop("One or more shapefile components are missing.")
}

# Read the exported shapefile back into R
od_check <- sf::st_read(
  shp_path,
  quiet = TRUE
)

cat("Exported OD relations:",
    nrow(od_check), "\n")

cat("Coordinate system:",
    sf::st_crs(od_check)$epsg, "\n")

cat("Geometry type:\n")
print(table(sf::st_geometry_type(od_check)))

stopifnot(
  file.exists(png_path),
  nrow(od_check) == 466,
  sf::st_crs(od_check)$epsg == 2056,
  all(sf::st_geometry_type(od_check) == "LINESTRING")
)

cat("\nAll export checks passed.\n")


# ================================================================
# Part B: Gravity Model
# ================================================================

# ---------------------------------------------------------------
# Step 1: Balance produced and attracted trips per zone
# ---------------------------------------------------------------

library(sf)
library(dplyr)
library(readr)

# 1.1 Define project directory

project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

path_21 <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1"
)

zones_file <- file.path(
  path_21,
  "trip_generation_provisional.gpkg"
)

totals_file <- file.path(
  path_21,
  "trip_generation_zones.csv"
)

stopifnot(
  file.exists(zones_file),
  file.exists(totals_file)
)

# 1.2 Load verified Assignment 2.1 results

npvm_zones <- st_read(
  zones_file,
  quiet = TRUE
)

zone_totals_21 <- read_csv(
  totals_file,
  show_col_types = FALSE
)

stopifnot(
  nrow(npvm_zones) == 143,
  nrow(zone_totals_21) == 143,
  st_crs(npvm_zones)$epsg == 2056
)

# 1.3 Verify zone identifiers and attributes

zone_ids <- as.character(
  npvm_zones$tg_ID_NPVM
)

csv_ids <- as.character(
  zone_totals_21$tg_ID_NPVM
)

if (anyNA(zone_ids) || anyNA(csv_ids) ||
    anyDuplicated(zone_ids) ||
    anyDuplicated(csv_ids) ||
    !setequal(zone_ids, csv_ids)) {
  stop("Invalid or inconsistent zone identifiers.")
}

csv_order <- match(zone_ids, csv_ids)

for (nm in c("trips_work", "attraction_work")) {
  
  if (!isTRUE(all.equal(
    npvm_zones[[nm]],
    zone_totals_21[[nm]][csv_order],
    tolerance = 1e-6
  ))) {
    stop("Inconsistent zone values: ", nm)
  }
}

# 1.4 Prepare Furness production and attraction

npvm_zones$trips_work_FUR <-
  npvm_zones$trips_work

# Use BALANCED attraction, not raw FTE.
npvm_zones$attractor_work_FUR <-
  npvm_zones$attraction_work

P <- npvm_zones$trips_work_FUR
A <- npvm_zones$attractor_work_FUR

if (any(!is.finite(P)) ||
    any(!is.finite(A)) ||
    any(P < 0) ||
    any(A < 0)) {
  stop("Invalid production or attraction values.")
}

# 1.5 Check regional balance

total_P <- sum(P)
total_A <- sum(A)

balance_difference <- total_P - total_A

cat("\n--- Part B: Production and attraction ---\n")

cat("Number of zones:", nrow(npvm_zones), "\n")

cat("Total production:",
    round(total_P, 2), "\n")

cat("Total attraction:",
    round(total_A, 2), "\n")

cat("Difference:",
    round(balance_difference, 6), "\n")

# Assignment 2.1 already balanced the totals.
# Do not rescale again unless the inputs differ.

if (abs(balance_difference) > 0.01) {
  stop(
    "Production and attraction are not balanced. ",
    "Investigate Assignment 2.1 inputs."
  )
}

cat("Production/attraction balance verified.\n")

# 1.6 Verify zone-level values

cat("\n--- Zonal statistics ---\n")

print(
  data.frame(
    Variable = c("Production", "Attraction"),
    Minimum = c(min(P), min(A)),
    Mean = c(mean(P), mean(A)),
    Maximum = c(max(P), max(A)),
    Zero_zones = c(sum(P == 0), sum(A == 0))
  )
)

cat("\nPart B, Step 1 completed.\n")

# ---------------------------------------------------------------
# Step 2: Get a network for travel-time estimation
# ---------------------------------------------------------------

library(sf)

required_packages <- c("osmextract", "dodgr")

missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

# ---------------------------------------------------------------
# 2.1 Load study-area geometry
# ---------------------------------------------------------------

project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

zones_file <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1",
  "trip_generation_provisional.gpkg"
)

stopifnot(file.exists(zones_file))

npvm_zones <- sf::read_sf(
  zones_file,
  quiet = TRUE
)

stopifnot(
  nrow(npvm_zones) == 143,
  sf::st_crs(npvm_zones)$epsg == 2056
)

# ---------------------------------------------------------------
# 2.2 Define network buffer
# ---------------------------------------------------------------

# Use Swiss LV95 for distances in metres.
# The buffer allows roads outside the study area
# to be included in possible shortest paths.

osm_margin_m <- 10000

bbox_lv95 <- sf::st_as_sfc(
  sf::st_bbox(npvm_zones)
)

bbox_buffer_lv95 <- sf::st_buffer(
  bbox_lv95,
  dist = osm_margin_m
)

# Convert the buffered polygon to WGS84.
bbox_wgs84 <- sf::st_transform(
  bbox_buffer_lv95,
  crs = 4326
)

cat("\n--- OSM study-area extent ---\n")

print(sf::st_bbox(bbox_wgs84))

cat("Buffer distance:",
    osm_margin_m / 1000, "km\n")


# ---------------------------------------------------------------
# 2.3 Define persistent cache
# ---------------------------------------------------------------

# Public OSM data can be saved locally.
# No confidential MZMV data are used here.

cache_dir <- file.path(
  project_dir,
  "cache"
)

osm_cache_dir <- file.path(
  cache_dir,
  "osmextract"
)

dir.create(
  osm_cache_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

network_file <- file.path(
  cache_dir,
  "motorcar_network_10km.rds"
)

# Record the geographic extent and network settings
# so an incompatible cached graph is not reused.

network_settings <- list(
  zone_ids = as.character(npvm_zones$tg_ID_NPVM),
  bbox_lv95 = unname(
    as.numeric(sf::st_bbox(npvm_zones))
  ),
  buffer_m = osm_margin_m,
  profile = "motorcar",
  source = "Geofabrik Switzerland",
  clean_output = TRUE
)



# ---------------------------------------------------------------
# Step 2.4: Obtain OSM network (osmextract 0.6.0)
# ---------------------------------------------------------------

library(sf)

stopifnot(
  exists("bbox_wgs84"),
  exists("osm_cache_dir"),
  exists("network_file"),
  exists("network_settings"),
  requireNamespace("osmextract", quietly = TRUE),
  requireNamespace("dodgr", quietly = TRUE)
)

cat("osmextract version:",
    as.character(packageVersion("osmextract")), "\n")

# Explicitly record manual one-way processing.
network_settings$clean_output <- "manual_v0.6"

if (file.exists(network_file)) {
  
  cached <- readRDS(network_file)
  
  if (!is.list(cached) ||
      is.null(cached$network) ||
      !identical(cached$settings, network_settings)) {
    stop("Cached network settings are incompatible.")
  }
  
  network <- cached$network
  rm(cached)
  
  cat("Cached motorcar network loaded.\n")
  
} else {
  
  # -------------------------------------------------------------
  # 2.4.1 Download the OSM driving network
  # -------------------------------------------------------------
  
  cat("\nObtaining Geofabrik road network...\n")
  
  osm_roads <- osmextract::oe_get_network(
    place = "Switzerland",
    mode = "driving",
    boundary = bbox_wgs84,
    boundary_type = "spat",
    download_directory = osm_cache_dir,
    force_download = FALSE,
    quiet = FALSE
  )
  
  if (nrow(osm_roads) == 0) {
    stop("The extracted OSM network is empty.")
  }
  
  required_columns <- c(
    "osm_id", "highway", "oneway"
  )
  
  if (!all(required_columns %in% names(osm_roads))) {
    stop("Required OSM road attributes are missing.")
  }
  
  cat("\nOSM road features:",
      nrow(osm_roads), "\n")
  
  # -------------------------------------------------------------
  # 2.4.2 Prepare network geometry
  # -------------------------------------------------------------
  
  osm_roads <- sf::st_cast(
    osm_roads,
    "LINESTRING",
    warn = FALSE
  )
  
  # -------------------------------------------------------------
  # 2.4.3 Normalise one-way restrictions
  # -------------------------------------------------------------
  
  # osmextract 0.6.0 supplies the oneway attribute
  # but does not provide clean_output.
  #
  # OSM:
  #   yes = travel in geometry direction
  #   no  = bidirectional travel
  #   -1  = travel against geometry direction
  
  oneway <- tolower(trimws(
    as.character(osm_roads$oneway)
  ))
  
  oneway[oneway == ""] <- NA_character_
  
  # Roundabouts imply one-way travel unless
  # explicitly tagged otherwise.
  if ("junction" %in% names(osm_roads)) {
    
    roundabout <- !is.na(osm_roads$junction) &
      tolower(as.character(osm_roads$junction)) ==
      "roundabout"
    
    oneway[roundabout & is.na(oneway)] <- "yes"
  }
  
  # Reverse geometries where oneway = -1.
  reverse_idx <- which(oneway == "-1")
  
  if (length(reverse_idx) > 0) {
    
    geometries <- sf::st_geometry(osm_roads)
    
    geometries[reverse_idx] <- sf::st_reverse(
      geometries[reverse_idx]
    )
    
    sf::st_geometry(osm_roads) <- geometries
    
    oneway[reverse_idx] <- "yes"
  }
  
  # Standardise alternative OSM values.
  oneway[oneway %in% c("1", "true")] <- "yes"
  oneway[oneway %in% c("0", "false")] <- "no"
  
  # Simplifying assumption for variable-direction roads:
  # treat reversible/alternating roads as bidirectional.
  oneway[oneway %in% c(
    "reversible", "alternating"
  )] <- "no"
  
  # Missing restrictions are treated as bidirectional.
  oneway[is.na(oneway)] <- "no"
  
  unknown_values <- setdiff(
    unique(oneway),
    c("yes", "no")
  )
  
  if (length(unknown_values) > 0) {
    stop(
      "Unrecognised oneway values: ",
      paste(unknown_values, collapse = ", ")
    )
  }
  
  osm_roads$oneway <- oneway
  
  cat("\n--- One-way road checks ---\n")
  print(table(osm_roads$oneway))
  cat("Reversed geometries:",
      length(reverse_idx), "\n")
  
  # -------------------------------------------------------------
  # Step 2.5: Construct the dodgr motorcar network
  # -------------------------------------------------------------
  
  cat("\nConstructing motorcar network...\n")
  
  network <- dodgr::weight_streetnet(
    osm_roads,
    wt_profile = "motorcar",
    type_col = "highway",
    id_col = "osm_id",
    left_side = FALSE
  )
  
  if (nrow(network) == 0) {
    stop("The weighted network is empty.")
  }
  
  required_network_columns <- c(
    "from_id",
    "to_id",
    "time",
    "time_weighted"
  )
  
  if (!all(required_network_columns %in%
           names(network))) {
    stop("Required routing columns are missing.")
  }
  
  # Save public network and reproducibility settings.
  saveRDS(
    list(
      settings = network_settings,
      network = network,
      osmextract_version = as.character(
        packageVersion("osmextract")
      ),
      dodgr_version = as.character(
        packageVersion("dodgr")
      ),
      created = Sys.Date()
    ),
    file = network_file
  )
  
  rm(osm_roads)
  gc()
  
  cat("\nMotorcar network saved to cache.\n")
}


# ---------------------------------------------------------------
# Step 2.6: Verify the network
# ---------------------------------------------------------------

stopifnot(
  is.data.frame(network),
  nrow(network) > 0,
  all(c("from_id", "to_id",
        "time", "time_weighted") %in% names(network))
)

n_vertices <- length(unique(c(
  as.character(network$from_id),
  as.character(network$to_id)
)))

cat("\n--- Motorcar network quality checks ---\n")

cat("Directed network edges:",
    nrow(network), "\n")

cat("Unique network vertices:",
    n_vertices, "\n")

cat("Non-finite travel times:",
    sum(!is.finite(network$time)), "\n")

cat("Non-finite weighted travel times:",
    sum(!is.finite(network$time_weighted)), "\n")

cat("Network cache exists:",
    file.exists(network_file), "\n")

if (!any(is.finite(network$time) &
         network$time > 0) ||
    !any(is.finite(network$time_weighted) &
         network$time_weighted > 0)) {
  stop("No usable positive travel times found.")
}

cat("\nPart B, Step 2 completed.\n")


# ---------------------------------------------------------------
# Step 3: Calculate travel times between zones
# ---------------------------------------------------------------

library(sf)

# ---------------------------------------------------------------
# 3.1 Load verified zones and motorcar network
# ---------------------------------------------------------------

project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

zones_file <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1",
  "trip_generation_provisional.gpkg"
)

network_file <- file.path(
  project_dir,
  "cache",
  "motorcar_network_10km.rds"
)

stopifnot(
  file.exists(zones_file),
  file.exists(network_file),
  requireNamespace("dodgr", quietly = TRUE)
)

npvm_zones <- sf::read_sf(
  zones_file,
  quiet = TRUE
)

zone_ids <- as.character(
  npvm_zones$tg_ID_NPVM
)

stopifnot(
  length(zone_ids) == 143,
  !anyNA(zone_ids),
  !anyDuplicated(zone_ids),
  sf::st_crs(npvm_zones)$epsg == 2056
)

# Reuse network already in memory if available.
# Otherwise load the previously cached graph.

if (!exists("network", inherits = FALSE)) {
  
  cached_network <- readRDS(network_file)
  
  if (!is.list(cached_network) ||
      is.null(cached_network$network)) {
    stop("Invalid cached network file.")
  }
  
  network <- cached_network$network
  rm(cached_network)
}

stopifnot(
  is.data.frame(network),
  nrow(network) > 0,
  all(c("time", "time_weighted") %in%
        names(network))
)

cat("Motorcar network ready.\n")
cat("Network edges:", nrow(network), "\n")


# ---------------------------------------------------------------
# 3.2 Calculate zone centroids
# ---------------------------------------------------------------

# Centroids are calculated in LV95.
# Only geometry is passed to st_centroid to avoid
# the polygon-attribute warning from Part A.

npvm_centroids <- st_sf(
  zone_id = zone_ids,
  geometry = st_centroid(
    st_geometry(npvm_zones)
  )
)

# Convert to WGS84 for dodgr routing.
centroids_wgs84 <- st_transform(
  npvm_centroids,
  crs = 4326
)

coordinates <- st_coordinates(
  centroids_wgs84
)

# Ensure exact origin/destination row ordering.
routing_points <- data.frame(
  lon = as.numeric(coordinates[, "X"]),
  lat = as.numeric(coordinates[, "Y"])
)

stopifnot(
  nrow(routing_points) == 143,
  all(is.finite(as.matrix(routing_points))),
  all(abs(routing_points$lon) <= 180),
  all(abs(routing_points$lat) <= 90)
)

cat("\n--- Routing coordinates ---\n")
cat("Centroids prepared:",
    nrow(routing_points), "\n")


# ---------------------------------------------------------------
# 3.3 Calculate all-pairs motorcar travel times
# ---------------------------------------------------------------

# Store the routing matrix separately so that
# shortest-path calculations need not be repeated.

time_cache_file <- file.path(
  project_dir,
  "cache",
  "motorcar_times_raw_143.rds"
)

# Record inputs to prevent accidental reuse of
# a matrix from a different network or zone order.

network_info <- file.info(network_file)

time_cache_settings <- list(
  zone_ids = zone_ids,
  network_size = as.numeric(network_info$size),
  network_modified = as.numeric(network_info$mtime),
  dodgr_version = as.character(
    packageVersion("dodgr")
  )
)

if (file.exists(time_cache_file)) {
  
  time_cache <- readRDS(time_cache_file)
  
  if (!is.list(time_cache) ||
      !identical(
        time_cache$settings,
        time_cache_settings
      )) {
    stop("Cached travel-time matrix is incompatible.")
  }
  
  raw_seconds <- time_cache$raw_seconds
  rm(time_cache)
  
  cat("\nLoaded cached travel-time matrix.\n")
  
} else {
  
  cat("\nCalculating all-pairs travel times...\n")
  
  # All origin/destination pairs.
  # shortest = FALSE selects fastest paths.
  # Returned travel times are in seconds.
  
  raw_seconds <- dodgr::dodgr_times(
    graph = network,
    from = routing_points,
    to = routing_points,
    shortest = FALSE
  )
  
  # Save public network-derived results only.
  saveRDS(
    list(
      settings = time_cache_settings,
      raw_seconds = raw_seconds
    ),
    file = time_cache_file
  )
  
  cat("Raw travel-time matrix cached.\n")
}


# ---------------------------------------------------------------
# 3.4 Check matrix dimensions and convert units
# ---------------------------------------------------------------

raw_seconds <- as.matrix(raw_seconds)

if (!identical(dim(raw_seconds), c(143L, 143L))) {
  stop("Unexpected travel-time matrix dimensions.")
}

storage.mode(raw_seconds) <- "double"

# Conversion from seconds to minutes.
raw_minutes <- raw_seconds / 60

# Preserve actual NPVM identifiers.
rownames(raw_minutes) <- zone_ids
colnames(raw_minutes) <- zone_ids

# Identify intrazonal and interzonal elements.
off_diagonal <- row(raw_minutes) != col(raw_minutes)

interzonal_times <- raw_minutes[off_diagonal]

valid_interzonal <- is.finite(interzonal_times) &
  interzonal_times > 0

missing_interzonal <- !valid_interzonal

cat("\n--- Raw travel-time matrix ---\n")

cat("Rows:", nrow(raw_minutes), "\n")
cat("Columns:", ncol(raw_minutes), "\n")

cat("Total OD pairs:",
    length(raw_minutes), "\n")

cat("Interzonal OD pairs:",
    length(interzonal_times), "\n")

cat("Valid interzonal travel times:",
    sum(valid_interzonal), "\n")

cat("Missing or invalid interzonal times:",
    sum(missing_interzonal), "\n")

cat("Non-finite interzonal times:",
    sum(!is.finite(interzonal_times)), "\n")

cat("Non-positive finite interzonal times:",
    sum(is.finite(interzonal_times) &
          interzonal_times <= 0), "\n")


# ---------------------------------------------------------------
# 3.5 Summarise valid interzonal travel times
# ---------------------------------------------------------------

valid_times <- interzonal_times[valid_interzonal]

if (length(valid_times) == 0) {
  stop("No valid interzonal travel times found.")
}

cat("\n--- Interzonal travel-time statistics ---\n")

cat("Minimum (min):",
    round(min(valid_times), 2), "\n")

cat("Median (min):",
    round(median(valid_times), 2), "\n")

cat("Mean (min):",
    round(mean(valid_times), 2), "\n")

cat("Maximum (min):",
    round(max(valid_times), 2), "\n")

cat("Times exceeding 90 min:",
    sum(valid_times > 90), "\n")

cat("\nStep 3.1-3.5 completed.\n")

# ---------------------------------------------------------------
# Step 3.6: Complete the OD travel-time matrix
# ---------------------------------------------------------------

library(sf)

# 3.6.1 Load verified zone and routing data

project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

zones_file <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1",
  "trip_generation_provisional.gpkg"
)

time_cache_file <- file.path(
  project_dir,
  "cache",
  "motorcar_times_raw_143.rds"
)

stopifnot(
  file.exists(zones_file),
  file.exists(time_cache_file)
)

npvm_zones <- sf::read_sf(
  zones_file, quiet = TRUE
)

zone_ids <- as.character(
  npvm_zones$tg_ID_NPVM
)

n_zones <- length(zone_ids)

time_cache <- readRDS(time_cache_file)

stopifnot(
  n_zones == 143,
  sf::st_crs(npvm_zones)$epsg == 2056,
  identical(time_cache$settings$zone_ids, zone_ids)
)

raw_minutes <- as.matrix(
  time_cache$raw_seconds
) / 60

stopifnot(
  identical(dim(raw_minutes),
            c(n_zones, n_zones))
)

rownames(raw_minutes) <- zone_ids
colnames(raw_minutes) <- zone_ids

# ---------------------------------------------------------------
# 3.6.2 Diagnose missing interzonal times
# ---------------------------------------------------------------

off_diagonal <- row(raw_minutes) !=
  col(raw_minutes)

missing_off <- off_diagonal &
  (!is.finite(raw_minutes) | raw_minutes <= 0)

n_missing <- sum(missing_off)

missing_by_origin <- rowSums(missing_off)
missing_by_destination <- colSums(missing_off)

cat("\n--- Missing travel-time diagnosis ---\n")

cat("Missing interzonal connections:",
    n_missing, "\n")

cat("Missing share (%):",
    round(100 * n_missing /
            (n_zones * (n_zones - 1)), 2),
    "\n")

cat("Origins with missing connections:",
    sum(missing_by_origin > 0), "\n")

cat("Destinations with missing connections:",
    sum(missing_by_destination > 0), "\n")

cat("Maximum missing per origin:",
    max(missing_by_origin), "\n")

cat("Maximum missing per destination:",
    max(missing_by_destination), "\n")

# Identify origins with the most missing routes.
origin_diagnostics <- data.frame(
  zone_id = zone_ids,
  missing_routes = missing_by_origin
)

origin_diagnostics <- origin_diagnostics[
  order(origin_diagnostics$missing_routes,
        decreasing = TRUE),
]

cat("\nTop five origins with missing routes:\n")
print(head(origin_diagnostics, 5),
      row.names = FALSE)

# ---------------------------------------------------------------
# Step 3.7: Impute missing travel times
# ---------------------------------------------------------------

# Use zone-centroid straight-line distances.
# LV95 coordinates are measured in metres.

centroid_geometry <- st_centroid(
  st_geometry(npvm_zones)
)

xy <- st_coordinates(centroid_geometry)

dx <- outer(xy[, 1], xy[, 1], "-")
dy <- outer(xy[, 2], xy[, 2], "-")

distance_km <- sqrt(dx^2 + dy^2) / 1000

# Explicit fallback assumptions
fallback_speed_kmh <- 40
fallback_circuity <- 1.3
min_interzonal_min <- 1
intrazonal_min <- 10

# Time = distance / speed * 60
fallback_minutes <- (
  distance_km * fallback_circuity /
    fallback_speed_kmh
) * 60

fallback_minutes <- pmax(
  fallback_minutes,
  min_interzonal_min
)

# Retain all valid routed times.
time_min <- raw_minutes

# Impute only missing interzonal connections.
time_min[missing_off] <-
  fallback_minutes[missing_off]

# ETH assumption: 10 minutes within each zone.
diag(time_min) <- intrazonal_min

rownames(time_min) <- zone_ids
colnames(time_min) <- zone_ids

# ---------------------------------------------------------------
# 3.7.1 Inspect imputed values
# ---------------------------------------------------------------

imputed_values <- time_min[missing_off]

cat("\n--- Imputed travel times ---\n")

cat("Number imputed:",
    length(imputed_values), "\n")

if (length(imputed_values) > 0) {
  cat("Minimum:",
      round(min(imputed_values), 2), "min\n")
  cat("Median:",
      round(median(imputed_values), 2), "min\n")
  cat("Mean:",
      round(mean(imputed_values), 2), "min\n")
  cat("Maximum:",
      round(max(imputed_values), 2), "min\n")
}

# ---------------------------------------------------------------
# Step 3.8: Final quality checks
# ---------------------------------------------------------------

stopifnot(
  nrow(time_min) == 143,
  ncol(time_min) == 143,
  all(is.finite(time_min)),
  all(time_min > 0),
  all(diag(time_min) == 10),
  identical(rownames(time_min), zone_ids),
  identical(colnames(time_min), zone_ids)
)

# Verify that valid routed values were unchanged.
valid_off <- off_diagonal & !missing_off

stopifnot(
  isTRUE(all.equal(
    time_min[valid_off],
    raw_minutes[valid_off],
    tolerance = 1e-12
  ))
)

final_interzonal <- time_min[off_diagonal]

cat("\n--- Final travel-time matrix ---\n")

cat("OD pairs:", length(time_min), "\n")

cat("Missing/non-finite entries:",
    sum(!is.finite(time_min)), "\n")

cat("Minimum interzonal time:",
    round(min(final_interzonal), 2),
    "min\n")

cat("Median interzonal time:",
    round(median(final_interzonal), 2),
    "min\n")

cat("Mean interzonal time:",
    round(mean(final_interzonal), 2),
    "min\n")

cat("Maximum interzonal time:",
    round(max(final_interzonal), 2),
    "min\n")

cat("Intrazonal time:",
    unique(diag(time_min)), "min\n")

# ---------------------------------------------------------------
# 3.8.1 Export completed OD travel-time table
# ---------------------------------------------------------------

output_dir <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# R matrices are flattened column by column.
od_travel_times <- data.frame(
  from_zone = rep(zone_ids, times = n_zones),
  to_zone = rep(zone_ids, each = n_zones),
  travel_time_min = as.vector(time_min),
  imputed = as.vector(missing_off)
)

stopifnot(
  nrow(od_travel_times) == n_zones^2,
  sum(od_travel_times$imputed) == n_missing
)

write.csv(
  od_travel_times,
  file = file.path(
    output_dir,
    "OD_travel_times_minutes.csv"
  ),
  row.names = FALSE
)

# Also save the square matrix for Part B, Step 4.
saveRDS(
  list(
    time_min = time_min,
    missing_off = missing_off,
    fallback_speed_kmh = fallback_speed_kmh,
    fallback_circuity = fallback_circuity,
    intrazonal_min = intrazonal_min
  ),
  file = file.path(
    output_dir,
    "OD_travel_times_matrix.rds"
  )
)

cat("\nStep 3 completed successfully.\n")
cat("Completed travel-time matrix exported.\n")

# ---------------------------------------------------------------
# Step 4: Implement the Furness algorithm / IPF
# ---------------------------------------------------------------

library(sf)

# ---------------------------------------------------------------
# 4.1 Load the verified inputs
# ---------------------------------------------------------------

project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

zones_file <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1",
  "trip_generation_provisional.gpkg"
)

time_file <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2",
  "OD_travel_times_matrix.rds"
)

stopifnot(
  file.exists(zones_file),
  file.exists(time_file)
)

npvm_zones <- sf::read_sf(zones_file, quiet = TRUE)
time_data <- readRDS(time_file)

zone_ids <- as.character(npvm_zones$tg_ID_NPVM)

P <- as.numeric(npvm_zones$trips_work)
A <- as.numeric(npvm_zones$attraction_work)

time_min <- time_data$time_min
missing_off <- time_data$missing_off

n <- length(zone_ids)

stopifnot(
  n == 143,
  identical(dim(time_min), c(n, n)),
  identical(dim(missing_off), c(n, n)),
  identical(rownames(time_min), zone_ids),
  identical(colnames(time_min), zone_ids),
  all(is.finite(P)), all(is.finite(A)),
  all(P > 0), all(A > 0),
  abs(sum(P) - sum(A)) < 0.01,
  all(is.finite(time_min)),
  all(time_min > 0)
)

# ---------------------------------------------------------------
# 4.2 Investigate disconnected network centroids
# ---------------------------------------------------------------

isolated_idx <- which(
  rowSums(missing_off) == n - 1
)

isolated_ids <- zone_ids[isolated_idx]

cat("\n--- Disconnected centroid diagnosis ---\n")

cat("Fully disconnected origin centroids:",
    length(isolated_idx), "\n")

print(isolated_ids)

if (length(isolated_idx) > 0) {
  
  isolated_zones <- data.frame(
    zone_id = isolated_ids,
    production = P[isolated_idx],
    attraction = A[isolated_idx]
  )
  
  print(isolated_zones, row.names = FALSE)
  
  cat(
    "Production share of affected zones (%):",
    round(100 * sum(P[isolated_idx]) / sum(P), 3),
    "\n"
  )
  
  cat(
    "Attraction share of affected zones (%):",
    round(100 * sum(A[isolated_idx]) / sum(A), 3),
    "\n"
  )
  
  involving_isolated <- outer(
    seq_len(n) %in% isolated_idx,
    seq_len(n) %in% isolated_idx,
    "|"
  )
  
  cat(
    "All missing pairs involve these zones:",
    all(!missing_off | involving_isolated),
    "\n"
  )
}


# ---------------------------------------------------------------
# 4.3 Implement the Furness function
# ---------------------------------------------------------------

Furness <- function(P, A, travel_time,
                    gamma,
                    epsilon = 1e-6,
                    max_iterations = 10000L) {
  
  # Verify inputs
  stopifnot(
    is.matrix(travel_time),
    nrow(travel_time) == length(P),
    ncol(travel_time) == length(A),
    all(is.finite(travel_time)),
    all(travel_time > 0),
    all(P > 0),
    all(A > 0),
    is.finite(gamma),
    gamma >= 0
  )
  
  if (abs(sum(P) - sum(A)) > 1e-8 * sum(P)) {
    stop("Production and attraction totals differ.")
  }
  
  # Exponential deterrence function
  deterrence <- exp(-gamma * travel_time)
  
  # Initial gravity matrix
  trips <- outer(P, A) * deterrence
  
  # Store convergence history
  error_history <- numeric(max_iterations)
  converged <- FALSE
  
  for (iteration in seq_len(max_iterations)) {
    
    # Step 1: Balance rows to production
    row_totals <- rowSums(trips)
    
    if (any(!is.finite(row_totals)) ||
        any(row_totals <= 0)) {
      stop("Invalid row totals during IPF.")
    }
    
    row_factors <- P / row_totals
    
    trips <- sweep(
      trips, 1, row_factors, "*"
    )
    
    # Step 2: Balance columns to attraction
    col_totals <- colSums(trips)
    
    if (any(!is.finite(col_totals)) ||
        any(col_totals <= 0)) {
      stop("Invalid column totals during IPF.")
    }
    
    col_factors <- A / col_totals
    
    trips <- sweep(
      trips, 2, col_factors, "*"
    )
    
    # Step 3: Calculate maximum relative error
    error <- max(
      abs(rowSums(trips) - P) / P,
      abs(colSums(trips) - A) / A
    )
    
    error_history[iteration] <- error
    
    # Step 4: Check convergence
    if (error <= epsilon) {
      converged <- TRUE
      break
    }
  }
  
  if (!converged) {
    stop(
      "Furness did not converge. Final error: ",
      signif(error, 6)
    )
  }
  
  return(list(
    trips = trips,
    iterations = iteration,
    error = error,
    error_history = error_history[
      seq_len(iteration)
    ]
  ))
}


# ---------------------------------------------------------------
# 4.4 Independent unit test
# ---------------------------------------------------------------

test_P <- c(120, 80)
test_A <- c(70, 130)

test_time <- matrix(
  c(10, 30, 20, 10),
  nrow = 2,
  byrow = TRUE
)

test_result <- Furness(
  P = test_P,
  A = test_A,
  travel_time = test_time,
  gamma = 0.01
)

stopifnot(
  max(abs(rowSums(test_result$trips) -
            test_P) / test_P) <= 1e-6,
  max(abs(colSums(test_result$trips) -
            test_A) / test_A) <= 1e-6
)

cat("\nFurness unit test passed.\n")


# ---------------------------------------------------------------
# 4.5 Apply Furness with an illustrative gamma
# ---------------------------------------------------------------

# gamma = 0.01 is ONLY for testing.
# It will be calibrated in the next step.

gamma_test <- 0.01

result_test <- Furness(
  P = P,
  A = A,
  travel_time = time_min,
  gamma = gamma_test,
  epsilon = 1e-6
)

T_test <- result_test$trips

rownames(T_test) <- zone_ids
colnames(T_test) <- zone_ids


# ---------------------------------------------------------------
# 4.6 Verify marginal constraints
# ---------------------------------------------------------------

production_error <- rowSums(T_test) - P
attraction_error <- colSums(T_test) - A

max_relative_P <- max(abs(production_error) / P)
max_relative_A <- max(abs(attraction_error) / A)

cat("\n--- Furness verification ---\n")

cat("Test gamma:", gamma_test, "per minute\n")

cat("Iterations:",
    result_test$iterations, "\n")

cat("Final maximum relative error:",
    signif(result_test$error, 6), "\n")

cat("Total production:",
    round(sum(P), 2), "\n")

cat("Total modelled trips:",
    round(sum(T_test), 2), "\n")

cat("Maximum absolute production error:",
    signif(max(abs(production_error)), 6), "\n")

cat("Maximum absolute attraction error:",
    signif(max(abs(attraction_error)), 6), "\n")

cat("Maximum relative production error:",
    signif(max_relative_P, 6), "\n")

cat("Maximum relative attraction error:",
    signif(max_relative_A, 6), "\n")

stopifnot(
  max_relative_P <= 1e-6,
  max_relative_A <= 1e-6
)


# ---------------------------------------------------------------
# 4.7 Preliminary model diagnostics
# ---------------------------------------------------------------

mean_model_time <- sum(
  T_test * time_min
) / sum(T_test)

intra_share <- sum(diag(T_test)) /
  sum(T_test)

imputed_demand_share <- sum(
  T_test[missing_off]
) / sum(T_test)

cat("\n--- Preliminary gravity-model diagnostics ---\n")

cat("Modelled mean travel time:",
    round(mean_model_time, 2), "min\n")

cat("Modelled intrazonal share:",
    round(100 * intra_share, 2), "%\n")

cat("Demand using imputed travel times:",
    round(100 * imputed_demand_share, 2), "%\n")

cat("\nStep 4.1 completed successfully.\n")

# ---------------------------------------------------------------
# Step 4.8: Calibrate the gravity deterrence parameter
# ---------------------------------------------------------------

# Requires the verified Step 4 inputs.
stopifnot(
  exists("Furness"),
  exists("P"),
  exists("A"),
  exists("time_min"),
  exists("zone_ids"),
  exists("missing_off")
)

# ---------------------------------------------------------------
# 4.8.1 Recover observed MZMV OD flows
# ---------------------------------------------------------------

# Use Part A's WP-weighted aggregated OD table.
# If necessary, reconstruct it from the in-memory
# internal trip records.

if (exists("tripsOD")) {
  
  observed_od <- as.data.frame(tripsOD)
  
} else if (exists("trips_internal")) {
  
  observed_od <- aggregate(
    WP ~ from_zone + to_zone,
    data = trips_internal,
    FUN = sum
  )
  
  names(observed_od)[
    names(observed_od) == "WP"
  ] <- "n_Work"
  
} else {
  
  stop(
    "Observed OD flows are not in memory. ",
    "Re-run Part A, Step 4.1 to recreate tripsOD."
  )
}

stopifnot(
  all(c("from_zone", "to_zone", "n_Work") %in%
        names(observed_od))
)

# Match observed zone IDs to the model matrix.
origin_idx <- match(
  as.character(observed_od$from_zone),
  zone_ids
)

destination_idx <- match(
  as.character(observed_od$to_zone),
  zone_ids
)

weights_observed <- as.numeric(
  observed_od$n_Work
)

stopifnot(
  !anyNA(origin_idx),
  !anyNA(destination_idx),
  all(is.finite(weights_observed)),
  all(weights_observed >= 0),
  sum(weights_observed) > 0,
  nrow(observed_od) == 524
)

# Extract network travel time for each
# observed OD relation.
times_observed <- time_min[
  cbind(origin_idx, destination_idx)
]

stopifnot(
  all(is.finite(times_observed)),
  all(times_observed > 0)
)

# Calculate weighted mean observed OD time.
observed_mean_time <- weighted.mean(
  times_observed,
  w = weights_observed
)

observed_intra_share <- sum(
  weights_observed[
    origin_idx == destination_idx
  ]
) / sum(weights_observed)

observed_imputed_share <- sum(
  weights_observed[
    missing_off[
      cbind(origin_idx, destination_idx)
    ]
  ]
) / sum(weights_observed)

cat("\n--- Observed OD calibration target ---\n")

cat("Observed OD pairs:",
    nrow(observed_od), "\n")

cat("Weighted observed trips:",
    round(sum(weights_observed), 2), "\n")

cat("Observed weighted mean network time:",
    round(observed_mean_time, 4), "min\n")

cat("Observed intrazonal share:",
    round(100 * observed_intra_share, 2), "%\n")

cat("Observed WP share on imputed connections:",
    round(100 * observed_imputed_share, 2), "%\n")


# ---------------------------------------------------------------
# 4.8.2 Define modelled mean travel time
# ---------------------------------------------------------------

model_mean_time <- function(gamma) {
  
  result <- Furness(
    P = P,
    A = A,
    travel_time = time_min,
    gamma = gamma,
    epsilon = 1e-6
  )
  
  sum(result$trips * time_min) /
    sum(result$trips)
}


# ---------------------------------------------------------------
# 4.8.3 Check calibration interval
# ---------------------------------------------------------------

gamma_lower <- 0
gamma_upper <- 0.20

mean_lower <- model_mean_time(gamma_lower)
mean_upper <- model_mean_time(gamma_upper)

cat("\n--- Calibration interval ---\n")

cat("Gamma lower:", gamma_lower, "\n")
cat("Model mean at lower:",
    round(mean_lower, 4), "min\n")

cat("Gamma upper:", gamma_upper, "\n")
cat("Model mean at upper:",
    round(mean_upper, 4), "min\n")

cat("Target observed mean:",
    round(observed_mean_time, 4), "min\n")

# The observed target must be bracketed
# by the modelled means at interval endpoints.

if (
  observed_mean_time > mean_lower ||
  observed_mean_time < mean_upper
) {
  stop(
    "Observed mean is outside the calibration interval. ",
    "Inspect the results before changing the search bounds."
  )
}


# ---------------------------------------------------------------
# 4.8.4 Find gamma by moment matching
# ---------------------------------------------------------------

calibration <- uniroot(
  f = function(gamma) {
    model_mean_time(gamma) -
      observed_mean_time
  },
  interval = c(gamma_lower, gamma_upper),
  tol = 1e-6
)

gamma_fit <- calibration$root

cat("\n--- Calibrated deterrence parameter ---\n")

cat("Calibrated gamma:",
    signif(gamma_fit, 7), "per minute\n")


# ---------------------------------------------------------------
# Step 4.9: Apply final calibrated Furness model
# ---------------------------------------------------------------

result_fit <- Furness(
  P = P,
  A = A,
  travel_time = time_min,
  gamma = gamma_fit,
  epsilon = 1e-6
)

T_fit <- result_fit$trips

rownames(T_fit) <- zone_ids
colnames(T_fit) <- zone_ids

# Verify production and attraction constraints.
relative_P_error <- max(
  abs(rowSums(T_fit) - P) / P
)

relative_A_error <- max(
  abs(colSums(T_fit) - A) / A
)

stopifnot(
  relative_P_error <= 1e-6,
  relative_A_error <= 1e-6,
  all(is.finite(T_fit)),
  all(T_fit >= 0)
)

# ---------------------------------------------------------------
# 4.9.1 Compare observed and modelled patterns
# ---------------------------------------------------------------

model_mean_fit <- sum(
  T_fit * time_min
) / sum(T_fit)

model_intra_share <- sum(
  diag(T_fit)
) / sum(T_fit)

model_imputed_share <- sum(
  T_fit[missing_off]
) / sum(T_fit)

cat("\n--- Calibrated Furness verification ---\n")

cat("Iterations:",
    result_fit$iterations, "\n")

cat("Maximum relative error:",
    signif(result_fit$error, 6), "\n")

cat("Total modelled trips:",
    round(sum(T_fit), 2), "\n")

cat("Observed mean network time:",
    round(observed_mean_time, 3), "min\n")

cat("Modelled mean network time:",
    round(model_mean_fit, 3), "min\n")

cat("Mean travel-time difference:",
    round(model_mean_fit -
            observed_mean_time, 5), "min\n")

cat("Observed intrazonal share:",
    round(100 * observed_intra_share, 2), "%\n")

cat("Modelled intrazonal share:",
    round(100 * model_intra_share, 2), "%\n")

cat("Modelled demand using imputed times:",
    round(100 * model_imputed_share, 2), "%\n")

cat("\nStep 4.8-4.9 completed.\n")


# ---------------------------------------------------------------
# Part B - Step 5: Output and visualisation
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# 5.1 Export the calibrated three-column OD matrix
# ---------------------------------------------------------------

stopifnot(
  exists("T_fit"),
  exists("P"),
  exists("A"),
  exists("zone_ids"),
  exists("gamma_fit"),
  exists("time_min"),
  exists("missing_off")
)

n_zones <- length(zone_ids)

stopifnot(
  n_zones == 143,
  identical(dim(T_fit), c(n_zones, n_zones)),
  identical(rownames(T_fit), zone_ids),
  identical(colnames(T_fit), zone_ids),
  all(is.finite(T_fit)),
  all(T_fit >= 0)
)

# ---------------------------------------------------------------
# 5.1.1 Convert square OD matrix to long format
# ---------------------------------------------------------------

# R stores matrices in column-major order:
# origins vary fastest, destinations vary slowest.

od_model <- data.frame(
  from_zone = rep(zone_ids, times = n_zones),
  to_zone = rep(zone_ids, each = n_zones),
  trips_work = as.vector(T_fit),
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(od_model) == n_zones^2,
  isTRUE(all.equal(
    sum(od_model$trips_work),
    sum(P),
    tolerance = 1e-8
  ))
)

# ---------------------------------------------------------------
# 5.1.2 Export CSV
# ---------------------------------------------------------------

output_dir <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

od_export_file <- file.path(
  output_dir,
  "PartB_Modelled_Home_Work_OD_3col.csv"
)

write.csv(
  od_model,
  file = od_export_file,
  row.names = FALSE
)

# ---------------------------------------------------------------
# 5.1.3 Save reproducible model results
# ---------------------------------------------------------------

model_file <- file.path(
  output_dir,
  "PartB_Gravity_Model_Calibrated.rds"
)

saveRDS(
  list(
    zone_ids = zone_ids,
    production = P,
    attraction = A,
    travel_time_min = time_min,
    missing_off = missing_off,
    gamma = gamma_fit,
    deterrence = "exponential",
    furness_tolerance = 1e-6,
    iterations = result_fit$iterations,
    final_error = result_fit$error,
    trips = T_fit,
    observed_mean_network_time = observed_mean_time
  ),
  file = model_file
)

# ---------------------------------------------------------------
# 5.1.4 Verify the exported CSV
# ---------------------------------------------------------------

od_check <- read.csv(
  od_export_file,
  colClasses = c(
    "character",
    "character",
    "numeric"
  )
)

# Reconstruct the square matrix.
T_check <- matrix(
  od_check$trips_work,
  nrow = n_zones,
  ncol = n_zones
)

rownames(T_check) <- zone_ids
colnames(T_check) <- zone_ids

stopifnot(
  nrow(od_check) == 20449,
  identical(
    od_check$from_zone,
    rep(zone_ids, times = n_zones)
  ),
  identical(
    od_check$to_zone,
    rep(zone_ids, each = n_zones)
  ),
  isTRUE(all.equal(
    T_check,
    T_fit,
    tolerance = 1e-10
  ))
)

# ---------------------------------------------------------------
# 5.1.5 Verify production and attraction constraints
# ---------------------------------------------------------------

max_P_error <- max(
  abs(rowSums(T_check) - P) / P
)

max_A_error <- max(
  abs(colSums(T_check) - A) / A
)

stopifnot(
  max_P_error <= 1e-6,
  max_A_error <= 1e-6
)

cat("\n--- Final model export verification ---\n")

cat("Calibrated gamma:",
    round(gamma_fit, 7), "\n")

cat("OD rows exported:",
    nrow(od_check), "\n")

cat("Number of columns:",
    ncol(od_check), "\n")

cat("Total modelled trips:",
    round(sum(od_check$trips_work), 2), "\n")

cat("Maximum relative production error:",
    signif(max_P_error, 6), "\n")

cat("Maximum relative attraction error:",
    signif(max_A_error, 6), "\n")

cat("CSV exists:",
    file.exists(od_export_file), "\n")

cat("RDS model exists:",
    file.exists(model_file), "\n")

cat("\nStep 5.1 completed successfully.\n")

# ---------------------------------------------------------------
# Part B - Step 5.2: Visualise modelled OD flows
# ---------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------------
# 5.2.1 Load the calibrated model and zone geometry
# ---------------------------------------------------------------

project_dir <- paste0(
  "/Users/aliozdemir/Documents/ETH BAUG/",
  "Bauingenieurwissenschaften/Master/",
  "3. Semester/Transport Planning Methods"
)

output_dir <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_2"
)

model_file <- file.path(
  output_dir,
  "PartB_Gravity_Model_Calibrated.rds"
)

zones_file <- file.path(
  project_dir,
  "Materials for Trip Generation",
  "output_2_1",
  "trip_generation_provisional.gpkg"
)

stopifnot(
  file.exists(model_file),
  file.exists(zones_file)
)

model <- readRDS(model_file)

npvm_zones <- st_read(
  zones_file,
  quiet = TRUE
)

zone_ids <- as.character(model$zone_ids)
n_zones <- length(zone_ids)

# Align the geometry exactly with the model matrix.
zone_order <- match(
  zone_ids,
  as.character(npvm_zones$tg_ID_NPVM)
)

if (anyNA(zone_order)) {
  stop("Model and geometry contain different zone IDs.")
}

npvm_zones <- npvm_zones[zone_order, ]

T_fit <- model$trips

stopifnot(
  n_zones == 143,
  identical(dim(T_fit), c(n_zones, n_zones)),
  identical(rownames(T_fit), zone_ids),
  identical(colnames(T_fit), zone_ids),
  st_crs(npvm_zones)$epsg == 2056,
  all(is.finite(T_fit)),
  all(T_fit >= 0)
)

# ---------------------------------------------------------------
# 5.2.2 Create the complete long-format OD table
# ---------------------------------------------------------------

od_model <- data.frame(
  from_idx = rep(seq_len(n_zones), times = n_zones),
  to_idx = rep(seq_len(n_zones), each = n_zones),
  from_zone = rep(zone_ids, times = n_zones),
  to_zone = rep(zone_ids, each = n_zones),
  trips_work = as.vector(T_fit)
)

od_inter <- od_model %>%
  filter(from_idx != to_idx)

od_intra <- od_model %>%
  filter(from_idx == to_idx)

stopifnot(
  nrow(od_inter) == 20306,
  nrow(od_intra) == 143,
  isTRUE(all.equal(
    sum(od_model$trips_work),
    sum(model$production),
    tolerance = 1e-8
  ))
)

# ---------------------------------------------------------------
# 5.2.3 Create centroid-to-centroid geometries
# ---------------------------------------------------------------

npvm_centroids <- st_sf(
  zone_id = zone_ids,
  geometry = st_centroid(
    st_geometry(npvm_zones)
  )
)

centroid_xy <- st_coordinates(npvm_centroids)

# All 20,306 directed interzonal connections.
inter_geometries <- lapply(
  seq_len(nrow(od_inter)),
  function(i) {
    
    st_linestring(
      rbind(
        centroid_xy[od_inter$from_idx[i], 1:2],
        centroid_xy[od_inter$to_idx[i], 1:2]
      )
    )
  }
)

od_lines_model_sf <- st_sf(
  od_inter %>%
    select(from_zone, to_zone, trips_work),
  geometry = st_sfc(
    inter_geometries,
    crs = st_crs(npvm_zones)
  )
)

stopifnot(
  nrow(od_lines_model_sf) == 20306,
  all(st_geometry_type(od_lines_model_sf) ==
        "LINESTRING")
)

# ---------------------------------------------------------------
# 5.2.4 Select strongest connections for the map
# ---------------------------------------------------------------

max_map_lines <- 350L

od_lines_display <- od_lines_model_sf %>%
  arrange(desc(trips_work)) %>%
  slice_head(n = max_map_lines)

displayed_demand <- sum(
  od_lines_display$trips_work
)

total_inter_demand <- sum(
  od_lines_model_sf$trips_work
)

display_coverage <- 100 *
  displayed_demand / total_inter_demand

# ---------------------------------------------------------------
# 5.2.5 Plot modelled interzonal commuting flows
# ---------------------------------------------------------------

plot_inter_model <- ggplot() +
  
  geom_sf(
    data = npvm_zones,
    fill = "grey96",
    colour = "grey80",
    linewidth = 0.15
  ) +
  
  geom_sf(
    data = od_lines_display,
    aes(linewidth = trips_work),
    colour = "#2166AC",
    alpha = 0.45
  ) +
  
  scale_linewidth_continuous(
    range = c(0.2, 2.8),
    name = "Trips per weekday"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Modelled interzonal Home-Work flows",
    subtitle = paste0(
      "Zurich region | ",
      "Calibrated gravity model | ",
      "Gamma = ",
      round(model$gamma, 4),
      " min^-1"
    ),
    caption = paste0(
      "Top ", nrow(od_lines_display),
      " of ", nrow(od_lines_model_sf),
      " directed interzonal OD relations displayed ",
      "(",
      round(display_coverage, 1),
      "% of modelled interzonal demand). ",
      "Straight lines connect NPVM zone centroids."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8,
      colour = "grey40"
    ),
    legend.position = "right"
  )

print(plot_inter_model)

# ---------------------------------------------------------------
# Part B - Step 5.3: Visualise intrazonal trips
# ---------------------------------------------------------------

# Extract diagonal values from calibrated OD matrix.
intra_trips <- diag(T_fit)

# Attach each value to its corresponding NPVM polygon.
intra_zones_model <- npvm_zones

intra_zones_model$intrazonal_trips <- as.numeric(
  intra_trips
)

stopifnot(
  nrow(intra_zones_model) == 143,
  all(is.finite(intra_zones_model$intrazonal_trips)),
  isTRUE(all.equal(
    sum(intra_zones_model$intrazonal_trips),
    sum(diag(T_fit)),
    tolerance = 1e-10
  ))
)

# ---------------------------------------------------------------
# 5.3.1 Plot intrazonal commuting trips
# ---------------------------------------------------------------

plot_intra_model <- ggplot() +
  
  geom_sf(
    data = intra_zones_model,
    aes(fill = intrazonal_trips),
    colour = "white",
    linewidth = 0.2
  ) +
  
  scale_fill_viridis_c(
    option = "C",
    name = "Intrazonal trips\nper weekday"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Modelled intrazonal Home-Work trips",
    subtitle = paste0(
      "Zurich region | ",
      "Calibrated gravity model | ",
      "Gamma = ",
      round(model$gamma, 4),
      " min^-1"
    ),
    caption = paste0(
      "Polygon colours represent modelled ",
      "weekday commuting trips with origins and ",
      "destinations in the same NPVM zone."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8,
      colour = "grey40"
    ),
    legend.position = "right"
  )

print(plot_intra_model)

# ---------------------------------------------------------------
# Part B - Step 5.4: Export maps and spatial data
# ---------------------------------------------------------------

inter_png <- file.path(
  output_dir,
  "PartB_Modelled_Interzonal_Flows.png"
)

intra_png <- file.path(
  output_dir,
  "PartB_Modelled_Intrazonal_Flows.png"
)

ggsave(
  filename = inter_png,
  plot = plot_inter_model,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = intra_png,
  plot = plot_intra_model,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

# Export all modelled interzonal relations,
# not only the 350 shown on the map.

inter_gpkg <- file.path(
  output_dir,
  "PartB_Modelled_Interzonal_Flows.gpkg"
)

st_write(
  od_lines_model_sf,
  dsn = inter_gpkg,
  delete_dsn = file.exists(inter_gpkg),
  quiet = TRUE
)

# Export intrazonal trips as NPVM polygons.
intra_gpkg <- file.path(
  output_dir,
  "PartB_Modelled_Intrazonal_Flows.gpkg"
)

st_write(
  intra_zones_model %>%
    select(tg_ID_NPVM, intrazonal_trips),
  dsn = intra_gpkg,
  delete_dsn = file.exists(intra_gpkg),
  quiet = TRUE
)

# ---------------------------------------------------------------
# 5.4.1 Final verification
# ---------------------------------------------------------------

cat("\n--- Modelled flow map verification ---\n")

cat("Modelled interzonal OD pairs:",
    nrow(od_lines_model_sf), "\n")

cat("Interzonal lines displayed:",
    nrow(od_lines_display), "\n")

cat("Displayed interzonal demand (%):",
    round(display_coverage, 2), "\n")

cat("Modelled intrazonal zones:",
    nrow(intra_zones_model), "\n")

cat("Modelled interzonal trips:",
    round(total_inter_demand, 2), "\n")

cat("Modelled intrazonal trips:",
    round(sum(intra_trips), 2), "\n")

cat("Modelled intrazonal share (%):",
    round(100 * sum(intra_trips) /
            sum(T_fit), 2), "\n")

cat("\n--- Export verification ---\n")

cat("Interzonal PNG:",
    file.exists(inter_png), "\n")

cat("Intrazonal PNG:",
    file.exists(intra_png), "\n")

cat("Interzonal GPKG:",
    file.exists(inter_gpkg), "\n")

cat("Intrazonal GPKG:",
    file.exists(intra_gpkg), "\n")

stopifnot(
  nrow(od_lines_model_sf) == 20306,
  nrow(od_lines_display) == 350,
  nrow(intra_zones_model) == 143,
  all(file.exists(c(
    inter_png, intra_png,
    inter_gpkg, intra_gpkg
  )))
)

cat("\nPart B, Step 5.2-5.4 completed.\n")
