library(data.table)
library(arrow)

# --------------------------------------------------
# LiftCompare - build analysis snapshot
# --------------------------------------------------
# What this script does:
# 1. Reads the OpenPowerlifting CSV
# 2. Keeps only the columns needed for matching
# 3. Cleans and filters the data
# 4. Keeps one best result per lifter/federation/class
# 5. Computes lift proportions for squat/bench/deadlift
# 6. Writes a compact Parquet snapshot for fast app use

# Path to the local CSV file
csv_path = "data/openpowerlifting/openpowerlifting.csv"

# Only read the columns we actually need
cols = c(
  "Name",
  "Sex",
  "Event",
  "Equipment",
  "Dots",
  "Federation",
  "WeightClassKg",
  "BodyweightKg",
  "Best3SquatKg",
  "Best3BenchKg",
  "Best3DeadliftKg",
  "TotalKg",
  "Date"
)

# Read the CSV
dt = fread(csv_path, select = cols, showProgress = TRUE)

# --------------------------------------------------
# Basic cleaning
# --------------------------------------------------

# Keep only full-power results
dt = dt[Event == "SBD"]

# Keep only raw lifting
dt = dt[Equipment == "Raw"]

# Keep rows with a valid total
dt = dt[!is.na(TotalKg)]

# Keep rows where all three best lifts are present
dt = dt[
  !is.na(Best3SquatKg) &
    !is.na(Best3BenchKg) &
    !is.na(Best3DeadliftKg)
]

# Keep rows with actual recorded bodyweight
dt = dt[!is.na(BodyweightKg)]

# Keep only federations you want available in the app
dt = dt[Federation %in% c("IPF", "USAPL")]

# Keep weight class as text so classes like "90+" stay distinct
dt = dt[!is.na(WeightClassKg)]
dt[, WeightClassKg := trimws(as.character(WeightClassKg))]
dt = dt[WeightClassKg != ""]

# --------------------------------------------------
# Keep one best row per lifter / federation / sex / equipment / class
# --------------------------------------------------
# Sort from highest total to lowest, then keep the first row
# within each grouping. This gives each lifter's best result
# in that specific federation/class category.
setorder(dt, -TotalKg)

snapshot = dt[, .SD[1], by = .(
  Name,
  Sex,
  Equipment,
  Federation,
  WeightClassKg
)]

# --------------------------------------------------
# Compute lift proportions
# --------------------------------------------------
# These are the relative contributions of each lift to the
# sum of squat + bench + deadlift. These will be used later
# for cosine similarity in the matcher.
snapshot[, TotalInput := Best3SquatKg + Best3BenchKg + Best3DeadliftKg]

# Remove impossible zero-total-input rows just in case
snapshot = snapshot[TotalInput > 0]

snapshot[, p_s := Best3SquatKg / TotalInput]
snapshot[, p_b := Best3BenchKg / TotalInput]
snapshot[, p_d := Best3DeadliftKg / TotalInput]
snapshot[, prop_norm := sqrt(p_s^2 + p_b^2 + p_d^2)]

# --------------------------------------------------
# Write snapshot
# --------------------------------------------------
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

write_parquet(snapshot, "data/derived/snapshot.parquet")

cat("Wrote", nrow(snapshot), "rows to data/derived/snapshot.parquet\n")