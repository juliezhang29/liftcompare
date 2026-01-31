library(data.table)
library(arrow)

# TODO: change this to the real CSV path you saw from list.files()
csv_path = "data/openpowerlifting/openpowerlifting-2026-01-31-2dabb714.csv"

# Read only the columns we need (faster, smaller)
cols = c(
  "Name", "Sex", "Equipment", "Event", "Federation",
  "WeightClassKg", "Best3SquatKg", "Best3BenchKg", "Best3DeadliftKg", "TotalKg",
  "Date"
)

dt = fread(csv_path, select = cols, showProgress = TRUE)

# Basic cleaning + v1 scope filters
dt = dt[!is.na(TotalKg)]
dt = dt[Event == "SBD"]                 # powerlifting total
dt = dt[Equipment %chin% c("Raw", "Classic")]  # keep raw/classic

# Keep lifters with all three lifts present
dt = dt[!is.na(Best3SquatKg) & !is.na(Best3BenchKg) & !is.na(Best3DeadliftKg)]

# One "best" row per lifter per category (you can refine later)
# Category key: Name + Sex + Equipment + WeightClassKg
setorder(dt, -TotalKg)
snapshot = dt[, .SD[1], by = .(Name, Sex, Equipment, WeightClassKg)]

# Add proportions (used for similarity later)
snapshot[, TotalInput := Best3SquatKg + Best3BenchKg + Best3DeadliftKg]
snapshot = snapshot[TotalInput > 0]
snapshot[, p_s := Best3SquatKg / TotalInput]
snapshot[, p_b := Best3BenchKg / TotalInput]
snapshot[, p_d := Best3DeadliftKg / TotalInput]

# Write snapshot
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
write_parquet(snapshot, "data/derived/snapshot.parquet")

cat("Wrote", nrow(snapshot), "rows to data/derived/snapshot.parquet\n")
