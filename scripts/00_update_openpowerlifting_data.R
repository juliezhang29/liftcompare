library(utils)

# --------------------------------------------------
# Download latest OpenPowerlifting dataset
# --------------------------------------------------

# Remove old zips
# if (file.exists(zip_path)) file.remove(zip_path) 

zip_url = "https://openpowerlifting.gitlab.io/opl-csv/files/openpowerlifting-latest.zip"

zip_path = "data/openpowerlifting/openpowerlifting-latest.zip"
extract_dir = "data/openpowerlifting"

dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

# Download latest zip
download.file(zip_url, zip_path, mode = "wb")

# Unzip
unzip(zip_path, exdir = extract_dir)

# Find the correct CSV (should contain "openpowerlifting")
csv_files = list.files(
  extract_dir,
  pattern = "openpowerlifting.*\\.csv$",
  full.names = TRUE
)

if (length(csv_files) == 0) {
  stop("No OpenPowerlifting CSV found after unzip.")
}

# Always use the newest file (by modified time)
csv_files = csv_files[order(file.info(csv_files)$mtime, decreasing = TRUE)]

final_csv_path = file.path(extract_dir, "openpowerlifting.csv")

file.copy(csv_files[1], final_csv_path, overwrite = TRUE)

cat("Updated dataset at:", final_csv_path, "\n")