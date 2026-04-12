library(data.table)
library(arrow)

get_class_table = function(sex, federation) {
  if (federation %in% c("IPF", "PLA")) {
    if (sex == "M") {
      classes = c("59", "66", "74", "83", "93", "105", "120", "120+")
    } else if (sex == "F") {
      classes = c("47", "52", "57", "63", "69", "76", "84", "84+")
    } else {
      stop("sex must be 'M' or 'F'.")
    }
  } else if (federation == "USAPL") {
    if (sex == "M") {
      classes = c("52", "56", "60", "67.5", "75", "82.5", "90", "100", "110", "125", "140", "140+")
    } else if (sex == "F") {
      classes = c("44", "48", "52", "56", "60", "67.5", "75", "82.5", "90", "100", "100+")
    } else {
      stop("sex must be 'M' or 'F'.")
    }
  } else {
    stop("federation must be 'IPF', 'PLA', or 'USAPL'.")
  }
  
  upper = as.numeric(gsub("\\+", "", classes))
  lower = c(-Inf, head(upper, -1))
  
  data.table(
    class_label = classes,
    lower = lower,
    upper = upper,
    is_plus = grepl("\\+$", classes)
  )
}

get_numeric_class_bounds = function(sex, federation, selected_class) {
  class_table = get_class_table(sex, federation)
  
  selected_class = gsub("\\s+", "", as.character(selected_class))
  is_plus = grepl("\\+$", selected_class)
  class_value = as.numeric(gsub("\\+", "", selected_class))
  
  if (is.na(class_value)) {
    stop("selected_class must be numeric like '60' or plus class like '120+'.")
  }
  
  if (is_plus) {
    plus_row = class_table[is_plus == TRUE][1]
    if (nrow(plus_row) == 0) return(NULL)
    
    return(list(
      lower = plus_row$lower[1],
      upper = Inf,
      is_plus = TRUE
    ))
  }
  
  containing_row = class_table[class_value <= upper][1]
  
  if (nrow(containing_row) == 0) {
    return(NULL)
  }
  
  list(
    lower = containing_row$lower[1],
    upper = class_value,
    is_plus = FALSE
  )
}

match_lifters = function(
    squat,
    bench,
    deadlift,
    sex,
    selected_class,
    federation = c("IPF/PLA", "USAPL", "both"),
    equipment = "Raw",
    top_n = 10,
    elite_n = 50,
    max_dist = NULL
) {
  federation = match.arg(federation)
  
  dt = as.data.table(read_parquet("data/derived/snapshot.parquet"))
  
  user_total = squat + bench + deadlift
  if (user_total <= 0) {
    stop("squat + bench + deadlift must be > 0.")
  }
  
  required_cols = c(
    "Sex", "Equipment", "Federation", "BodyweightKg",
    "p_s", "p_b", "p_d", "TotalKg", "Dots"
  )
  
  missing_cols = setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(paste("snapshot.parquet missing:", paste(missing_cols, collapse = ", ")))
  }
  
  dt = dt[
    Sex == sex &
      Equipment == equipment &
      !is.na(BodyweightKg) &
      !is.na(Dots)
  ]
  
  if (federation == "IPF/PLA") {
    dt = dt[Federation %in% c("IPF", "PLA")]
  } else if (federation == "USAPL") {
    dt = dt[Federation == "USAPL"]
  }
  
  if (nrow(dt) == 0) return(dt)
  
  if (!is.null(selected_class) && selected_class != "") {
    ipf_bounds = get_numeric_class_bounds(sex, "IPF", selected_class)
    usapl_bounds = get_numeric_class_bounds(sex, "USAPL", selected_class)
    
    ipf_ok = rep(FALSE, nrow(dt))
    usapl_ok = rep(FALSE, nrow(dt))
    
    if (!is.null(ipf_bounds)) {
      if (ipf_bounds$is_plus) {
        ipf_ok = dt$Federation %in% c("IPF", "PLA") &
          dt$BodyweightKg > ipf_bounds$lower
      } else {
        ipf_ok = dt$Federation %in% c("IPF", "PLA") &
          dt$BodyweightKg > ipf_bounds$lower &
          dt$BodyweightKg <= ipf_bounds$upper
      }
    }
    
    if (!is.null(usapl_bounds)) {
      if (usapl_bounds$is_plus) {
        usapl_ok = dt$Federation == "USAPL" &
          dt$BodyweightKg > usapl_bounds$lower
      } else {
        usapl_ok = dt$Federation == "USAPL" &
          dt$BodyweightKg > usapl_bounds$lower &
          dt$BodyweightKg <= usapl_bounds$upper
      }
    }
    
    dt = dt[ipf_ok | usapl_ok]
    if (nrow(dt) == 0) return(dt)
  }
  
  setorder(dt, -Dots)
  dt = dt[1:min(elite_n, .N)]
  
  user_vec = c(
    squat / user_total,
    bench / user_total,
    deadlift / user_total
  )
  
  dt[, dist := sqrt(
    (p_s - user_vec[1])^2 +
      (p_b - user_vec[2])^2 +
      (p_d - user_vec[3])^2
  )]
  
  if (!is.null(max_dist)) {
    dt = dt[dist <= max_dist]
  }
  
  if (nrow(dt) == 0) return(dt)
  
  dt[, similarity := 1 / (1 + dist)]
  dt[, pct_match := pmin(100, round(similarity * 100, 1))]
  
  setorder(dt, Name, -similarity, -Dots)
  dt = dt[, .SD[1], by = Name]
  
  setorder(dt, -similarity, -Dots)
  dt = dt[1:min(top_n, .N)]
  dt[, rank := .I]
  
  keep_cols = intersect(
    c(
      "rank",
      "Name",
      "Sex",
      "Federation",
      "WeightClassKg",
      "BodyweightKg",
      "Best3SquatKg",
      "Best3BenchKg",
      "Best3DeadliftKg",
      "TotalKg",
      "Dots",
      "pct_match"
    ),
    names(dt)
  )
  
  result = dt[, ..keep_cols]
  
  setnames(
    result,
    old = c(
      "WeightClassKg",
      "BodyweightKg",
      "Best3SquatKg",
      "Best3BenchKg",
      "Best3DeadliftKg",
      "TotalKg",
      "Dots",
      "pct_match"
    ),
    new = c(
      "Class",
      "Bodyweight",
      "Squat",
      "Bench",
      "Deadlift",
      "Total",
      "DOTS",
      "Match %"
    ),
    skip_absent = TRUE
  )
  
  result
}