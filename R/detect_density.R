#' Detect Density-Based Anomalies (LOF)
#'
#' Uses the Local Outlier Factor (LOF) algorithm to identify anomalies based on local density.
#' It is useful for detecting outliers in multi-dimensional data that Z-score misses.
#'
#' @param data A data frame (only numeric columns will be used).
#' @param k Integer. The number of neighbors to consider. Defaults to 5.
#' @param threshold Numeric. The LOF score cutoff. Values > 1 indicate potential outliers. Defaults to 1.5.
#'
#' @return A data frame with the outliers and their LOF score.
#' @export
#'
#' @examples
#' df <- data.frame(x = c(rnorm(50), 5), y = c(rnorm(50), 5))
#' detect_density(df, k = 5)
detect_density <- function(data, k = 5, threshold = 1.5) {

  # Filter numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 1) stop("Data must contain at least one numeric column.")

  data_num <- data[, numeric_cols]

  # Check for NAs (LOF doesn't like NAs)
  if (any(is.na(data_num))) stop("Data contains NAs. Please remove or impute them before running LOF.")

  # Calculate LOF using dbscan package
  lof_scores <- dbscan::lof(data_num, minPts = k)

  # Filter
  is_outlier <- lof_scores > threshold
  result <- data[is_outlier, ]

  if (nrow(result) > 0) {
    result$lof_score <- round(lof_scores[is_outlier], 2)
  } else {
    message("No density outliers detected.")
  }

  return(result)
}
