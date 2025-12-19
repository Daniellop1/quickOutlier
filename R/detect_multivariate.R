#' Detect Multivariate Anomalies (Mahalanobis Distance)
#'
#' Identifies outliers based on the relationship between multiple variables using Mahalanobis Distance.
#' This is useful when individual values are normal, but their combination is anomalous (e.g., high weight for low height).
#'
#' @param data A data frame.
#' @param columns Vector of column names to analyze (must be numeric).
#' @param confidence_level Numeric (0 to 1). The confidence cutoff for the Chi-square distribution. Defaults to 0.99 (99%).
#'
#' @return A data frame with the multivariate outliers and their Mahalanobis distance.
#' @export
#'
#' @examples
#' # Generate dataset (n=50) with strong correlation
#' df <- data.frame(x = rnorm(50), y = rnorm(50))
#' df$y <- df$x * 2 + rnorm(50, sd = 0.5) # y depends on x
#'
#' # Add an anomaly: normal x, but impossible y
#' anomaly <- data.frame(x = 0, y = 10)
#' df <- rbind(df, anomaly)
#'
#' # Detect
#' detect_multivariate(df, columns = c("x", "y"))
detect_multivariate <- function(data, columns, confidence_level = 0.99) {

  if (!all(columns %in% names(data))) stop("Some columns were not found in the data frame.")

  subset_data <- data[, columns]
  if (!all(sapply(subset_data, is.numeric))) stop("All selected columns must be numeric.")

  complete_cases <- complete.cases(subset_data)
  subset_clean <- subset_data[complete_cases, ]

  if (nrow(subset_clean) < length(columns) + 1) {
    stop("Not enough data points to calculate Covariance Matrix.")
  }

  center <- colMeans(subset_clean)
  cov_mat <- cov(subset_clean)

  m_dist <- stats::mahalanobis(subset_clean, center, cov_mat)

  cutoff <- stats::qchisq(confidence_level, df = length(columns))

  outlier_indices <- which(m_dist > cutoff)

  result <- data[complete_cases, ][outlier_indices, ]

  if (nrow(result) > 0) {
    result$mahalanobis_dist <- round(m_dist[outlier_indices], 2)
  } else {
    message("No multivariate outliers detected.")
  }

  return(result)
}
