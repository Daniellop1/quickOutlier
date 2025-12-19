#' Detect Anomalies in a Data Frame
#'
#' This function identifies rows containing outliers in a specific numeric column.
#' It supports two methods:
#' \itemize{
#'   \item \strong{zscore}: Based on the standard deviation (statistical approach). Best for normal distributions.
#'   \item \strong{iqr}: Based on the Interquartile Range (robust approach). Best for data with skewness or extreme outliers.
#' }
#'
#' @param data A data frame containing the data to analyze.
#' @param column A string specifying the name of the numeric column to analyze.
#' @param method A character string. "zscore" or "iqr". Defaults to "zscore".
#' @param threshold A numeric value. The cutoff limit. Defaults to 3 for "zscore" and 1.5 for "iqr".
#'
#' @return A data frame containing only the rows considered outliers, with an additional column displaying the calculated score or bounds.
#' @export
#'
#' @examples
#' # Example with a clear outlier
#' df <- data.frame(
#'   id = 1:6,
#'   value = c(10, 12, 11, 10, 500, 11)
#' )
#'
#' # Detect using IQR (Robust)
#' detect_outliers(df, column = "value", method = "iqr")
#'
#' # Detect using Z-Score
#' detect_outliers(df, column = "value", method = "zscore")
detect_outliers <- function(data, column, method = "zscore", threshold = 3) {

  if (!column %in% names(data)) stop(paste("Column", column, "not found in the data frame."))

  x <- data[[column]]
  if (!is.numeric(x)) stop("The selected column must be numeric.")

  if (method == "zscore") {
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)

    if (sd_val == 0) {
      warning("Standard deviation is 0. Cannot calculate Z-score.")
      return(invisible(data[0, ]))
    }

    scores <- abs((x - mean_val) / sd_val)
    is_outlier <- scores > threshold
    data$z_score <- round(scores, 2)

  } else if (method == "iqr") {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1

    lower <- q1 - (threshold * iqr_val)
    upper <- q3 + (threshold * iqr_val)

    is_outlier <- x < lower | x > upper
    data$iqr_bounds <- paste0("[", round(lower, 2), " - ", round(upper, 2), "]")

  } else {
    stop("Method not recognized. Please use 'zscore' or 'iqr'.")
  }

  result <- data[is_outlier, ]

  if (nrow(result) == 0) {
    message("No outliers detected.")
    return(invisible(result))
  }

  return(result)
}
