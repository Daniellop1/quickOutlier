#' Scan Entire Dataset for Outliers
#'
#' Iterates through all numeric columns in the dataset and provides a summary table of outliers found.
#'
#' @param data A data frame.
#' @param method "iqr" or "zscore". Defaults to "iqr".
#'
#' @return A summary data frame with columns: Column, Outlier_Count, and Percentage.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = c(1:10, 100),
#'   b = c(1:10, 1)
#' )
#' scan_data(df, method = "iqr")
scan_data <- function(data, method = "iqr") {

  numeric_cols <- names(data)[sapply(data, is.numeric)]

  results <- data.frame(
    Column = character(),
    Outlier_Count = integer(),
    Percentage = numeric(),
    stringsAsFactors = FALSE
  )

  for (col in numeric_cols) {
    x <- data[[col]]
    n_total <- length(x)

    if (method == "iqr") {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      lower <- q1 - 1.5 * (q3 - q1)
      upper <- q3 + 1.5 * (q3 - q1)
      n_outliers <- sum(x < lower | x > upper, na.rm = TRUE)
    } else {
      # Zscore
      mean_val <- mean(x, na.rm = TRUE)
      sd_val <- sd(x, na.rm = TRUE)
      if (sd_val > 0) {
        z <- abs((x - mean_val) / sd_val)
        n_outliers <- sum(z > 3, na.rm = TRUE)
      } else {
        n_outliers <- 0
      }
    }

    pct <- round((n_outliers / n_total) * 100, 2)
    results <- rbind(results, data.frame(Column = col, Outlier_Count = n_outliers, Percentage = pct))
  }

  return(results)
}
