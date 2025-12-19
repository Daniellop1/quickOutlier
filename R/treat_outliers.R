#' Treat Outliers (Winsorization/Capping)
#'
#' Instead of removing outliers, this function replaces extreme values with the calculated
#' upper and lower boundaries (caps). This technique is often called "Winsorization".
#'
#' @param data A data frame.
#' @param column The numeric column to treat.
#' @param method "iqr" or "zscore".
#' @param threshold Numeric (1.5 for IQR, 3 for zscore).
#'
#' @return A data frame with the modified column values.
#' @export
#'
#' @examples
#' # Example: 100 is an outlier
#' df <- data.frame(val = c(1, 2, 3, 2, 1, 100))
#'
#' # The 100 will be replaced by the maximum allowed IQR value
#' clean_df <- treat_outliers(df, "val", method = "iqr")
#' print(clean_df$val)
treat_outliers <- function(data, column, method = "iqr", threshold = 1.5) {

  if (!column %in% names(data)) stop(paste("Column", column, "not found."))

  x <- data[[column]]

  if (method == "iqr") {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1

    lower_cap <- q1 - (threshold * iqr_val)
    upper_cap <- q3 + (threshold * iqr_val)

  } else if (method == "zscore") {
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)

    lower_cap <- mean_val - (threshold * sd_val)
    upper_cap <- mean_val + (threshold * sd_val)
  } else {
    stop("Method not recognized.")
  }

  data[[column]] <- pmin(data[[column]], upper_cap)
  data[[column]] <- pmax(data[[column]], lower_cap)

  return(data)
}
