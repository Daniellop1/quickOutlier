#' Plot Outliers with ggplot2
#'
#' Visualizes the distribution of a variable and highlights detected outliers in red.
#' It combines a boxplot (for context) and jittered points (for individual data visibility).
#'
#' @param data A data frame.
#' @param column The name of the numeric column to plot.
#' @param method "zscore" or "iqr". Defaults to "zscore".
#' @param threshold Numeric. Defaults to 3 for zscore, 1.5 for IQR.
#'
#' @return A ggplot object. You can add more layers to it using `+`.
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(val = c(rnorm(50), 10)) # 50 normal points and one outlier
#' plot_outliers(df, "val", method = "iqr")
plot_outliers <- function(data, column, method = "zscore", threshold = 3) {

  if (!column %in% names(data)) stop(paste("Column", column, "not found."))

  x <- data[[column]]
  is_outlier <- rep(FALSE, length(x))

  if (method == "zscore") {
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    if(sd_val > 0) is_outlier <- abs((x - mean_val) / sd_val) > threshold

  } else if (method == "iqr") {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    lower <- q1 - (threshold * iqr_val)
    upper <- q3 + (threshold * iqr_val)
    is_outlier <- x < lower | x > upper
  }

  plot_data <- data
  plot_data$Type <- ifelse(is_outlier, "Outlier", "Normal")

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = .data[[column]])) +
    ggplot2::geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.3) +
    ggplot2::geom_jitter(ggplot2::aes(color = Type), width = 0.1, size = 2) +
    ggplot2::scale_color_manual(values = c("Normal" = "grey50", "Outlier" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Outlier Detection: ", column),
      subtitle = paste("Method:", method, "| Threshold:", threshold),
      x = "",
      color = "Status"
    )

  return(p)
}
