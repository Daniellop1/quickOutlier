#' Detect Anomalies in Time Series using STL Decomposition
#'
#' Performs a seasonal-trend decomposition using Loess (STL) to separate the time series
#' into three components: Trend, Seasonality, and Remainder (Noise). Outliers are then
#' detected in the Remainder component using the Interquartile Range (IQR) method.
#'
#' @details
#' This method is superior to simple thresholding for time series because it accounts for:
#' \itemize{
#'   \item \strong{Seasonality:} Repeating patterns (e.g., higher sales in December).
#'   \item \strong{Trend:} Long-term increase or decrease.
#' }
#' An observation is flagged as an outlier only if it is unusual \emph{after} accounting for
#' these normal temporal patterns.
#'
#' @param data A numeric vector representing the time series values.
#' @param frequency Integer. The number of observations per cycle (e.g., 12 for monthly data, 7 for daily data).
#'
#' @return A data frame containing:
#' \item{Original}{The original values.}
#' \item{Trend}{The extracted long-term trend component.}
#' \item{Seasonal}{The extracted seasonal component.}
#' \item{Remainder}{The remaining noise after removing trend and seasonality.}
#' \item{Is_Outlier}{Logical flag. TRUE if the Remainder value is an outlier based on IQR (3 * IQR).}
#'
#' @export
#' @importFrom stats ts stl quantile
#'
#' @examples
#' # Example: Synthetic monthly data with a clear anomaly
#' sales <- c(sin(seq(1, 20, 0.5)) * 10 + 50) # Normal pattern
#' sales[10] <- 200 # Inject outlier
#' result <- detect_ts_outliers(sales, frequency = 12)
#' subset(result, Is_Outlier == TRUE)
detect_ts_outliers <- function(data, frequency = 12) {
  if (!is.numeric(data)) stop("Data input must be a numeric vector.")
  if (length(data) < 2 * frequency) stop("Data length must be at least 2 times the frequency to detect seasonality.")

  # Crear objeto de serie temporal
  ts_data <- ts(data, frequency = frequency)

  # Descomposición STL robusta (s.window = "periodic" asume estacionalidad constante)
  decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)

  # Extraer componentes
  trend <- as.numeric(decomp$time.series[, "trend"])
  seasonal <- as.numeric(decomp$time.series[, "seasonal"])
  remainder <- as.numeric(decomp$time.series[, "remainder"])

  # Detección de outliers en el residuo (IQR Robusto)
  # Usamos un multiplicador de 3 (conservador) en lugar del estándar 1.5
  q1 <- quantile(remainder, 0.25, na.rm = TRUE)
  q3 <- quantile(remainder, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower_limit <- q1 - 3 * iqr_val
  upper_limit <- q3 + 3 * iqr_val

  is_outlier <- remainder < lower_limit | remainder > upper_limit

  # Construir resultado
  res <- data.frame(
    Original = as.numeric(data),
    Trend = trend,
    Seasonal = seasonal,
    Remainder = remainder,
    Is_Outlier = is_outlier
  )

  return(res)
}
