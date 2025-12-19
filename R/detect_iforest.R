#' Detect Outliers using Isolation Forest (Machine Learning)
#'
#' This function applies the Isolation Forest algorithm to detect anomalies in high-dimensional
#' or complex datasets. Unlike statistical methods that measure distance from a mean,
#' Isolation Forest isolates observations by randomly selecting a feature and then
#' randomly selecting a split value between the maximum and minimum values of the selected feature.
#'
#' @details
#' Recursive partitioning can be represented by a tree structure, and the number of splittings
#' required to isolate a sample is equivalent to the path length from the root node to the
#' terminating node. Random trees produce shorter path lengths for anomalies, as they are
#' essentially "fewer" and "different" from normal observations.
#'
#' The function relies on the efficient `isotree` package for computation.
#'
#' @param data A data frame containing at least one numeric column. Non-numeric columns are ignored.
#' @param ntrees Integer. The number of trees to grow in the forest. Defaults to 100.
#' Increasing this number improves accuracy but increases computation time.
#' @param contamination Numeric (0 to 0.5). The expected proportion of outliers in the dataset.
#' Used to calculate the threshold for the binary `Is_Outlier` flag. Defaults to 0.05 (5%).
#'
#' @return A data frame with the original columns plus:
#' \item{If_Score}{Numeric score between 0 and 1. Higher values indicate higher anomaly likelihood.}
#' \item{Is_Outlier}{Logical flag. TRUE if the score exceeds the quantile defined by the contamination rate.}
#'
#' @note This function requires the \code{isotree} package.
#' @seealso \code{\link[isotree]{isolation.forest}}
#'
#' @export
#' @importFrom isotree isolation.forest predict.isolation_forest
#' @importFrom stats predict quantile
#'
#' @examples
#' # Example: Detect anomalies in a generated dataset
#' df <- data.frame(x = c(rnorm(100), 1000), y = c(rnorm(100), 1000))
#' result <- detect_iforest(df, ntrees = 50, contamination = 0.02)
#' tail(result)
detect_iforest <- function(data, ntrees = 100, contamination = 0.05) {
  # Validar que isotree está instalado
  if (!requireNamespace("isotree", quietly = TRUE)) {
    stop("Package 'isotree' is needed for this function to work. Please install it.", call. = FALSE)
  }

  # Filtrar columnas numéricas
  nums <- unlist(lapply(data, is.numeric))
  data_num <- data[, nums, drop = FALSE]

  if (ncol(data_num) == 0) stop("No numeric columns found in the provided data frame.")

  # Entrenar modelo
  model <- isotree::isolation.forest(data_num, ntrees = ntrees, ndim = 1)

  # Calcular scores
  scores <- predict(model, data_num)

  # Definir umbral dinámico basado en la contaminación esperada
  threshold <- quantile(scores, 1 - contamination)

  # Preparar resultado
  res <- data
  res$If_Score <- scores
  res$Is_Outlier <- scores > threshold

  return(res)
}
