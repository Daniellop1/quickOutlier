#' Diagnose Influential Points in Linear Models (Cook's Distance)
#'
#' Fits a linear model between two variables and calculates Cook's Distance to identify
#' influential points. An influential point is an outlier that specifically affects
#' the slope of the regression line.
#'
#' @details
#' Cook's distance (\eqn{D_i}) measures the effect of deleting a given observation.
#' Points with a large \eqn{D_i} are considered to have high leverage and influence.
#'
#' The default threshold for detection is calculated as:
#' \deqn{Threshold = \frac{4}{n}}
#' Where \eqn{n} is the number of observations. This is a standard rule of thumb
#' in regression diagnostics.
#'
#' @param data A data frame containing the variables.
#' @param target Character. The name of the dependent variable (Y).
#' @param predictor Character. The name of the independent variable (X).
#' @param cutoff Numeric (Optional). A custom threshold for Cook's Distance.
#' If NULL, it defaults to 4/n.
#'
#' @return A data frame with the original data plus:
#' \item{Cooks_Dist}{The calculated Cook's distance for each point.}
#' \item{Is_Influential}{Logical flag. TRUE if Cooks_Dist > cutoff.}
#'
#' @export
#' @importFrom stats lm cooks.distance as.formula
#'
#' @examples
#' # Example: A point that pulls the regression line
#' df <- mtcars
#' # Artificially create a leverage point
#' df[1, "wt"] <- 10
#' df[1, "mpg"] <- 50
#' result <- diagnose_influence(df, "mpg", "wt")
#' head(result)
diagnose_influence <- function(data, target, predictor, cutoff = NULL) {
  if (!all(c(target, predictor) %in% names(data))) stop("Columns not found in data frame.")

  # Ajustar modelo lineal
  formula <- as.formula(paste(target, "~", predictor))
  model <- lm(formula, data = data)

  # Calcular distancias
  cooks_d <- cooks.distance(model)

  # Definir umbral
  if (is.null(cutoff)) {
    cutoff <- 4 / nrow(data)
  }

  res <- data
  res$Cooks_Dist <- cooks_d
  res$Is_Influential <- cooks_d > cutoff

  return(res)
}
