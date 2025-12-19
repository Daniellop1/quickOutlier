#' Detect Rare Categories (Categorical Outliers)
#'
#' Identifies categories in a character or factor vector that appear less frequently
#' than a specified threshold.
#'
#' @details
#' The function calculates the relative frequency of each unique level.
#' If the frequency is below \code{min_freq}, the category is flagged as an outlier.
#'
#' @param data A vector (character or factor).
#' @param min_freq Numeric. The minimum percentage (0 to 1) required to be considered normal.
#' Defaults to 0.01 (1 percent).
#'
#' @return A data frame summarizing the categories:
#' \item{Category}{The name of the level.}
#' \item{Count}{Absolute frequency.}
#' \item{Frequency}{Relative frequency.}
#' \item{Is_Outlier}{Logical flag.}
#'
#' @export
#' @examples
#' cities <- c(rep("Madrid", 10), "Barcalona")
#' detect_categorical_outliers(cities, min_freq = 0.1)
detect_categorical_outliers <- function(data, min_freq = 0.01) {
  if (!is.character(data) && !is.factor(data)) stop("Input must be a character or factor vector.")

  tbl <- table(data)

  df_res <- data.frame(
    Category = names(tbl),
    Count = as.numeric(tbl),
    Frequency = as.numeric(prop.table(tbl)),
    stringsAsFactors = FALSE
  )

  df_res$Is_Outlier <- df_res$Frequency < min_freq
  df_res <- df_res[order(df_res$Frequency), ]
  rownames(df_res) <- NULL

  return(df_res)
}
