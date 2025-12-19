#' Create an Interactive Outlier Plot
#'
#' Generates an interactive scatter plot using 'plotly'. Outliers detected by the specified
#' method are highlighted in red.
#'
#' @param data A data frame.
#' @param x_col The name of the numeric column for the X-axis.
#' @param y_col The name of the numeric column for the Y-axis.
#' @param confidence_level Numeric. Threshold for detection (0.95, 0.99). Lower values make detection stricter.
#'
#' @return A plotly object.
#' @export
#' @importFrom plotly plot_ly layout
plot_interactive <- function(data, x_col, y_col, confidence_level = 0.95) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for this function.")
  }

  # Detectar outliers pasando el nivel de confianza
  res <- detect_multivariate(data, c(x_col, y_col), confidence_level = confidence_level)

  plot_data <- res
  # La última columna es el flag
  is_outlier <- plot_data[[ncol(plot_data)]]

  colors <- ifelse(is_outlier, "red", "blue")
  symbols <- ifelse(is_outlier, "x", "circle")

  # 1. Crear el objeto base (SIN PIPE %>%)
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~get(x_col),
    y = ~get(y_col),
    type = 'scatter',
    mode = 'markers',
    marker = list(color = colors, symbol = symbols, size = 10),
    text = ~paste("Val:", is_outlier)
  )

  # 2. Añadir el layout al objeto p
  p <- plotly::layout(
    p,
    title = paste("Interactive Detection (Conf:", confidence_level, ")"),
    xaxis = list(title = x_col),
    yaxis = list(title = y_col)
  )

  return(p)
}
