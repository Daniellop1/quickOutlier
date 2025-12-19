library(testthat)
library(quickOutlier)

# ==============================================================================
# 1. TEST BÁSICOS (Univariantes)
# ==============================================================================
test_that("detect_outliers identifies correct rows", {
  df <- data.frame(val = c(1, 2, 1, 2, 100))

  res_iqr <- detect_outliers(df, "val", method = "iqr")
  expect_equal(nrow(res_iqr), 1)
  expect_equal(res_iqr$val, 100)

  res_z <- detect_outliers(df, "val", method = "zscore", threshold = 1)
  expect_equal(nrow(res_z), 1)
  expect_equal(res_z$val, 100)

  expect_error(detect_outliers(df, "columna_fantasma"))
})

test_that("treat_outliers modifies values correctly", {
  # CORRECCIÓN: Usamos datos con varianza (1, 2, 3...) para que el IQR no sea 0
  df <- data.frame(val = c(1, 2, 3, 4, 100))
  clean_df <- treat_outliers(df, "val", method = "iqr")

  # El valor 100 debe haber sido reducido (Winsorizado)
  expect_lt(clean_df$val[5], 100)
  # Pero debe seguir siendo alto (mayor o igual al máximo normal, que es 4)
  expect_gte(clean_df$val[5], 4)
})

test_that("scan_data summarizes dataset", {
  df <- data.frame(a = c(1, 100), b = c(1, 2), char = c("x", "y"))
  res <- scan_data(df, method = "iqr")

  expect_true("Column" %in% names(res))
  expect_equal(nrow(res), 2)
})

# ==============================================================================
# 2. TEST GRÁFICOS
# ==============================================================================
test_that("plot_outliers returns a ggplot object", {
  df <- data.frame(val = rnorm(20))
  p <- plot_outliers(df, "val")
  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# 3. TEST AVANZADOS
# ==============================================================================
test_that("detect_multivariate uses Mahalanobis correctly", {
  df <- data.frame(x = 1:20, y = 1:20)
  outlier <- data.frame(x = 1, y = 100)
  df_final <- rbind(df, outlier)

  res <- detect_multivariate(df_final, c("x", "y"), confidence_level = 0.95)
  expect_gte(nrow(res), 1)
  expect_equal(res$y[nrow(res)], 100)
})

test_that("detect_density (LOF) works", {
  df <- data.frame(
    x = c(0.1, 0.2, 0.1, 0.2, 10),
    y = c(0.1, 0.1, 0.2, 0.2, 10)
  )
  res <- detect_density(df, k = 4, threshold = 1)
  expect_equal(nrow(res), 1)
  expect_equal(res$x, 10)
})

# ==============================================================================
# 4. TEST EXPERTOS
# ==============================================================================
test_that("detect_iforest returns scores and flags", {
  skip_if_not_installed("isotree")

  df <- data.frame(x = rnorm(100), y = rnorm(100))
  df[1, ] <- c(100, 100)

  # CORRECCIÓN: Usamos suppressWarnings para evitar el aviso de OpenMP en Mac
  res <- suppressWarnings(detect_iforest(df, ntrees = 10, contamination = 0.05))

  expect_true("If_Score" %in% names(res))
  expect_true(res$Is_Outlier[1])
})

test_that("detect_ts_outliers identifies seasonality breaks", {
  x <- seq(1, 10, length.out = 50)
  y <- sin(x)
  y[25] <- 10

  res <- detect_ts_outliers(y, frequency = 10)

  expect_true(res$Is_Outlier[25])
})

# ==============================================================================
# 5. TEST NUEVAS FUNCIONES (Categóricos e Influencia)
# ==============================================================================

test_that("detect_categorical_outliers finds rare items", {
  # Caso: "typo" aparece 1 vez de 11 total (aprox 9%)
  # Si ponemos min_freq = 0.1 (10%), debería detectarlo.
  vec <- c(rep("Normal", 10), "Typo")

  res <- detect_categorical_outliers(vec, min_freq = 0.10)

  expect_true("Is_Outlier" %in% names(res))
  # El "Typo" debe ser outlier
  expect_true(res[res$Category == "Typo", "Is_Outlier"])
  # "Normal" no debe ser outlier
  expect_false(res[res$Category == "Normal", "Is_Outlier"])
})

test_that("diagnose_influence detects high leverage points", {
  # Dataset simple lineal
  df <- data.frame(x = 1:10, y = 1:10)

  # Añadimos un punto que rompe la línea (x=20, y=0)
  # Este punto tiene mucha "palanca" (leverage)
  outlier <- data.frame(x = 20, y = 0)
  df <- rbind(df, outlier)

  res <- diagnose_influence(df, target = "y", predictor = "x")

  # El último punto (índice 11) debe ser influyente
  expect_true(res$Is_Influential[11])
  expect_gt(res$Cooks_Dist[11], 4/11) # Debe superar el umbral 4/n
})
