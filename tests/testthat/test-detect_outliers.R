test_that("detect_outliers works correctly", {
  # Caso simple: 100 es un outlier claro
  df <- data.frame(val = c(1, 1, 1, 1, 100))

  # Test IQR (Este funcionaba bien)
  res_iqr <- detect_outliers(df, "val", method = "iqr")
  expect_equal(nrow(res_iqr), 1)
  expect_equal(res_iqr$val, 100)

  # Test Z-Score
  # CORRECCION: Bajamos el threshold a 1 para que detecte el outlier en una muestra tan pequeña
  res_z <- detect_outliers(df, "val", method = "zscore", threshold = 1)
  expect_equal(nrow(res_z), 1)
  expect_equal(res_z$val, 100)
})

test_that("treat_outliers modifies values", {
  df <- data.frame(val = c(1, 1, 1, 100))
  clean <- treat_outliers(df, "val", method = "iqr")

  # El valor 100 debe haber bajado (ser menor que 100)
  expect_lt(clean$val[4], 100)
})
