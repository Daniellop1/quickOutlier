# R/globals.R

# 1. Importar funciones básicas de estadística para que estén disponibles en todo el paquete
#' @importFrom stats sd quantile cov complete.cases median mad
NULL

# 2. Declarar variables globales para evitar notas de "no visible binding" en ggplot2
utils::globalVariables(c("Type", ".data", "z_score", "iqr_bounds", "mahalanobis_dist"))
