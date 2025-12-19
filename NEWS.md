# quickOutlier 0.1.5

* Added a `NEWS.md` file to track package changes.

## Functions

*   `detect_categorical_outliers()`: Detects low-frequency outliers in categorical variables based on a percentage threshold.
*   `detect_lof()`: Implements density-based outlier detection using the Local Outlier Factor (LOF) algorithm (via `dbscan`).
*   `detect_iforest()`: Detects outliers using the Isolation Forest algorithm (via `isotree`), effective for high-dimensional data.
*   `detect_multivariate()`: Identifies multivariate outliers using Mahalanobis distance with a Chi-square threshold.
*   `detect_outliers_univ()`: Performs univariate outlier detection using either Z-score or Interquartile Range (IQR) methods.
*   `detect_ts_outliers()`: Identifies anomalies in time series data using STL decomposition.
*   `diagnose_influence()`: Diagnoses influential observations in linear regression models using Cook's distance.
*   `plot_interactive()`: Creates interactive scatter plots using `plotly` to visualize multivariate outliers.
*   `plot_outliers()`: Generates static `ggplot2` visualizations combining boxplots and jittered points to show outliers.
*   `scan_data()`: Scans the entire dataset and provides a summary table of outlier counts and percentages for all numeric columns.
*   `treat_outliers()`: Implements Winsorization (capping) to treat outliers by replacing extreme values with calculated thresholds.
