library(ggplot2)

# ---------------- DSA ---------------------
# ARIMAX
arimax_log <- read.csv("./data/forecasts/16-18/log/peaks_16-18_model-arimax.csv")
arimax_nolog <- read.csv("./data/forecasts/16-18/nolog/peaks-NOLOG_16-18_model-arimax.csv")

# Combine the residuals into a single data frame for plotting
residuals_df <- data.frame(
    log_residuals = arimax_log$residuals,
    nolog_residuals = arimax_nolog$residuals
)

# Create a scatter plot of the residuals
ggplot(residuals_df, aes(x = nolog_residuals, y = log_residuals)) +
    geom_point() +
    labs(
        title = "DSA - ARIMAX",
        x = "No Log Residuals",
        y = "Log Residuals"
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal()

# --------
# ARX
arx_log <- read.csv("./data/forecasts/16-18/log/peaks_16-18_model-arx.csv")
arx_nolog <- read.csv("./data/forecasts/16-18/nolog/peaks-NOLOG_16-18_model-arx.csv")

# Combine the residuals into a single data frame for plotting
residuals_arx_df <- data.frame(
    log_residuals = arx_log$residuals,
    nolog_residuals = arx_nolog$residuals
)

# Create a scatter plot of the residuals for ARX
ggplot(residuals_arx_df, aes(x = nolog_residuals, y = log_residuals)) +
    geom_point() +
    labs(
        title = "DSA - ARX",
        x = "No Log Residuals",
        y = "Log Residuals"
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal()

# -----
# RF
rf_log <- read.csv("./data/forecasts/16-18/log/peaks_16-18_model-rf.csv")
rf_nolog <- read.csv("./data/forecasts/16-18/nolog/peaks-NOLOG_16-18_model-rf.csv")

# Combine the residuals into a single data frame for plotting
residuals_rf_df <- data.frame(
    log_residuals = rf_log$residuals,
    nolog_residuals = rf_nolog$residuals
)

# Create a scatter plot of the residuals for RF
ggplot(residuals_rf_df, aes(x = nolog_residuals, y = log_residuals)) +
    geom_point() +
    labs(
        title = "DSA - RF",
        x = "No Log Residuals",
        y = "Log Residuals"
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal()

# -------------------- DSB ---------------------------
# ARIMAX
arimax_log <- read.csv("./data/forecasts/16-18/log/loads_16-18_model-arimax.csv")
arimax_nolog <- read.csv("./data/forecasts/16-18/nolog/loads-NOLOG_16-18_model-arimax.csv")

# Combine the residuals into a single data frame for plotting
residuals_df <- data.frame(
    log_residuals = arimax_log$residuals,
    nolog_residuals = arimax_nolog$residuals
)

# Create a scatter plot of the residuals
ggplot(residuals_df, aes(x = nolog_residuals, y = log_residuals)) +
    geom_point() +
    labs(
        title = "DSB - ARIMAX",
        x = "No Log Residuals",
        y = "Log Residuals"
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal()

# --------
# ARX
arx_log <- read.csv("./data/forecasts/16-18/log/loads_16-18_model-arx.csv")
arx_nolog <- read.csv("./data/forecasts/16-18/nolog/loads-NOLOG_16-18_model-arx.csv")

# Combine the residuals into a single data frame for plotting
residuals_arx_df <- data.frame(
    log_residuals = arx_log$residuals,
    nolog_residuals = arx_nolog$residuals
)

# Create a scatter plot of the residuals for ARX
ggplot(residuals_arx_df, aes(x = nolog_residuals, y = log_residuals)) +
    geom_point() +
    labs(
        title = "DSB - ARX",
        x = "No Log Residuals",
        y = "Log Residuals"
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal()

# -----
# RF
rf_log <- read.csv("./data/forecasts/16-18/log/loads_16-18_model-rf.csv")
rf_nolog <- read.csv("./data/forecasts/16-18/nolog/loads-NOLOG_16-18_model-rf.csv")

# Combine the residuals into a single data frame for plotting
residuals_rf_df <- data.frame(
    log_residuals = rf_log$residuals,
    nolog_residuals = rf_nolog$residuals
)

# Create a scatter plot of the residuals for RF
ggplot(residuals_rf_df, aes(x = nolog_residuals, y = log_residuals)) +
    geom_point() +
    labs(
        title = "DSB - RF",
        x = "No Log Residuals",
        y = "Log Residuals"
    ) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    theme_minimal()
