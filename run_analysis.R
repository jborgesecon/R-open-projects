# Libraries
library(car)
library(dplyr)
library(dynlm)
library(forecast)
library(ggplot2)
library(MSGARCH)
library(patchwork)
library(rugarch)
library(tseries)
library(tidyr)
library(urca)
library(xts)
library(yfR)
library(zoo)

# Modules
source(file.path("setup", "data.R"))
source(file.path("visualization", "graphs.R"))
source(file.path("analysis", "stationarity.R"))
source(file.path("analysis", "garch.R"))
source(file.path("analysis", "asymmetry.R"))

# ---------------------------
# MAIN SECTION - START
# ---------------------------

# STEP 1: Data Loading
data <- get_data()
# basic_check(data) # Ensuring common dates and checking for nulls


# STEP 2: Stationarity Check
tslist <- get_ts(data, log_returns = TRUE)
adf(tslist, extra = TRUE)


# STEP 3: GARCH modeling
# # Univariate Garch
# # -> understanding the limitations of single-regime garch
# sgarch_volatility <- s_garch(tslist, display = TRUE)  

# # Regime Switching Garch
msgarch_volatility <- ms_garch(tslist, display = TRUE)

# Checking the behaviour of the estimated \sigma
adf(msgarch_volatility, model = 3, extra = TRUE)


# STEP 4: Granger-Causality Test
# # Robustness and HC1 for ARCH consistency (\sigma ADF diagnostics)
granger_adl(msgarch_volatility)


# ---------------------------
# MAIN SECTION - END
# ---------------------------

# Visualization
s_plot(msgarch_volatility, y_axis = "Volatility", save = TRUE, highlight = TRUE)
