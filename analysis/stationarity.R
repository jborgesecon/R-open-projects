adf <- function(tslist, model = 2, extra = FALSE) {
  choice <- c("none", "drift", "trend")
  if (!(model %in% seq_along(choice))) stop("model must be 1, 2 or 3")

  for (i in names(tslist)) {
    print(paste0("Analyzing ticker: ", i))
    adf_test <- urca::ur.df(tslist[[i]], type = choice[model], selectlags = "AIC")

    print("--- ADF Test Summary ---")
    print(summary(adf_test))

    if (extra) {
      print(paste0("Ljung-Box Test for ", i, " --- lags = 15"))
      print(Box.test(tslist[[i]], lag = 15, type = "Ljung-Box"))

      print(paste0("ARCH-LM Test for ", i, " --- lags = 15"))
      print(FinTS::ArchTest(tslist[[i]], lags = 15))
    }
    print("------------------------")
  }
}

arma_residuals <- function(tslist) {
  series <- list()
  for (i in names(tslist)) {
    arma_fit <- auto.arima(
      tslist[[i]],
      max.p = 5, max.q = 5,
      max.P = 0, max.Q = 0,
      d = 0,
      ic = "aic",
      stepwise = FALSE,
      approximation = FALSE
    )
    series[[i]] <- residuals(arma_fit)
  }
  series
}

