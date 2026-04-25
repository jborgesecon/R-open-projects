s_garch <- function(tslist, display = FALSE) {
  spec <- ugarchspec(
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(1, 0),
      include.mean = TRUE
    ),
    distribution.model = "std"
  )

  series <- list()
  for (i in names(tslist)) {
    fit <- ugarchfit(
      spec = spec,
      data = tslist[[i]],
      solver = "hybrid"
    )
    series[[i]] <- rugarch::residuals(fit, standardize = TRUE)

    if (display) {
      print(paste0("Simple Garch for ticker: ", i))
      print(fit)
      print("---------------------")
    }
  }

  return(series)
}

ms_garch <- function(tslist, display = TRUE) {
  spec <- CreateSpec(
    variance.spec = list(model = c("eGARCH")),
    distribution.spec = list(distribution = c("sstd")),
    switch.spec = list(K = 2) # Number of regimes
  )

  series <- list()

  for (i in names(tslist)) {
    fit <- FitML(
      spec = spec,
      data = tslist[[i]]
    )
    sigma <- as.xts(Volatility(fit))
    y <- as.xts(fit$data)
    series[[i]] <- log(sigma)
    # series[[i]] <- y / sigma  # uncomment this line to evaluate the residuals

    if (display) {
      print(paste0("Markov-Switching Garch for ticker: ", i))
      print(fit)
      print("---------------------")
    }
  }

  return(series)
}

