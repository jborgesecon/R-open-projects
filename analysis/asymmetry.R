granger_adl <- function(tslist, max_lag = 7) {
  if (!is.list(tslist) || length(tslist) < 2) {
    stop("'tslist' must be a named list with at least 2 time series.")
  }

  varnames <- names(tslist)
  if (is.null(varnames) || any(varnames == "")) {
    stop("All elements in 'tslist' must be named.")
  }

  ts_data <- do.call(merge, lapply(tslist, as.zoo))
  colnames(ts_data) <- varnames

  results <- list()

  for (dep in varnames) {
    aic_vec <- rep(NA_real_, max_lag)

    for (p in seq_len(max_lag)) {
      rhs <- paste(
        c(
          # "trend(ts_data)",
          sapply(varnames, function(v) {
            sprintf("L(%s, 1:%d)", v, p)
          })
        ),
        collapse = " + "
      )
      fml <- as.formula(sprintf("%s ~ %s", dep, rhs))

      fit <- tryCatch(dynlm(fml, data = ts_data), error = function(e) NULL)
      if (!is.null(fit)) aic_vec[p] <- AIC(fit)
    }

    best_lag <- which.min(aic_vec)
    best_aic <- aic_vec[best_lag]

    rhs_best <- paste(
      c(
        # "trend(ts_data)",
        sapply(varnames, function(v) {
          sprintf("L(%s, 1:%d)", v, best_lag)
        })
      ),
      collapse = " + "
    )
    fml_best <- as.formula(sprintf("%s ~ %s", dep, rhs_best))
    model <- dynlm(fml_best, data = ts_data)

    vcov_hc1 <- hccm(model, type = "hc1")

    granger <- list()

    for (pred in varnames[varnames != dep]) {
      coef_names <- grep(
        sprintf("L(%s,", pred),
        names(coef(model)),
        fixed = TRUE,
        value = TRUE
      )

      if (length(coef_names) == 0) next

      wt <- tryCatch(
        linearHypothesis(model, coef_names, vcov. = vcov_hc1),
        error = function(e) NULL
      )

      granger[[pred]] <- list(
        H0          = sprintf("%s does NOT Granger-cause %s", pred, dep),
        coef_tested = coef_names,
        wald_test   = wt,
        F_stat      = if (!is.null(wt)) wt[["F"]][2] else NA_real_,
        p_value     = if (!is.null(wt)) wt[["Pr(>F)"]][2] else NA_real_
      )
    }

    results[[dep]] <- list(
      dependent = dep,
      best_lag  = best_lag,
      best_aic  = best_aic,
      model     = model,
      vcov_hc1  = vcov_hc1,
      granger   = granger
    )
  }

  class(results) <- "granger_adl"
  return(results)
}


# ─────────────────────────────────────────────────────────────
#  print method
# ─────────────────────────────────────────────────────────────
print.granger_adl <- function(x, ...) {
  cat("╔════════════════════════════════════════════════════╗\n")
  cat("║  Granger Causality — ADL levels, HC1-robust Wald   ║\n")
  # cat("║  Deterministic controls: intercept + trend         ║\n")
  cat("║  Deterministic controls: intercept only            ║\n")
  cat("╚════════════════════════════════════════════════════╝\n\n")

  for (dep in names(x)) {
    res <- x[[dep]]
    cat(sprintf("Dependent : %s  (levels)\n", dep))
    cat(sprintf(
      "Optimal lag (AIC): %d   |   AIC = %.4f\n",
      res$best_lag, res$best_aic
    ))
    cat("H0: predictor does NOT Granger-cause dependent\n")
    cat(sprintf(
      "  %-24s  %10s  %10s\n",
      "pred → dep", "F-stat", "p-value"
    ))
    cat(sprintf(
      "  %-24s  %10s  %10s\n",
      "────────────────────────", "──────", "───────"
    ))

    for (pred in names(res$granger)) {
      g <- res$granger[[pred]]
      sig <- dplyr::case_when(
        is.na(g$p_value) ~ "  ",
        g$p_value < 0.01 ~ "***",
        g$p_value < 0.05 ~ "** ",
        g$p_value < 0.10 ~ "*  ",
        TRUE ~ "   "
      )
      pad <- strrep(" ", max(0, 14 - nchar(pred) - nchar(dep)))
      cat(sprintf(
        "  %s -> %s%s  %10.4f  %10.4f  %s\n",
        pred, dep, pad,
        ifelse(is.na(g$F_stat), NA, g$F_stat),
        ifelse(is.na(g$p_value), NA, g$p_value),
        sig
      ))
    }
    cat("\n")
  }

  cat("Significance: *** 0.01  ** 0.05  * 0.10\n")
  cat("Note: Wald tests use HC1-robust standard errors\n")
  invisible(x)
}
