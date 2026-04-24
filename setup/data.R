# Get normailzed dataset
get_data <- function(tickers = c("^BVSP", "EWZ")) {
  prices <- yf_get(
    tickers = tickers,
    first_date = "2019-01-01",
    last_date = "2026-01-01",
    # freq_date = 'daily'   # default
  )

  cols <- c("ticker", "ref_date", "price_close")
  prices_long <- prices |>
    select(all_of(cols)) |>
    mutate(ticker = gsub("\\^BVSP", "IBOV", ticker)) |>
    rename(date = ref_date, price = price_close) |>
    group_by(date)

  common_dates <- prices_long |>
    dplyr::summarise(n_tickers = n_distinct((ticker))) |>
    dplyr::filter(n_tickers == length(tickers)) |>
    dplyr::select(date)

  prices_long <- prices_long |>
    semi_join(common_dates, by = "date") |>
    dplyr::filter(date %in% common_dates$date)
}

basic_check <- function(df) {
  # 1. Check for Nulls (NAs)
  null_counts <- colSums(is.na(df))
  cat("1. Missing Values per Column:\n")
  print(null_counts)
  cat("\n")

  # 2. Check if length of all tickers are the same
  ticker_lengths <- df |>
    group_by(ticker) |>
    summarize(row_count = n(), .groups = "drop")

  unique_lengths <- unique(ticker_lengths$row_count)

  if (length(unique_lengths) == 1) {
    cat("2. Row Consistency: PASS (All tickers have", unique_lengths, "rows).\n")
  } else {
    cat("2. Row Consistency: FAIL (Tickers have varying row counts):\n")
    print(ticker_lengths)
  }
  cat("\n")
}

get_ts <- function(data = get_data(), y = "price", log_returns = TRUE) {
  series <- list()
  tickers <- unique(data$ticker)

  for (i in tickers) {
    ts_vals <- data |>
      dplyr::filter(ticker == i) |>
      dplyr::arrange(date)

    x <- xts(
      ts_vals[[y]],
      order.by = as.Date(ts_vals$date)
    )

    if (log_returns) {
      x <- diff(log(x))
      x <- na.omit(x)
    }

    series[[i]] <- x
  }

  return(series)
}
