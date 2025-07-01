# libs
library(yfR)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggcorrplot)

# sources
source('connection.r')

# gather data using yfR
symbols <- c('EWZ', '^BVSP')
prices <- yf_get(
    tickers = symbols,
    first_date = '2008-01-01',
    last_date = Sys.Date(),
    freq_data = 'daily'
)

prices <- prices %>%
    mutate(ticker=gsub('\\^', '', ticker))

cols <- c(
  'ticker',
  'ref_date',
  'price_open',
  'price_close',
  'volume'
)

prices <- prices[ ,cols]

feed_db(prices, "stock_market", "ewz_bvsp")
# # check dataframe
