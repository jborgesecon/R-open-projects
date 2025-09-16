# Carregar bibliotecas necessárias
library(yfR)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggcorrplot)
library(quantmod)

# import all sources from analysis/ at once


# # Importar e normalizar os dados via Yahoo Finance
# Definir simbolos e datas
symbols <- c('EWZ', '^BVSP')
prices <- yf_get(
    tickers = symbols,
    first_date = '2025-01-01',
    last_date = '2025-04-01',
    freq_data = 'daily'
)

# Renomear para remover '^'
prices <- prices %>%
    mutate(ticker=gsub('\\^', '', ticker))

# Creating a candle bar for BVSP
ref_ticker <- 'BVSP'
columns_needed <- c("ref_date", "price_open", "price_high", "price_low", "price_close")

prices_candle <- prices %>%
    filter(ticker == ref_ticker) %>%
    dplyr::select(all_of(columns_needed)) %>%
    arrange(ref_date)

# Create a candlestick chart using ggplot2
# Set up plotting device with custom dimensions for interactive viewing
windows(width = 12, height = 3)  # For Windows - using your desired wide aspect ratio
# Alternative for other systems: quartz(width = 12, height = 3) for Mac, x11(width = 12, height = 3) for Linux

p <- ggplot(prices_candle, aes(x = ref_date)) +
    geom_linerange(aes(ymin = price_low, ymax = price_high), color = "black") +
    geom_rect(aes(xmin = ref_date - 0.2, xmax = ref_date + 0.2,
                  ymin = pmin(price_open, price_close),
                  ymax = pmax(price_open, price_close),
                  fill = price_close > price_open), color = "black", show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "red")) +
    labs(title = "BVSP Candlestick Chart", x = "Date", y = "Price") +
    theme_minimal() +
    theme(plot.margin = margin(5, 5, 5, 5))

# Now you can run print(p) multiple times to iterate on your plot
print(p)

# Uncomment the line below when you're ready to save the final version
# ggsave(file.path(tempdir(), "myplot.png"), plot = p, width = 12, height = 3, dpi = 300)


