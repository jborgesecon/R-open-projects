# Carregar bibliotecas necessárias
library(yfR)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggcorrplot)


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

# Unindo preços
prices_merged <- prices %>%
    dplyr::select(ticker, ref_date, price_close) %>%
    filter(!is.null(price_close))


# # Verificação de séries em nível
grafico_1 <- ggplot(prices_merged, aes(x=ref_date, y=price_close, color=ticker)) +
    geom_line(size=1.2) +

    # Facet para separar escalas
    facet_wrap(~ticker, scales="free_y", ncol=1) +

    # legendas
    labs(
        title="Preço Diário: EWZ x BVSP",
        x="Data",
        y="Preço de Fechamento",
        color="Ativo"
    ) +

    theme_minimal() +

    # Ajuste de escala
    scale_x_date(
        breaks=scales::pretty_breaks(n=10),
        date_labels="%b %Y"
    ) + 

    # Formato Moeda
    # scale_y_continuous(labels=scales::dollar_format()) +

    # Ajustando o tema
    theme(
        legend.position="none",
        strip.text=element_text(size=12, face="bold"),
        plot.title=element_text(hjust=0.5, size=16)
    )


print(grafico_1)