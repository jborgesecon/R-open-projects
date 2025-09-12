# Carregar bibliotecas necessárias
library(yfR)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggcorrplot)

# library(BatchGetSymbols) -> Substituída por yfR


##############################
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

# glimpse(prices_merged)


# Gráfico das duas séries com escalas adequadas
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
    scale_y_continuous(labels=scales::dollar_format()) +

    # Ajustando o tema
    theme(
        legend.position="none",
        strip.text=element_text(size=12, face="bold"),
        plot.title=element_text(hjust=0.5, size=16)
    )


# # Gerar Matriz de Correlação
# Gerar tabela pivot com 'ticker' em colunas separadas
prices_wide <- prices_merged %>%
    pivot_wider(
        id_cols=ref_date,
        names_from=ticker,
        values_from=price_close,
    ) %>%
    dplyr::select(-ref_date)

# Definir matriz
corr_matrix <- cor(prices_wide, use="pairwise.complete.obs")  # superior a "na.or.complete"

# Definir Grafico da Matriz de Correlação
grafico_2 <- ggcorrplot(
    corr_matrix,
    method="square",
    colors=c("#e3120b", "white", '#91b8bd'),
    lab=TRUE,
    lab_size=7
) +
    labs(title="Matriz de Correlação entre EWZ e BVSP")

# Criando variáveis log-transformadas
prices_log <- prices_merged %>%
    pivot_wider(
        id_cols=ref_date,
        names_from=ticker,
        values_from=price_close
    ) %>%
    mutate(
        log_BVSP=log(BVSP),
        log_EWZ=log(EWZ)
    )



# Estimar regressão log-log
modelo_loglog <- lm(log_BVSP ~ log_EWZ, data=prices_log)
# print(summary(modelo_loglog))

# Grafico de dispersão com reta de regressão (log-log)
grafico_3 <- ggplot(
    prices_log,
    aes(x=log_EWZ, y=log_BVSP)) +
    geom_point(color="blue") +
    geom_smooth(method="lm", color="red", se=FALSE) +
    labs(
        title="Regressão Log-Log: BVSP x EWZ",
        x="log(EWZ)",
        y="log(BVSP)"
        ) +
    theme_minimal()

# coeficientes do modelo
coeficientes <- coef(modelo_loglog)
beta <- coeficientes["log_EWZ"]

prices_linear <- prices_merged %>%
    pivot_wider(
        id_cols=ref_date,
        names_from=ticker,
        values_from=price_close,
    )

modelo_nivel <- lm(BVSP ~ EWZ, data=prices_linear)
# print(summary(modelo_nivel))

# Grafico de dispersão com reta de regressão (linear)
grafico_4 <- ggplot(
    prices_linear,
    aes(x=EWZ, y=BVSP)) +
    geom_point(color="blue") +
    geom_smooth(method="lm", color="red", se=FALSE) +
    labs(
        title="Regressão Linear: BVSP x EWZ",
        x="EWZ",
        y="BVSP"
        ) +
    theme_minimal()

coeficientes_lin <- coef(modelo_nivel)
coef_EWZ <- coeficientes_lin["EWZ"]

# Variação BVSP com base no movimento do EWZ
get_var_bvsp <- function(movimento_ewz) {
    var_bvsp <- coef_EWZ * movimento_ewz
    return(var_bvsp)
}

movimentacoes_ewz <- c(0.01,0.02,0.03,0.05,0.06,0.10,0.15,0.20,0.30,0.50,1.0)
variacoes_bvsp <- sapply(movimentacoes_ewz, get_var_bvsp)
names(variacoes_bvsp) <- paste("Movimentação EWZ", movimentacoes_ewz, "pontos")

resultados <- data.frame(
    Movimentação_EWZ=movimentacoes_ewz,
    Variações_BVSP=variacoes_bvsp
)

# # Previsão para EWZ = x
data <- prices_linear %>%
    dplyr::select(-ref_date)

# print(glimpse(data))

# Predict
ewz_atual <- 25.95
ibov_previsto <- predict(modelo_nivel, newdata=data.frame(EWZ=ewz_atual))
cat("Estimativa do IBOVESPA para EWZ =", ewz_atual, "é", round(ibov_previsto,2), "\n")