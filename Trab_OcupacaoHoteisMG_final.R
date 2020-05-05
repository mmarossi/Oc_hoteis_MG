
rm(list = ls())

install.packages("forecast")
library(forecast)
install.packages("readxls")
library(readxl)
install.packages("zoo")
library(zoo)
install.packages("urca")
library(urca)

ocupacao <- read_excel("C:\\Users\\User\\Documents\\MBA\\Analise Series Temporais\\Trabalho\\OcupHoteis_MG.xlsx")

#coverte a serie para serie temporal
ocupacao_ts <- ts(ocupacao, start=c(2008,1), end=c(2018,12), frequency = 12)
summary(ocupacao_ts)

#gráficos verif comportamento
plot(ocupacao_ts, xlab="Tempo", ylab="Ocupação", main="Taxa de Ocupação Hoteis MG")
dec <-decompose(ocupacao_ts)
plot(dec)
#plot tendencia da série
plot(dec$trend, main="Tendência")
#plot sazonalidade por ano
ggseasonplot(ocupacao_ts)

#define o tamanho da amostra de teste
tam_amostra_teste <- 40
#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(ocupacao_ts) - tam_amostra_teste

#serie temporal de treinamento
treinamento_ts <- window(ocupacao_ts, start=c(2008, 1), end=c(2008,tam_amostra_treinamento))

#serie temporal de teste 
validacao_ts <- window(ocupacao_ts, start=c(2008, tam_amostra_treinamento + 1), end=c(2008,tam_amostra_treinamento+tam_amostra_teste))
--------------------------------------------------------------------------------------------------------------------------------------------
#Aplicação de modelos
  
#modelo naive
modelo_naive <- naive(treinamento_ts, level=0, h=tam_amostra_teste)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_naive, xlab="Tempo", ylab="Ocupação", ylim=c(0.3, 0.8))
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(validacao_ts)

#acuracia 
accuracy(modelo_naive, validacao_ts)
-------------------------------------------------------------------------------------------------------------
#modelo de tendência linear 
modelo_tendencia_linear <- tslm(treinamento_ts ~ trend)
summary(modelo_tendencia_linear)

#Análise dos residuos
checkresiduals(modelo_tendencia_linear, test="LB")

#plot modelo com tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Ocupaçao")
lines(modelo_tendencia_linear$fitted.values, lwd=2)

#projeta o modelo durante a validaçao
modelo_tendencia_linear_proj <- forecast(modelo_tendencia_linear, h = tam_amostra_teste, level=0.95)

#grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_linear_proj, xlab="Tempo", ylab="Ocupaçao")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(validacao_ts)
lines(modelo_tendencia_linear_proj$fitted, lwd=2, col="blue")

#acuracia do modelo
accuracy(modelo_tendencia_linear_proj, validacao_ts)

--------------------------------------------------------------------------------------------------------------------------
#modelo de tendencia exponencial
modelo_tendencia_exp <- tslm(treinamento_ts ~ trend, lambda=0)
summary(modelo_tendencia_exp)

#verifica os residuos e teste de Ljung-Box
checkresiduals(modelo_tendencia_exp, test="LB")

#grafico do modelo com tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Ocupação")
lines(modelo_tendencia_exp$fitted.values, lwd=2)

#projeta o modelo durante o validação
modelo_tendencia_exp_proj <- forecast(modelo_tendencia_exp, h = tam_amostra_teste, level=0)

#grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_exp_proj, xlab="Tempo", ylab="Ocupação", xaxt="n" , ylim=c(0.3, 0.8), xlim=c(2008, 2018.25), bty="l", flty=2 ,main="Forecast from Exp regression model")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(validacao_ts)
lines(treinamento_ts)
lines(modelo_tendencia_exp_proj$fitted, lwd=2, col="blue")

#acurácia do modelo
accuracy(modelo_tendencia_exp_proj, validacao_ts)

--------------------------------------------------------------------------------------------------------------
#modelo de tendencia polinomial
  
modelo_tendencia_poli <- tslm(treinamento_ts ~ trend + I(trend^2))
summary(modelo_tendencia_poli)

#residuos com teste de Ljung-Box
checkresiduals(modelo_tendencia_poli, test="LB")

#plot modelo com tendencia
plot(treinamento_ts, xlab="Tempo", ylab="Ocupação", ylim=c(0.3, 0.8), bty="l")
lines(modelo_tendencia_poli$fitted.values, lwd=2) 

#projeta o modelo durante o periodo de validaçao
modelo_tendencia_poli_proj <- forecast(modelo_tendencia_poli, h = tam_amostra_teste, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_tendencia_poli_proj, xlab="Tempo", ylab="Ocupação", xaxt="n" , ylim=c(0.3, 0.8), xlim=c(2008, 2018.25), bty="l", flty=2,main="Forecast from Polynomial regression model")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(validacao_ts)
lines(modelo_tendencia_poli_proj$fitted, lwd=2, col="blue")

#acuracia do modelo
accuracy(modelo_tendencia_poli_proj, validacao_ts)
--------------------------------------------------------------------------------------------------------------
#Modelo Sazonal
  
install.packages("lubridate")
library(lubridate)

modelo_sazonalidade_linear <- tslm(treinamento_ts ~ season)
summary(modelo_sazonalidade_linear)

plot(modelo_sazonalidade_linear$residuals, xlab="Tempo", ylab="Resíduos")

#residuos com teste de Ljung-Box
checkresiduals(modelo_sazonalidade_linear, test="LB")

#plot modelo com sazonalidade
plot(treinamento_ts, xlab="Tempo", ylab="Ocupação", bty="l")
lines(modelo_sazonalidade_linear$fitted.values, lwd=2, col="blue")

modelo_sazonalidade_linear_proj <- forecast(modelo_sazonalidade_linear, h = tam_amostra_teste, level=0.95)

#grafico da serie temporal de treinamento e teste
plot(modelo_sazonalidade_linear_proj, xlab="Tempo", ylab="Ocupação", xaxt="n" , ylim=c(0.3, 0.8), xlim=c(2008, 2018.25), bty="l", flty=2, main="Forecast from Seasonal regression model")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(validacao_ts)
lines(modelo_sazonalidade_linear_proj$fitted, lwd=2, col="blue")

#acuracia do modelo
accuracy(modelo_sazonalidade_linear_proj, validacao_ts)
---------------------------------------------------------------------------------------------------------------
#Modelo sazonal com tendência

modelo_sazonal_tend_linear <- tslm(treinamento_ts ~ season + trend + I(trend^2))
summary(modelo_sazonal_tend_linear)

#plot modelo com sazonal_tend
plot(treinamento_ts, xlab="Tempo", ylab="Ocupação", ylim=c(0.3 , 0.8))
lines(modelo_sazonal_tend_linear$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o periodo de validação
modelo_sazonal_tend_linear_proj <- forecast(modelo_sazonal_tend_linear, h = tam_amostra_teste, level=0)

#grafico da serie temporal de treinamento e teste
plot(modelo_sazonal_tend_linear_proj, xlab="Tempo", ylab="Ocupação", xaxt="n" , ylim=c(0.0 , 0.8), xlim=c(2008, 2018.25), bty="l", flty=2, main="Forecast from Seasonal & Tendencia regression model")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(validacao_ts)
lines(modelo_sazonal_tend_linear_proj$fitted, lwd=2, col="blue")

#acuracia do modelo
accuracy(modelo_sazonal_tend_linear_proj, validacao_ts)

--------------------------------------------------------------------------------------------------------------
#média movel
#calcula a media movel simples e centrada
ma_simples <- rollmean(ocupacao_ts, k=12, align="right")
ma_centrada <- ma(ocupacao_ts, order=12)

#plota as medias 
plot(ocupacao_ts, ylab="Ocupação", xlab="Tempo", bty="l", xaxt="n", xlim=c(2008,2018.25))
axis(1, at=seq(2008, 2018.25, 1), labels=format(seq(2008, 2018.25, 1)))
lines(ma_centrada, lwd=2)
lines(ma_simples, lwd=2, lty=2)

#MA na base de treinamento
ma_simples <- rollmean(treinamento_ts, k=12, align="right")


#obtem a media da ultima janela movel de 12 meses para projeçao
ultima_ma <- tail(ma_simples, 1)

#cria uma projeção
ma_simples_proj <- ts(rep(ultima_ma, tam_amostra_teste), start=c(2008, tam_amostra_treinamento+1), end = c(2008, tam_amostra_treinamento + tam_amostra_teste), freq=12)


#plota o grafica da projecao
plot(treinamento_ts, ylab="Ocupação", xlab="Tempo", ylim=c(0.3, 0.8),bty="l", xaxt="n", xlim=c(2008,2018.25))
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018, 1)))
lines(ma_simples, lwd=2, col="blue")
lines(ma_simples_proj, lwd=2, lty=2, col="blue")
lines(validacao_ts)

#valida a precisao da estimacao no periodo de treinamento
accuracy(ma_simples, treinamento_ts)

#valida a precisao da estimacao no periodo de validacao
accuracy(ma_simples_proj, validacao_ts)

#residuos com teste de Ljung-Box
checkresiduals(treinamento_ts-ma_simples, test="LB")


---------------------------------------------------------------------------------------------------------------
#modelos de suavizacao 
#por se tratarem de modelos mais "robustos" chegamos a fazer as projeções para vizualização 
  
#tendencia aditiva de Holt
modelo_ANN <- ets(treinamento_ts, model = "ANN")
summary(modelo_ANN)

#projeta os proximos 24 meses
modelo_ANN_proj <- forecast(modelo_ANN, h=tam_amostra_teste, level=0.95)

plot(modelo_ANN_proj, ylab="Ocupação", xlab="Tempo", bty="l", xaxt="n", xlim=c(2008,2018.25), flty=2)
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018, 1)))
lines(modelo_ANN$fitted, lwd=2, col="blue")
lines(validacao_ts)

#residuos com teste de Ljung-Box
checkresiduals(modelo_ANN, test="LB")

#acurácia do modelo
accuracy(modelo_ANN_proj, ocupacao_ts)


#modelo com todos os dados de treinamento e validacao
modelo_ANN_final <- ets(ocupacao_ts, model = "ANN")
summary(modelo_ANN_final)

#projeta os 36 meses do futuro
modelo_ANN_final_proj <- forecast(modelo_ANN_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_ANN_final_proj, xlab="Tempo", ylab="Ocupaçao", bty="l", flty=2, main="Forecast from Polynomial regression model ANN")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(modelo_ANN_final_proj$fitted, lwd=2, col="blue")


# tendência multiplicativa (Holt)

modelo_MMN <- ets(treinamento_ts, model = "MMN")
summary(modelo_MMN)

#projeta os proximos 24 meses
modelo_MMN_proj <- forecast(modelo_MMN, h=tam_amostra_teste, level=0.95)

plot(modelo_MMN_proj, ylab="Ocupação", xlab="Tempo", bty="l", xaxt="n", xlim=c(2008,2018.25), flty=2)
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018, 1)))
lines(modelo_MMN$fitted, lwd=2, col="blue")
lines(validacao_ts)

#residuos com teste de Ljung-Box
checkresiduals(modelo_MMN, test="LB")

#acurácia do modelo
accuracy(modelo_MMN_proj, ocupacao_ts)

#Preparar projecao

#modelo com todos os dados de treinamento e validacao
modelo_MMN_final <- ets(ocupacao_ts, model = "MMN")
summary(modelo_MMN_final)

#projeta os 36 meses do futuro
modelo_MMN_final_proj <- forecast(modelo_MMN_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_MMN_final_proj, xlab="Tempo", ylab="Ocupaçao", bty="l", flty=2, main="Forecast from Polynomial regression model MMN")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(modelo_MMN_final_proj$fitted, lwd=2, col="blue")

#Modelo tendência e sazonalidade aditiva

modelo_AAA <- ets(treinamento_ts, model = "AAA")
summary(modelo_AAA)

#projeta os proximos 24 meses
modelo_AAA_proj <- forecast(modelo_AAA, h=tam_amostra_teste, level=0.95)

plot(modelo_AAA_proj, ylab="Ocupação", xlab="Tempo", bty="l", xaxt="n", xlim=c(2008,2018.25), flty=2)
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018, 1)))
lines(modelo_AAA$fitted, lwd=2, col="blue")
lines(validacao_ts)

#residuos com teste de Ljung-Box
checkresiduals(modelo_AAA, test="LB")

#acurácia do modelo
accuracy(modelo_AAA_proj, ocupacao_ts)

#modelo com todos os dados de treinamento e validacao
modelo_AAA_final <- ets(ocupacao_ts, model = "AAA")
summary(modelo_AAA_final)

#projeta os 36 meses do futuro
modelo_AAA_final_proj <- forecast(modelo_ses_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_AAA_final_proj, xlab="Tempo", ylab="Ocupaçao", bty="l", flty=2, main="Forecast from Polynomial regression model AAA")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(modelo_AAA_final_proj$fitted, lwd=2, col="blue")


#Modelo tendência e sazonalidade multiplicativa

modelo_MAM <- ets(treinamento_ts, model = "MAM")
summary(modelo_MAM)

#projeta os proximos 24 meses
modelo_MAM_proj <- forecast(modelo_MAM, h=tam_amostra_teste, level=0.95)

plot(modelo_MAM_proj, ylab="Ocupação", xlab="Tempo", bty="l", xaxt="n", xlim=c(2008,2018.25), flty=2)
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018, 1)))
lines(modelo_MAM$fitted, lwd=2, col="blue")
lines(validacao_ts)

#residuos com teste de Ljung-Box
checkresiduals(modelo_MAM, test="LB")

#acurácia do modelo
accuracy(modelo_MAM_proj, ocupacao_ts)

#Preparar projecao

#modelo com todos os dados de treinamento e validacao
modelo_MAM_final <- ets(ocupacao_ts, model = "MAM")
summary(modelo_MAM_final)

#projeta os 36 meses do futuro
modelo_MAM_final_proj <- forecast(modelo_MAM_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(modelo_MAM_final_proj, xlab="Tempo", ylab="Ocupaçao", bty="l", flty=2, main="Forecast from Polynomial regression model MAM")
axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018,1)))
lines(modelo_MAM_final_proj$fitted, lwd=2, col="blue")

---------------------------------------------------------------------------------------------------------------
#Modelo SARIMA
  #Modelo SARIMA
  Modelo_ARIMA <- auto.arima(treinamento_ts, stepwise=FALSE, approximation = FALSE)

#resumo modelo
summary(Modelo_ARIMA)

#projeta os proximos 12 meses
modelo_ARIMA_proj <- forecast(Modelo_ARIMA, h=tam_amostra_teste, level=0.95)

#plota o grafica da projecao
plot(modelo_ARIMA_proj, ylab="ocupaçao", xlab="Tempo", bty="l", xaxt="n", xlim=c(2008,2018.25), flty=2)

axis(1, at=seq(2008, 2018, 1), labels=format(seq(2008, 2018, 1)))

lines(Modelo_ARIMA$fitted, lwd=2, col="blue")

lines(validacao_ts)

#verifica precisao
accuracy(modelo_ARIMA_proj, validacao_ts)

checkresiduals(modelo_ARIMA_proj)


#Preparar projecao

#primeiramente reestimamos o modelo com todos os dados de treinamento e validacao
Modelo_ARIMA_final <- auto.arima(ocupacao_ts, stepwise=FALSE, approximation = FALSE)

#sumario do modelo
summary(Modelo_ARIMA_final)

#projeta os proximos 36 meses do futuro
Modelo_ARIMA_final_proj <- forecast(Modelo_ARIMA_final, h=36, level=0.95)

#plota o grafico da serie temporal de treinamento e teste
plot(Modelo_ARIMA_final_proj, xlab="Tempo", ylab="Ocupação", ylim=c(0.2, 0.8), xlim=c(2008, 2022), bty="l", flty=2, main="Forecast from Polynomial regression model")
axis(1, at=seq(2008, 2022, 1), labels=format(seq(2008, 2022,1)))
lines(Modelo_ARIMA_final_proj$fitted, lwd=2, col="blue")

