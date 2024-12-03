#Pacotes necessários
{
library(TSA)
library(readr)
library(forecast)
require(graphics)
library(readxl)
library(urca)
library(astsa)
library(fpp2)
library(readxl)
library(dplyr)
library(ggplot2)
library(tseries)
library(fUnitRoots) 

}

dados <- read_excel("H:/Meu Drive/10º SEMESTRE/LABORATORIO/Projeto 1/D5.xlsx")
exportacoes_ts <- ts(dados$Exportações, frequency = 4, start = c(2002, 1))
exportacoes_treino <- ts(dados[0:60,]$Exportações, frequency = 4, start = c(2002, 1))
exportacoes_teste <- ts(dados[61:72,]$Exportações, frequency = 4, start = c(2017, 1))


dados$periodo =paste(dados$Trimestre, dados$Ano, sep = "-")
dados$periodo = factor(dados$periodo, levels = dados$periodo[order(dados$Index)])

graf_serie <- ggplot(dados, aes(x = periodo, y = Exportações,group = 4)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Preço da Exportação de Minério de Ferro\n ao longo do tempo",
       x = "Período",
       y = "Preço (milhões de doláres)") +
  theme_minimal()+ 
  scale_x_discrete(breaks = dados$periodo[seq(1, length(dados$periodo), by = 4)]) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Centralizado, negrito e tamanho 22
    axis.title.x = element_text(size = 14, face = "bold"),  # Negrito para eixo x
    axis.title.y = element_text(size = 14, face = "bold"),  # Negrito para eixo y
    axis.text.x = element_text(angle = 45, hjust = 0.9, face = "bold"),  # Negrito para valores do eixo x
    axis.text.y = element_text(face = "bold")  # Negrito para valores do eixo y
  )

ggplot(dados, aes(x = Exportações)) + 
  geom_boxplot(color = 'black',fill='purple',width =0.2) + theme_minimal() + 
  labs(title = "Preço de Exportação de Minério de Ferro", x = "Preço (milhões de doláres)")+ 
  theme( plot.title = element_text(hjust = 0.5, size = 16), axis.title = element_text(size = 14), axis.text = element_text(size = 12), legend.position = "none" ) +
  coord_cartesian(ylim = c(-0.2,0.2))

##########################################################################
############## Análise Descritiva ########################################

media.bloco<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstar<-rep(NA,N)
valor<-N%/%k
for (i in 1:valor)
  xstar[((i-1)*k+1):(i*k)]<-rep(mean(x[((i-1)*k+1):(i*k)],na.rm=TRUE),k)
ts.plot(x,main="Média Anual")
lines(xstar, col=2,lwd=3)
xstar}

amplitude.bloco<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstarM<-rep(NA,N)
xstarm<-rep(NA,N)
valor<-N%/%k
for (i in 1:valor){
  xstarM[((i-1)*k+1):(i*k)]<-rep(max(x[((i-1)*k+1):(i*k)],na.rm=TRUE),k)
  xstarm[((i-1)*k+1):(i*k)]<-rep(min(x[((i-1)*k+1):(i*k)],na.rm=TRUE),k)
}
ts.plot(x,main="Amplitude por bloco")
lines(xstarM, col=2,lty=3,lwd=3)
lines(xstarm, col=2,lty=3,lwd=3)
}

med.var<-function(x,k)
{N<-length(x)
x.m<-rep(0,(N-k))
x.r<-rep(0,(N-k))
for (i in 1:(N-k)) x.m[i]<-mean(x[i:(i+k)])
for (i in 1:(N-k)) x.r[i]<-max(x[i:(i+k)])-min(x[i:(i+k)])
plot(x.m,x.r,xlab="médias",ylab="amplitude",main='Média vs Variância')
aa1<-lm(x.r~x.m)
abline(aa1$coef[1],aa1$coef[2],col=2,lwd=3)
summary(aa1)
}

# verificar a homocedasticidade da série
par(mfrow=c(1,2))
med.var(exportacoes_ts,4)
amplitude.bloco(exportacoes_ts,4)
aa1<-lm(x.r~x.m)


# tendencia
media.bloco(exportacoes_ts,4)

library(randtests)
runs.test(as.factor(exportacoes_ts))
cox.stuart.test(diff(exportacoes_ts)) #1 diferença

# diferenças sazonais 
nsdiffs(exportacoes_ts) #0

## Verificar a estacionariedade da série
adf.test(diff(exportacoes_ts,4))
adfTest(diff(exportacoes_ts,4))
ur.df(diff(dados$Exportações),type="trend", lags=4)

exportacoes_ts <- ts(dados$Exportações, frequency = 4, start = c(2002, 1))
ggseasonplot(diff(exportacoes_ts,4), year.labels = FALSE, year.labels.left = FALSE) +
  labs(title = "Seasonal plot",
       x = "Trimestres",
       y = "Preço (milhões de doláres)") +
  theme_minimal() + theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Centralizado, negrito e tamanho 22
    axis.title.x = element_text(size = 14, face = "bold"),  # Negrito para eixo x
    axis.title.y = element_text(size = 14, face = "bold"),  # Negrito para eixo y
    axis.text.x = element_text(angle = 45, hjust = 0.9, face = "bold"),  # Negrito para valores do eixo x
    axis.text.y = element_text(face = "bold")  # Negrito para valores do eixo y
  )

par(mfrow = c(1, 2)) 

tsdisplay(exportacoes_ts,lag.max=20)
tsdisplay(diff(exportacoes_ts),lag.max=12)

acf(diff(exportacoes_ts), main = "ACF")
pacf(diff(exportacoes_ts), main = "PACF")
auto.arima(exportacoes_ts)

###########################################################################################
############################## MODELOS #################################################### 
m1 = sarima(exportacoes_treino,1,1,1,1,0,0,4) 
m2 = sarima(exportacoes_treino,1,1,1,1,0,0,4,
              xreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13))) 
m3 = sarima(exportacoes_treino,0,1,1,1,1,0,4,
            xreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13))) 
m4 = sarima(exportacoes_treino,1,1,1,2,0,0,4,
            xreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13))) 


fit_sarima <- arima(exportacoes_treino, order = c(1, 1, 1), 
                    seasonal = list(order = c(1, 0, 0), 
                                    period = 4),
                    xreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)))



acf(residuals(m2$fit))
auto.arima(exportacoes_ts)

detectAO(fit_sarima)
detectAO(m2$fit)
detectAO(m2$fit,robust = F)
detectAO(fit_sarima,robust=F)
detectIO(fit_sarima)
detectIO(fit_sarima,robust=F)
detectIO(m2$fit)
detectIO(m2$fit, robust=F)

m1 <- HoltWinters(exportacoes_treino, seasonal = "multiplicative")
m2 <- HoltWinters(exportacoes_treino, seasonal = "additive")

shapiro.test(residuals(m1))
shapiro.test(residuals(m2))

##################################################################################################
############################## PREVISÃO ##########################################################

exportacoes_treino <- ts(dados[0:60,]$Exportações, frequency = 4, start = c(2002, 1))
exportacoes_teste  <- ts(dados[61:72,]$Exportações, frequency = 4, start = c(2017, 1))

m2 = sarima(exportacoes_treino,1,1,1,2,0,0,4,
            xreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13,57))) 

fit_sarima <- arima(exportacoes_treino, order = c(1, 1, 1), 
                    seasonal = list(order = c(2, 0, 0), 
                                    period = 4),
                    xreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13,57)))

previsao <- predict(fit_sarima,n.ahead=12,newxreg=data.frame(rep(0,12)),n.start=12)



forecast1 = sarima.for(exportacoes_treino,12,1,1,1,1,0,0,4,
                       newxreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)), 
                       main = "Previsão com SARIMA(1,1,1,1,0,0,4)")

forecast2 = sarima.for(exportacoes_treino,12,0,1,1,1,1,0,4,
                       newxreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)),
                       main = "Previsão com SARIMA(0,1,1,1,1,0,4)")

forecast3 = sarima.for(exportacoes_treino,12,1,1,1,2,0,0,4,
                       newxreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)), 
                       main = "Previsão com SARIMA(1,1,1,2,0,0,4)")

m1 <- HoltWinters(exportacoes_treino, seasonal = "multiplicative")
m2 <- HoltWinters(exportacoes_treino, seasonal = "additive")


predições1 = forecast1$pred
predições2 = forecast2$pred
predições3 = forecast3$pred
predições4 = predict(m1,12)
predições5 = predict(m2,12)

par(mar = c(6, 5, 4, 2) + 0.1)
ts.plot(exportacoes_ts, ylim = c(0, 20000), main = "Previsões dos modelos", 
        xlab = "Período", ylab = "Preço (milhões de dólares)",xlim=c(2015,2019))
lines(predições1, col = "green3")
lines(predições2, col = "red")
lines(predições3, col = "purple")
lines(predições4, col = "orange")
lines(predições5, col = "blue")

legend("bottom", inset = -0.4, legend = c("Mod 1", "Mod 2", "Mod 3", "Mod 4", "Mod 5"),
       col = c("green3", "red", "purple", "orange", "blue"), lty = 1, bty = "n", xpd = TRUE, horiz = TRUE)


data.frame(Real = exportacoes_teste, 
           Mod1 = predições1,
           Mod2 = predições2,
           Mod3 = predições3,
           Mod4 = predições4,
           Mod5 = predições5)

################# Medidas de Desempenho cada modelos #########################

accuracy(exportacoes_teste,predições1)
accuracy(exportacoes_teste,predições2)
accuracy(exportacoes_teste,predições3)
accuracy(exportacoes_teste,predições4)
accuracy(exportacoes_teste,predições5)

# Modelo de Alisamento Exponencial 
m <- HoltWinters(exportacoes_treino, seasonal = "multiplicative")
predict(m,12)

################# Previsão 2017 a 2022 ######################################

par(mfrow = c(1, 3)) 

forecast1 = sarima.for(exportacoes_ts,12,1,1,1,1,0,0,4,
                       newxreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)), 
                       main = "Previsão com SARIMA(1,1,1,1,0,0,4)")

forecast2 = sarima.for(exportacoes_ts,12,0,1,1,1,1,0,4,
                       newxreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)),
                       main = "Previsão com SARIMA(0,1,1,1,1,0,4)")

forecast3 = sarima.for(exportacoes_ts,12,1,1,1,2,0,0,4,
                       newxreg=data.frame(AO=seq(exportacoes_treino) %in% c(41,13)), 
                       main = "Previsão com SARIMA(1,1,1,2,0,0,4)")

m1 <- HoltWinters(exportacoes_ts, seasonal = "multiplicative")
m2 <- HoltWinters(exportacoes_ts, seasonal = "additive")
forecast_m1 <- forecast(m1, h = 12)
forecast_m2 <- forecast(m2, h = 12)
df_plot <- data.frame(
  Periodo = c(time(exportacoes_ts), time(forecast_m1$mean), time(forecast_m2$mean)),
  Exportacoes = c(as.numeric(exportacoes_ts), as.numeric(forecast_m1$mean), as.numeric(forecast_m2$mean)),
  Tipo = c(
    rep("Observado", length(exportacoes_ts)), 
    rep("HW Multiplicativo", length(forecast_m1$mean)), 
    rep("HW Aditivo", length(forecast_m2$mean))
  )
)
ggplot(df_plot, aes(x = Periodo, y = Exportacoes, color = Tipo)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c(
      "Observado" = "black", 
      "HW Multiplicativo" = "orange", 
      "HW Aditivo" = "blue"
    )
  ) +
  labs(
    title = "Previsões dos Modelos Holt-Winters",
    x = "Período",
    y = "Preço (milhões de dólares)",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )




