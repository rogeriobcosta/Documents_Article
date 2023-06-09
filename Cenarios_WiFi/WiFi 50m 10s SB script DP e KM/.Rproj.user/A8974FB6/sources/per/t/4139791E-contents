library(readxl)
plan_ESP1_WiFi_50m_10s_SB <- read_excel("Esp1 WiFi 50m 10s SB.xlsx")
View(plan_ESP1_WiFi_50m_10s_SB)

valor<-plan_ESP1_WiFi_50m_10s_SB$Column9
posicoes<-c(1:length(valor))

corte_1<-min(posicoes[which(valor==3.2)])
corte_2<-min(posicoes[which(valor==2.8)])

valor[which(posicoes==corte_1)]
valor[which(posicoes==corte_2)]

a<- rep(1,corte_1)
b<- rep(2,corte_2-corte_1)
c<- rep(3,length(valor)-corte_2)

plan_ESP1_WiFi_50m_10s_SB$nivel<-c(a,b,c)


###########################################################

require(lubridate)
plan_ESP1_WiFi_50m_10s_SB$t<-(plan_ESP1_WiFi_50m_10s_SB$Column1)
plan_ESP1_WiFi_50m_10s_SB$t0<-min((plan_ESP1_WiFi_50m_10s_SB$Column1))
plan_ESP1_WiFi_50m_10s_SB$survt<-(plan_ESP1_WiFi_50m_10s_SB$t-plan_ESP1_WiFi_50m_10s_SB$t0)
segundos<-round(as.numeric(diff(plan_ESP1_WiFi_50m_10s_SB$survt,1)))
plan_ESP1_WiFi_50m_10s_SB$segundos<-c(0,segundos)
plan_ESP1_WiFi_50m_10s_SB$segundos<- ifelse(plan_ESP1_WiFi_50m_10s_SB$segundos<0,10,plan_ESP1_WiFi_50m_10s_SB$segundos)
plan_ESP1_WiFi_50m_10s_SB$segundos_acum<-cumsum(plan_ESP1_WiFi_50m_10s_SB$segundos)
plan_ESP1_WiFi_50m_10s_SB$minutos_acum<-round(cumsum(plan_ESP1_WiFi_50m_10s_SB$segundos)/60)
plan_ESP1_WiFi_50m_10s_SB$minuto = cut(as.numeric(plan_ESP1_WiFi_50m_10s_SB$minutos_acum), c(0,5, 10,15, 20,25, 30,35, 40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140, Inf), right=FALSE)


tapply(plan_ESP1_WiFi_50m_10s_SB$Column9,plan_ESP1_WiFi_50m_10s_SB$nivel,sd)
tapply(plan_ESP1_WiFi_50m_10s_SB$Column9,plan_ESP1_WiFi_50m_10s_SB$minuto,sd)

a<-tapply(plan_ESP1_WiFi_50m_10s_SB$Column9,list(plan_ESP1_WiFi_50m_10s_SB$minuto, plan_ESP1_WiFi_50m_10s_SB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(a,file="saida_dp_ESP1_WiFi_50m_10s_SB.csv")

#grafico

par(mfrow = c(2, 2))
plot(b$`>3.2`,col="blue",type="l")
plot(b$`<=3.2`,col="green",type="l")
plot(b$`<=2.8`,col="red",type="l")

b$indice<-c(1:nrow(b))

library(tidyr)
library(ggplot2)

dados_long <- b %>% gather(variavel, valor, -indice)

ggplot(data = dados_long, aes(x = indice, y = valor, group = variavel)) +
  geom_line(aes(colour = variavel)) +
labs(title = "ESP1_WiFi_50m_10s_SB", subtitle = "Gráfico Desvio Padrão")

ggsave("Grafico_DP_ESP1_WiFi_50m_10s_SB.png")
