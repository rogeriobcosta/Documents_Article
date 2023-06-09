library(readxl)
plan_ESP4_WiFi_50m_10s_CB <- read_excel("ESP4 WiFi 50m 10s CB.xlsx")
View(plan_ESP4_WiFi_50m_10s_CB)

valor<-plan_ESP4_WiFi_50m_10s_CB$Column9
posicoes<-c(1:length(valor))

corte_1<-min(posicoes[which(valor==3.2)])
corte_2<-min(posicoes[which(valor==2.8)])

valor[which(posicoes==corte_1)]
valor[which(posicoes==corte_2)]

a<- rep(1,corte_1)
b<- rep(2,corte_2-corte_1)
c<- rep(3,length(valor)-corte_2)

plan_ESP4_WiFi_50m_10s_CB$nivel<-c(a,b,c)


###########################################################

require(lubridate)
plan_ESP4_WiFi_50m_10s_CB$t<-(plan_ESP4_WiFi_50m_10s_CB$Column1)
plan_ESP4_WiFi_50m_10s_CB$t0<-min((plan_ESP4_WiFi_50m_10s_CB$Column1))
plan_ESP4_WiFi_50m_10s_CB$survt<-(plan_ESP4_WiFi_50m_10s_CB$t-plan_ESP4_WiFi_50m_10s_CB$t0)
segundos<-round(as.numeric(diff(plan_ESP4_WiFi_50m_10s_CB$survt,1)))
plan_ESP4_WiFi_50m_10s_CB$segundos<-c(0,segundos)
plan_ESP4_WiFi_50m_10s_CB$segundos<- ifelse(plan_ESP4_WiFi_50m_10s_CB$segundos<0,5,plan_ESP4_WiFi_50m_10s_CB$segundos)
plan_ESP4_WiFi_50m_10s_CB$segundos_acum<-cumsum(plan_ESP4_WiFi_50m_10s_CB$segundos)
plan_ESP4_WiFi_50m_10s_CB$minutos_acum<-round(cumsum(plan_ESP4_WiFi_50m_10s_CB$segundos)/60)
plan_ESP4_WiFi_50m_10s_CB$minuto = cut(as.numeric(plan_ESP4_WiFi_50m_10s_CB$minutos_acum), c(0,5, 10,15, 20,25, 30,35, 40,45,50,55,60,65,70,75,80,Inf), right=FALSE)


tapply(plan_ESP4_WiFi_50m_10s_CB$Column9,plan_ESP4_WiFi_50m_10s_CB$nivel,sd)
tapply(plan_ESP4_WiFi_50m_10s_CB$Column9,plan_ESP4_WiFi_50m_10s_CB$minuto,sd)

a<-tapply(plan_ESP4_WiFi_50m_10s_CB$Column9,list(plan_ESP4_WiFi_50m_10s_CB$minuto, plan_ESP4_WiFi_50m_10s_CB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(a,file="saida_dp_ESP4_WiFi_50m_10s_CB.csv")

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
labs(title = "ESP4_WiFi_50m_10s_CB", subtitle = "Gráfico Desvio Padrão")

ggsave("Grafico_DP_ESP4_WiFi_50m_10s_CB.png")
