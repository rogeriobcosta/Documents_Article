library(readxl)
plan_ESP4_EspNow_8m_5s_CB <- read_excel("ESP4 EspNow 8m 5s CB.xlsx")
View(plan_ESP4_EspNow_8m_5s_CB)

valor<-plan_ESP4_EspNow_8m_5s_CB$Column9
posicoes<-c(1:length(valor))

corte_1<-min(posicoes[which(valor==3.2)])
corte_2<-min(posicoes[which(valor==2.8)])

valor[which(posicoes==corte_1)]
valor[which(posicoes==corte_2)]

a<- rep(1,corte_1)
b<- rep(2,corte_2-corte_1)
c<- rep(3,length(valor)-corte_2)

plan_ESP4_EspNow_8m_5s_CB$nivel<-c(a,b,c)


###########################################################

require(lubridate)
plan_ESP4_EspNow_8m_5s_CB$t<-(plan_ESP4_EspNow_8m_5s_CB$Column1)
plan_ESP4_EspNow_8m_5s_CB$t0<-min((plan_ESP4_EspNow_8m_5s_CB$Column1))
plan_ESP4_EspNow_8m_5s_CB$survt<-(plan_ESP4_EspNow_8m_5s_CB$t-plan_ESP4_EspNow_8m_5s_CB$t0)
segundos<-round(as.numeric(diff(plan_ESP4_EspNow_8m_5s_CB$survt,1)))
plan_ESP4_EspNow_8m_5s_CB$segundos<-c(0,segundos)
plan_ESP4_EspNow_8m_5s_CB$segundos<- ifelse(plan_ESP4_EspNow_8m_5s_CB$segundos<0,5,plan_ESP4_EspNow_8m_5s_CB$segundos)
plan_ESP4_EspNow_8m_5s_CB$segundos_acum<-cumsum(plan_ESP4_EspNow_8m_5s_CB$segundos)
plan_ESP4_EspNow_8m_5s_CB$minutos_acum<-round(cumsum(plan_ESP4_EspNow_8m_5s_CB$segundos)/60)
plan_ESP4_EspNow_8m_5s_CB$minuto = cut(as.numeric(plan_ESP4_EspNow_8m_5s_CB$minutos_acum), c(0,5, 10,15, 20,25, 30,35, 40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,245,250,255,260,265,270,275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,360,365,370,375,380,385,390,395,400,405,410,415,420,425,430,440,445,450,455,460,465,470,475,480,485,490,495,500,505,510,515,520,525,530,535,540,545,550,555,560,565,570,575,580,585,590,595,600,605,610,615,620,625,630,640,650,655,Inf), right=FALSE)


tapply(plan_ESP4_EspNow_8m_5s_CB$Column9,plan_ESP4_EspNow_8m_5s_CB$nivel,sd)
tapply(plan_ESP4_EspNow_8m_5s_CB$Column9,plan_ESP4_EspNow_8m_5s_CB$minuto,sd)

a<-tapply(plan_ESP4_EspNow_8m_5s_CB$Column9,list(plan_ESP4_EspNow_8m_5s_CB$minuto, plan_ESP4_EspNow_8m_5s_CB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(a,file="saida_dp_ESP4_EspNow_8m_5s_CB.csv")

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
labs(title = "ESP4_EspNow_8m_5s_CB", subtitle = "Gráfico Desvio Padrão")

ggsave("Grafico_DP_ESP4_EspNow_8m_5s_CB.png")
