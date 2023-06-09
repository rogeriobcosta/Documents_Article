library(readxl)
Esp1_BT_5m_10s_CB <- read_excel("Esp1 BT 5m 10s CB.xlsx")
Esp2_BT_5m_10s_CB <- read_excel("Esp2 BT 5m 10s CB.xlsx")
Esp3_BT_5m_10s_CB <- read_excel("Esp3 BT 5m 10s CB.xlsx")
Esp4_BT_5m_10s_CB <- read_excel("Esp4 BT 5m 10s CB.xlsx")

View(Esp1_BT_5m_10s_CB)
View(Esp2_BT_5m_10s_CB)
View(Esp3_BT_5m_10s_CB)
View(Esp4_BT_5m_10s_CB)


require(lubridate)
Esp1_BT_5m_10s_CB$t<-hour(Esp1_BT_5m_10s_CB$Column1)
Esp1_BT_5m_10s_CB$status<-ifelse(Esp1_BT_5m_10s_CB$Column9<=2.8,1,0)

Esp2_BT_5m_10s_CB$t<-hour(Esp2_BT_5m_10s_CB$Column1)
Esp2_BT_5m_10s_CB$status<-ifelse(Esp2_BT_5m_10s_CB$Column9<=2.8,1,0)

Esp3_BT_5m_10s_CB$t<-hour(Esp3_BT_5m_10s_CB$Column1)
Esp3_BT_5m_10s_CB$status<-ifelse(Esp3_BT_5m_10s_CB$Column9<=2.8,1,0)

Esp4_BT_5m_10s_CB$t<-hour(Esp4_BT_5m_10s_CB$Column1)
Esp4_BT_5m_10s_CB$status<-ifelse(Esp4_BT_5m_10s_CB$Column9<=2.8,1,0)

dados1_5m_10s_CB<-Esp1_BT_5m_10s_CB[,9:10]
dados1_5m_10s_CB$Barreira="Esp1_BT_5m_10s_CB"
dados1_5m_10s_CB$t0=min(dados1_5m_10s_CB$t)
dados1_5m_10s_CB$survt<-dados1_5m_10s_CB$t-dados1_5m_10s_CB$t0

dados2_5m_10s_CB<-Esp2_BT_5m_10s_CB[,9:10]
dados2_5m_10s_CB$Barreira="Esp2_BT_5m_10s_CB"
dados2_5m_10s_CB$t0=min(dados2_5m_10s_CB$t)
dados2_5m_10s_CB$survt<-dados2_5m_10s_CB$t-dados2_5m_10s_CB$t0

dados3_5m_10s_CB<-Esp3_BT_5m_10s_CB[,9:10]
dados3_5m_10s_CB$Barreira="Esp3_BT_5m_10s_CB"
dados3_5m_10s_CB$t0=min(dados3_5m_10s_CB$t)
dados3_5m_10s_CB$survt<-dados3_5m_10s_CB$t-dados3_5m_10s_CB$t0

dados4_5m_10s_CB<-Esp4_BT_5m_10s_CB[,9:10]
dados4_5m_10s_CB$Barreira="Esp4_BT_5m_10s_CB"
dados4_5m_10s_CB$t0=min(dados4_5m_10s_CB$t)
dados4_5m_10s_CB$survt<-dados4_5m_10s_CB$t-dados4_5m_10s_CB$t0

dados_5m_10s<-rbind.data.frame(dados1_5m_10s_CB, dados2_5m_10s_CB, dados3_5m_10s_CB, dados4_5m_10s_CB)
dados_5m_10s$Barreira <- as.factor(dados_5m_10s$Barreira)
dados_5m_10s$status <- as.numeric(dados_5m_10s$status)
dados_5m_10s$t <- as.numeric(dados_5m_10s$t)
dados_5m_10s$survt <- as.numeric(dados_5m_10s$survt)

library("survival")
# Survival times until event
Surv(time = dados_5m_10s$survt, event = dados_5m_10s$status == 1)
summary(Surv(time = dados_5m_10s$survt, event = dados_5m_10s$status == 1))

# Surv creates survival object which is the response variable
Y = Surv(dados_5m_10s$survt, dados_5m_10s$status)
# Stratify by barreira variable:
kmfit = survfit(Y ~ dados_5m_10s$Barreira)

summary(kmfit, times = c(seq(0, 20, by = 1)))
saida<-summary(kmfit, times = c(seq(0, 20, by = 1)))
saida2<-data.frame(saida$time,saida$n.risk,saida$n.event,saida$surv,saida$std.err,
                   saida$lower,saida$upper)

# exportando saida para csv
write.csv(saida2,file="saida.csv")


plot(kmfit, lty = c("solid", "dashed", "dotted", "solid"), col = c("black", "grey", "black", "grey"), xlab = "Survival Time In Days", ylab = "Survival Probabilities")
legend("topright", c("Esp1", "Esp2", "Esp3", "Esp4"), lty = c("solid", "dashed", "dotted", "solid"), col = c("black", "grey", "black", "grey"))





library(ggplot2)
library(ggfortify)

model_fit <- survfit(Surv(survt, status) ~ Barreira, data = dados_5m_10s)

autoplot(model_fit) + 
  labs(x = "\n Tempo de Sobrevivência em horas", y = "Probabilidade de Sobrevivência \n", 
       title = "Tempo de Sobrevivência\n Bluetooth 5m 10s CB \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        legend.title = element_text(face="bold", size = 10))


ggsave("Grafico_KM_ESP1_BT_5m_10s_CB.png")








