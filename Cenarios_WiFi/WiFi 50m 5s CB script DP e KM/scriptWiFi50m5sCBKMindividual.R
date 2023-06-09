library(readxl)
Esp1_WiFi_50m_5s_CB <- read_excel("ESP1 WiFi 50m 5s CB.xlsx")
Esp2_WiFi_50m_5s_CB <- read_excel("ESP2 WiFi 50m 5s CB.xlsx")
Esp3_WiFi_50m_5s_CB <- read_excel("ESP3 WiFi 50m 5s CB.xlsx")
Esp4_WiFi_50m_5s_CB <- read_excel("ESP4 WiFi 50m 5s CB.xlsx")

View(Esp1_WiFi_50m_5s_CB)
View(Esp2_WiFi_50m_5s_CB)
View(Esp3_WiFi_50m_5s_CB)
View(Esp4_WiFi_50m_5s_CB)


require(lubridate)
Esp1_WiFi_50m_5s_CB$t<-hour(Esp1_WiFi_50m_5s_CB$Column1)
Esp1_WiFi_50m_5s_CB$status<-ifelse(Esp1_WiFi_50m_5s_CB$Column9<=2.8,1,0)

Esp2_WiFi_50m_5s_CB$t<-hour(Esp2_WiFi_50m_5s_CB$Column1)
Esp2_WiFi_50m_5s_CB$status<-ifelse(Esp2_WiFi_50m_5s_CB$Column9<=2.8,1,0)

Esp3_WiFi_50m_5s_CB$t<-hour(Esp3_WiFi_50m_5s_CB$Column1)
Esp3_WiFi_50m_5s_CB$status<-ifelse(Esp3_WiFi_50m_5s_CB$Column9<=2.8,1,0)

Esp4_WiFi_50m_5s_CB$t<-hour(Esp4_WiFi_50m_5s_CB$Column1)
Esp4_WiFi_50m_5s_CB$status<-ifelse(Esp4_WiFi_50m_5s_CB$Column9<=2.8,1,0)

dados1_50m_5s_CB<-Esp1_WiFi_50m_5s_CB[,9:10]
dados1_50m_5s_CB$Barreira="Esp1_WiFi_50m_5s_CB"
dados1_50m_5s_CB$t0=min(dados1_50m_5s_CB$t)
dados1_50m_5s_CB$survt<-dados1_50m_5s_CB$t-dados1_50m_5s_CB$t0

dados2_50m_5s_CB<-Esp2_WiFi_50m_5s_CB[,9:10]
dados2_50m_5s_CB$Barreira="Esp2_WiFi_50m_5s_CB"
dados2_50m_5s_CB$t0=min(dados2_50m_5s_CB$t)
dados2_50m_5s_CB$survt<-dados2_50m_5s_CB$t-dados2_50m_5s_CB$t0

dados3_50m_5s_CB<-Esp3_WiFi_50m_5s_CB[,9:10]
dados3_50m_5s_CB$Barreira="Esp3_WiFi_50m_5s_CB"
dados3_50m_5s_CB$t0=min(dados3_50m_5s_CB$t)
dados3_50m_5s_CB$survt<-dados3_50m_5s_CB$t-dados3_50m_5s_CB$t0

dados4_50m_5s_CB<-Esp4_WiFi_50m_5s_CB[,9:10]
dados4_50m_5s_CB$Barreira="Esp4_WiFi_50m_5s_CB"
dados4_50m_5s_CB$t0=min(dados4_50m_5s_CB$t)
dados4_50m_5s_CB$survt<-dados4_50m_5s_CB$t-dados4_50m_5s_CB$t0

dados_50m_5s<-rbind.data.frame(dados1_50m_5s_CB,dados2_50m_5s_CB,dados3_50m_5s_CB,dados4_50m_5s_CB)
dados_50m_5s$Barreira <- as.factor(dados_50m_5s$Barreira)
dados_50m_5s$status <- as.numeric(dados_50m_5s$status)
dados_50m_5s$t <- as.numeric(dados_50m_5s$t)
dados_50m_5s$survt <- as.numeric(dados_50m_5s$survt)

library("survival")
# Survival times until event
Surv(time = dados_50m_5s$survt, event = dados_50m_5s$status == 1)
summary(Surv(time = dados_50m_5s$survt, event = dados_50m_5s$status == 1))

# Surv creates survival object which is the response variable
Y = Surv(dados_50m_5s$survt, dados_50m_5s$status)
# Stratify by barreira variable:
kmfit = survfit(Y ~ dados_50m_5s$Barreira)

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

model_fit <- survfit(Surv(survt, status) ~ Barreira, data = dados_50m_5s)

autoplot(model_fit) + 
  labs(x = "\n Tempo de Sobrevivência em horas ", y = "Probabilidade de Sobrevivência \n", 
       title = "Tempo de Sobrevivência\n WiFi_50m_5s_CB \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 12),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 12),
        
        legend.title = element_text(face="bold", size = 10))



ggsave("Grafico_KM_ESP_WiFi_50m_5s_CB.png")




