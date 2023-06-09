library(readxl)
plan_ESP1_EspNow_50m_10s_CB <- read_excel("Esp1 EspNow 50m 10s CB.xlsx")
View(plan_ESP1_EspNow_50m_10s_CB)
plan_ESP2_EspNow_50m_10s_CB <- read_excel("Esp2 EspNow 50m 10s CB.xlsx")
View(plan_ESP2_EspNow_50m_10s_CB)
plan_ESP3_EspNow_50m_10s_CB <- read_excel("Esp3 EspNow 50m 10s CB.xlsx")
View(plan_ESP3_EspNow_50m_10s_CB)
plan_ESP4_EspNow_50m_10s_CB <- read_excel("Esp4 EspNow 50m 10s CB.xlsx")
View(plan_ESP4_EspNow_50m_10s_CB)

plan_ESP1_EspNow_50m_10s_CB$nivel<-ifelse(plan_ESP1_EspNow_50m_10s_CB$Column9<=2.8,3,
                                        ifelse(plan_ESP1_EspNow_50m_10s_CB$Column9<=3.2,2,1))

table(plan_ESP1_EspNow_50m_10s_CB$nivel)


plan_ESP2_EspNow_50m_10s_CB$nivel<-ifelse(plan_ESP2_EspNow_50m_10s_CB$Column9<=2.8,3,
                                        ifelse(plan_ESP2_EspNow_50m_10s_CB$Column9<=3.2,2,1))

table(plan_ESP2_EspNow_50m_10s_CB$nivel)

plan_ESP3_EspNow_50m_10s_CB$nivel<-ifelse(plan_ESP3_EspNow_50m_10s_CB$Column9<=2.8,3,
                                        ifelse(plan_ESP3_EspNow_50m_10s_CB$Column9<=3.2,2,1))

table(plan_ESP3_EspNow_50m_10s_CB$nivel)

plan_ESP4_EspNow_50m_10s_CB$nivel<-ifelse(plan_ESP4_EspNow_50m_10s_CB$Column9<=2.8,3,
                                        ifelse(plan_ESP4_EspNow_50m_10s_CB$Column9<=3.2,2,1))

table(plan_ESP4_EspNow_50m_10s_CB$nivel)





require(lubridate)
plan_ESP1_EspNow_50m_10s_CB$t<-(plan_ESP1_EspNow_50m_10s_CB$Column1)
plan_ESP1_EspNow_50m_10s_CB$t0<-min((plan_ESP1_EspNow_50m_10s_CB$Column1))
plan_ESP1_EspNow_50m_10s_CB$survt<-(plan_ESP1_EspNow_50m_10s_CB$t-plan_ESP1_EspNow_50m_10s_CB$t0)

plan_ESP1_EspNow_50m_10s_CB$survt_minutos<-(plan_ESP1_EspNow_50m_10s_CB$survt)

t0<-min((plan_ESP1_EspNow_50m_10s_CB$Column1))
t<-plan_ESP1_EspNow_50m_10s_CB$Column1

difftime(t0, t, tz,
         units = c("auto", "secs", "mins", "hours",
                   "days", "weeks"))

plan_ESP1_EspNow_50m_10s_CB$survt<-difftime(plan_ESP1_EspNow_50m_10s_CB$t, plan_ESP1_EspNow_50m_10s_CB$t0, tz,
         units = c("mins"))


summary(plan_ESP1_EspNow_50m_10s_CB$survt)


plan_ESP1_EspNow_50m_10s_CB$minuto = cut(as.numeric(plan_ESP1_EspNow_50m_10s_CB$survt), c(0,60,120,180,240,300,360,420,480,540,600,660,720,780,840,900,960,1020,1080,1140,1200,Inf), right=FALSE)


tapply(plan_ESP1_EspNow_50m_10s_CB$Column9,plan_ESP1_EspNow_50m_10s_CB$nivel,sd)
tapply(plan_ESP1_EspNow_50m_10s_CB$Column9,plan_ESP1_EspNow_50m_10s_CB$minuto,sd)

a<-tapply(plan_ESP1_EspNow_50m_10s_CB$Column9,list(plan_ESP1_EspNow_50m_10s_CB$minuto, plan_ESP1_EspNow_50m_10s_CB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(b,file="saida_dp_ESP1_EspNow_50m_10s_CB.csv")

#################################################################################################################################################################################
#####################################################################################################################
require(lubridate)
plan_ESP2_EspNow_50m_10s_CB$t<-(plan_ESP2_EspNow_50m_10s_CB$Column1)
plan_ESP2_EspNow_50m_10s_CB$t0<-min((plan_ESP2_EspNow_50m_10s_CB$Column1))
plan_ESP2_EspNow_50m_10s_CB$survt<-(plan_ESP2_EspNow_50m_10s_CB$t-plan_ESP2_EspNow_50m_10s_CB$t0)

plan_ESP2_EspNow_50m_10s_CB$survt_minutos<-(plan_ESP2_EspNow_50m_10s_CB$survt)

t0<-min((plan_ESP2_EspNow_50m_10s_CB$Column1))
t<-plan_ESP2_EspNow_50m_10s_CB$Column1

difftime(t0, t, tz,
         units = c("auto", "secs", "mins", "hours",
                   "days", "weeks"))

plan_ESP2_EspNow_50m_10s_CB$survt<-difftime(plan_ESP2_EspNow_50m_10s_CB$t, plan_ESP2_EspNow_50m_10s_CB$t0, tz,
                                        units = c("mins"))


summary(plan_ESP2_EspNow_50m_10s_CB$survt)


plan_ESP2_EspNow_50m_10s_CB$minuto = cut(as.numeric(plan_ESP2_EspNow_50m_10s_CB$survt), c(0, 10, 20, 30, 40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,660,670,680,690,700,710,720,730,740,750,760,770,780,790,800,810,820,830,840,850,860,870,880,890,900,910,920,930,940,950,960,970,980,990,1000,1010,1020,1030,1040,1050,1060,1080,1090,1100,1110,1120,1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230,1240,1250,1260,1270,1280,1290,1300,1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,1410,1420,1430,1440,1450,1460,1470,1480,1490,1500,1510,1520,1530,1540,1550,1560,1570,1580,1590,1600,Inf), right=FALSE)


tapply(plan_ESP2_EspNow_50m_10s_CB$Column9,plan_ESP2_EspNow_50m_10s_CB$nivel,sd)
tapply(plan_ESP2_EspNow_50m_10s_CB$Column9,plan_ESP2_EspNow_50m_10s_CB$minuto,sd)

a<-tapply(plan_ESP2_EspNow_50m_10s_CB$Column9,list(plan_ESP2_EspNow_50m_10s_CB$minuto, plan_ESP2_EspNow_50m_10s_CB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(b,file="saida_dp_ESP2_EspNow_50m_10s_CB.csv")

#################################################################################################################################################################################
#####################################################################################################################
require(lubridate)
plan_ESP3_EspNow_50m_10s_CB$t<-(plan_ESP3_EspNow_50m_10s_CB$Column1)
plan_ESP3_EspNow_50m_10s_CB$t0<-min((plan_ESP3_EspNow_50m_10s_CB$Column1))
plan_ESP3_EspNow_50m_10s_CB$survt<-(plan_ESP3_EspNow_50m_10s_CB$t-plan_ESP3_EspNow_50m_10s_CB$t0)

plan_ESP3_EspNow_50m_10s_CB$survt_minutos<-(plan_ESP3_EspNow_50m_10s_CB$survt)

t0<-min((plan_ESP3_EspNow_50m_10s_CB$Column1))
t<-plan_ESP3_EspNow_50m_10s_CB$Column1

difftime(t0, t, tz,
         units = c("auto", "secs", "mins", "hours",
                   "days", "weeks"))

plan_ESP3_EspNow_50m_10s_CB$survt<-difftime(plan_ESP3_EspNow_50m_10s_CB$t, plan_ESP3_EspNow_50m_10s_CB$t0, tz,
                                        units = c("mins"))


summary(plan_ESP3_EspNow_50m_10s_CB$survt)


plan_ESP3_EspNow_50m_10s_CB$minuto = cut(as.numeric(plan_ESP3_EspNow_50m_10s_CB$survt), c(0, 10, 20, 30, 40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,660,670,680,690,700,710,720,730,740,750,760,770,780,790,800,810,820,830,840,850,860,870,880,890,900,910,920,930,940,950,960,970,980,990,1000,1010,1020,1030,1040,1050,1060,1080,1090,1100,1110,1120,1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230,1240,1250,1260,1270,1280,1290,1300,1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,1410,1420,1430,1440,1450,1460,1470,1480,1490,1500,1510,1520,1530,1540,1550,1560,1570,1580,1590,1600,Inf), right=FALSE)


tapply(plan_ESP3_EspNow_50m_10s_CB$Column9,plan_ESP3_EspNow_50m_10s_CB$nivel,sd)
tapply(plan_ESP3_EspNow_50m_10s_CB$Column9,plan_ESP3_EspNow_50m_10s_CB$minuto,sd)

a<-tapply(plan_ESP3_EspNow_50m_10s_CB$Column9,list(plan_ESP3_EspNow_50m_10s_CB$minuto, plan_ESP3_EspNow_50m_10s_CB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(b,file="saida_dp_ESP3_EspNow_50m_10s_CB.csv")

#################################################################################################################################################################################
#####################################################################################################################
require(lubridate)
plan_ESP4_EspNow_50m_10s_CB$t<-(plan_ESP4_EspNow_50m_10s_CB$Column1)
plan_ESP4_EspNow_50m_10s_CB$t0<-min((plan_ESP4_EspNow_50m_10s_CB$Column1))
plan_ESP4_EspNow_50m_10s_CB$survt<-(plan_ESP4_EspNow_50m_10s_CB$t-plan_ESP4_EspNow_50m_10s_CB$t0)

plan_ESP4_EspNow_50m_10s_CB$survt_minutos<-(plan_ESP4_EspNow_50m_10s_CB$survt)

t0<-min((plan_ESP4_EspNow_50m_10s_CB$Column1))
t<-plan_ESP4_EspNow_50m_10s_CB$Column1

difftime(t0, t, tz,
         units = c("auto", "secs", "mins", "hours",
                   "days", "weeks"))

plan_ESP4_EspNow_50m_10s_CB$survt<-difftime(plan_ESP4_EspNow_50m_10s_CB$t, plan_ESP4_EspNow_50m_10s_CB$t0, tz,
                                        units = c("mins"))


summary(plan_ESP4_EspNow_50m_10s_CB$survt)


plan_ESP4_EspNow_50m_10s_CB$minuto = cut(as.numeric(plan_ESP4_EspNow_50m_10s_CB$survt), c(0, 10, 20, 30, 40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,660,670,680,690,700,710,720,730,740,750,760,770,780,790,800,810,820,830,840,850,860,870,880,890,900,910,920,930,940,950,960,970,980,990,1000,1010,1020,1030,1040,1050,1060,1080,1090,1100,1110,1120,1130,1140,1150,1160,1170,1180,1190,1200,1210,1220,1230,1240,1250,1260,1270,1280,1290,1300,1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,1410,1420,1430,1440,1450,1460,1470,1480,1490,1500,1510,1520,1530,1540,1550,1560,1570,1580,1590,1600,Inf), right=FALSE)


tapply(plan_ESP4_EspNow_50m_10s_CB$Column9,plan_ESP4_EspNow_50m_10s_CB$nivel,sd)
tapply(plan_ESP4_EspNow_50m_10s_CB$Column9,plan_ESP4_EspNow_50m_10s_CB$minuto,sd)

a<-tapply(plan_ESP4_EspNow_50m_10s_CB$Column9,list(plan_ESP4_EspNow_50m_10s_CB$minuto, plan_ESP4_EspNow_50m_10s_CB$nivel),sd)
b<-data.frame(a)
names(b)<-c(">3.2","<=3.2","<=2.8")

# exportando saida para csv
write.csv(b,file="saida_dp_ESP4_EspNow_50m_10s_CB.csv")

#################################################################################################################################################################################
