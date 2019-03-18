library(dplyr)
library(ggplot2)
library(ChainLadder)

dt_KPI <- read.csv("C:\\Users\\Romanka Szabová\\Documents\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")
dt_LatestView <- read.csv("C:\\Users\\Romanka Szabová\\Documents\\GeneralInsurance_Class\\Data\\lesson4_latestView.csv")
dt_PaidCase <- read.csv("C:\\Users\\Romanka Szabová\\Documents\\GeneralInsurance_Class\\Data\\lesson4_PaidCase.csv")

summary(dt_PaidCase)
head(dt_PaidCase)

#all types of buiness and sizes of claims

#filter
Paid_HH_sml <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small" & dataset_type == "PAID")
Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large" & dataset_type == "PAID")
Paid_3P_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small" & dataset_type == "PAID")
Paid_3P_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large" & dataset_type == "PAID")


#trojuholniky
Paid_HH_sml_triangle <- Paid_HH_sml %>% as.triangle(origin = "ay", dev = "dy", "SumOfamount")
Paid_HH_lrg_triangle <- Paid_HH_lrg %>% as.triangle(origin = "ay", dev = "dy", "SumOfamount")
Paid_3P_sml_triangle <- Paid_3P_sml %>% as.triangle(origin = "ay", dev = "dy", "SumOfamount")
Paid_3P_lrg_triangle <- Paid_3P_lrg %>% as.triangle(origin = "ay", dev = "dy", "SumOfamount")

#grafy
plot(Paid_HH_sml_triangle)
plot(predict(chainladder(Paid_HH_sml_triangle)))

plot(Paid_HH_lrg_triangle)
plot(predict(chainladder(Paid_HH_lrg_triangle)))

plot(Paid_3P_sml_triangle)
plot(predict(chainladder(Paid_3P_sml_triangle)))

plot(Paid_3P_lrg_triangle)
plot(predict(chainladder(Paid_3P_lrg_triangle)))

#age to age
ata(Paid_HH_sml_triangle)
ata(Paid_HH_lrg_triangle)
ata(Paid_3P_sml_triangle)
ata(Paid_3P_lrg_triangle)

# Houses maju vyssie simple averages ako 3rd Party, pri Houses je zrejme
# postacujuce jedno plnenie a uz je vestko ok (lahke chvosty) na rozdiel od 3rd party,
# kde sa mozu rokmi objavovat dalsie problemy (tazke chvosty)
# rozdiel medzi small a large houses nie je takmer ziadny
# rozdiel medzi small a large 3rd Party nie je dost velky co je prirodzene

#using additional information

Paid_HH_sml1 <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small")
Paid_HH_sml1_triangle <- Paid_HH_sml1 %>% as.triangle(origin = "ay", dev = "dy", "SumOfamount")
plot(Paid_HH_sml1_triangle)
plot(predict(chainladder(Paid_HH_sml1_triangle)))
ata(Paid_HH_sml1_triangle)

#...a tak dalej pre ostatne kombinacie

###############################################################################

#Excercise3

summary(dt_KPI)
head(dt_KPI)

# Priklad pre Business = Housing

dt_KPI_HH <- dt_KPI %>% filter(Business == "Housing")
attach(dt_KPI_HH)
dt_KPI_HH_O <- dt_KPI_HH[order(Year),]
detach(dt_KPI_HH)

#odhadnute delays
q<-c(sample(1:5,22,replace=TRUE),sample(1:4,22,replace=TRUE),
     sample(1:3,16,replace=TRUE), sample(1:2,16,replace=TRUE), rep(1,21))

dt_KPI_HH_q<-cbind(dt_KPI_HH_O,q)

#trojuholnik
dt_KPI_HH_triangle <- dt_KPI_HH_q %>% as.triangle(origin = "Year", dev = "q", "Losses")

#grafy
plot(dt_KPI_HH_triangle)
plot(predict(chainladder(dt_KPI_HH_triangle)))




