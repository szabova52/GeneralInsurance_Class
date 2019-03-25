library(ggplot2)
library(dplyr)

dt_Policy <- read.csv("D:\\General\\GeneralInsurance_Class-Class2019\\Data\\lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Policy %>% nrow()
dt_Policy %>% distinct(NrPolicy, NrObject) %>% nrow() 

dt_Claims <- read.csv("D:\\General\\GeneralInsurance_Class-Class2019\\Data\\lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)
dt_Claims %>% nrow()
dt_Claims %>% distinct(NrClaim) %>% nrow()

dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
                    )
head(dt_pol_w_claims)


dt_pol_w_claims %>% group_by(is.na(Paid)) %>% summarise(cnt = n())


dt_pol_w_claims %>% 
  filter(!is.na(Paid)) %>% 
  select(Paid) %>% 
  arrange(desc(Paid)) %>% 
  head()

dt_pol_w_claims %>% 
  filter(!is.na(Paid)) %>% 
  select(Paid, Dt_Exp_Start, Dt_Exp_End) %>% 
  arrange(desc(Paid)) %>% 
  head()


library(lubridate)
dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start))


dt_pol_w_claims %>% 
  filter(!is.na(Paid)) %>% 
  select(Paid, Dt_Exp_Start, Dt_Exp_End, Time_Exposure)  %>% 
  arrange(desc(Paid)) %>% head()

dt_pol_w_claims <- 
  dt_pol_w_claims %>% 
  mutate(Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure))
  )
head(dt_pol_w_claims)


dt_pol_w_claims %>% 
  filter(!is.na(Paid)) %>% 
  select(Paid, Reserves, Ult_Loss, Burning_Cost) %>% head()
########################################################################

#Construct_year

#skumame construct year,vyssi vek auta moze napr. negativne
#ovpyvnit technicky stav vozidla, s vekom sa meni aj hodnota auta

dt_pol_w_claims[, 17] <- as.factor(dt_pol_w_claims[, 17])

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_boxplot() +
  ylim(0, 100)

#z grafov sa zda, ze najvyssie burning costs su v rokoch 2009 az 2014
#vidime, ze pre starsie vozidla je malo dat

dt_pol_w_claims %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))


#najvacsi priemer maju vozidla vyrobene v roku 1997, 	
#avsak tato info nie je velmi relevantna nakolko z toho roku mame malo dat
#s dostatocnym poctom dat ma najvyssi priemer rok vyroby 2011,
#no od nasledujucich rokov sa velmi nelisi,nevyzera to ako signifikantny faktor

#VEH_brand 

#skumame, ci niektore typy vozidiel nie su kazivejsie

dt_pol_w_claims[, 16] <- as.factor(dt_pol_w_claims[, 16])

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = VEH_brand )) + 
  geom_jitter()

dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = VEH_brand )) + 
  geom_boxplot() +
  ylim(0, 100)

#v grafoch je velmi vela roznorodych dat a tazko sa v nich orientuje
#zda sa, ze pri autach s nizsim cislom su burning costs vyssie

dt_pol_w_claims %>% 
  group_by(VEH_brand) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))

#najvacsi priemer aj median ma znacka 021, ale jedno pozorovanie nestaci
#zdaleka ho nemozeme povazovat za relevantne
#na druhom mieste je s poctom merani 99 vozidlo 950, toto moze byt kazivejsie

####################################
ggplot(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                aes(x = Burning_Cost)) +
geom_histogram()


model1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year + VEH_brand,
              family = Gamma())
summary(model1)

#tento model nie je vhodny,p-value je viac ako 5%
#model by som vylepsia odobratim premennej Construct_year, ktora model kazi 
#bez tejto premennej je model statisticky vyznamny
#vylepsienie by mohlo spocivataj v pridani dalsich vhodnych parametrov 
#dalej by sa mohli odstranit outlieri, nakolko ti robia problemy uz pri priemeroch...


