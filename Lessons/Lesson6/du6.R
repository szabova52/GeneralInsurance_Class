library(dplyr)
library(ggplot2)
dt_pol_w_claims <- readRDS("D:\\General\\GeneralInsurance_Class-Class2019\\Data\\lesson6_dt_pol_w_claims.rds")

ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20))

dt_pol_w_claims <- mutate(dt_pol_w_claims,
                data_status = ifelse(ind == 1, 
                                     "Training",
                                     ifelse(ind == 2, 
                                            "Validation", 
                                            "Unseen")
                )
)
head(dt_pol_w_claims)

train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}
#########################################################################################

#povodny model 
model1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year + BonusMalus,
              family = Gamma())

#model na trenovacaj vzorke
model1a <- glm(data = train %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year + BonusMalus,
              family = Gamma())
summary(model1a)

mse(predict(model1a,val, type = "response"), val$Burning_Cost) 
mse(predict(model1a,train, type = "response"), train$Burning_Cost)

#pridame premennu 
model1b <- glm(data = train %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age + BonusMalus + Construct_year ,
              family = Gamma())
summary(model1b)

mse(predict(model1b,val, type = "response"), val$Burning_Cost) 
mse(predict(model1b,train, type = "response"), train$Burning_Cost)

#model sa pridanim novej premennej zlepsil,aj ked nie o vela (mse je mensie)

#vyskusame odobrat premennu, ktoramanajvyssiu p-hodnotu 

model1c <- glm(data = train %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ D_age + BonusMalus,
              family = Gamma())
summary(model1c)

mse(predict(model1c,val, type = "response"), val$Burning_Cost) 
mse(predict(model1c,train, type = "response"), train$Burning_Cost)

#model sa o malicko zhorsil, toto nie je cesta k vylepseniu modelu 

#zgrupovanie

source("D:\\General\\GeneralInsurance_Class-Class2019\\Lessons\\Lesson6\\Support\\emb_chart.R")
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model1b, train, type = "response"))),
  x_var =  "BonusMalus",
  target = "Burning_Cost",
  prediction =  "pred"
  )

#spojime BonusMalus mensie ako nula a vacsie ako 10

train <- train %>% 
  mutate(BonusMalus = ifelse(BonusMalus <= 0, 0, BonusMalus))
train <- train %>% 
  mutate(BonusMalus = ifelse(BonusMalus >= 10, 10, BonusMalus)) 

emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model1b, train, type = "response"))),
  x_var =  Construct_year"",
  target = "Burning_Cost",
  prediction =  "pred"
  )

#spojime construct year mensie ako 2017 

train <- train %>% 
  mutate(Construct_year = ifelse(Construct_year <= 2007, 2007, Construct_year))

model2 <- glm(data = train %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year + BonusMalus,
              family = Gamma())
summary(model2)

mse(predict(model2,val, type = "response"), val$Burning_Cost) 
mse(predict(model2,train, type = "response"), train$Burning_Cost)

#zgrupovanie nepomohlo, model sa o malo zhorsil, mse sa zvysilo
#najlepsie to vyzeralo po pridani novej premennej
