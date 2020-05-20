
# zad 1 -------------------------------------------------------------------
#wczytanie danych

Automobile <- read.csv("http://ls.home.amu.edu.pl/data_sets/Automobile.csv", na.strings = c("?"))

#1
Automobile <- na.omit(Automobile, fill = NULL)

attach(Automobile)
#2
pairs(~horsepower+city.mpg+peak.rpm+curb.weight+num.of.doors, data = Automobile)

lm(formula = price ~ horsepower+city.mpg+peak.rpm+curb.weight+num.of.doors, data = Automobile)
model <- lm(formula = price ~ horsepower+city.mpg+peak.rpm+curb.weight+num.of.doors, data = Automobile)


coef(model)


confint(model)

summary(model)


fitted(model)

residuals(model)
#3
#3.1
step(model)

#3.2

step(model, k=log(nrow(Automobile)))

#3.3
model0 <-  lm(price ~ 1,data = Automobile)
step(model0, direction = "forward", scope = formula(model))


step(model0, direction = "forward", scope = formula(model), k=log(nrow(Automobile)))
#4

step(model, direction = "backward")

#a <- summary(lm(formula = price ~ horsepower + city.mpg + curb.weight + num.of.doors, data = Automobile))
model


summary(lm(formula = price ~ horsepower + city.mpg + curb.weight + num.of.doors, data = Automobile))$coefficients
summary(lm(formula = price ~ horsepower + city.mpg + curb.weight + num.of.doors, data = Automobile))$adj.r.squared


summary(lm(formula = price ~ horsepower +  curb.weight + num.of.doors, data = Automobile))$coefficients
summary(lm(formula = price ~ horsepower +  curb.weight + num.of.doors, data = Automobile))$adj.r.squared


summary(lm(formula = price ~ horsepower + curb.weight , data = Automobile))$coefficients
summary(lm(formula = price ~ horsepower + curb.weight, data = Automobile))$adj.r.squared



#5


Automobile1 <- read.csv("http://ls.home.amu.edu.pl/data_sets/Automobile.csv", na.strings = c("?"))

#6
