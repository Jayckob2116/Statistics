
# zad 1 -------------------------------------------------------------------
#wczytanie danych

rok <- seq(1995,2002,by=1)
  
liczba_przypadkow <- c(39.7,38.2,34.7,33.1,30.1,28.4,26.3,24.7)


dane <- data.frame(rok = rok, liczba_przypadkow = liczba_przypadkow)
#1Przedstaw dane na wykresie rozrzutu.
#Czy model regresji liniowej wydaje się adekwatny? 

#Wykres rozrzutu
plot(dane, main = "Wykres rozrzutu", pch = 16)


#2 Dopasuj model regresji liniowej do tych danych. Jakie są wartości
# estymatorów współczynników regresji i przedziały ufności?
# Narysuj uzyskaną prostą regresji na schemacie punktowym.



#Model regresji liniowej(dopasowanie)
model <- lm(liczba_przypadkow ~ rok, data = dane)

#wynik
model$coefficients

#dodanie lini( prostej regresji) na schemacie punktowym

plot(dane, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)


# estymacja parametrów

coef(model)


confint(model)
#3 Które współczynniki są istotne statystycznie w skonstruowanym modelu?
# Jakie jest dopasowanie modelu?

summary(model)

#4 Oblicz wartości dopasowane przez model, a także reszty.

#dopasowanie przez model

fitted(model)

model$fitted.values	

#reszty

residuals(model)

model$residuals

#sprawdzenie(sprawdzam otrzymane reszty)

liczba_przypadkow - fitted(model)


#5 Na wykresie rozrzutu przedstaw granice przedziału prognozy 95%

temp = data.frame(rok = seq(1995,2002), length=100)
pred <-  predict(model,temp,interval = "prediction")

plot(dane, main= "wykres rozrzutu", pch=16,xlim = c(1995, 2002), ylim = c(10, 40))
abline(model,col="red", lwd =2)
lines(temp$rok,pred[,2], lty = 2, col = "red")
lines(temp$rok,pred[,3], lty = 2, col = "red")




#6 Dokonaj predykcji liczby przypadków gruźlicy układu oddechowego w latach 2003-2007.
# Zilustruj wyniki na wykresie rozrzutu.

#predykcja dla lat 2003-2007
nowe_lata <- data.frame(rok = 2003:2007)
(pred_2003_2007 <- predict(model, nowe_lata, interval = 'prediction'))
plot(dane, main = "Wykres rozrzutu z predykcją na lata 2003-2007", pch = 16, 
     xlim = c(1995, 2007), ylim = c(10, 40))
abline(model, col = "red", lwd = 2)
points(2003:2007, pred_2003_2007[, 1], col = "blue", pch = 16)
temp_rok <- data.frame(rok = seq(1994, 2008, length = 100))
pred <- predict(model, temp_rok, interval = "prediction")
lines(temp_rok$rok, pred[, 2], lty = 2, col = "red")
lines(temp_rok$rok, pred[, 3], lty = 2, col = "red")



#7




# zad 2 -------------------------------------------------------------------
#wczytanie danych
load(url("http://ls.home.amu.edu.pl/data_sets/braking.RData"))
attach(braking)

#1 Wykres rozrzutu

plot(braking, main="wykres rozrzutu", pch = 16)

#2 Model dla pełnych danych

model2 <- lm(distance ~ speed,data = braking)

plot(braking, main="wykres rozrzutu", pch = 16)
abline(model2, col="red")

model2$coefficients

coef(model2)


confint(model2)
#3
summary(model2)

#4

#dopasowanie przez model

fitted(model2)

model2$fitted.values	

#reszty

residuals(model2)

model2$residuals

#5


#6



#7


