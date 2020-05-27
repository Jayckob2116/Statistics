
# zad 1 -------------------------------------------------------------------
#ładowanie danych
load(url("http://ls.home.amu.edu.pl/data_sets/liver_data.RData"))
attach(liver_data)
#1
model <- glm(condition ~ bilirubin+ldh,data = liver_data, family = "binomial")
model
#2
summary(model)

#3
step(model)
summary(model)$coefficients
#4
exp(coef(model)[3])
# wzrost o 1 jednostke, sprawia że warotsc zmiennej przewidywnej wzrośnie  17 krotnie 
exp(coef(model)[2])
#wzrost o 2% procent 
#5
library(ROCR)
pred_1 <- prediction(model$fitted,condition)
par(mfrow = c(1,2))
plot(performance(pred_1,'tpr','fpr'), main="model")


#AUC
performance(pred_1,'auc')@y.values

#6#
#predykcja
predict_1 <- predict(model, 
                        data.frame(bilirubin = c(0.9, 2.1,3.4),
                                   ldh=c(100,200,300)), 
                        type = 'response')

predict_1

x_temp <- seq(min(bilirubin) - 10, max(bilirubin) + 10, length.out = 50)
y_temp <- exp(coef(model)[3] + coef(model)[2] * x_temp) / 
  (1 + exp(coef(model)[3] + coef(model)[2] * x_temp))
plot(x_temp, y_temp, type = "l", xlab = "x2", ylab = "y", ylim = c(-0.1, 1.1),xlim =c(-5,5))
points(100,bilirubin, pch = 16)
#points(, predict_1, pch = 16, col = "red")


#kod od Prowadzącego
# liver_data_new <- data.frame(bilirubin = c(0.9, 2.1, 3.4), ldh = c(100, 200, 300))
# (predict_glm <- predict(model_1, 
#                         liver_data_new, 
#                         type = 'response'))
# model_1_hat <- coef(model_1)[1] + 
#   coef(model_1)[2] * liver_data$bilirubin + 
#   coef(model_1)[3] * liver_data$ldh
# model_1_temp <- seq(min(model_1_hat) - 1, max(model_1_hat) + 2.5, length.out = 100)
# condition_temp <- exp(model_1_temp) / (1 + exp(model_1_temp))
# plot(model_1_temp, condition_temp, type = "l", xlab = "X beta", ylab = "condition", 
#      xlim = c(-6, 9), ylim = c(-0.1, 1.1))
# points(model_1_hat, liver_data$condition, pch = 16)
# points(coef(model_1)[1] + 
#          coef(model_1)[2] * liver_data_new$bilirubin + 
#          coef(model_1)[3] * liver_data_new$ldh, 
#        predict_glm, pch = 16, col = "red")
#7

# zad 2 -------------------------------------------------------------------

#1

#2


#3


#4


#5


