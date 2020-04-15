
# zad 1 -------------------------------------------------------------------
load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))

m <- 200
lambda_est <- mean(Centrala$Liczba)

probs <- ppois(sort(unique(Centrala$Liczba)) ,lambda = lambda_est)

epois(Centrala$Liczba,ci.method = "pearson.hartley.approx", conf.level = 0.95, ci=TRUE)$interval$limits 

epois(Centrala$Liczba, ci.method ="normal.approx", conf.level = 0.95, ci=TRUE)$interval$limits 

epois(Centrala$Liczba, ci.method ="exact", conf.level = 0.95, ci=TRUE)$interval$limits 

# zad 2 -------------------------------------------------------------------
awarie <- read.table(url("http://ls.home.amu.edu.pl/data_sets/awarie.txt"))

attach(awarie)
a_est <- mean(V1) 


#nwm co dalej
lambda_limits <- eexp(V1,ci=T,conf.level = 0.95)$interval$limits
rev(1 / lambda_limits)
rev(1 / lambda_limits^2)


# zad 3 -------------------------------------------------------------------


# zad 4 -------------------------------------------------------------------


