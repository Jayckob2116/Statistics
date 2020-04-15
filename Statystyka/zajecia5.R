
# zad 1 -------------------------------------------------------------------

#1
print(eunif)

#2
load(url("http://ls.home.amu.edu.pl/data_sets/czas_oczek_tramwaj.RData"))
# estmatory
(a_est <- min(czas_oczek_tramwaj))
## [1] 0.01
(b_est <- max(czas_oczek_tramwaj))
## [1] 13.92
library(EnvStats)
EnvStats::eunif(czas_oczek_tramwaj, method = "mme")


#3

a_mme <- 0.1040974
b_mme <- 13.8551026

hist(czas_oczek_tramwaj, 
     xlab = "Czas oczekiwania na tramwaj", 
     main = "Rozkład empiryczny i teoretyczny czasu oczekiwania na tramwaj",
     probability = TRUE)
lines(density(czas_oczek_tramwaj), col = "red", lwd = 2)
curve(dunif(x, a_mme, b_mme), 
      add = TRUE, col = "blue", lwd = 2)
curve(dunif(x, a_est, b_est), 
      add = TRUE, col = "green", lwd = 2)
legend(x = 5, y = 0.04, legend = c("empiryczny", "teoretyczny ENW","teoretyczny MME"), col = c("red", "blue","green"), lwd = 2)



# zad 2 -------------------------------------------------------------------
load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))
#1


m <- 200
est <- mean(Centrala$Liczba)
#2
#jak do tego doszło nie wiem
probs <- dbinom(sort(unique(Centrala$Liczba)), size = m, prob = p_est)
sum(probs)

#3
#model empiryczny
data.frame(cbind(liczebnosc = table(Centrala$Liczba),
                 procent = prop.table(table(Centrala$Liczba))))

#model teoretyczny
counts <- matrix(c(prop.table(table(Centrala$Liczba)), probs), nrow = 2, byrow = TRUE)
rownames(counts) <- c("empiryczny", "teoretyczny")
colnames(counts) <- sort(unique(Centrala$Liczba))
counts


#4

#5

#6


# zad 3 -------------------------------------------------------------------

#1

#2

#3

#4



# zad 4 -------------------------------------------------------------------

#1

#2

#3

#4

#5

#6

#7
