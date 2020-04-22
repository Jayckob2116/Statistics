
# zad 1 -------------------------------------------------------------------
x <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872)

alfa <-  0.05
#1

shapiro.test(x)$p.value
#2
qqnorm(x)
qqline(x)

#3
mean(x)
#4

t.test(x,mu = 870, alternative = "less")$p.value
# zad 2 -------------------------------------------------------------------
a <- c(78.2,78.5,75.6,78.5,78.5,77.4,76.6)

b <- c(76.1,75.2,75.8,77.3,77.3,77.0,74.4,76.2,73.5,77.4)
#1
boxplot(a,b)

#2
shapiro.test(a)$p.value
qqnorm(a)
qqline(a)


#3

shapiro.test(b)$p.value
qqnorm(b)
qqline(b)

#4
var(a)

var(b)

var.test(a,b,alternative = "less")$p.value
#5
mean(a)

mean(b)

t.test(a,b,var.equal = T,alternative = "greater")$p.value


# zad 3 -------------------------------------------------------------------
przed <- c(84, 87, 87, 90, 90, 90, 90, 93, 93, 96)

po <- c(89, 92, 98, 95, 95, 92, 95, 92, 98, 101)

#1

boxplot(przed,po)

#2
shapiro.test(przed)$p.value
qqnorm(przed)
qqline(przed)

shapiro.test(po)$p.value
qqnorm(po)
qqline(po)
#3 srednie
mean(przed)

mean(po)

#4 test t-studenta dla zalenzych prob
t.test(przed,po,alternative = "less",paired = T)$p.value

#warto wiedziec czemu mamy przyjmowac less/greater
#
#

# zad 4 -------------------------------------------------------------------

m <- c(171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177)

k <- c(161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172)

#1

boxplot(m,k)

#2
shapiro.test(m)$p.value
qqnorm(m)
qqline(m)

shapiro.test(k)$p.value
qqnorm(k)
qqline(k)

#3
var(m)

var(k)


var.test(m,k,alternative = "greater")$p.value
# wniosek: różnica wariacji jest istotna( p.value <0.05)
#4
mean(m)

mean(k)


t.test(m,k,var.equal = F,alternative = "greater")$p.value
# zad 5 -------------------------------------------------------------------

w_test <- function(x,lambda_zero,alter="two.sided")
{
  
  
  
  
}


awarie <- read.table(url("http://ls.home.amu.edu.pl/data_sets/awarie.txt"))

#mam tu poszczególne lementy, teraz musze je wpadkować do funkci,
#i tam do htest objextu
#
#ps info w linku( patrz zakładka UAM)
#
#dokończyć to
lam= 0.001
attach(awarie)  

mean(awarie$V1)

n <- length(awarie$V1)

TT <- 2*0.001*n*mean(awarie$V1)

aa=0.05

k <- qchisq(1-aa,2*n)

pv=1-pchisq(TT,2*n)


