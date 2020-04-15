#
#info na start
# napisać do kazdego zadania wady zalety prezenotwaonyc danych, wnioski i obserwacje
#
# wczytanie danych --------------------------------------------------------

ankieta <- read.table("ankieta.txt",header = TRUE)


# zad 1 -------------------------------------------------------------------
#
# cut da sie definiowac przedzialy, szczegoly w materialach
#
#a
data.frame(cbind(liczebnosc = table(ankieta$wynik),
                 procent = prop.table(table(ankieta$wynik))))

#b


data.frame(cbind(liczebnosc = table(ankieta$wynik[ankieta$szkola=="p"]),
                 procent = prop.table(table(ankieta$wynik[ankieta$szkola=="p"]))))  
#c
barplot(table(ankieta$wynik), xlab="Odpowiedzi",ylab = "Liczebnosc",main = "Rozkład empiryczny zmiennej wynik",col = 1:5)

#d

barplot(prop.table(table(ankieta$wynik)), xlab="Odpowiedzi",ylab = "Liczebnosc",main = "Rozkład empiryczny zmiennej wynik",col = 1:5)
#e
pie(table(ankieta$wynik))
#f

barplot(table(ankieta$wynik,ankieta$plec), xlab="Odpowiedzi",ylab = "Liczebnosc",main = "Rozkład empiryczny zmiennej wynik",col = 1:5,beside=TRUE,legend.text= c("a","b","c","d","e"))

#Miejsce na interpretacje danych

# zad 2 -------------------------------------------------------------------

load("Centrala.RData")

data.frame(cbind(liczebnosc = table(Centrala$Liczba),
                 procent = prop.table(table(Centrala$Liczba))))


barplot(table(Centrala$Liczba), xlab="Odpowiedzi",ylab = "Liczebnosc",main = "Rozkład empiryczny zmiennej wynik",col = 1:6)

barplot(prop.table(table(Centrala$Liczba)), xlab="Odpowiedzi",ylab = "Liczebnosc",main = "Rozkład empiryczny zmiennej wynik",col = 1:6)

pie(table(Centrala$Liczba))

#srednia
mean(Centrala$Liczba)
#mediana
median(Centrala$Liczba)
#sd - odczhylenie standartowe
sd(Centrala$Liczba)
#wspolczinik zmiennosci
sd(Centrala$Liczba)/mean(Centrala$Liczba)*100

#Miejsce na interpretacje danych


# zad 3 -------------------------------------------------------------------

dane <- c(0.9,6.2,2.1,4.1,7.3,1.0,4.6,6.4,3.8,5.0,2.7,9.2,5.9,7.4,3.0,4.9,8.2,5.0,1.2,10.1,12.2,2.8,5.9,8.2,0.5)

data.frame(cbind(liczebnosc=table(cut(dane, breaks = seq(0,14,2))),
                 procent=prop.table(table(cut(dane, breaks = seq(0,14,2))))))

#Histogram z tymi zaznaczeniami na dole
hist(dane,xlab = "Średnia predkość wiatru", main = "rozkład empiryczny średniej szybkości wiatru")
rug(jitter(dane))

hist(dane,xlab = "Średnia predkość wiatru", main = "rozkład empiryczny średniej szybkości wiatru",probability = TRUE, col ="lightblue1" )
lines(density(dane), col="red",lwd=2)

boxplot(dane, ylab = "Średnia predkość wiatru" , main="rozkład empiryczny średniej szybkości wiatru")


#srednia
mean(dane)
#mediana
median(dane)
#sd - odczhylenie standartowe
sd(dane)
#wspolczinik zmiennosci
sd(dane)/mean(dane)*100


#pakiet potrzebny
#install.packages("e1071")
#library(e1071)

#współczynik asymetri
skewness(dane)

# kurtoza średniej szybkości wiatru
kurtosis(dane)


#Miejsce na interpretacje danych


# zad 4 -------------------------------------------------------------------

  wspolczynnik_zmiennosci <-  function(x,na.rm=FALSE){
    if (na.rm){
      
      return(sd(x)/mean(x)*100)
    }
        else{
          if (any(is.na(x)))
      return("NA")
          else
            {return(sd(x)/mean(x)*100)}
      
    }
    
  }


wspolczynnik_zmiennosci(c(1,3))
