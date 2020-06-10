
# przyk³ad ----------------------------------------------------------------

head(USArrests)

dim(USArrests)

#metoda hierarchiczna

(skupienia_1 <- hclust(dist(USArrests)))

#dendrogram
plot(skupienia_1, hang = -1)

#automatyczny podzia³ na skupienia i nanoszenie ich na dendrogram
plot(skupienia_1, hang = -1)
(podzial_1 <- rect.hclust(skupienia_1, k = 3))


(podzial_2 <- cutree(skupienia_1, k = 3))

#zmiana skali ma wp³yw na analizê skupieñ
par(mfrow = c(1, 2))
plot(hclust(dist(USArrests)), hang = -1)
plot(hclust(dist(scale(USArrests))), hang = -1)

par(mfrow = c(1, 1))

#parametry metody hierarchicznej

# inna miara niepodobieñstwa
(skupienia_2 <- hclust(dist(USArrests, method = 'manhattan')))

# inna miara niepodobieñstwa i inna metoda wi¹zania skupieñ
(skupienia_3 <- hclust(dist(USArrests, method = 'manhattan'), 'ward.D'))



# porównianie dendrogramów
par(mfrow = c(1, 3))
plot(skupienia_1, hang = -1)
rect.hclust(skupienia_1, k = 3)
plot(skupienia_2, hang = -1)
rect.hclust(skupienia_2, k = 3)
plot(skupienia_3, hang = -1)
rect.hclust(skupienia_3, k = 3)

par(mfrow = c(1, 1))

#metoda K-œrednich

set.seed(1234)
(skupienia_4 <- kmeans(USArrests, centers = 3, nstart = 1000))



# wykres danych w uk³adzie Murder-Assault z podzia³em na 
# otrzymane skupienia i centrami skupieñ
plot(USArrests[, 1:2], pch = skupienia_4$cluster, 
     col = skupienia_4$cluster, lwd = 2)
points(skupienia_4$centers, pch = 18, cex = 4)
text(USArrests[, 1:2] + 0.5, substring(row.names(USArrests), 1, 4), 
     col = skupienia_4$cluster)


#metoda  K-œrednich z wyborem optymalnej liczby skupieñ poprzez indeks Caliñskiego-Harabasza

library(vegan)
set.seed(1234)
(model <- cascadeKM(USArrests, 2, 5))


# wykres podzia³u na grupy 
# (na osi x obserwacje, na osi y liczba skupieñ, kolory oznaczaj¹ skupienia)
# oraz wykres wartoœci indeksu Caliñskiego-Harabasza dla 
# poszczególnych liczb skupieñ (czerwona kropka oznacza 
# optymaln¹ liczbê skupieñ wed³ug tego kryterium)
plot(model)


# zad 1 -------------------------------------------------------------------

#woj <- read.table("wojewodztwa.txt", header = TRUE)

#head(woj)

#dim(woj)


#1
var(wojewodztwa)

USArrests_scale <- scale(wojewodztwa)
var(USArrests_scale)
USArrests_scale


#2
(skupienia_1 <- hclust(dist(USArrests_scale, method = "euclidean"),method="average"))


(skupienia_2 <- hclust(dist(USArrests_scale, method = 'manhattan'),method="average"))

(skupienia_3 <- hclust(dist(USArrests_scale, method = 'minkowski'),method="average"))

par(mfrow = c(1, 3))
plot(skupienia_1, hang = -1)
rect.hclust(skupienia_1, k = 4)
plot(skupienia_2, hang = -1)
rect.hclust(skupienia_2, k = 4)
plot(skupienia_3, hang = -1)
rect.hclust(skupienia_3, k = 4)

#3
(skupienia_4 <- hclust(dist(USArrests_scale, method = "euclidean"),method="single"))


(skupienia_5 <- hclust(dist(USArrests_scale, method = 'euclidean'),method="complete"))

(skupienia_6 <- hclust(dist(USArrests_scale, method = 'euclidean'),method="average"))
(skupienia_7 <- hclust(dist(USArrests_scale, method = 'euclidean'),method="ward.D"))
par(mfrow = c(1, 4))
plot(skupienia_4, hang = -1)

plot(skupienia_5, hang = -1)

plot(skupienia_6, hang = -1)
plot(skupienia_7, hang = -1)

#4


library(vegan)
set.seed(1234)
(model <- cascadeKM(USArrests_scale, 2, 5))
model$results
plot(model)

#5


# zad 2 -------------------------------------------------------------------

w <- USArrests

