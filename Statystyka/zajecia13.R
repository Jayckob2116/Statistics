
# przyk³ad ----------------------------------------------------------------
head(USArrests)

dim(USArrests)

# sprawdzamy czy wariancje (,,zmiennoœci'') zmiennych s¹ bardzo zró¿nicowane
var(USArrests)

# tak s¹, wiêc centrujemy i skalujemy funkcj¹ scale
USArrests_scale <- scale(USArrests)
var(USArrests_scale)

pca_t <- prcomp(USArrests, scale = TRUE)
# lub
# pca <- prcomp(USArrests_scale)
pca_t

# bez skalowania
prcomp(USArrests)

summary(pca_t)

# wyniki (ang. scores) - wspó³rzêdne obserwacji w nowym
# uk³adzie wspó³rzêdnych utworzonym przez sk³adowe 
# g³ówne (to one najczêœciej podlegaj¹ wizualizacji)
head(pca_t$x)


#³adunki (ang. loadings) - wspó³czynniki pokazuj¹ce wk³ad 
# poszczególnych zmiennych bazowych w tworzenie sk³adowych 
# g³ównych (im wartoœæ bezwzglêdna z ³adunku jest wiêksza, 
# tym zmienna ma wiêkszy wk³ad w budowê sk³adowej g³ównej)

pca_t$rotation

# wykres osypiska (piargowy, ang. scree plot) - 
# wykres wariancji poszczególnych sk³adowych g³ównych

pca_t$sdev^2


apply(pca_t$x, 2, var)


plot(pca_t)

biplot(pca_t)

library(ape)
plot(mst(dist(USArrests_scale)), x1 = pca_t$x[, 1], x2 = pca_t$x[, 2])



# odczytywanie nazw obserwacji
row.names(USArrests_scale[c(24, 33),])
## [1] "Mississippi"    "North Carolina"





# zad 1 -------------------------------------------------------------------

#1
USArrests[1-4]

var(USArrests[1-4])
summary(USArrests[1-4])
# tak s¹, wiêc centrujemy i skalujemy funkcj¹ scale
USArrests_scale2 <- scale(USArrests[1-4])
var(USArrests_scale2)

pca <- prcomp(USArrests[1-4], scale = TRUE)
# lub
# pca <- prcomp(USArrests_scale)
pca

# bez skalowania
prcomp(USArrests[1-4])

#w odpowiedziach jest zeby uwzgledniæ skalowanie

#2
summary(pca)

#3
head(pca$x)
pca$x


#4

pca$rotation
#to do wykres

#5

plot(pca)

# 1 lub 2

#6

biplot(pca)

#7
plot(mst(dist(USArrests_scale2)), x1 = pca$x[, 1], x2 = pca$x[,2] )
# zad 2 -------------------------------------------------------------------
#to do

