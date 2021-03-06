
# przyk�ad ----------------------------------------------------------------
head(USArrests)

dim(USArrests)

# sprawdzamy czy wariancje (,,zmienno�ci'') zmiennych s� bardzo zr�nicowane
var(USArrests)

# tak s�, wi�c centrujemy i skalujemy funkcj� scale
USArrests_scale <- scale(USArrests)
var(USArrests_scale)

pca_t <- prcomp(USArrests, scale = TRUE)
# lub
# pca <- prcomp(USArrests_scale)
pca_t

# bez skalowania
prcomp(USArrests)

summary(pca_t)

# wyniki (ang. scores) - wsp�rz�dne obserwacji w nowym
# uk�adzie wsp�rz�dnych utworzonym przez sk�adowe 
# g��wne (to one najcz�ciej podlegaj� wizualizacji)
head(pca_t$x)


#�adunki (ang. loadings) - wsp�czynniki pokazuj�ce wk�ad 
# poszczeg�lnych zmiennych bazowych w tworzenie sk�adowych 
# g��wnych (im warto�� bezwzgl�dna z �adunku jest wi�ksza, 
# tym zmienna ma wi�kszy wk�ad w budow� sk�adowej g��wnej)

pca_t$rotation

# wykres osypiska (piargowy, ang. scree plot) - 
# wykres wariancji poszczeg�lnych sk�adowych g��wnych

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
# tak s�, wi�c centrujemy i skalujemy funkcj� scale
USArrests_scale2 <- scale(USArrests[1-4])
var(USArrests_scale2)

pca <- prcomp(USArrests[1-4], scale = TRUE)
# lub
# pca <- prcomp(USArrests_scale)
pca

# bez skalowania
prcomp(USArrests[1-4])

#w odpowiedziach jest zeby uwzgledni� skalowanie

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

