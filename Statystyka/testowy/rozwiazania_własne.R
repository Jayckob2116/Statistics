# zad 3 -------------------------------------------------------------------


spotify <- read.csv("testowy/spotify.csv")


spotify2 <-  spotify[,1:9]
attach(spotify2)


model_1 <- lm(valence ~ ., data = spotify2)

model_1


summary(model_1)


model2 <- step(model_1)

summary(model2)
#odp to 0.68 ( Estimate z model2 na danceability)



# zad 5 -------------------------------------------------------------------
computers <- read.csv("testowy/computers.csv")

#filtrujemy tlko 14 calowce

comp14 <- computers[computers$screen==14,]

prop.table(table(comp14$ram))
#odp C
# 2          4          8         16         24 
# 0.08768096 0.44796504 0.34061732 0.10734772 0.01638896 
# zad 6 -------------------------------------------------------------------


spotify <- read.csv("testowy/spotify.csv")
spotify2 <-  spotify[,1:9]
attach(spotify2)

var(spotify2)

spoti_scale <- scale(spotify2)

pca <- prcomp(spoti_scale)
pca
summary(pca)

#odp 0.2976 + 0.1668 + 0.1182 = 0,5826
#odp to 58.26


# zad 7 -------------------------------------------------------------------

war.test = function(x, war0, alt = c('two.sided', 'less', 'greater')){
  statistic = (length(x)-1)*var(x)/war0
  d = length(x) - 1
  alt = match.arg(alt)
  p.value = pchisq(statistic, d)
  p.value = switch (alt, 'two.sided' = 2*min(p.value, 1-p.value), 'greater' = 1 - p.value, 'less' = p.value)
  names(statistic) = 'T'
  names(d) = 'num df'
  wynik = list(statistic = statistic, parameter = d, p.value = p.value, alternative = alt, method = 'Test istotno?ci dla wariacji w modelu normalnym', data.name = deparse(substitute(x))) #paste(deparse(substitute(x))))
  class(wynik) = 'htest'
  return(wynik)
}

# zad 9 -------------------------------------------------------------------

weight <- read.csv("testowy/weight-height.csv")


height_m <-  weight[weight$Gender=="Male","Height"]

height_w <-  weight[weight$Gender=="Female","Height"]

shapiro.test(height_m)

shapiro.test(height_w)


t.test(height_m,height_w, alternative = "greater", paired = FALSE)

#odp t=95.603, czli 95,60
