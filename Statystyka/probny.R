#http://ls.home.amu.edu.pl/DSTALIO2020ls.html

#http://ls.home.amu.edu.pl/data_sets/computers.csv

#http://ls.home.amu.edu.pl/data_sets/spotify.csv


#http://ls.home.amu.edu.pl/data_sets/weight-height.csv



spotify <- read.csv(url("http://ls.home.amu.edu.pl/data_sets/spotify.csv"))
attach(spotify)

model1 <- lm(valence ~ ., data = spotify)
dd <- step(model1, direction = "forward", scope = formula(valence ~ .))

comp <- read.csv(url("http://ls.home.amu.edu.pl/data_sets/computers.csv"))

data.frame(cbind(liczebnosc = table(comp$ram),
                 procent = prop.table(table(comp$ram[comp$screen==14]))))


var(spotify)

USArrests_scale <- scale(spotify[1:9])
pca <- prcomp(USArrests_scale)
pca
summary(pca)


ww <- read.csv(url("http://ls.home.amu.edu.pl/data_sets/weight-height.csv"))

shapiro.test(ww$Height)


