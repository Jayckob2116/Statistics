#1

#moja_lista <- list(c("Jakub","Ratajczak"),3.14,sqrt,seq(0.02,1,by=0.02))
moja_lista <- list(c("Jakub","Ratajczak"),3.14,sqrt,seq(0.1,1,by=0.1))

moja_lista[c(1,3)] <- NULL

lapply(moja_lista,gamma)


#2

matr <- matrix(data=c(1,2,1,5,0,2,3,5,1),nrow=3, ncol=3,)


rankMatrix(matr)

det(matr)

eigen(matr)


rowSums(matr)

rowMeans(matr)

colSums(matr)

colMeans(matr)


matr %*% solve(matr)


#3

kw <- seq(1,100)^2

resztamod <- kw %% 10

(jednosci <- factor(resztamod))

table(jednosci)

#table(factor(seq(1,100)^2%%10))

#4




outer(1:5,1:5,FUN = function(x,y) paste(x,"*",y,"=",x*y))


#5
dane <- read.csv2("dane1.csv") 
dane1=read.csv("dane1.csv",sep=";")

dane1[((1:nrow(dane1)) %% 2) == 0, ]

dane1[dane1$Wiek>50 & dane1$Wezly.chlonne == 1,]


#6
