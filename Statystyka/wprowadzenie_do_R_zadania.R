#zad 2

x <- c(rep (TRUE, times=3),rep(FALSE, times=4),rep(TRUE,times=2),rep(FALSE, times=5))

num_x = as.numeric(x)


#zad 3

pali <- c(c(1:20),rep(0,times=10),seq(2,40,by=2),seq(40,2,by=-2),rep(0,times=10),c(20:1))

#zad 4

vek <- letters[seq(5,25,by=5)]

#zad 5

tysiac=c(1:1000)

tysiac[seq(2,1000,by=2)] <- tysiac[seq(2,1000,by=2)]^-1
#tysiac[seq(2,1000,by=2)] <- 1/tysiac[seq(2,1000,by=2)]

#zad 6

ord=c(6,3,4,5,2,3)
#sort(ord)

test <- ord[order(ord,decreasing = TRUE)]

#zad 7

w=c( -1.876 , -1.123 , -0.123, 0, 0.123, 1.123, 1.876)

sig=sign(w)

zao=round(w,digits = 2)

cal=floor(w)

#zad 8

pie <- c(1:100000000)
Sys.time()
pierw <- sqrt(c(1:100000000))
Sys.time()


Sys.time()
pierw2 <- pie^(1/2)
Sys.time()

#zad 9

