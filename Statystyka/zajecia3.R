#zajecia 3  25-03-20

# zad1 -------------------------------------------------------------------- to do


x=1:5
a<-function(x,z=1){for(y in x) z=y*z 
                  print(z)}

a(1:5)
#dokoñcz potem
#b<-function(x,z=1){}



# zad2 -------------------------------------------------------------------- to do



wynik<-function(r){count<-0
  for(x in r)
  {
    n<-x
    s<-x
    while(n<=100){
      
    
      if(1000000<choose(s,n))count <- count + 1
   
      n <- n + 1}
  }
print(count)
}

wynik(1:100)




# zad3 -------------------------------------------------------------------- done

palindrom <- function(x) {all(x == rev(x))}
palindrom(c(1,2,3,3,2,1))



# zad 4 ------------------------------------------------------------------- done
kat<-function(...){

  return(unlist(lapply(...,deg2rad)))
}
k<-kat(c(0,30,45,60,90))

tryg <- data.frame(sin = unlist(lapply(k, sin)),
                  cos = unlist(lapply(k, cos)),
                  tg = unlist(lapply(k, tan)),
                  ctg = c(1,1,1,1,1)/unlist(lapply(k,tan)))
                  
                  

# zad 5 ------------------------------------------------------------------- to do
















# materia³y Jeremiasza ----------------------------------------------------

# zad 1 -------------------------------------------------------------------
#Oblicz iloczyn elementow dowolnego wektora x za
#pomoca petli while, repeat i for (kazdej z osobna).
x <- 1:5
res_for <- 1

for (i in x) res_for <- res_for * i
res_for

res_while <- 1
j <- 1
while (j <= length(x)){
  res_while <- res_while * j
  j <- j + 1 
}
res_while

res_rep <- 1
k <- 1
repeat{
  res_rep <- res_rep * k
  if (k==length(x)) break
  k <- k + 1
}
res_rep

# zad 2 -------------------------------------------------------------------
#Ile liczb postaci (n,r) jest wiekszych od miliona dla 1???r???n???100?

counter <- 0
for (r in c(1:100)){
  for (n in c(1:100))
    if (choose(n,r) > 1000000)
      counter <- counter + 1
}
counter

# zad 3 -------------------------------------------------------------------
#czy wektor jest palindromem

palindrom <- function(v){
  return (all(v == rev(v)))
}

palindrom(c(1,2,3,2,1))
palindrom(c(1, 2, 3, 3, 2, 2))

# zad 4 -------------------------------------------------------------------
# stopnie na radiany dla 0,30,45,60,90
# dataframe dla sin,cos,tg,ctg tych wartosci

radians <- function(deg){
  return((deg*pi)/(180))
}
degs <- c(0,30,45,60,90)
sinus <- c(sin(radians(degs)))
cosinus <- c(cos(radians(degs)))
tangens <- c(tan(radians(degs)))
cotangens <- c(1/tan(radians(degs)))
df <- data.frame(sinus, cosinus,tangens,cotangens)
df

# zad 5 -------------------------------------------------------------------
# funkcja ktora zwraca 3 najmniejsze i 3 najwieksze liczby z wektora

wektor <- function(v){
  if (length(v) < 3)
    return ("za krotki argument")
  y <- v
  res_min <- character(0)
  res_max <- character(0)
  for (i in c(1,2,3)){
    ind_min <- which.min(y)
    res_min <- c(res_min,y[ind_min])
    y <- setdiff(y,y[ind_min])
    
    ind_max <- which.max(v)
    res_max <- c(res_max,v[ind_max])
    v <- setdiff(v,v[ind_max])
  }
  return (c(res_min,rev(res_max)))
}

wektor(c(7,1,2,3,4))
