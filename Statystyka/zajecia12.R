
# zad 1 -------------------------------------------------------------------

#1 Wykonaj wykres rozrzutu dla badanych cech.

# wykres rozrzutu

plot(mtcars$mpg, mtcars$wt, xlab = "mog", ylab = "wt", pch = 16)

#2 Sprawd� za�o�enia testu istotno�ci dla wsp�czynnika korelacji.

# za�o�enia dla mtcars$mpg
shapiro.test(mtcars$mpg)
shapiro.test(mtcars$mpg)$p.value

qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "red")
# za�o�enia dla mtcars$wt
shapiro.test(mtcars$wt)
shapiro.test(mtcars$wt)$p.value

qqnorm(mtcars$wt)
qqline(mtcars$wt, col = "red")

#3 

# testy method = "pearson"
cor.test(mtcars$mpg, mtcars$wt, method = "pearson")

cor.test(mtcars$mpg, mtcars$wt, method = "pearson")$p.value

cor.test(mtcars$mpg, mtcars$wt, method = "pearson")$estimate

cor.test(mtcars$mpg, mtcars$wt, method = "pearson")$conf.int


#4

# wsp�czynnik�w Kendalla

cor.test(mtcars$mpg, mtcars$wt, method = "kendall")

cor.test(mtcars$mpg, mtcars$wt, method = "kendall")$p.value

cor.test(mtcars$mpg, mtcars$wt, method = "kendall")$estimate



# wsp�czynnik�w Spearmana

cor.test(mtcars$mpg, mtcars$wt, method = "spearman")

cor.test(mtcars$mpg, mtcars$wt, method = "spearman")$p.value

cor.test(mtcars$mpg, mtcars$wt, method = "spearman")$estimate





