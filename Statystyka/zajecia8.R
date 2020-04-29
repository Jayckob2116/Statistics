
library(PBImisc)
head(vaccination)


# zad 1 -------------------------------------------------------------------
ramka <- data.frame(Same = c(25,26,17,15,14,17,14,20,11,21),
                    Different=c(11,21,9,6,7,14,12,4,7,19),
                    Imagery=c(14,15,29,10,12,22,14,20,22,12),
                    Photo=c(25,15,23,21,18,24,14,27,12,11),
                    Placebo=c(8,20,10,7,15,7,1,17,11,4))


ramka2 <- data.frame(slowa=c(25,26,17,15,14,17,14,20,11,21,11,21,9,6,7,14,12,4,7,19,
                             14,15,29,10,12,22,14,20,22,12,25,15,23,21,18,24,14,27,12,11,
                             8,20,10,7,15,7,1,17,11,4),
                     kontekst=c(rep("Same",times=10),rep("Different",times=10),
                                rep("Imagery",times=10),rep("Photo",times=10),
                                rep("Placebo",times=10)))
contex = c("Same","Different","Imagery","Photo","Placebo")

attach(ramka)
#1
sapply(ramka, mean)

lapply(ramka, mean)

summary(ramka2)



boxplot(ramka, xlab="kontekst",ylab="liczba slow")

#to jest oki
aggregate(ramka2$slowa,list(contex=ramka2$kontekst),FUN=mean)

boxplot(slowa ~ kontekst,data=ramka2)

#2
attach(ramka2)

summary(aov(slowa ~ kontekst, data=ramka2))



#3
shapiro.test(lm(slowa ~ kontekst, data=ramka2)$residuals)$p.value


qqplot(lm(slowa ~ kontekst, data=ramka2)$residuals)
qqline(lm(slowa ~ kontekst, data=ramka2)$residuals)


bartlett.test(slowa ~ kontekst, data=ramka2)$p.value

fligner.test(slowa ~ kontekst, data=ramka2)$p.value

library(car)

leveneTest(slowa ~ kontekst, data=ramka2)$Pr

leveneTest(slowa ~ kontekst, data=ramka2, center = "mean")$P
#4
attach(ramka2)

pairwise.t.test(slowa, kontekst, data = ramka2)

model_aov <- aov(slowa ~ kontekst, data = ramka2)
TukeyHSD(model_aov)

plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "konteks", console = TRUE)

SNK.test(model_aov, "konteks", console = TRUE)

LSD.test(model_aov, "konteks", p.adj = "holm", console = TRUE)

scheffe.test(model_aov, "konteks", console = TRUE)
# zad 2 -------------------------------------------------------------------



