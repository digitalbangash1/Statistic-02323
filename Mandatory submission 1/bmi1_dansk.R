
###########################################################################
## Sæt working directory

## I RStudio kan man nemt sætte working directory med menuen 
## "Session -> Set Working Directory -> To Source File Location" 
## Bemærk: i R bruges kun "/" til separering i stier 
## (altså ingen backslash).
setwd("Erstat her med stien til den mappe, hvor projektfilerne er gemt.")


###########################################################################
## Indlæs data

## Indlæs data fra bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)



###########################################################################
## Simpel opsummering af data

## Dimensionen af D (antallet af rækker og søjler)
dim(D)
## Søjle-/variabelnavne
names(D)
## De første rækker/observationer
head(D)
## De sidste rækker/observationer
tail(D)
## Udvalgte opsummerende størrelser
summary(D)
## En anden type opsummering af datasættet
str(D)


###########################################################################
## Beregn BMI

## Beregn BMI og tilføj som ny variabel i D
D$bmi <- D$weight/(D$height/100)^2



###########################################################################
## Histogram (empirisk tæthed)

## Histogram der beskriver den empiriske tæthed for BMI
## (histogram for BMI normaliseret så arealet er lig 1)
hist(D$bmi, xlab="BMI", prob=TRUE)

############ gennemsnit/middelv�rdien for bmi
mean(D$bmi)

############ median of bmi
median(D$bmi)

###### spredning 
sd(D$bmi)



###########################################################################
## Deldatasæt vha. 'subset'

## Opdel i to deldatasæt (hhv. kvinder og mænd)
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)


###########################################################################
## Density histogrammer for kvinder hhv. mænd

## Density histogrammer der beskriver den empiriske
## tæthed for BMI for hhv. kvinder og mænd
hist(Dfemale$bmi, xlab="BMI (kvinder)", prob=TRUE)
hist(Dmale$bmi, xlab="BMI (mænd)", prob=TRUE)

## median female
median(Dfemale$bmi)

##mean female 
mean(Dfemale$bmi)

## median male
median(Dmale$bmi)

## mean male
mean(Dmale$bmi)



###########################################################################
## Boxplot opdelt efter køn

## Boxplot af BMI opdelt efter køn
boxplot(Dfemale$bmi, Dmale$bmi, names=c("Kvinder", "Mænd"), 
        xlab="Køn", ylab="BMI")

## Kvartiler for male og female.
summary(Dfemale$bmi)
summary(Dmale$bmi)


###########################################################################
## e))

## Opsummerende størrelser for BMI

## Antal observationer i alt
## (medregner ej eventuelle manglende værdier)
sum(!is.na(D$bmi))
## Stikprøvegennemsnit (ej kønsopdelt)
mean(D$bmi, na.rm=TRUE)
## Stikprøvevarians (ej kønsopdelt)
var(D$bmi, na.rm=TRUE)
## standard afvigelse
sd(D$bmi)
## kvartiler
summary(D$bmi)
## osv. 
####for kvinder
sum(!is.na(Dfemale$bmi))

mean(Dfemale$bmi, na.rm=TRUE)
##variance
var(Dfemale$bmi, na.rm=TRUE)
##Stikpr�ve-standard-afvigelse
sd(Dfemale$bmi, na.rm=TRUE)
## kvartiler
summary(Dfemale$bmi)

##for m�nd
sum(!is.na(Dmale$bmi))
##Stikpr�ve-gennemsnit
mean(Dmale$bmi, na.rm=TRUE)
##variance
var(Dmale$bmi, na.rm=TRUE)
##Stikpr�ve-standard-afvigelse
sd(Dmale$bmi, na.rm=TRUE)
## kvartiler
summary(Dmale$bmi)
## Argumentet 'na.rm=TRUE' sørger for at størrelsen
## udregnes selvom der eventuelt er manglende værdier


###########################################################################
## f))
## qq-plot til modelkontrol

## Ny variabel 'logbmi' med log-transformeret BMI
D$logbmi <- log(D$bmi)
## qq-plot for log-transformeret BMI
qqnorm(D$logbmi)
qqline(D$logbmi)
mean(D$logbmi)
sd(D$logbmi)

############################################################################
##g))
## standard afvigelse for logBMI
sd(D$logbmi)
## T kvanlier for n=145
qt(0.975,144)
##middel v�rdien for logBmi
mean(D$logbmi)
## median 
KI <- t.test(D$logbmi, conf.level=0.95)$conf.int
KI
exp(KI)




############################################################################

###########################################################################
##h))
## middelv�rdien for logBMI
mean(D$logbmi)
##standard afvigelse for logBMI
sd(D$logbmi)
## p-v�rdi
2*pt(-abs(0.096980),df=145-1)
## T-test for en enkelt stikprøve


## T-test for en enkelt stikprøve foretaget på log-transformeret BMI
t.test(D$logbmi, mu=log(25))
###########################################################################
##i)

## for m�nd

Dlogmale <- log(Dmale$bmi)
mean(Dlogmale)
sd(Dlogmale)

## for kvinder
Dlogfemale <- log(Dfemale$bmi)
mean(Dlogfemale)
sd(Dlogfemale)

## qq plots for kvinder
qqnorm(Dlogfemale)
qqline(Dlogfemale)
## qq plots for m�nd
qqnorm(Dlogmale)
qqline(Dlogmale)


###########################################################################
## Konfidensinterval (KI) for middelværdi og median
## middelv�rdien for kvinder
mean(Dlogfemale)
## t-kvantiler
qt(0.975,71)
## standard afvigelse 
sd(Dlogfemale)
## Udtag data kun for kvinder
Dfemale <- subset(D, gender == 0)
## KI for middelværdien af log-BMI for kvinder
KIkvinder <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
KIkvinder
## Transformer tilbage for at få KI for median BMI for kvinder
exp(KIkvinder)

## For m�nd
mean(Dlogmale)
qt(0.975,72)
sd(Dlogmale)
Dmale <- subset(D,gender ==1)
## KI for middelvaerdien af log-BMI for maend
KImaend <- t.test(Dmale$logbmi, conf.level=0.95)$conf.int
KImaend
##Transformer tilbage for at få KI for median BMI for kvinder
exp(KImaend)

##########################################################################
##  K)
2*pt(-abs(3.78),df=133.75-1)


###########################################################################
## Welch t-test for sammenligning af to stikprøver

## Sammenligning af logBMI for kvinder og mænd
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])


###########################################################################
## m)

## Beregning af korrelation

## Beregning af korrelation mellem udvalgte variable
cor(D[,c("weight","bmi")], use="pairwise.complete.obs")
cor(D[,c("fastfood","bmi")], use="pairwise.complete.obs")
cor(D[,c("weight","fastfood")], use="pairwise.complete.obs")

#the scatterplot for weight/bmi
plot(D$weight, D$bmi, main = "Scatterplot weight/bmi" , xlab = "weight" , ylab = "bmi" , col = "red")


#the scatterplot for bmi/fastfood

plot(D$fastfood, D$bmi, main = "Scatterplot fastfood/bmi" , xlab = "fastfood" , ylab = "bmi" , col = "red")

#the scatterplot for fastfood/weight.

plot(D$weight, D$fastfood,  main = "Scatterplot weight/food" , xlab = "weight" , ylab = "fastfood" , col = "purple")
###############################################################################

  
###########################################################################
## Delmængder i R

## Ekstra bemærkning om måder at udtage delmænger i R
##
## En logisk (logical) vektor med sandt (TRUE) eller falsk (FALSE) for 
## hver værdi i en kolonne i D - f.eks: Find alle kvinder i datasættet
D$gender == 0
## Vektoren kan bruges til at udtage data for kvinderne
D[D$gender == 0, ]
## Alternativt kan man bruge funktionen 'subset'
subset(D, gender == 0)
## Mere komplekse logiske udtryk kan laves, f.eks.: 
## Find alle kvinder under 55 kg
subset(D, gender == 0 & weight < 55)


###########################################################################
## Flere R-tips

## Lav en for-løkke med beregning af et par opsummerende størrelser
## og gem resultatet i en ny data.frame
Tbl <- data.frame()
for(i in 0:1){
  Tbl[i+1, "mean"] <- mean(D$bmi[D$gender == i])
  Tbl[i+1, "var"] <- var(D$bmi[D$gender == i])
}
row.names(Tbl) <- c("Kvinder","Mænd")
## Se hvad der er i Tbl
Tbl

## I R er der endnu mere kortfattede måder sådanne udregninger kan 
## udføres. For eksempel
aggregate(D$bmi, by=list(D$gender), function(x){ 
  c(mean=mean(x), var=var(x)) 
})
## Se flere smarte funktioner med: ?apply, ?aggregate og ?lapply
## og for ekstremt effektiv databehandling se f.eks. pakkerne: dplyr,
## tidyr, reshape2 og ggplot2.

## LaTeX tips:
##
## R-pakken "xtable" kan generere LaTeX tabeller og skrive dem direkte 
## ind i en fil, som derefter kan inkluderes i et .tex dokument.
## 
## R-pakken "knitr" kan anvendes meget elegant til at lave et .tex 
## dokument der inkluderer R-koden direkte i dokumentet. Dette 
## dokument og bogen er lavet med knitr.
