### DATA INLEZEN EN MANIPULEREN

# dataset inlezen
supermarkt = read.table("supermarkt.txt", header = TRUE, sep = ";", na.strings = "-", dec = ',')

# labels aanpassen
supermarkt$store[supermarkt$store=="1"] = "Brugge"
supermarkt$store[supermarkt$store=="2"] = "Gent"

supermarkt$sex[supermarkt$sex=="1"] = "Man"
supermarkt$sex[supermarkt$sex=="2"] = "Vrouw"

# totale tijd besteed in de supermarkt (in minuten)
supermarkt$t_tot = (supermarkt$t_out - supermarkt$t_in)*24*60

# categorische veranderlijke: bezoek in de week of in het weekend
supermarkt$t_week = factor(supermarkt$day, levels=c(1,2,3), labels=c("week","weekend","weekend"), ordered=TRUE)

#TIM
# labels aanpassen
supermarkt$day[supermarkt$day=="1"] = "Donderdag"
supermarkt$day[supermarkt$day=="2"] = "Vrijdag"
supermarkt$day[supermarkt$day=="3"] = "Zaterdag"

# categorische veranderlijke: opleiding
supermarkt$education_cat = cut(supermarkt$education, c(0,3,6,Inf), labels=c("laag", "midden", "hoog"), ordered_result = TRUE, include.lowest = TRUE)

# Verwijder alle <var>_out
supermarkt$t_out = NULL
supermarkt$line_out = NULL
supermarkt$registers_out = NULL
supermarkt$prod_out = NULL
supermarkt$spending_out = NULL
supermarkt$satisfaction_out = NULL




### BESCHRIJVENDE STATISTIEK

attach(supermarkt)

# store -> kwalitatief nominaal
# day -> kwalitatief nominaal
# age -> kwantitatief discreet
# sex -> kwalitatief nominaal
# education -> kwantitatief discreet
# prod_exp -> kwantitatief discreet
# t_exp -> kwantitatief discreet
# t_in -> kwantitatief continu
# line_in -> kwantitatief discreet
# registers_in -> kwantitatief discreet
# t_tot -> kwantitatief discreet
# t_week -> kwalitatief nominaal
# education_cat -> kwalitatief ordinaal



## kwalitatieve variabelen
table(sex)
table(sex)/length(sex)
barplot(table(sex))
barplot(table(store))
table(store)
barplot(table(day))
table(day)/length(day)
barplot(table(t_week))
table(t_week)
table(t_week)/length(t_week)
barplot(table(education_cat))
table(education_cat)
table(education_cat)/length(education_cat)

## kwantitatieve variabelen

summary(age)
boxplot(age)
x = age
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE)
qqnorm(age)
qqline(age, col='red') 
shapiro.test(age)
qqnorm(log(age))
shapiro.test(log(age))
# rechtsscheef - normaal verdeeld(?)/lognormaal verdeeld(?)

summary(prod_exp)
boxplot(prod_exp)
x = prod_exp
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(prod_exp)
qqline(prod_exp, col='red') 
qqnorm(log(prod_exp))
shapiro.test(prod_exp)
shapiro.test(log(prod_exp))
# rechtsscheef - veel outliers rechts - exponentieel verdeeld?

summary(line_in)
boxplot(line_in)
x = line_in
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(line_in)
qqline(line_in, col='red') 
shapiro.test(line_in)
# rechtsscheef - 1 outlier - normaal verdeeld
sd(line_in, na.rm=TRUE)/sqrt(200) # = STANDAARDFOUT: standaarddeviatie / sqrt(n) , na.rm = TRUE -> verwijdert 'NA' uit berekening

summary(registers_in)
boxplot(registers_in)
x = registers_in
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(registers_in)
qqline(registers_in, col='red') 
shapiro.test(registers_in)
# symmetrisch

summary(t_tot)
boxplot(t_tot)
x = t_tot
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(t_tot)
qqline(t_tot, col='red') 
shapiro.test(t_tot)
qqnorm(log(t_tot))
shapiro.test(log(t_tot))
# rechtsscheef - veel outliers rechts - lognormaal verdeeld

summary(education)
boxplot(education)
x = education
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(education)
qqline(education, col='red') 
shapiro.test(education)
sd(education, na.rm=TRUE)/sqrt(200)


summary(t_in)
boxplot(t_in)
x = t_in
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(t_in)
qqline(t_in, col='red') 
shapiro.test(t_in)
# bimodaal - symmetrisch

summary(t_exp)
boxplot(t_exp)
x = t_exp
hist(x, freq=FALSE)          
curve(dnorm(x, mean(x), sd(x)), col='red', add=TRUE) 
qqnorm(t_exp)
qqline(t_exp, col='red') 
shapiro.test(t_exp)
qqnorm(log(t_exp))
shapiro.test(log(t_exp))
# rechtsscheef - lognormaal?

## kwantitatieve vgl

plot(age,t_tot)
plot(prod_exp,t_tot) # lineair verband? positieve associatie
plot(line_in,t_tot)
plot(t_tot,registers_in)

## Kwalitatieve en kwantitatieve vgl

boxplot(t_tot ~ education_cat)
boxplot(t_tot ~ store)
boxplot(t_tot ~ sex)
boxplot(t_tot ~ day)
boxplot(t_tot ~ t_week)


save(supermarkt, file="supermarkt.RData")

