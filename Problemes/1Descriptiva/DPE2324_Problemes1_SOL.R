#############################
##### DPE - Problemes 1 #####
#############################


# Variancia no corregida
varp <- function(x){
    n <- length(x)
    varpbl <- var(x)*(n-1)/n
    varpbl
}

# Desviacio tipica no corregida
sdp <- function(x){
    sdpbl <- sqrt(varp(x))
    sdpbl
}

# Covariancia no corregida
covp <- function(x, y){
    n <- length(x)
    covpbl <- cov(x, y)*(n-1)/n
    covpbl
}

# Coeficient de variacio
coef_varp <- function(x){
    sdp(x)/mean(x)
}

# Coeficient de variacio (amb sd corregida)
coef_var <- function(x){
    sd(x)/mean(x)
}


### Exercici 2
eritrocits <- c(4.2, 5.7, 6.1, 3.8, 4.5, 5.2, 4.6, 4.3)
ferro <- c(33, 48, 53, 44, 41, 39, 42, 36)

## a)
mean(eritrocits); mean(ferro)

## b)
varp(eritrocits); varp(ferro)
sdp(eritrocits); sdp(ferro)

## c)
# Coeficient de variació (amb sd corregida)
coef_varp(eritrocits); coef_varp(ferro)

## d)
covp(eritrocits,ferro)
cor(eritrocits,ferro)


### Exercici 3
x <- c(7, 3, 2, 4, 5, 1, 8, 6, 1, 5, 3, 2, 4, 9, 8, 1, 0, 2, 4, 1,2, 5, 6, 5, 4, 7, 1, 3, 0, 5, 8, 6, 3, 4, 0, 10, 2, 5, 7, 4)

## b)
mean(x)
sd(x)*sqrt((length(x)-1)/length(x))
median(x)


### Exercici 5
## Codi de l'enunciat
stol <- c(271, 428, 381, 366, 411, 193, 178, 178, 427, 180)
mean(stol)
var(stol)

## a)
sort(stol)
median(stol)
table(stol)

## b)
varp(stol); sd(stol); sdp(stol)

## c)
k <- 3.28084
mean(stol)*k
var(stol)*k^2
varp(stol)*k^2


### Exercici 6
## Codi de l'enunciat
temperatura <- c(65.8, 69.4, 69.4, 69.7, 71.5, 72.2, 74.1, 75.4, 75.8, 76.3, 77.2, 77.6, 77.6, 77.9, 78.3, 78.8, 78.9, 81.2, 81.2, 81.7, 82.3, 82.3, 82.4, 84.5, 84.7, 85.2, 85.4, 88.2, 90.2, 92.3)
summary(temperatura)
hist(temperatura, breaks = c(65,69,73,77,81,85,89,93), main = '', xlab = 'Temperatura', ylab = 'Freqüències', xlim = c(65,95))

## a)
# Rang
max(temperatura)-min(temperatura)
# Rang interquantil.lic
summary(temperatura)[5]-summary(temperatura)[2]

## b)
boxplot(temperatura)


### Exercici 8
## Codi de l'enunciat
Demanda <- c(200, 220, 400, 330, 210, 390, 280, 140, 280, 290, 180, 200, 350, 310, 230, 240)
Vendes <- c(9, 6, 12, 7, 5, 10, 8, 4, 7, 10, 6, 6, 10, 9, 6, 8)
previsions <- data.frame(Demanda, Vendes)

attach(previsions)
head(previsions)
dim(previsions)
mean(Demanda); mean(Vendes)
var(Demanda); var(Vendes)
cov(Demanda, Vendes)
reg <- lm(Vendes~Demanda)
summary(reg)

## c)
cor(Demanda, Vendes)    # Correlació lineal moderada positiva

## d)
reg
plot (Demanda, Vendes, xlab = "Demanda", ylab = "Vendes", pch =19)
abline (reg)

## e)
new.x <- data.frame(Demanda = c(150, 210, 380))
predict(reg, newdata = new.x)

## f)
# Valors ajustats
v_ajustats <- predict(reg)
v_ajustats[1:3]
# Residus
residus <- Vendes-v_ajustats
residus[1:3]


### Exercici 9
## Codi de l'enunciat
carbohidrats <- c(4.9, 5.7, 6.0, 5.3, 5.2, 6.5, 4.8, 5.2, 4.7, 5.9)
calories <- c(141, 155, 158, 149, 148, 163, 150, 160, 135, 148)

mean(carbohidrats); mean(calories)
var(carbohidrats); var(calories)
cov(carbohidrats, calories)
reg <- lm(calories~carbohidrats)
summary(reg)

## b)
cor(carbohidrats, calories)    # Correlació lineal moderada positiva

## c)
reg

## d)
new.x <- data.frame(carbohidrats = 5)
predict(reg, newdata = new.x)


### Exercici 10
padres <- c(165, 160, 170, 162, 173, 157, 178, 168, 173, 170)
fills <- c(173, 168, 173, 167, 175, 168, 173, 170, 174, 170)

## a)
mean(padres); mean(fills)
varp(padres); varp(fills)
sdp(padres); sdp(fills)

## b)
# Covariancia
cov(padres, fills)
# Coeficient de correlacio
cor(padres, fills)
# Coeficient de determinacio
cor(padres, fills)^2

## c)
reg <- lm(fills~padres)
reg
plot (padres, fills, xlab = "Padres", ylab = "Fills", pch =19)
abline (reg)

## d)
new.p <- data.frame(padres = 166)
predict(reg, newdata = new.p)


### Exercici 11
x <- c(1, 2, 6, 3, 5, 10)
y <- c(3.8, 8.2, 19.1, 9.5, 15, 31.1)

## a)
reg <- lm(y~x)
reg

## b)
# Covariancia
cov(x, y)
# Coeficient de correlacio
cor(x, y)
# Coeficient de determinacio
cor(x, y)^2

## c)
# Valors ajustats
y_ajustats <- predict(reg)
y_ajustats
# Residus
residus <- y-y_ajustats
residus

