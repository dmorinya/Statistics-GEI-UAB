# Comprova el directori actual
getwd()
# Estableix el directori correcte
setwd("/Users/sergigimenezgabarro/Documents/UAB/Primer semestre/Estadística (Eng. inf.)/practica2")

# Carrega les dades del fitxer de text
dades1 <- read.table("DADES PRAC1.txt", header = TRUE, sep = " ")
dades2 <- read.table("DADES PRAC2.txt", header = TRUE, sep = " ")

# Mostra les primeres files per verificar que s'ha carregat correctament
head(dades1)
head(dades2)

# Exercici 1
alcades_dones <- dades1$alt[dades1$sex == 2]

mitjana_alcada <- mean(alcades_dones)
mediana_alcada <- median(alcades_dones)

# Mostrem els resultats
print(paste("Mitjana: ", mitjana_alcada))
print(paste("Mediana: ", mediana_alcada))

# Diagrama de caixa
boxplot(alcades_dones, main="Diagrama de Caixa de l'Alçada de les Dones", ylab="Alçada (cm)")

# Diagrama de barres
barplot(table(alcades_dones), main="Diagrama de Barres de l'Alçada de les Dones", xlab="Alçada (cm)", ylab="Freqüència")

# Diagrama de sectors
percentatges <- round(table(alcades_dones) / length(alcades_dones) * 100, 1)
pie(table(alcades_dones), labels = paste(names(table(alcades_dones)), percentatges, "%"), main="Diagrama de Sectors de l'Alçada de les Dones")


# Exercici 2
pes <- dades1$pes
alçada <- dades1$alt

# Correlació entre pes i alçada
correlacio_pes_alçada <- cor(pes, alçada) # Coeficient de Pearson per defecte

# Mostra resultats
cat("Coeficient de correlació entre pes i alçada: ", correlacio_pes_alçada, "\n")

# Gràfica de dispersió
plot(alçada, pes, main="Gràfica de dispersió entre pes i alçada", xlab="Alçada (cm)", ylab="Pes (kg)")

# No podem utilitzar el coeficient de correlació de Pearson per avaluar la relació entre una variable quantitativa (pes) i una qualitativa (sexe), 
# ja que Pearson mesura correlacions entre variables numèriques. En comptes d'això, podem comparar les mitjanes de pes entre els grups de sexe 
# com una primera aproximació. Encara que no és un mètode inferencial formal, ens pot donar una idea preliminar de la relació.

# Càlcul de les mitjanes de pes per sexe
mitjanes_pes <- tapply(dades1$pes, dades1$sex, mean)
print(mitjanes_pes)  # Si les mitjanes són molt diferents, això pot indicar una possible associació entre pes i sexe.

# Gràfic de caixes per comparar la distribució del pes segons el sexe
boxplot(pes ~ sex, data = dades1, main = "Distribució del pes per sexe", 
        xlab = "Sexe", ylab = "Pes (kg)", col = c("lightblue", "lightpink"))

# Interpretació: Si observem diferències clares en la distribució del pes entre sexes, això suggereix que el pes podria estar influenciat pel sexe. 
# Tot i així, per confirmar-ho amb rigor estadístic, caldria un test d'hipòtesi, com el test t de Student o ANOVA, que compari formalment les mitjanes.


# Exercici 3
# Dades proporcionades
TEMP <- dades2$TEMP
RESIST <- dades2$RESIST

# Ajust de la recta de regressió
model <- lm(RESIST ~ TEMP)

# Mostra resultats
summary(model) # Model resultant: RESIST = 23.6416 * TEMP - 81.6829
# Residual standard error: 113.7

plot(TEMP, RESIST, 
    main = "Resistència vs Temperatura",
    xlab = "Temperatura (°C)", 
    ylab = "Resistència (Ohms)", 
    pch = 19, col = "blue")

# Gràfica amb la recta de regressió
plot(TEMP, RESIST, main="Regressió lineal entre temperatura i resistència", xlab="Temperatura", ylab="Resistència")
abline(model, col="blue")


# Exercici 4

# PRIMER ENFOCAMENT (no recomanable): Canviar el model a una regressió polinòmica

# Model polinòmic de segon grau (quadràtic)
model_polinomic_2 <- lm(RESIST ~ poly(TEMP, 2))
summary(model_polinomic_2)

# Genera una seqüència de valors de TEMP per fer les prediccions (per suavitzar la corba)
TEMP_seq <- seq(min(TEMP), max(TEMP), length.out = 100)

# Prediu els valors de RESIST per a la seqüència de TEMP amb el model de segon grau
predictions_model_2 <- predict(model_polinomic_2, newdata = data.frame(TEMP = TEMP_seq))

# Gràfica de dispersió de les dades originals
plot(TEMP, RESIST, main="Comparació de models polinòmics", xlab="Temperatura", ylab="Resistència")

# Dibuixa la corba del model polinòmic de segon grau
lines(TEMP_seq, predictions_model_2, col="red", lwd=2)  # Línia vermella pel model de segon grau

# Model polinòmic de tercer grau (cúbic)
model_polinomic_3 <- lm(RESIST ~ poly(TEMP, 3))
summary(model_polinomic_3)

# Prediu els valors de RESIST per a la seqüència de TEMP amb el model de tercer grau
predictions_model_3 <- predict(model_polinomic_3, newdata = data.frame(TEMP = TEMP_seq))

# Dibuixa la corba del model polinòmic de tercer grau
lines(TEMP_seq, predictions_model_3, col="green", lwd=2)  # Línia verda pel model de tercer grau

# Afegir llegenda per identificar les corbes
legend("topright", legend=c("Model polinòmic de 2n grau", "Model polinòmic de 3r grau"),
       col=c("red", "green"), lwd=2)

# SEGON ENFOCAMENT (recomanable): Transformar les dades per provar un model de potència

# Farem la següent transformació
# De: RESIST = A*TEMP^n + B
# A: log(RESIST-B) = log(A*TEMP^n) = log(A) + n*log(TEMP)

# Transformació logarítmica de les dades
log_RESIST <- log(dades2$RESIST) # Assumim que B=0, podria ser una suposició incorrecta, però cal fer l'estimació en aquest pas
log_TEMP <- log(dades2$TEMP)

# Ajust de la regressió lineal sobre les dades logarítmiques
model_log <- lm(log_RESIST ~ log_TEMP)

# Resum del model
summary(model_log)

# Gràfica de dispersió de les dades transformades a logaritmes amb la recta de regressió
plot(log_TEMP, log_RESIST, 
     main = "Regressió lineal sobre dades logarítmiques", 
     xlab = "Log(Temperatura)", 
     ylab = "Log(Resistència)")
abline(model_log, col = "blue", lwd = 2)

# INTERPRETACIÓ DEL MODEL LOGARÍTMIC:
# L'equació del model logarítmic és de la forma:
# log(RESIST) = log(A) + n * log(TEMP)
# Això implica que la relació entre TEMP i RESIST és de potència:
# RESIST = A * TEMP^n, on n és el pendent obtingut del model.

# En aquest cas:
# - El coeficient de la intersecció és log(A) = 2.99056, que implica A ≈ e^2.99056 = 19.87
# - El coeficient del pendent n = 1.02944, indica que la resistència augmenta gairebé proporcionalment amb la temperatura (n ≈ 1).
# Per tant, el model ajustat seria RESIST = 19.87 * TEMP ^ 1.02944
#
# El Residual Standard Error (RSE) del model logarítmic és de 0.0488, però aquest valor no és comparable directament amb el RSE del model lineal original
# perquè les dades transformades en logaritmes tenen una escala diferent. Per fer una comparació adequada, hem de revertir la transformació logarítmica 
# (tornar a l'escala original).

# Convertir les prediccions del model logarítmic de tornada a l'escala original
log_predictions <- predict(model_log)  # Prediccions a l'escala logarítmica
predictions_original_scale <- exp(log_predictions)  # Tornem a l'escala original aplicant l'exponencial

# Calcular els residus a l'escala original
residuals_original_scale <- dades2$RESIST - predictions_original_scale

# Calcular el Residual Standard Error (RSE) a l'escala original
RSE_original_scale_log_model <- sqrt(sum(residuals_original_scale^2) / 
                                       (length(dades2$RESIST) - length(coef(model_log))))

# Mostrem el Residual Standard Error a l'escala original
cat("Residual Standard Error del model logarítmic a l'escala original: ", RSE_original_scale_log_model, "\n")

# Exercici 5
# Definim les dades del problema

# Temps en hores
t <- 0:10

# Població observada en nombre de bacteris (amb k=0.7 i P_0=100)
P <- sapply(t, function(x) round(100 * exp(0.7 * x)))

# Logaritme natural de la població
ln_P <- log(P)

# Gràfica del creixement bacterià (Població vs Temps)
plot(t, P, 
     main = "Creixement de la població bacteriana",
     xlab = "Temps (hores)", 
     ylab = "Població (nombre de bacteris)", 
     pch = 19, col = "blue")

# Gràfica de ln(P) vs Temps (relació lineal esperada després de la transformació logarítmica)
plot(t, ln_P, 
     main = "Logaritme de la població vs Temps",
     xlab = "Temps (hores)", 
     ylab = "ln(Població)", 
     pch = 19, col = "green")

# Creació d'un data frame per a la regressió
data <- data.frame(t, ln_P)

# Regressió lineal de ln(P) vs t
model <- lm(ln_P ~ t, data = data)

# Resum del model
summary(model)

# Gràfica de ln(P) vs Temps amb la recta de regressió ajustada
plot(t, ln_P, 
     main = "Logaritme de la població vs Temps amb regressió",
     xlab = "Temps (hores)", 
     ylab = "ln(Població)", 
     pch = 19, col = "green")

# Afegim la recta de regressió al gràfic
abline(model, col = "red", lwd = 2)

# Interpretació del model

# El pendent de la regressió (coeficient associat a t) és la taxa de creixement k
# La intersecció (intercept) és log(P_0), el logaritme de la població inicial

# Mostrem els valors obtinguts
cat("Intersecció (log(P_0)) = ", coef(model)[1], "\n")
cat("Pendent (k) = ", coef(model)[2], "\n")

# La població inicial P_0 és l'exponencial de la intersecció
P_0 <- exp(coef(model)[1])
cat("Població inicial estimada (P_0) = ", P_0, "\n") # Tal com la que haviem escollit

# La taxa de creixement k és el pendent del model
k <- coef(model)[2]
cat("Taxa de creixement estimada (k) = ", k, "\n") # Tal com la que haviem escollit
