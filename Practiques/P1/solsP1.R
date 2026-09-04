getwd() # Comprova el directori actual
setwd("/Users/sergigimenezgabarro/Documents/UAB/Primer semestre/Estadística (Eng. inf.)/practica1") # Estableix el directori correcte

# Carrega les dades del fitxer de text
dades <- read.table("DADES PRAC1.txt", header = TRUE, sep = " ")

# Mostra les primeres files per verificar que s'ha carregat correctament
head(dades)

# Exercici 1
# Assumint que s ja està carregat des del fitxer de dades
s <- dades$s  # reemplaça 'dades' pel dataframe que conté les dades reals

# Primera suma
# Mètode 1
# Vector d'índexs del 4 al 46
vector_i <- 4:46

# Càlcul vectoritzat de la suma
suma1 <- sum((3 ^ vector_i) / (vector_i^vector_i * factorial(vector_i - 1)))

# Mètode 2
suma1 <- sum(sapply(4:46, function(i) (3 ^ i) / (i ^ i * factorial(i-1))))

# Segona suma
# Mètode 1
vector_i_2 <- 42:156

# Assegura't que s[i-1] existeix per a tots els i en el rang
# s[i-1] correspon a s[vector_i_2 - 1], i s[i] correspon a s[vector_i_2]
vector_s <- s[vector_i_2]
vector_s_prev <- s[vector_i_2 - 1]

# Càlcul vectoritzat de la segona suma
suma2 <- sum((vector_s * (2^vector_s_prev)) / (2 * vector_i_2 + 2))
# Mètode 2
suma2 <- sum(sapply(42:156, function(i) (s[i] * (2^s[i-1])) / (2 * i + 2)))

cat("Primera suma:", suma1, "\n")
cat("Segona suma:", suma2, "\n")

# Exercici 2
# Valors distintius de "pes"
pesos_unics <- unique(dades$pes)
nombre_distint <- length(pesos_unics)
pes_minim <- min(pesos_unics)

cat("Nombre de valors distintius de 'pes':", nombre_distint, "\n")
cat("Valor mínim de 'pes':", pes_minim, "\n")

# Exercici 3
# Filtrar fumadors homes
# Selecciona totes les files que verifiquin dades$fum == 1 & dades$sex == 1; i selecciona totes les columnes
fumadors_homes <- dades[dades$fum == 1 & dades$sex == 1, ] 

# Calcular l'alçada mitjana dels fumadors homes
mitjana_alt_fumadors_homes <- mean(fumadors_homes$alt)

# Convertir pes de lliures a kg i alçada de polzades a metres per calcular l'IMC
fumadors_homes$pes_kg <- fumadors_homes$pes * 0.453592
fumadors_homes$alt_m <- fumadors_homes$alt * 0.0254
fumadors_homes$imc <- fumadors_homes$pes_kg / (fumadors_homes$alt_m^2)

# Calcular mitjana i mediana de l'IMC
mitjana_imc_fumadors_homes <- mean(fumadors_homes$imc)
mediana_imc_fumadors_homes <- median(fumadors_homes$imc)

cat("Alçada mitjana dels fumadors homes:", mitjana_alt_fumadors_homes, "\n")
cat("IMC mitjà dels fumadors homes:", mitjana_imc_fumadors_homes, "\n")
cat("IMC mitjà dels fumadors homes:", mediana_imc_fumadors_homes, "\n")

# Exercici 4
# Filtrar no fumadors
no_fumadors <- dades[dades$fum == 2, ]

# Calcular l'alçada mitjana d'homes i dones
mitjana_alt_dones <- mean(dades$alt[dades$sex == 2])
mitjana_alt_homes <- mean(dades$alt[dades$sex == 1])

# Proporció d'homes no fumadors més alts que la dona mitjana
proporcio_homes_mes_alts_dona_mitjana <- sum(no_fumadors$alt[no_fumadors$sex == 1] >= mitjana_alt_dones) / 
  sum(no_fumadors$sex == 1)

# Proporció de dones no fumadores més altes que l'home mitjà
proporcio_dones_mes_altes_home_mitja <- sum(no_fumadors$alt[no_fumadors$sex == 2] >= mitjana_alt_homes) / 
  sum(no_fumadors$sex == 2)

cat("Proporció d'homes no fumadors més alts que la dona mitjana:", proporcio_homes_mes_alts_dona_mitjana, "\n")
cat("Proporció de dones no fumadores més altes que l'home mitjà:", proporcio_dones_mes_altes_home_mitja, "\n")

# Exercici 5
# Què és el coeficient de variació de Pearson? El coeficient de variació (CV) s’obté dividint la 
# desviació típica corregida (n-1) pel valor absolut de la mitjana. Quan la mitjana és a prop de 
# zero, perd de significat.
# Filtrar dones
dones <- dades[dades$sex == 2, ]

# Ordenar dones per alçada
dones_ordenades <- dones[order(dones$alt), ]

# Dividir en el 25% superior i el 25% inferior basat en l'alçada
n <- nrow(dones_ordenades)
inferior_25 <- dones_ordenades[1:(n/4), ]
superior_25 <- dones_ordenades[(3*n/4 + 1):n, ]

# Calcular el CV pel pes en ambdós grups
cv <- function(x) {
  sd(x) / abs(mean(x)) # usa n-1 com a denominador
}

cv_inferior_25 <- cv(inferior_25$pes)
cv_superior_25 <- cv(superior_25$pes)

cat("CV del 25% inferior de dones (alçada):", cv_inferior_25, "\n")
cat("CV del 25% superior de dones (alçada):", cv_superior_25, "\n")

# El fet que el CV sigui més alt en el grup de les dones més baixes (7.24%) que en 
# el de les dones més altes (5.34%) ens indica que hi ha més variabilitat en el pes 
# de les dones més baixes.
