# Comprova el directori actual
getwd()
# Estableix el directori correcte
setwd("/Users/sergigimenezgabarro/Documents/UAB/Primer semestre/Estadística (Eng. inf.)/practica4")


# PROBLEMA 1
# Tenim una mostra de 200 articles i en trobem 8 de defectuosos. Estem interessats en saber quina
# és la proporció d'articles defectuosos de la població amb un 95% i 99% de confiança. El fabricant
# afirma que aquesta proporció és del 1%, veurem amb quin nivell de confiança ens en podem refiar

# Dades
n <- 200  # mida de la mostra
x <- 8    # nombre d'articles defectuosos

# Proporció muestral
p_hat <- x / n

# Ens n'assegurem de que n*p_hat >= 5 i n*(1-p_hat) >= 5
print(n*p_hat)
print(n*(1-p_hat))

# Nivells de confiança
conf_levels <- c(0.95, 0.99, 0.93)

# Càlcul dels intervals de confiança
for (conf_level in conf_levels) {
  alpha <- 1 - conf_level # 0.05, 0.01, 0.07
  z <- qnorm(1 - alpha/2) # 0.975, 0.995, 0.965
  SE <- sqrt(p_hat * (1 - p_hat) / n) # standard error
  lower <- p_hat - z * SE
  upper <- p_hat + z * SE
  cat(sprintf("Interval de confiança al %.0f%%: [%.4f, %.4f]\n", conf_level*100, lower, upper))
}
# Això es llegeix així: podem assegurar amb un 95% de confiança que p està entre [0.0128, 0.0672].
# Amb això ja podriem dir que l'afirmació del fabricant és probablement falsa ja que podem afirmar
# amb un 95% de confiança que està entre 1.28% i 6.72%

# EXTRA:
# Però jo realment vull expresar algo de l'estil amb un X% de confiança p és superior a 0.01
p0 <- 0.01      # proporció sota H0

# Estadístic z
SE_H0 <- sqrt(p0 * (1 - p0) / n)
z <- (p_hat - p0) / SE_H0

# Càlcul del p-valor (prova unilaterial dreta)
# It quantifies the probability of obtaining the observed data, or something more extreme, 
# assuming that the null hypothesis is true
p_value <- 1 - pnorm(z)

# Nivell de confiança
confidence_level <- (1 - p_value) * 100

# Resultats
cat(sprintf("Estadístic z: %.4f\n", z))
cat(sprintf("p-valor: %.6f\n", p_value))
cat(sprintf("Amb un nivell de confiança del %.4f%%, podem afirmar que la proporció real és superior a 0,01\n", confidence_level))


# PROBLEMA 2
# Carregar dades
dades <- read.table("DADES_PRAC_1.txt", header = TRUE)

# Seleccionar dones fumadores
dones_fumadores <- subset(dades, sex == 2 & fum == 1)

n_dones <- nrow(dones_fumadores)
mean_dones <- mean(dones_fumadores$alt)
sd_dones <- sd(dones_fumadores$alt)

# Interval de confiança al 90% per a les dones fumadores (distribució normal)
conf_level <- 0.90
alpha <- 1 - conf_level
z_value <- qnorm(1 - alpha/2)
SE_dones <- sd_dones / sqrt(n_dones)
lower_dones <- mean_dones - z_value * SE_dones
upper_dones <- mean_dones + z_value * SE_dones

cat(sprintf("Interval de confiança al 90%% per a l'alçada mitjana de les dones fumadores: [%.2f, %.2f]\n", lower_dones, upper_dones))


# Repetir el mateix per als homes fumadors
# Seleccionar homes fumadors
homes_fumadors <- subset(dades, sex == 1 & fum == 1)

n_homes <- nrow(homes_fumadors)
mean_homes <- mean(homes_fumadors$alt)
sd_homes <- sd(homes_fumadors$alt)

# Interval de confiança al 90% pels homes fumadors (distribució normal)
conf_level <- 0.90
alpha <- 1 - conf_level
z_value <- qnorm(1 - alpha/2)
SE_homes <- sd_homes / sqrt(n_homes)
lower_homes <- mean_homes - z_value * SE_homes
upper_homes <- mean_homes + z_value * SE_homes
cat(sprintf("Interval de confiança al 90%% per a l'alçada mitjana dels homes fumadors: [%.2f, %.2f]\n", lower_homes, upper_homes))

# No es pot afirmar res en general sobre l'alçada dels homes vs dones perquè els intervals de 
# confiança només es refereixen als homes o dones fumadors/es

# PROBLEMA 3

# Dades de gruix
gruixos <- c(12.6, 11.9, 12.3, 12.8, 11.8, 11.7, 12.4, 12.1, 12.3, 12.0, 12.5, 12.9)
n <- length(gruixos)

# Calcular la desviació estàndard mostral
s <- sd(gruixos)

# Graus de llibertat
df <- n - 1

# Nivell de confiança
conf_level <- 0.95
alpha <- 1 - conf_level

# Valors crítics chi-quadrat
chi_sq_lower <- qchisq(alpha/2, df)
chi_sq_upper <- qchisq(1 - alpha/2, df)

# Límits de l'interval de confiança per a la variància
var_lower <- (df * s^2) / chi_sq_upper
var_upper <- (df * s^2) / chi_sq_lower

# Límits de l'interval de confiança per a la desviació estàndard
sd_lower <- sqrt(var_lower)
sd_upper <- sqrt(var_upper)

cat(sprintf("Interval de confiança al 95%% per a la desviació estàndard: [%.3f, %.3f]\n", sd_lower, sd_upper))





