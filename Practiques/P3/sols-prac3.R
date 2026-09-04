# Comprova el directori actual
getwd()
# Estableix el directori correcte
setwd("/Users/sergigimenezgabarro/Documents/UAB/Primer semestre/Estadística (Eng. inf.)/practica3")

# Problema 1

# a) Calcula la probabilitat que un paquet tingui exactament 4 fàrmacs defectuosos.
n <- 10       # Nombre de fàrmacs per paquet
p <- 0.05     # Probabilitat que un fàrmac sigui defectuós
k <- 4        # Nombre de fàrmacs defectuosos

probabilitat <- dbinom(k, size = n, prob = p) # dbinom és la funció de probabilitat
print(probabilitat)

# b) Si a un client li han venut 20 paquets, calcula la probabilitat que l'hagin d'indemnitzar.
# Primer cal calcular la probabilitat de que un paquet tingui almenys 2 fàrmacs defectuosos
p0 <- dbinom(0, size = n, prob = p)
p1 <- dbinom(1, size = n, prob = p)
p_almenys2 <- 1 - (p0 + p1)
# p0 + p1 és igual a pbinom(1, size=n, prob=p)
print(p_almenys2)

# Ara, l'indemnitzaran si com a mínim un paquet té almenys dos fàrmacs defectuosos. 
n_paquets <- 20
llindar <- 20*0.05 # Si més d'un paquet té almenys 2 fàrmacs defectuosos, s'indemnitzarà
# És més fàcil calcular la probabilitat del complementari. No s'indemnitzarà si
# hi ha 1 o menys paquets amb almenys 2 fàrmacs defectuosos
p_indemnitzar <- 1 - pbinom(llindar, size = n_paquets, prob = p_almenys2)
print(p_indemnitzar)

# c) Si a un client li han venut 400 paquets, calcula la probabilitat que l'hagin 
# d'indemnitzar. Compara aquest resultat amb el que obtindries aplicant l'aproximació 
# del Teorema Central del Límit (TCL).
n_paquets <- 400
llindar <- 400*0.05
prob_exacta <- 1 - pbinom(llindar, size = n_paquets, prob = p_almenys2)
print(prob_exacta)

# Aproximació amb el Teorema Central del Límit (TCL)
# El Teorema Central del Límit (TCL) és un dels resultats més importants de la 
# probabilitat i estadística. Diu que:
# Quan es repeteixen moltes vegades proves independents i idèntiques (per exemple,
# n vegades), la distribució de la mitjana de les observacions tendeix a una 
# distribució normal, independentment de la distribució original, sempre que n 
# sigui suficientment gran.
# X ~ B(n,p) ≈ N(μ, σ²) on μ = np (mitjana de la distribució binomial)
# σ² = np(1-p) (variància de la distribució binomial)
mu <- n_paquets * p_almenys2
sigma <- sqrt(n_paquets * p_almenys2 * (1 - p_almenys2))
# P(Binomial ≥ 21) ≈ P(Normal > 20.5)
# Estandaritzant. Per què sumem el 0.5? Per què volem saber quina és la probabilitat
# de que P(Binomial > 20) = P(Binomial >= 21). Llavors tant P(Normal > 20) com P(Normal > 21)
# haurien de ser correctes en principi, agafem la meitat per millorar l'aproximació. 
z <- (llindar + 0.5 - mu) / sigma 
prob_TCL <- 1 - pnorm(z)
print(prob_TCL)
# Sense estandaritzar
z <- llindar + 0.5
prob_TCL <- 1 - pnorm(z, mean=mu, sd=sigma)
print(prob_TCL)

# d) Proveu diferents valors de n i de p i compareu els resultats que obteniu amb
# l'aproximació del TCL.
n_paquets <- 100
p <- 0.1
p0 <- dbinom(0, size = n, prob = p)
p1 <- dbinom(1, size = n, prob = p)
p_almenys2 <- 1 - (p0 + p1)

mu <- n_paquets * p_almenys2
sigma <- sqrt(n_paquets * p_almenys2 * (1 - p_almenys2))
llindar <- floor(0.05 * n_paquets)
z <- (llindar + 0.5 - mu) / sigma
prob_TCL <- 1 - pnorm(z)

prob_exacta <- 1 - pbinom(llindar, size = n_paquets, prob = p_almenys2)

print(c(Prob_Exacta = prob_exacta, Prob_TCL = prob_TCL))
# Hauriem de veure que quan trenquem alguna d'aquestes condicions:
# - La mida de la mostra (n) ha de ser suficientment gran.
# - Tant np com n(1-p) han de ser majors que 5.
# Les aproximacions són dolentes



# Observem que si p_almenys2 és 5% llavors la probabilitat de que s'hagi d'indemnitzar
# tendeix al 50%. Distribució simètrica amb mitjana de paquets defectuosos igual
# al 5% (així que és 50-50 haver d'indemnitzar o no)
p_almenys2 <- 0.05
n_paquets <- 100000
llindar <- 100000*0.05
prob_exacta <- 1 - pbinom(llindar, size = n_paquets, prob = p_almenys2)
print(prob_exacta)

# Si p_almenys2 és superior a 0.05, la probabilitat tendeix a 1. I si és inferior
# la probabilitat tendeix a 0


# e) Si a un client li han venut 400 paquets, quin percentatge de paquets amb 
# almenys 2 fàrmacs defectuosos haurien d'estipular com a límit per decidir que 
# cal indemnitzar al client si volem que la probabilitat d'indemnitzar el client 
# sigui del 2%?
n <- 10       # Nombre de fàrmacs per paquet
p <- 0.05     # Probabilitat que un fàrmac sigui defectuós
p0 <- dbinom(0, size = n, prob = p)
p1 <- dbinom(1, size = n, prob = p)
p_almenys2 <- 1 - (p0 + p1)

# Busquem el valor k tal que: P(X ≥ k) = 0.02. On X és el nombre de paquets amb 
# almenys 2 defectuosos en 400 paquets.
k <- 0.115 # Aquesta és la variable que hem d'anar calibrant (percentatge de paquets 
# que poden tenir almenys dos fàrmacs defectuosos)
n <- 400
llindar <- 400*k
prob_exacta <- 1 - pbinom(llindar, size = n, prob = p_almenys2)

# Segona manera
num_paquets_def <- qbinom(0.98, size = n, prob = p_almenys2)
k<-num_paquets_def/n


# f) Amb quina probabilitat haurem obert més de 40 paquets per trobar-ne un que 
# tingui almenys 2 fàrmacs defectuosos (busqueu Distribució Geomètrica).
# X ~ Geom(p=p_almenys2). Volem calcular P(X>40)=P(X>=41)=1-P(X<=40)
prob_mes40 <- 1 - pgeom(40, prob = p_almenys2)
print(prob_mes40)


# Problema 2
ruina2 <- function(nmax=1000, a, b, p){
  x= rbinom(nmax, 1, p) # resultats de les rondes: 0 i 1 Bernoulli~p
  x[x==0] = -1 # canviem el 0 per -1
  traj = cumsum(x)+a # trajectoria
  if ({ all( traj<a+b ) & all( traj>0 )})
  { res<-"Joc inacabat";nrond<-nmax }
  if( {any( which(traj>=a+b) ) | any( which(traj<=0) )} )
  { trunc<- 1+min( which(traj>=a+b), which(traj<=0) )
  if (trunc <= nmax) {traj[trunc:nmax] <- NA};nrond<-trunc-1
  if (traj[nrond]==a+b) {res<-"A guanya"} else {res<-"B guanya"}}
  list(traj=traj,nrond=nrond,guanyador=res)
}
set.seed(8471104)
num_simulacions <- 10000
nmax <- 561
a <- 39
b <- 39
p <- 0.5695

# Inicialitza vectors per emmagatzemar els resultats
guanyador <- character(num_simulacions)
nrondes <- integer(num_simulacions)

# Simula els jocs
for(i in 1:num_simulacions){
  simulacio <- ruina2(nmax = nmax, a = a, b = b, p = p)
  guanyador[i] <- simulacio$guanyador
  nrondes[i] <- simulacio$nrond
}

# Comptabilitza les victòries d'A en jocs que acaben en ≤ 561 rondes
victories_A <- sum(guanyador == "A guanya" & nrondes <= 561)
print(victories_A)

