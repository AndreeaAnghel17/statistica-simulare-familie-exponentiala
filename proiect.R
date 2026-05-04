#----------3.1 RECONSTRUCTIA DEMONSTRATIEI PRIN SIMULARE-------------
# 1) Alegem n >= 6 si setul de valori proprii (lambda)
n <- 10
m <- 2
M <- 20
lambdas <- seq(m, M, length.out = n)

# 2) Generam vectorul aleator y (ex: Normal(0,1)) si definim p_i
y <- rnorm(n, mean = 0, sd = 1)
p <- (y^2) / sum(y^2)

# 3) Definim variabila aleatoare T
# T ia valorile lambdas cu probabilitatile p

# 4) Simulam N observatii din T
N <- 10000
sim_T <- sample(lambdas, size = N, replace = TRUE, prob = p)

# Estimam mediile
E_T <- mean(sim_T)        # Estimarea lui E(T)
E_inv_T <- mean(1/sim_T)  # Estimarea lui E(1/T)
R_N <- E_T * E_inv_T      # Produsul estimat

# 5) Comparam cu limita teoretica B
B <- ((M + m)^2) / (4 * M * m)

# Afisare rezultate
cat("Rezultatul estimat (R_N):", R_N, "\n")
cat("Limita teoretica (B):", B, "\n")
cat("Verificare (R_N <= B):", R_N <= B, "\n")

#-----------3.2 CONVERGENTA ESTIMATORILOR-------------
#1
# Parametrii initiali (din sectiunea 3.1)
n <- 10
lambdas <- seq(2, 20, length.out = n)
y <- rnorm(n)
p <- (y^2) / sum(y^2)

# Valorile teoretice (E(T) si E(1/T))
E_T_teoretic <- sum(p * lambdas)
E_inv_T_teoretic <- sum(p * (1/lambdas))

# Valorile N cerute
N_values <- c(100, 1000, 10000)

# Vectori pentru stocarea rezultatelor in vederea graficului
est_T <- c()
est_inv_T <- c()

cat("Valori teoretice: E(T) =", E_T_teoretic, "| E(1/T) =", E_inv_T_teoretic, "\n\n")

# Simularea convergentei
for (N in N_values) {
  # Generam esantionul
  sim_T <- sample(lambdas, size = N, replace = TRUE, prob = p)
  
  # Estimam valorile
  E_N_T <- mean(sim_T)
  E_N_inv_T <- mean(1/sim_T)
  
  # Salvam rezultatele
  est_T <- c(est_T, E_N_T)
  est_inv_T <- c(est_inv_T, E_N_inv_T)
  
  # Afisam rezultatele pentru a vedea convergenta
  cat("Pentru N =", N, ":\n")
  cat("  Estimare E_N(T)     :", E_N_T, " (Eroare:", abs(E_N_T - E_T_teoretic), ")\n")
  cat("  Estimare E_N(1/T)   :", E_N_inv_T, " (Eroare:", abs(E_N_inv_T - E_inv_T_teoretic), ")\n\n")
}

#3.2.2. 
# 1. Configurare parametri (aceiasi de la 3.1)
n <- 10
lambdas <- seq(2, 20, length.out = n)
y <- rnorm(n)
p <- (y^2) / sum(y^2)

# 2. Valoarea teoretica R
R_teoretic <- sum(p * lambdas) * sum(p * (1/lambdas))

# 3. Dimensiuni esantion pentru testare
N_values <- c(10^2, 10^3, 10^4, 10^5)

cat("Valoarea tinta R =", R_teoretic, "\n\n")

for (N in N_values) {
  # Generam esantionul din variabila T
  sim_T <- sample(lambdas, size = N, replace = TRUE, prob = p)
  
  # Calculam estimatorii
  E_N_T <- mean(sim_T)
  E_N_inv_T <- mean(1/sim_T)
  
  # Calculam produsul estimat R_N
  R_N <- E_N_T * E_N_inv_T
  
  # Eroarea absoluta
  eroare <- abs(R_N - R_teoretic)
  
  cat(sprintf("N = %-7d | R_N = %.6f | Eroare = %.6f\n", N, R_N, eroare))
}

#3.2.3. 
# 1) Parametrii de baza
n <- 10
m <- 2
M <- 20
lambdas <- seq(m, M, length.out = n)

# 2) Generam p_i (distributie Normala)
set.seed(42)
y <- rnorm(n)
p <- (y^2) / sum(y^2)

# 3) Limita teoretica B
B <- ((M + m)^2) / (4 * M * m)

# 4) Functie simpla pentru calculul R_N
simuleaza_RN <- function(N, replicari = 100) {
  rezultate <- replicate(replicari, {
    sim_T <- sample(lambdas, size = N, replace = TRUE, prob = p)
    mean(sim_T) * mean(1/sim_T)
  })
  return(rezultate)
}

# 5) Colectam date pentru N = 10^2, 10^3, 10^4
date_grafic <- list(
  "N=100"   = simuleaza_RN(100),
  "N=1000"  = simuleaza_RN(1000),
  "N=10000" = simuleaza_RN(10000)
)


#-----------4.APLICATIE PRACTICA. MA VS MH---------------
# 4 distributii pe [m, M]
# Estimam A = E(T), H = 1/E(1/T), A/H si verificam limita Kantorovich
# ------------------------------------------------------------

# Intervalul [m, M]
m <- 1
M <- 5

# Numar de simulari
N <- 10^6

# Limita Kantorovich
B <- (M + m)^2 / (4 * M * m)

cat("Interval [m, M] = [", m, ",", M, "]\n")
cat("Limita Kantorovich B =", B, "\n\n")


# 1) Distributia Uniforma pe [m, M]
T1 <- runif(N, min = m, max = M)

A1 <- mean(T1)
H1 <- 1 / mean(1 / T1)
AH1 <- A1 / H1

cat("1) Uniforma[m, M]\n")
cat("A =", A1, "\n")
cat("H =", H1, "\n")
cat("A/H =", AH1, "\n")
cat("Respecta limita? ", AH1 <= B, "\n\n")

hist(T1, breaks=60, main="Histograma: Uniforma", xlab="T", col="lightgray")
abline(v=A1, lwd=2)   # linie la media aritmetica
abline(v=H1, lwd=2, lty=2)  # linie la media armonica

hist(T2, breaks=60, main="Histograma: Beta rescalata", xlab="T", col="lightgray")
abline(v=A2, lwd=2)
abline(v=H2, lwd=2, lty=2)


# 3) Distributie Bimodala: amestec de doua uniforme
# Distributia bimodala a fost construita generand jumatate dintre observatii dintr-o
# distributie uniforma pe un subinterval inferior si jumatate dintr-o distributie uniforma 
# pe un subinterval superior, obtinand astfel o concentrare a valorilor in jurul extremitatilor 
# intervalului.

# Prima jumatate: valori mici
T3a <- runif(N/2, min = m, max = 2)

# A doua jumatate: valori mari
T3b <- runif(N/2, min = 4, max = M)

# Le combinam
T3 <- c(T3a, T3b)

# Le amestecam
T3 <- sample(T3)

# Calcule
A3 <- mean(T3)
H3 <- 1 / mean(1 / T3)
AH3 <- A3 / H3

cat("3) Bimodala\n")
cat("A =", A3, "\n")
cat("H =", H3, "\n")
cat("A/H =", AH3, "\n")
cat("Respecta limita? ", AH3 <= B, "\n\n")

hist(T3, breaks=60, main="Histograma: Bimodala", xlab="T", col="lightgray")
abline(v=A3, lwd=2)
abline(v=H3, lwd=2, lty=2)


# ------------------------------------------------------------
# 4) Distributie Discreta: valori finite
# Valori: {m, (m+M)/2, M} cu probabilitati {0.3, 0.4, 0.3}
# ------------------------------------------------------------

# Distributia discreta a fost construita considerand un numar finit de valori posibile in 
# intervalul [m,M], fiecare asociata cu o probabilitate data.
# Prezenta unei probabilitati semnificative pentru valorile mici conduce la o scadere a 
# mediei armonice si, implicit, la cresterea raportului A/H.

valori <- c(m, (m + M) / 2, M)
prob <- c(0.3, 0.4, 0.3)

T4 <- sample(valori, size = N, replace = TRUE, prob = prob)

A4 <- mean(T4)
H4 <- 1 / mean(1 / T4)
AH4 <- A4 / H4

cat("4) Discreta (3 valori)\n")
cat("A =", A4, "\n")
cat("H =", H4, "\n")
cat("A/H =", AH4, "\n")
cat("Respecta limita? ", AH4 <= B, "\n\n")

plot(valori, prob, type = "h", lwd = 8,
     main = "Distributie discreta",
     xlab = "T",
     ylab = "Probabilitate")

AH <- c(AH1, AH2, AH3, AH4)
nume <- c("Uniforma", "Beta", "Bimodala", "Discreta")

barplot(AH, names.arg=nume, main="Comparatie heterogenitate (A/H)",
        ylab="A/H", col="lightgray")

abline(h=B, lwd=2)  # linia limitei Kantorovich

# Graficul comparativ al raportului A/H evidentiaza diferente semnificative de heterogenitate 
# intre distributiile analizate, desi toate sunt definite pe acelasi interval [m,M].
# Distributia Beta rescalata prezinta cea mai mica heterogenitate, deoarece valorile sunt 
# concentrate in jurul mediei si exista putine valori mici.
# Distributia uniforma are o heterogenitate intermediara, intrucat valorile sunt distribuite
# egal pe interval.
# Distributia bimodala este mai eterogena, deoarece valorile sunt concentrate 
# in apropierea extremitatilor intervalului. 
# Cea mai mare heterogenitate este observata in cazul distributiei discrete, unde probabilitatea 
# semnificativa asociata valorii minime conduce la o scadere accentuata a mediei armonice. 

cat("\n--- Verificare Kantorovich ---\n")
cat("B (limita Kantorovich) =", B, "\n\n")

cat("Uniforma: A =", A1, " H =", H1, " A/H =", AH1, "\n")
cat("Beta:     A =", A2, " H =", H2, " A/H =", AH2, "\n")
cat("Bimodala: A =", A3, " H =", H3, " A/H =", AH3, "\n")
cat("Discreta: A =", A4, " H =", H4, " A/H =", AH4, "\n")


# 1) A (media aritmetica) depinde de valorile lui T.
# 2) H (media armonica) este foarte sensibila la valori mici ale lui T,
#    deoarece 1/T devine mare cand T este aproape de m.
# 3) De aceea, distributiile care au mai multe valori mici (aproape de m)
#    au H mai mica si raportul A/H mai mare (heterogenitate mai mare).
# 4) Chiar daca distributiile sunt diferite in interiorul intervalului [m,M],
#    toate respecta aceeasi limita universala Kantorovich B, care depinde doar de m si M.



#--------------II. FAMILIE EXPONENTIALA--------------
#---------------1. APARTENENTA + REPREZENTARE GRAFICA--------------
# culori folosite pentru cele 4 curbe/serii
col4 <- c("red", "blue", "darkgreen", "purple")


# =========================
# 1) BINOMIALA Bin(20,p)  (discreta)
# =========================
n <- 20
x <- 0:n

y1 <- dbinom(x, n, 0.2)
y2 <- dbinom(x, n, 0.4)
y3 <- dbinom(x, n, 0.6)
y4 <- dbinom(x, n, 0.8)

plot(x, y1, type="h", lwd=3, col=col4[1],
     main="Binomiala Bin(20,p)",
     xlab="x", ylab="P(X=x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, type="h", lwd=3, col=col4[2])
lines(x, y3, type="h", lwd=3, col=col4[3])
lines(x, y4, type="h", lwd=3, col=col4[4])

legend("topright",
       legend=c("p=0.2","p=0.4","p=0.6","p=0.8"),
       col=col4, lwd=3, bty="n")


# =========================
# 2) GEOMETRICA Geom(p) (R: x=0,1,2,...)  (discreta)
# =========================
x <- 0:10

y1 <- dgeom(x, 0.2)
y2 <- dgeom(x, 0.35)
y3 <- dgeom(x, 0.5)
y4 <- dgeom(x, 0.75)

plot(x, y1, type="h", lwd=3, col="red",
     main="Geometrica Geom(p)",
     xlab="x", ylab="P(X=x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, type="h", lwd=3, col="blue")
lines(x, y3, type="h", lwd=3, col="darkgreen")
lines(x, y4, type="h", lwd=3, col="purple")

legend("topright",
       legend=c("p=0.2","p=0.35","p=0.5","p=0.75"),
       col=c("red","blue","darkgreen","purple"),
       lwd=3)


# =========================
# 3) POISSON Pois(lambda)  (discreta)
# =========================
x <- 0:20

y1 <- dpois(x, 1)
y2 <- dpois(x, 3)
y3 <- dpois(x, 6)
y4 <- dpois(x, 10)

plot(x, y1, type="h", lwd=3, col=col4[1],
     main="Poisson Pois(lambda)",
     xlab="x", ylab="P(X=x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, type="h", lwd=3, col=col4[2])
lines(x, y3, type="h", lwd=3, col=col4[3])
lines(x, y4, type="h", lwd=3, col=col4[4])

legend("topright",
       legend=c("lambda=1","lambda=3","lambda=6","lambda=10"),
       col=col4, lwd=3, bty="n")


# =========================
# 4) EXPONENTIALA Exp(lambda)  (continua)
# =========================
x <- seq(0, 20, 0.01)

y1 <- dexp(x, rate = 0.5)
y2 <- dexp(x, rate = 1)
y3 <- dexp(x, rate = 2)
y4 <- dexp(x, rate = 4)

plot(x, y1, type="l", lwd=2, col="red",
     main="Exponentiala Exp(lambda)",
     xlab="x", ylab="f(x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, lwd=2, col="blue")
lines(x, y3, lwd=2, col="darkgreen")
lines(x, y4, lwd=2, col="purple")

legend("topright",
       legend=c("lambda=0.5","lambda=1","lambda=2","lambda=4"),
       col=c("red","blue","darkgreen","purple"),
       lwd=2)


# =========================
# 5) GAMMA Gamma(alpha,beta)  (continua)
# In R: shape=alpha, rate=beta
# =========================
x <- seq(0, 12, 0.01)

y1 <- dgamma(x, shape=1, rate=1)
y2 <- dgamma(x, shape=2, rate=1)
y3 <- dgamma(x, shape=5, rate=1)
y4 <- dgamma(x, shape=9, rate=2)

plot(x, y1, type="l", lwd=2, col=col4[1],
     main="Gamma Gamma(alpha,beta)",
     xlab="x", ylab="f(x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, lwd=2, col=col4[2])
lines(x, y3, lwd=2, col=col4[3])
lines(x, y4, lwd=2, col=col4[4])

legend("topright",
       legend=c("a=1,b=1","a=2,b=1","a=5,b=1","a=9,b=2"),
       col=col4, lwd=2, bty="n")


# =========================
# 6) BETA Beta(alpha,beta)  (continua, pe (0,1))
# =========================
x <- seq(0.001, 0.999, 0.001)

y1 <- dbeta(x, 0.5, 0.5)
y2 <- dbeta(x, 2, 2)
y3 <- dbeta(x, 5, 2)
y4 <- dbeta(x, 2, 5)

plot(x, y1, type="l", lwd=2, col=col4[1],
     main="Beta Beta(alpha,beta)",
     xlab="x", ylab="f(x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, lwd=2, col=col4[2])
lines(x, y3, lwd=2, col=col4[3])
lines(x, y4, lwd=2, col=col4[4])

legend("topright",
       legend=c("a=0.5,b=0.5","a=2,b=2","a=5,b=2","a=2,b=5"),
       col=col4, lwd=2, bty="n")


# =========================
# 7) UNIFORMA U(a,b)  (continua)
# =========================
x <- seq(-2, 6, 0.01)

y1 <- dunif(x, min=-1, max=1)
y2 <- dunif(x, min=0,  max=1)
y3 <- dunif(x, min=0,  max=3)
y4 <- dunif(x, min=2,  max=5)

plot(x, y1, type="l", lwd=2, col=col4[1],
     main="Uniforma U(a,b)",
     xlab="x", ylab="f(x)",
     ylim=c(0, max(y1,y2,y3,y4)))

lines(x, y2, lwd=2, col=col4[2])
lines(x, y3, lwd=2, col=col4[3])
lines(x, y4, lwd=2, col=col4[4])

legend("topright",
       legend=c("a=-1,b=1","a=0,b=1","a=0,b=3","a=2,b=5"),
       col=col4, lwd=2, bty="n")


#---------  II.2. CONCAVITATEA FUNCTIEI DE LOG-VEROSIMILITATE---------
# -------geometrica------
# datele observate
m <- 50
date_geom <- rgeom(m, prob = 0.25) # exemplu de date generate

# Definirea functiei de log-verosimilitate (l)
# Calculele se bazeaza pe forma: l = ln(1-p)*sum(x) + m*ln(p)
log_lik_manual <- function(p, x) {
  # protejam logaritmul pentru a evita valori negative sau zero
  if (p <= 0 | p >= 1) return(-Inf)
  
  m <- length(x)
  suma_x <- sum(x)
  
  # Implementarea formulei deduse anterior
  valoare <- log(1 - p) * suma_x + m * log(p)
  return(valoare)
}

# Gasim punctul de maxim folosind functia optimize
# Cautam in intervalul (0, 1) deoarece p este o probabilitate
# Conform calculelor de concavitate, avem un singur maxim global
rezultat <- optimize(f = log_lik_manual, interval = c(0, 0.99), 
                     x = date_geom, maximum = TRUE)

# Afisarea rezultatului in consola
cat("Punctul de maxim (MLE) pentru p este:", rezultat$maximum)

# Reprezentare grafica pentru vizualizarea concavitatii
p_grid <- seq(0.01, 0.9, length.out = 100)
y_plot <- sapply(p_grid, log_lik_manual, x = date_geom)

plot(p_grid, y_plot, type = "l", col = "blue", lwd = 2,
     main = "Graficul functiei de log-verosimilitate (Geometrica)",
     xlab = "Parametrul p", ylab = "Log-Likelihood")

# Marcam punctul de maxim determinat prin optimize
abline(v = rezultat$maximum, col = "red", lty = 2)


#-------binomiala---------
# datele observate
m <- 50          # numarul de observatii din esantion
n_fixat <- 10    # numarul de incercari pentru fiecare observatie
date_bin <- rbinom(m, size = n_fixat, prob = 0.6) # exemplu de date generate

# Definirea functiei de log-verosimilitate
log_lik_bin_manual <- function(p, x, n) {
  # protejam logaritmul
  if (p <= 0 | p >= 1) return(-Inf)
  
  m <- length(x)
  suma_x <- sum(x)
  
  # Formula bazata pe descompunerea familiei exponentiale
  # l = suma_x * log(p) + (m*n - suma_x) * log(1-p)
  valoare <- suma_x * log(p) + (m * n - suma_x) * log(1 - p)
  return(valoare)
}

# Gasim punctul de maxim folosind optimize
# Cautam in intervalul (0, 1) pentru probabilitatea p
rezultat_bin <- optimize(f = log_lik_bin_manual, interval = c(0.01, 0.99), 
                         x = date_bin, n = n_fixat, maximum = TRUE)

cat("Punctul de maxim (MLE) pentru p la Binomiala este:", rezultat_bin$maximum)

# Reprezentare grafica
p_grid <- seq(0.01, 0.99, length.out = 100)
y_plot <- sapply(p_grid, log_lik_bin_manual, x = date_bin, n = n_fixat)

plot(p_grid, y_plot, type = "l", col = "darkgreen", lwd = 2,
     main = "Log-verosimilitate Binomiala (n fixat)",
     xlab = "Parametrul p", ylab = "Log-Likelihood")

# Marcam punctul de maxim determinat
abline(v = rezultat_bin$maximum, col = "red", lty = 2)


# Poisson
# datele observate
m <- 50
lambda_real <- 4
date_pois <- rpois(m, lambda = lambda_real) # exemplu de date generate

# Definirea functiei de log-verosimilitate
log_lik_pois_manual <- function(lambda, x) {
  # lambda trebuie sa fie strict pozitiv
  if (lambda <= 0) return(-Inf)
  
  m <- length(x)
  suma_x <- sum(x)
  
  # Formula: ln(lambda) * sum(x) - m * lambda
  valoare <- log(lambda) * suma_x - m * lambda
  return(valoare)
}

# Gasim punctul de maxim folosind optimize
# Cautam intr-un interval rezonabil pentru lambda (ex: 0.1 la 20)
rezultat_pois <- optimize(f = log_lik_pois_manual, interval = c(0.01, 20), 
                          x = date_pois, maximum = TRUE)

cat("Punctul de maxim (MLE) pentru lambda la Poisson este:", rezultat_pois$maximum)

# Reprezentare grafica
lambda_grid <- seq(0.1, 10, length.out = 100)
y_plot <- sapply(lambda_grid, log_lik_pois_manual, x = date_pois)

plot(lambda_grid, y_plot, type = "l", col = "purple", lwd = 2,
     main = "Log-verosimilitate Poisson",
     xlab = "Parametrul lambda", ylab = "Log-Likelihood")

# Marcam punctul de maxim determinat
abline(v = rezultat_pois$maximum, col = "red", lty = 2)


# Exponentiala
# datele observate
m <- 50
lambda_real <- 0.5
set.seed(101)
date_exp <- rexp(m, rate = lambda_real) # exemplu de date generate

# Definirea functiei de log-verosimilitate
log_lik_exp_manual <- function(lambda, x) {
  # lambda (rata) trebuie sa fie strict pozitiva
  if (lambda <= 0) return(-Inf)
  
  m <- length(x)
  suma_x <- sum(x)
  
  # Formula dedusa: m * ln(lambda) - lambda * sum(x)
  valoare <- m * log(lambda) - lambda * suma_x
  return(valoare)
}

# Gasim punctul de maxim folosind functia optimize
# Pentru exponentiala, MLE-ul teoretic este 1/media(x)
rezultat_exp <- optimize(f = log_lik_exp_manual, interval = c(0.01, 10), 
                         x = date_exp, maximum = TRUE)

cat("Punctul de maxim (MLE) pentru lambda la Exponentiala este:", rezultat_exp$maximum)

# Reprezentare grafica
lambda_grid <- seq(0.01, 2, length.out = 100)
y_plot <- sapply(lambda_grid, log_lik_exp_manual, x = date_exp)

plot(lambda_grid, y_plot, type = "l", col = "red", lwd = 2,
     main = "Log-verosimilitate Exponentiala",
     xlab = "Parametrul lambda (rata)", ylab = "Log-Likelihood")

# Marcam punctul de maxim determinat prin optimize
abline(v = rezultat_exp$maximum, col = "black", lty = 2)


#--------------d-------
# Gamma (fixam alpha, optimizam beta)
alpha_fixat <- 2
beta_real <- 3
date_gamma <- rgamma(50, shape = alpha_fixat, scale = beta_real)

# Functia de log-verosimilitate pentru beta (cu alpha fixat)
# Formula: (alpha-1)*sum(log(x)) - (1/beta)*sum(x) - m*alpha*log(beta) - m*log(gamma(alpha))
log_lik_gamma_beta <- function(beta, x, alpha) {
  if (beta <= 0) return(-Inf)
  m <- length(x)
  valoare <- (alpha - 1) * sum(log(x)) - (1/beta) * sum(x) - 
    m * alpha * log(beta) - m * lgamma(alpha)
  return(valoare)
}

# Optimizare pentru beta
rezultat_gamma <- optimize(f = log_lik_gamma_beta, interval = c(0.1, 20), 
                           x = date_gamma, alpha = alpha_fixat, maximum = TRUE)

cat("Pentru Gamma (alpha=2), maximul pentru beta este:", rezultat_gamma$maximum)

# Grafic
beta_grid <- seq(0.5, 10, length.out = 100)
y_gamma <- sapply(beta_grid, log_lik_gamma_beta, x = date_gamma, alpha = alpha_fixat)
plot(beta_grid, y_gamma, type = "l", col = "blue", lwd = 2,
     main = "Log-verosimilitate Gamma (beta) cu alpha=2 fixat",
     xlab = "Parametrul beta (scale)", ylab = "Log-Likelihood")
abline(v = rezultat_gamma$maximum, col = "red", lty = 2)


# Beta (fixam beta, optimizam alpha)
set.seed(456)
alpha_real <- 2
beta_fixat <- 5
date_beta <- rbeta(50, shape1 = alpha_real, shape2 = beta_fixat)

# Functia de log-verosimilitate pentru alpha (cu beta fixat)
# Formula: (alpha-1)*sum(log(x)) + (beta-1)*sum(log(1-x)) - m*log(beta_func(alpha, beta))
log_lik_beta_alpha <- function(alpha, x, beta) {
  if (alpha <= 0) return(-Inf)
  m <- length(x)
  # log(beta_func) se scrie in R ca lbeta(alpha, beta)
  valoare <- (alpha - 1) * sum(log(x)) + (beta - 1) * sum(log(1 - x)) - 
    m * lbeta(alpha, beta)
  return(valoare)
}

# Optimizare pentru alpha
rezultat_beta <- optimize(f = log_lik_beta_alpha, interval = c(0.1, 10), 
                          x = date_beta, beta = beta_fixat, maximum = TRUE)

cat("Pentru Beta (beta=5), maximul pentru alpha este:", rezultat_beta$maximum)

# Grafic
alpha_grid <- seq(0.5, 5, length.out = 100)
y_beta <- sapply(alpha_grid, log_lik_beta_alpha, x = date_beta, beta = beta_fixat)
plot(alpha_grid, y_beta, type = "l", col = "darkgreen", lwd = 2,
     main = "Log-verosimilitate Beta (alpha) cu beta=5 fixat",
     xlab = "Parametrul alpha (shape1)", ylab = "Log-Likelihood")
abline(v = rezultat_beta$maximum, col = "red", lty = 2)

#--------II.3b------------
# Volumul esantionului cerut
n_volum <- 1000

# --- 1. REPARTITIA GEOMETRICA ---
p_real_geom <- 0.4
date_geom <- rgeom(n_volum, prob = p_real_geom)

# MLE si MM coincid teoretic: 1 / (mean(x) + 1)
est_geom <- 1 / (mean(date_geom) + 1)

cat("GEOMETRICA (p real =", p_real_geom, ")\n")
cat("Estimare (MLE/MM):", round(est_geom, 4), "\n\n")

# --- 2. REPARTITIA BINOMIALA (m=10 incercari) ---
p_real_bin <- 0.7
m_incercari <- 10
date_bin <- rbinom(n_volum, size = m_incercari, prob = p_real_bin)

# MLE si MM: mean(x) / m
est_bin <- mean(date_bin) / m_incercari

cat("BINOMIALA (p real =", p_real_bin, ")\n")
cat("Estimare (MLE/MM):", round(est_bin, 4), "\n\n")

# --- 3. REPARTITIA GAMMA ---
alpha_real <- 2
beta_real <- 3 # scale
date_gamma <- rgamma(n_volum, shape = alpha_real, scale = beta_real)

# Metoda Momentelor (MM)
mean_g <- mean(date_gamma)
var_g <- var(date_gamma)
alpha_mm <- mean_g^2 / var_g
beta_mm <- var_g / mean_g

# Verosimilitate Maxima (MLE) - optimizare numerica
log_lik_gamma <- function(par) {
  -sum(dgamma(date_gamma, shape = par[1], scale = par[2], log = TRUE))
}
res_gamma <- optim(c(1, 1), log_lik_gamma)$par

cat("GAMMA (alpha =", alpha_real, ", beta =", beta_real, ")\n")
cat("MM:   alpha =", round(alpha_mm, 4), ", beta =", round(beta_mm, 4), "\n")
cat("MLE: alpha =", round(res_gamma[1], 4), ", beta =", round(res_gamma[2], 4), "\n\n")

# --- 4. REPARTITIA BETA ---
a_real <- 2
b_real <- 5
date_beta <- rbeta(n_volum, shape1 = a_real, shape2 = b_real)

# Metoda Momentelor (MM)
m1 <- mean(date_beta)
v1 <- var(date_beta)
a_mm <- m1 * (m1 * (1 - m1) / v1 - 1)
b_mm <- (1 - m1) * (m1 * (1 - m1) / v1 - 1)

# Verosimilitate Maxima (MLE)
log_lik_beta <- function(par) {
  -sum(dbeta(date_beta, shape1 = par[1], shape2 = par[2], log = TRUE))
}
res_beta <- optim(c(1, 1), log_lik_beta)$par

cat("BETA (alpha =", a_real, ", beta =", b_real, ")\n")
cat("MM:   alpha =", round(a_mm, 4), ", beta =", round(b_mm, 4), "\n")
cat("MLE: alpha =", round(res_beta[1], 4), ", beta =", round(res_beta[2], 4), "\n")


# --- 5. REPARTITIA POISSON ---
lambda_real_pois <- 5
# Generam date conform distributiei
date_pois <- rpois(n_volum, lambda = lambda_real_pois)

# MLE si MM coincid: media aritmetica (x_bar)
est_pois <- mean(date_pois)

cat("POISSON (lambda real =", lambda_real_pois, ")\n")
cat("Estimare (MLE/MM):", round(est_pois, 4), "\n\n")


# --- 6. REPARTITIA EXPONENTIALA ---
lambda_real_exp <- 0.5
date_exp <- rexp(n_volum, rate = lambda_real_exp)

# MLE si MM coincid: 1 / media aritmetica (1/x_bar)
est_exp <- 1 / mean(date_exp)

cat("EXPONENTIALA (lambda real =", lambda_real_exp, ")\n")
cat("Estimare (MLE/MM):", round(est_exp, 4), "\n\n")


# --- 7. REPARTITIA GAMMA (2 parametri) ---
# Parametrii: alpha (shape) si beta (scale)
alpha_real <- 2
beta_real <- 4 
date_gamma <- rgamma(n_volum, shape = alpha_real, scale = beta_real)

# Metoda Momentelor (MM) - Formule directe din momentele teoretice
mean_g <- mean(date_gamma)
var_g <- var(date_gamma)
alpha_mm <- mean_g^2 / var_g
beta_mm <- var_g / mean_g

# Verosimilitate Maxima (MLE) - Necesita optimizare numerica
# 'par' este vectorul [alpha, beta]
log_lik_gamma <- function(par, date) {
  # Constrangem parametrii sa fie pozitivi
  if(any(par <= 0)) return(Inf)
  # Returnam minus log-verosimilitatea pentru minimizare
  return(-sum(dgamma(date, shape = par[1], scale = par[2], log = TRUE)))
}

res_gamma_mle <- optim(par = c(1, 1), fn = log_lik_gamma, date = date_gamma)$par

cat("GAMMA (alpha real =", alpha_real, ", beta real =", beta_real, ")\n")
cat("MM:   alpha =", round(alpha_mm, 4), ", beta =", round(beta_mm, 4), "\n")
cat("MLE: alpha =", round(res_gamma_mle[1], 4), ", beta =", round(res_gamma_mle[2], 4), "\n")


#----------4. INEGALITATEA BERRY-ESSEEN------------
############################################################
# calculul lui rho = E|X - mu|^3
# pentru mai multe repartitii (discrete si continue)
# Ideea:
#   - Discret:  rho = sum_x |x - mu|^3 * P(X=x)
#   - Continuu: rho = integral |x - mu|^3 * f(x) dx
############################################################

############################
############################
# 1) Functii ajutatoare
############################

# rho pentru o variabila discreta data prin:
#   xs = vector de valori posibile (suport)
#   ps = vector de probabilitati P(X=xs[i])
#   mu = media (E[X])
rho_discrete <- function(xs, ps, mu) {
  sum(abs(xs - mu)^3 * ps)
}

# rho pentru o variabila continua data prin:
#   f  = densitatea f(x)
#   mu = media (E[X])
#   lower, upper = capete de integrare (pot fi -Inf, Inf)
rho_continuous <- function(f, mu, lower, upper) {
  # integrate() vrea o functie care primeste vector x
  g <- function(x) abs(x - mu)^3 * f(x)
  
  # integrate returneaza o lista; valoarea e in $value
  integrate(g, lower = lower, upper = upper, rel.tol = 1e-10)$value
}

# Pentru distributii discrete cu suport infinit (Poisson, Geom),
# facem trunchiere numerica: luam xs = 0..K, unde K e un cuantila mare
# (aproape de 1), de ex. q*(1 - eps).
#
# eps mic => K mai mare => aproximare mai buna, dar mai lenta.
default_eps <- 1e-12


############################
# 2) Distributii discrete
############################

# 2.1) Binomiala X ~ Bin(m, p), suport {0,1,...,m}
rho_binom <- function(m, p) {
  xs <- 0:m
  ps <- dbinom(xs, size = m, prob = p)
  mu <- m * p
  rho_discrete(xs, ps, mu)
}

# 2.2) Geometrica (R): X ~ Geom(p), suport {0,1,2,...}
# Interpretare R: X = nr. de esecuri inainte de primul succes
# P(X=k) = (1-p)^k * p
# Media: mu = (1-p)/p
rho_geom <- function(p, eps = default_eps) {
  # alegem un K suficient de mare incat P(X <= K) ~ 1 - eps
  K <- qgeom(1 - eps, prob = p)  # cuantila pentru suport 0..Inf
  
  xs <- 0:K
  ps <- dgeom(xs, prob = p)
  mu <- (1 - p) / p
  
  rho_discrete(xs, ps, mu)
}

# 2.3) Poisson X ~ Pois(lambda), suport {0,1,2,...}
# Media: mu = lambda
rho_pois <- function(lambda, eps = default_eps) {
  K <- qpois(1 - eps, lambda = lambda)
  
  xs <- 0:K
  ps <- dpois(xs, lambda = lambda)
  mu <- lambda
  
  rho_discrete(xs, ps, mu)
}

# 2.4) Uniforma discreta pe {a, a+1, ..., b}
# Media: mu = (a+b)/2
rho_unif_disc <- function(a, b) {
  xs <- a:b
  N  <- length(xs)
  ps <- rep(1 / N, N)
  mu <- (a + b) / 2
  
  rho_discrete(xs, ps, mu)
}


############################
# 3) Distributii continue
############################

# 3.1) Uniforma continua X ~ U(a, b)
# Exista formula inchisa: rho = (b - a)^3 / 32
# O dau direct (e exact), dar poti verifica si numeric cu integrate().
rho_unif_cont <- function(a, b) {
  (b - a)^3 / 32
}

# 3.2) Exponentiala X ~ Exp(rate = lambda)
# Media: mu = 1/lambda, suport [0, Inf)
# Facem numeric cu integrate().
rho_exp <- function(lambda) {
  mu <- 1 / lambda
  f  <- function(x) dexp(x, rate = lambda)
  rho_continuous(f, mu, lower = 0, upper = Inf)
}

# 3.3) Gamma X ~ Gamma(shape = alpha, rate = beta)
# (Atentie: in R, dgamma foloseste shape si rate; scale = 1/rate)
# Media: mu = alpha / beta, suport [0, Inf)
rho_gamma <- function(alpha, beta) {
  mu <- alpha / beta
  f  <- function(x) dgamma(x, shape = alpha, rate = beta)
  rho_continuous(f, mu, lower = 0, upper = Inf)
}

# 3.4) Beta X ~ Beta(alpha, beta), suport [0, 1]
# Media: mu = alpha / (alpha + beta)
rho_beta <- function(alpha, beta) {
  mu <- alpha / (alpha + beta)
  f  <- function(x) dbeta(x, shape1 = alpha, shape2 = beta)
  rho_continuous(f, mu, lower = 0, upper = 1)
}


############################################################
#Grafice pentru rho ca functie de parametru
# Folosim functiile rho_* definite la 7(c).
############################################################

############################
# 1) Binomiala: rho(p) pentru m fix
############################
m_fixed <- 20
p_grid  <- seq(0.01, 0.99, by = 0.01)   # evitam 0 si 1 (degenerate)

rho_binom_vals <- sapply(p_grid, function(p) rho_binom(m = m_fixed, p = p))

plot(p_grid, rho_binom_vals, type = "l",
     xlab = "p", ylab = expression(rho),
     main = paste0("Binomiala: rho(p), m = ", m_fixed))

############################
# 2) Geometrica (conventia R): rho(p)
############################
p_grid_geom <- seq(0.05, 0.95, by = 0.01)  # evitam p prea mic (coada foarte lunga)

rho_geom_vals <- sapply(p_grid_geom, function(p) rho_geom(p))

plot(p_grid_geom, rho_geom_vals, type = "l",
     xlab = "p", ylab = expression(rho),
     main = "Geometrica (R): rho(p)")

############################
# 3) Poisson: rho(lambda)
############################
lambda_grid <- seq(0.5, 20, by = 0.5)

rho_pois_vals <- sapply(lambda_grid, function(lam) rho_pois(lam))

plot(lambda_grid, rho_pois_vals, type = "l",
     xlab = expression(lambda), ylab = expression(rho),
     main = expression(paste("Poisson: ", rho(lambda))))

############################
# 4) Uniforma discreta: rho in functie de "latime" N
# Exemplu: suport {1,2,...,N} (a=1, b=N)
############################
N_grid <- 2:50  # N=1 ar fi degenerata (rho=0)
rho_unif_disc_vals <- sapply(N_grid, function(N) rho_unif_disc(a = 1, b = N))

plot(N_grid, rho_unif_disc_vals, type = "l",
     xlab = "N (numar valori in suport)", ylab = expression(rho),
     main = "Uniforma discreta pe {1,...,N}: rho(N)")

############################
# 5) Uniforma continua: rho in functie de lungimea intervalului L = b-a
# Exemplu: U(0, L)
############################
L_grid <- seq(0.1, 10, by = 0.1)
rho_unif_cont_vals <- (L_grid^3) / 32  # formula exacta, deoarece a=0, b=L => (b-a)^3/32

plot(L_grid, rho_unif_cont_vals, type = "l",
     xlab = "L = b - a", ylab = expression(rho),
     main = "Uniforma continua U(0,L): rho(L) = L^3/32")

############################
# 6) Exponentiala: rho(lambda) (rate lambda)
############################
lambda_grid_exp <- seq(0.2, 5, by = 0.1)  # lambda>0

rho_exp_vals <- sapply(lambda_grid_exp, function(lam) rho_exp(lam))

plot(lambda_grid_exp, rho_exp_vals, type = "l",
     xlab = expression(lambda), ylab = expression(rho),
     main = expression(paste("Exponentiala: ", rho(lambda), " (rate)")))

############################
# 7) Gamma: rho(alpha) cu rate beta fix
############################
beta_fixed <- 1
alpha_grid <- seq(0.5, 10, by = 0.25)  # alpha>0

rho_gamma_vals <- sapply(alpha_grid, function(a) rho_gamma(alpha = a, beta = beta_fixed))

plot(alpha_grid, rho_gamma_vals, type = "l",
     xlab = expression(alpha), ylab = expression(rho),
     main = bquote("Gamma: " ~ rho(alpha) ~ ", rate" ~ beta == .(beta_fixed)))

############################
# 8) Beta: rho(alpha) cu beta fix (sau invers)
############################
beta_fixed_beta <- 2
alpha_grid_beta <- seq(0.5, 10, by = 0.25)

rho_beta_vals <- sapply(alpha_grid_beta, function(a) rho_beta(alpha = a, beta = beta_fixed_beta))

plot(alpha_grid_beta, rho_beta_vals, type = "l",
     xlab = expression(alpha), ylab = expression(rho),
     main = bquote("Beta: " ~ rho(alpha) ~ ", " ~ beta == .(beta_fixed_beta)))


############################################################
# EXERCITIUL 4(c)
# Construim in R:
#   1) functie care calculeaza rho 
#   2) functie care calculeaza marginea Berry–Esseen
#   3) functie care aproximeaza numeric sup_x |F_n(x) - Phi(x)|
############################################################


############################
# 2) Marginea Berry–Esseen
############################
# Inegalitatea Berry–Esseen (forma din proiect):
#   sup_x |F_n(x) - Phi(x)| <= C * rho / (sigma^3 * sqrt(n))
#
# - n = volumul esantionului
# - rho = E|X - mu|^3
# - sigma = sd(X)
# - C = constanta (in teorie exista o constanta universala; in exercitiul 5 o estimati)
berry_esseen_bound <- function(n, rho, sigma, C = 0.56) {
  if (any(n <= 0)) stop("n trebuie sa fie pozitiv.")
  if (any(sigma <= 0)) stop("sigma trebuie sa fie > 0.")
  
  C * rho / (sigma^3 * sqrt(n))
}


############################
# 3) Aproximare numerica a sup_x |F_n(x) - Phi(x)|
############################
# In practica, supremumul se aproximeaza pe o grila fina de x.
# Grila o alegem folosind cuantilele normale (cum zice cerinta):
#   x_grid = qnorm(seq(eps, 1-eps, ...))
make_x_grid <- function(eps = 1e-4, n_points = 3000) {
  qnorm(seq(eps, 1 - eps, length.out = n_points))
}

# Functie generica: primeste Fn(x) si o grila x_grid,
# returneaza max_x |Fn(x) - Phi(x)| pe grila.
sup_diff_numeric <- function(Fn, x_grid) {
  diffs <- abs(Fn(x_grid) - pnorm(x_grid))
  max(diffs)
}


############################################################
# 4) Functii Fn(x) pentru Z_n
############################################################
# Zn = sqrt(n) * (Xbar - mu) / sigma
# Fn(x) = P(Zn <= x) = P(Xbar <= mu + x*sigma/sqrt(n))

# 4.1) Fn exact pentru Binomiala:
# Daca Xi ~ Bin(m,p), atunci Sn = sum Xi ~ Bin(n*m, p).
Fn_Zn_binom <- function(x, n, m, p) {
  mu <- m * p
  sigma <- sqrt(m * p * (1 - p))
  
  # prag pentru media:
  t_bar <- mu + x * sigma / sqrt(n)
  # prag pentru suma:
  t_sum <- n * t_bar
  
  # Sn discret => folosim floor
  pbinom(floor(t_sum), size = n * m, prob = p)
}

# 4.2) Fn exact pentru Poisson:
# Daca Xi ~ Pois(lambda), atunci Sn ~ Pois(n*lambda).
Fn_Zn_pois <- function(x, n, lambda) {
  mu <- lambda
  sigma <- sqrt(lambda)
  
  t_bar <- mu + x * sigma / sqrt(n)
  t_sum <- n * t_bar
  
  ppois(floor(t_sum), lambda = n * lambda)
}


############################################################
# 5) Varianta universala: Fn(x) prin SIMULARE (merge pentru orice distributie)
############################################################
# Ideea:
#   - simulezi M replicari ale lui Zn
#   - Fn(x) se aproximeaza cu CDF empirica: mean(Zn_sim <= x)
#
# rX = o functie care genereaza n valori din distributia lui X (ex: rpois, rbinom etc.)
# mu, sigma = parametrii teoretici ai lui X (ca sa standardizam corect)
Fn_Zn_sim <- function(x, n, rX, mu, sigma, M = 20000) {
  # generam M valori ale lui Zn
  Z <- numeric(M)
  for (i in 1:M) {
    X <- rX(n)                # esantion de marime n
    Xbar <- mean(X)
    Z[i] <- sqrt(n) * (Xbar - mu) / sigma
  }
  
  # CDF empirica in punctele x (vectorizat cu sapply)
  sapply(x, function(xx) mean(Z <= xx))
}

# EXERCITIUL 7(d)
# Dataframe cu marginile Berry–Esseen pentru mai multe distributii
# si mai multe valori n (FARA GRAFIC)
############################################################

set.seed(123)

# Valori pentru n (poti schimba)
n_values <- c(10, 20, 30, 50, 100, 200)

# Constanta C (poate fi schimbata)
C_const <- 0.56

############################################################
# Scenarii: distributie + parametri + mu, sigma, rho
############################################################

scenarios <- list(
  list(name = "Binom(m=20,p=0.3)",
       mu = 20 * 0.3,
       sigma = sqrt(20 * 0.3 * 0.7),
       rho = rho_binom(20, 0.3)),
  
  # Geometrica in R: suport {0,1,2,...} (failures)
  list(name = "Geom(p=0.3) [R failures]",
       mu = (1 - 0.3) / 0.3,
       sigma = sqrt((1 - 0.3) / (0.3^2)),
       rho = rho_geom(0.3)),
  
  list(name = "Pois(lambda=5)",
       mu = 5,
       sigma = sqrt(5),
       rho = rho_pois(5)),
  
  list(name = "UnifDisc(0..9)",
       mu = (0 + 9) / 2,
       sigma = sqrt((10^2 - 1) / 12),   # N = 10 valori (0..9)
       rho = rho_unif_disc(0, 9)),
  
  list(name = "UnifCont(0,1)",
       mu = 0.5,
       sigma = 1 / sqrt(12),
       rho = rho_unif_cont(0, 1)),
  
  list(name = "Exp(lambda=2)",
       mu = 1 / 2,
       sigma = 1 / 2,
       rho = rho_exp(2)),
  
  list(name = "Gamma(alpha=2,beta=1) [shape=2,rate=1]",
       mu = 2 / 1,
       sigma = sqrt(2) / 1,   # sd = sqrt(shape)/rate
       rho = rho_gamma(2, 1)),
  
  list(name = "Beta(alpha=2,beta=5)",
       mu = 2 / (2 + 5),
       sigma = sqrt(2 * 5 / ((2 + 5)^2 * (2 + 5 + 1))),
       rho = rho_beta(2, 5))
)

############################################################
# Construim dataframe-ul final
############################################################

df_BE <- do.call(rbind, lapply(scenarios, function(sc) {
  data.frame(
    dist = sc$name,
    n = n_values,
    mu = sc$mu,
    sigma = sc$sigma,
    rho = sc$rho,
    C = C_const,
    BE_bound = berry_esseen_bound(
      n = n_values,
      rho = sc$rho,
      sigma = sc$sigma,
      C = C_const
    )
  )
}))

# Afisare tabel
print(df_BE)


############################################################
# EXERCITIUL 4(e)
# Grafic: |F_n(x) - Phi(x)| in functie de x, pentru mai multe n
############################################################

# Grila de x (din cuantile normale)
xg <- make_x_grid(eps = 1e-4, n_points = 2000)

# Fix util daca iti apare "figure margins too large"
par(mar = c(4, 4, 3, 1))

############################################################
# Functie: curba |Fn - Phi| pentru un Fn dat
############################################################
diff_curve <- function(Fn, x_grid) {
  abs(Fn(x_grid) - pnorm(x_grid))
}

############################################################
# Simulare RAPIDA pentru Zn + Fn empiric (ecdf)
############################################################
Zn_sim_fast <- function(n, rX, mu, sigma, M = 30000) {
  # genereaza M replicari ale lui Zn rapid, fara for
  X <- matrix(rX(n * M), nrow = M, ncol = n)
  Z <- sqrt(n) * (rowMeans(X) - mu) / sigma
  Z
}

Fn_from_Z <- function(Z) {
  ecdf(Z)  # returneaza o functie Fn(x)
}

############################################################
# Functie: ploteaza evolutia |Fn - Phi| pentru mai multe n
# + optional linia Berry–Esseen
############################################################
plot_diff_evolution <- function(n_values, Fn_builder, title,
                                add_BE = FALSE, rho = NULL, sigma = NULL, C = 0.56) {
  # prima curba (ca sa setam axele)
  Fn0 <- Fn_builder(n_values[1])
  d0  <- diff_curve(Fn0, xg)
  
  plot(xg, d0, type = "l",
       xlab = "x", ylab = expression(abs(F[n](x) - Phi(x))),
       main = title)
  
  # restul curbelor
  if (length(n_values) > 1) {
    for (k in 2:length(n_values)) {
      Fn_k <- Fn_builder(n_values[k])
      dk   <- diff_curve(Fn_k, xg)
      lines(xg, dk)
    }
  }
  
  legend("topright",
         legend = paste0("n = ", n_values),
         lty = 1, bty = "n")
  
  # optional: linie Berry–Esseen (pentru fiecare n ar fi alta;
  # punem cea mai mare (n minim) ca referinta conservatoare)
  if (add_BE) {
    if (is.null(rho) || is.null(sigma)) stop("Pentru BE trebuie sa dai rho si sigma.")
    bound_max <- berry_esseen_bound(n = min(n_values), rho = rho, sigma = sigma, C = C)
    abline(h = bound_max, lty = 2)
  }
  
  # returnam si un mic rezumat numeric (sup pe grila)
  sups <- sapply(n_values, function(nn) {
    Fn_n <- Fn_builder(nn)
    max(diff_curve(Fn_n, xg))
  })
  data.frame(n = n_values, sup_on_grid = sups)
}
n_show <- c(10, 30, 100, 200)


############################################################
# (1) BINOMIALA – Fn exact
############################################################
m <- 20; p <- 0.3
rho_b <- rho_binom(m, p)
sigma_b <- sqrt(m * p * (1 - p))

res_binom <- plot_diff_evolution(
  n_values = n_show,
  Fn_builder = function(n) function(x) Fn_Zn_binom(x, n = n, m = m, p = p),
  title = paste0("Binomiala: |Fn(x) - Phi(x)| (m=", m, ", p=", p, ")"),
  add_BE = TRUE, rho = rho_b, sigma = sigma_b, C = 0.56
)
print(res_binom)


############################################################
# (2) POISSON – Fn exact
############################################################
lambda <- 5
rho_p <- rho_pois(lambda)
sigma_p <- sqrt(lambda)

res_pois <- plot_diff_evolution(
  n_values = n_show,
  Fn_builder = function(n) function(x) Fn_Zn_pois(x, n = n, lambda = lambda),
  title = paste0("Poisson: |Fn(x) - Phi(x)| (lambda=", lambda, ")"),
  add_BE = TRUE, rho = rho_p, sigma = sigma_p, C = 0.56
)
print(res_pois)


############################################################
# (3) EXEMPLU CONTINUU (EXPONENTIALA) – Fn prin simulare
############################################################
lambda_e <- 2
mu_e <- 1 / lambda_e
sigma_e <- 1 / lambda_e
rho_e <- rho_exp(lambda_e)

# aici Fn_builder simuleaza Z o data pentru fiecare n (rapid), apoi foloseste ecdf
res_exp <- plot_diff_evolution(
  n_values = n_show,
  Fn_builder = function(n) {
    Z <- Zn_sim_fast(n, rX = function(k) rexp(k, rate = lambda_e),
                     mu = mu_e, sigma = sigma_e, M = 40000)
    Fn_from_Z(Z)
  },
  title = paste0("Exponentiala (sim): |Fn(x) - Phi(x)| (lambda=", lambda_e, ")"),
  add_BE = TRUE, rho = rho_e, sigma = sigma_e, C = 0.56
)
print(res_exp)


############################################################
# EXERCITIUL 5(a)
# Definim C_n = sup_x |F_n(x) - Phi(x)| * sigma^3 * sqrt(n) / rho
# Functie care calculeaza C_n pentru un anumit n,
# folosind:
#   - Fn(x): CDF pentru Z_n (poate fi exact sau prin simulare)
#   - rho, sigma: valori teoretice
#   - x_grid: grila pe care aproximam supremumul
Cn_from_Fn <- function(n, Fn, rho, sigma, x_grid = make_x_grid()) {
  sup_val <- sup_diff_numeric(Fn, x_grid)  # aprox sup |Fn - Phi|
  Cn <- sup_val * (sigma^3) * sqrt(n) / rho
  list(Cn = Cn, sup_val = sup_val)
}

# Varianta universala: folosim Fn prin simulare (merge pt orice distributie)
# rX: functie de generare (ex: function(nn) rexp(nn, rate=2))
Cn_sim <- function(n, rX, mu, sigma, rho, M = 20000, x_grid = make_x_grid()) {
  Fn_hat <- function(x) Fn_Zn_sim(x, n = n, rX = rX, mu = mu, sigma = sigma, M = M)
  Cn_from_Fn(n = n, Fn = Fn_hat, rho = rho, sigma = sigma, x_grid = x_grid)
}

# Pentru Binomiala si Poisson putem folosi Fn exact (mai precis si rapid)
Cn_binom_exact <- function(n, m, p, x_grid = make_x_grid()) {
  rho <- rho_binom(m, p)
  sigma <- sqrt(m * p * (1 - p))
  Fn <- function(x) Fn_Zn_binom(x, n = n, m = m, p = p)
  Cn_from_Fn(n = n, Fn = Fn, rho = rho, sigma = sigma, x_grid = x_grid)
}

Cn_pois_exact <- function(n, lambda, x_grid = make_x_grid()) {
  rho <- rho_pois(lambda)
  sigma <- sqrt(lambda)
  Fn <- function(x) Fn_Zn_pois(x, n = n, lambda = lambda)
  Cn_from_Fn(n = n, Fn = Fn, rho = rho, sigma = sigma, x_grid = x_grid)
}


############################################################
# EXERCITIUL 5(b)
# Valorile lui C_n estimate prin simulare Monte Carlo
# pentru n in {30, 50, 100, 200, 300, 400, 500}.
#
# Definitie:
#   C_n = sup_x |F_n(x) - Phi(x)| * sigma^3 * sqrt(n) / rho
# unde F_n este CDF-ul lui Z_n = sqrt(n) (Xbar - mu) / sigma.
############################################################

set.seed(123)

n_values <- c(30, 50, 100, 200, 300, 400, 500)

M_MC <- 20000

# grila pentru aproximarea supremului (cu cuantile normale)
x_grid <- make_x_grid(eps = 1e-4, n_points = 3000)

############################################################
# Functie generala: estimeaza C_n prin simulare
############################################################

estimate_Cn_MC <- function(n, rX, mu, sigma, rho, M = 20000, x_grid = x_grid) {
  
  # construim Fn estimat prin CDF empirica a lui Z_n simulat
  Fn_hat <- function(x) Fn_Zn_sim(x, n = n, rX = rX, mu = mu, sigma = sigma, M = M)
  
  # sup_x |Fn - Phi| aproximat pe grila
  sup_val <- sup_diff_numeric(Fn_hat, x_grid)
  
  # definim C_n
  Cn <- sup_val * sigma^3 * sqrt(n) / rho
  
  # returnam si sup_val (uneori e util in raport)
  list(Cn = Cn, sup_val = sup_val)
}

############################################################
# Definim scenariile (distributie + parametri + mu,sigma,rho + generator rX)
# Parametrii ii alegem ca in 7(d) (poti schimba daca vrei).
############################################################

scenarios_8 <- list(
  
  list(
    name = "Binom(m=20,p=0.3)",
    mu = 20 * 0.3,
    sigma = sqrt(20 * 0.3 * 0.7),
    rho = rho_binom(20, 0.3),
    rX = function(nn) rbinom(nn, size = 20, prob = 0.3)
  ),
  
  # Geometrica in R: suport {0,1,2,...} ("failures")
  list(
    name = "Geom(p=0.3) [R failures]",
    mu = (1 - 0.3) / 0.3,
    sigma = sqrt((1 - 0.3) / 0.3^2),
    rho = rho_geom(0.3),
    rX = function(nn) rgeom(nn, prob = 0.3)
  ),
  
  list(
    name = "Pois(lambda=5)",
    mu = 5,
    sigma = sqrt(5),
    rho = rho_pois(5),
    rX = function(nn) rpois(nn, lambda = 5)
  ),
  
  # Uniforma discreta pe {0,1,...,9}
  list(
    name = "UnifDisc(0..9)",
    mu = (0 + 9) / 2,
    sigma = sqrt(((10^2) - 1) / 12),  # N=10
    rho = rho_unif_disc(0, 9),
    rX = function(nn) sample(0:9, size = nn, replace = TRUE)
  ),
  
  # Uniforma continua U(0,1)
  list(
    name = "UnifCont(0,1)",
    mu = 0.5,
    sigma = 1 / sqrt(12),
    rho = rho_unif_cont(0, 1),
    rX = function(nn) runif(nn, min = 0, max = 1)
  ),
  
  # Exponentiala Exp(rate=2)
  list(
    name = "Exp(lambda=2)",
    mu = 1 / 2,
    sigma = 1 / 2,
    rho = rho_exp(2),
    rX = function(nn) rexp(nn, rate = 2)
  ),
  
  # Gamma(shape=2, rate=1)  (in codul tau: rho_gamma(alpha,beta))
  list(
    name = "Gamma(alpha=2,beta=1)",
    mu = 2,
    sigma = sqrt(2),
    rho = rho_gamma(2, 1),
    rX = function(nn) rgamma(nn, shape = 2, rate = 1)
  ),
  
  # Beta(2,5)
  list(
    name = "Beta(alpha=2,beta=5)",
    mu = 2 / 7,
    sigma = sqrt(2 * 5 / ((7^2) * 8)),
    rho = rho_beta(2, 5),
    rX = function(nn) rbeta(nn, shape1 = 2, shape2 = 5)
  )
)

############################################################
# Rulam estimarea pentru fiecare distributie si fiecare n
# si construim dataframe-ul final
############################################################

df_Cn <- do.call(rbind, lapply(scenarios_8, function(sc) {
  
  # pentru fiecare n calculam Cn (si optional sup_val)
  out <- lapply(n_values, function(n) {
    est <- estimate_Cn_MC(n = n, rX = sc$rX, mu = sc$mu, sigma = sc$sigma, rho = sc$rho,
                          M = M_MC, x_grid = x_grid)
    data.frame(n = n, Cn = est$Cn, sup_val = est$sup_val)
  })
  
  tmp <- do.call(rbind, out)
  tmp$dist <- sc$name
  tmp$mu <- sc$mu
  tmp$sigma <- sc$sigma
  tmp$rho <- sc$rho
  tmp$M <- M_MC
  tmp
}))

# rearanjam coloanele ca sa fie mai clar
df_Cn <- df_Cn[, c("dist", "n", "Cn", "sup_val", "mu", "sigma", "rho", "M")]

print(df_Cn)


############################################################
# EXERCITIUL 5(c)
# Analizam cum variaza C_n in functie de n si intre distributii.
############################################################

# 1) Rezumat: pentru fiecare distributie, luam valorile Cn la n mari
# (de ex. n >= 100) si calculam media si maximul.
df_large_n <- subset(df_Cn, n >= 100)

summary_Cn <- aggregate(Cn ~ dist, data = df_large_n,
                        FUN = function(v) c(mean = mean(v), max = max(v), min = min(v)))

# aggregate intoarce o coloana "Cn" cu vectori; o "despachetam"
summary_Cn <- do.call(data.frame, summary_Cn)
colnames(summary_Cn) <- c("dist", "Cn_mean", "Cn_max", "Cn_min")

print(summary_Cn)

# 2) "C recomandat" (empiric): luam maximul lui Cn pe n mari, peste toate distributiile
C_recommended <- max(df_large_n$Cn)
C_recommended


###########################################################
# EXERCITIUL 5(d)
# Prezentare rezultate: tabele + grafice pentru C_n
############################################################

# Presupunem ca ai df_Cn din 8(b)
# df_Cn are coloane: dist, n, Cn, sup_val, mu, sigma, rho, M

############################
# 1) Tabel "wide": cate o coloana pentru fiecare n
############################
# Convertim df_Cn din "long" in "wide" ca sa fie usor de pus in raport

df_wide <- reshape(df_Cn[, c("dist", "n", "Cn")],
                   timevar = "n", idvar = "dist", direction = "wide")

# Ca sa arate frumos: ordonam coloanele crescator dupa n
# (reshape pune Cn.30, Cn.50, etc.)
df_wide <- df_wide[, c("dist", sort(setdiff(names(df_wide), "dist")))]

print(df_wide)

############################
# 2) Rezumat pe distributie (ex: media/max pe n mari)
############################
df_large_n <- subset(df_Cn, n >= 100)

summary_Cn <- aggregate(Cn ~ dist, data = df_large_n,
                        FUN = function(v) c(mean = mean(v), max = max(v), min = min(v)))
summary_Cn <- do.call(data.frame, summary_Cn)
colnames(summary_Cn) <- c("dist", "Cn_mean_n>=100", "Cn_max_n>=100", "Cn_min_n>=100")

print(summary_Cn)

############################
# 3) Grafice separate (2x4): Cn vs n pentru fiecare distributie
############################
dists <- unique(df_Cn$dist)

par(mfrow = c(2, 4))
for (d in dists) {
  tmp <- subset(df_Cn, dist == d)
  plot(tmp$n, tmp$Cn, type = "b",
       xlab = "n", ylab = expression(C[n]),
       main = d)
}
par(mfrow = c(1, 1))

############################
# 4) Grafic comparativ: toate distributiile pe aceeasi figura
############################
# Facem un plot gol, apoi adaugam linii pentru fiecare dist.

plot(range(df_Cn$n), range(df_Cn$Cn),
     type = "n", xlab = "n", ylab = expression(C[n]),
     main = expression(paste("Comparatie ", C[n], " intre distributii")))

for (d in dists) {
  tmp <- subset(df_Cn, dist == d)
  lines(tmp$n, tmp$Cn, type = "b")
}

legend("topright", legend = dists, cex = 0.6, bty = "n")