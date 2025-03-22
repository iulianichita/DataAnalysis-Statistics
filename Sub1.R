#SUBIECT 1

#a
# Setăm parametrii problemei
n <- 10  # Numărul de etape
lambda <- 1  # Parametrul ratei pentru distribuția exponențială
alpha <- rep(0.8, n)  # Probabilitățile de a continua la fiecare etapă (pot fi ajustate)
num_simulations <- 6^5  # Numărul de simulări - ar trebui 6^10 dar dureaza prea mult 

# Funcția pentru a simula timpul total T pentru o singură realizare
simulate_T <- function() {
  T_total <- 0
  for (i in 1:n) {
    Ti <- rexp(1, lambda)  # Generăm timpul pentru etapa i
    T_total <- T_total + Ti
    if (runif(1) > alpha[i]) {  # Decidem dacă ne oprim
      break
    }
  }
  return(T_total)
}

# Simulăm valorile lui T
set.seed(123)  # Pentru reproducibilitate
T_values <- replicate(num_simulations, simulate_T())

# Aproximăm E(T)
E_T <- mean(T_values)
cat("E(T) aproximat:", E_T, "\n")

# Reprezentăm grafic distribuția lui T
hist(T_values, breaks = 100, probability = TRUE, 
     main = "Distribuția empirică a lui T", 
     xlab = "Timp total (T)", 
     col = "skyblue", border = "white")
lines(density(T_values), col = "red", lwd = 2)

#b
# Parametrii problemei
n <- 10  # Numărul de etape
lambda <- 1  # Parametrul ratei pentru distribuția exponențială
alpha <- rep(0.8, n)  # Probabilitățile de tranziție

# Calculăm valoarea teoretică a lui E(T)
E_T_theoretical <- 0
prod_alpha <- 1  # Inițializarea produsului probabilităților
for (i in 1:n) {
  E_T_theoretical <- E_T_theoretical + (1 / lambda) * prod_alpha
  prod_alpha <- prod_alpha * alpha[i]  # Actualizăm produsul probabilităților
}

cat("E(T) teoretic:", E_T_theoretical, "\n")

cat("E(T) aproximat prin simulare:", E_T, "\n")
cat("E(T) teoretic:", E_T_theoretical, "\n")
cat("Diferența:", abs(E_T - E_T_theoretical), "\n")


#c
# Funcția pentru o singură simulare, verificând finalizarea
simulate_T_and_check_completion <- function() {
  for (i in 1:n) {
    rexp(1, lambda)  # Generăm timpul (nu îl stocăm aici)
    if (runif(1) > alpha[i]) {  # Decidem dacă ne oprim
      return(FALSE)  # Activitatea nu a fost finalizată
    }
  }
  return(TRUE)  # Activitatea a fost finalizată
}

# Simulăm toate valorile
set.seed(123)  # Pentru reproducibilitate
completions <- replicate(num_simulations, simulate_T_and_check_completion())

# Probabilitatea aproximată
P_completion_simulated <- mean(completions)
cat("Probabilitatea aproximată prin simulare:", P_completion_simulated, "\n")

# Probabilitatea teoretică
P_completion_theoretical <- prod(alpha)
cat("Probabilitatea teoretică:", P_completion_theoretical, "\n")

# Diferența dintre simulare și valoarea teoretică
cat("Diferența:", abs(P_completion_simulated - P_completion_theoretical), "\n")



#d
sigma <- 5  # Valoarea pragului pentru timp (σ)
# Aproximăm probabilitatea
P_T_leq_sigma <- mean(T_values <= sigma)
cat("Probabilitatea aproximată ca T <= sigma:", P_T_leq_sigma, "\n")

# Distribuția valorilor T (opțional, pentru vizualizare)
hist(T_values, breaks = 30, main = "Distribuția valorilor T", xlab = "Timp total (T)", col = "skyblue", border = "white")
abline(v = sigma, col = "red", lwd = 2, lty = 2)  # Linia pragului sigma


#e
# Determinăm timpul minim și timpul maxim
T_min <- min(T_values)
T_max <- max(T_values)

cat("Timpul minim de finalizare:", T_min, "\n")
cat("Timpul maxim de finalizare:", T_max, "\n")

# Reprezentăm grafic timpii de finalizare
hist(T_values, breaks = 30, main = "Distribuția timpilor de finalizare", 
     xlab = "Timp total (T)", col = "lightblue", border = "white")
abline(v = T_min, col = "red", lwd = 2, lty = 2)  # Linie pentru T_min
abline(v = T_max, col = "blue", lwd = 2, lty = 2)  # Linie pentru T_max
legend("topright", legend = c("T_min", "T_max"), col = c("red", "blue"), lwd = 2, lty = 2)


#f
simulate_steps <- function() {
  for (i in 1:n) {
    if (runif(1) > alpha[i]) {  # Probabilitatea de oprire
      return(i)  # Ne oprim la etapa i
    }
  }
  return(n + 1)  # Dacă finalizăm toate etapele
}

# Simulăm toate valorile
completed_steps <- replicate(num_simulations, simulate_steps())

# Calculăm probabilitățile
probabilities <- sapply(1:n, function(k) {
  mean(completed_steps < k)  # Probabilitatea de oprire înainte de etapa k
})

# Reprezentăm grafic probabilitățile
barplot(probabilities, names.arg = 1:n, col = "skyblue", border = "white",
        main = "Probabilitatea de oprire înainte de etapa k", 
        xlab = "Etapa k", ylab = "Probabilitatea de oprire")


