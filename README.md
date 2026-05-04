# Simulare Statistică și Familie Exponențială

Acest proiect analizează inegalități fundamentale și metode de estimare statistică prin simulări numerice în limbajul R.

## Componente Principale

### 1. Inegalitatea Kantorovich & Demonstrația lui Anderson
* Reconstrucția prin simulare a demonstrației lui Anderson (1971).[cite: 3]
* Verificarea limitei teoretice $B = \frac{(M+m)^2}{4Mm}$ pentru diverse seturi de valori proprii.[cite: 2, 3]
* Analiza heterogenității prin raportul $A/H$ pe distribuții Uniforme, Beta, Bimodale și Discrete.[cite: 3]

### 2. Familia Exponențială & Estimatori
* Studiul apartenenței la familia exponențială (Binomială, Poisson, Gamma, Beta, Exponențială).[cite: 3]
* Demonstrarea concavității funcției de log-verosimilitate.[cite: 3]
* Compararea estimațiilor MLE (Maximum Likelihood) și MM (Metoda Momentelor) pe eșantioane de $n=1000$.

### 3. Inegalitatea Berry-Esseen
* Calculul numeric al momentului de ordin 3 ($\rho$).
* Analiza evoluției diferenței $|F_n(x) - \Phi(x)|$ și a vitezei de convergență către normalitate.[cite: 2, 3]

## Tehnologii
* **Limbaj:** R.[cite: 2]
* **Tehnici:** Simulări Monte Carlo, Optimizare numerică (`optimize`, `optim`).[cite: 2]
