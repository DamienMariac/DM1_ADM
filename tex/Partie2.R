
# Partie 2 : 
m(list = ls())
data <- read.csv("./Datagenus.csv", sep=";")
data2 <- data[1 :1000,] #On enlève la ligne qui pose problème

##### Rappel des résulats obtenu dans la partie 1 #### 

# Sélectionner les colonnes des espèces
espece <- paste0("gen", 1:27)

# Calculer la densité de peuplement pour chaque espèce
densité <- data2[espece] / data2$surface
densité <- as.matrix(densité) # Convertir la dataframe densité en matrice densité
# Calcul de la moyenne des densités pour chaque espèce
moyenne_densité <- colMeans(densité)
#Centrons nos densité :
densités_centrées <- densité - matrix(moyenne_densité, nrow = nrow(densité), ncol = ncol(densité), byrow = TRUE)
#On calcul les écarts-types
ecarts_type_densité <- apply(densités_centrées, 2, sd)
#Enfin on standarise nos densité
densités_standardisées <- densités_centrées / matrix(ecarts_type_densité, nrow = nrow(densité), ncol = ncol(densité), byrow = TRUE)

##### Commencement partie 2 #####

# On créer les matrices du sujet

X <- as.matrix(densités_standardisées)

Y <- matrix(0, nrow = 1000, ncol = 7)
# Remplir la matrice Y
for (i in 1:nrow(Y)) {
  type <- data2$forest[i]  # Récupérer le type de la parcelle i
  if (type >= 1 && type <= 7) {  # Assurer que le type est entre 1 et 7
    Y[i, type] <- 1  # Mettre 1 dans la colonne correspondante
  }
}

Z <- matrix(0, nrow = 1000, ncol = 5)
# Remplir la matrice Z
for (i in 1:nrow(Y)) {
  type <- data2$geology[i]  # Récupérer le type de la parcelle i
  if (type==5){
    type =4
    Z[i, type] <- 1  # Mettre 1 dans la colonne correspondante
  }
  if (type==6){
    type = 5
    Z[i, type] <- 1  # Mettre 1 dans la colonne correspondante
  }
  else{
    Z[i, type] <- 1  # Mettre 1 dans la colonne correspondante
  }
}

W <- diag(1/1000, 1000)
M <- diag(1/27,27)

#Question 1 b)
# On programme la projection de Y et celle de x^j
P_Y <- Y %*% solve(t(Y)%*% W %*% Y) %*% t(Y) %*% W
P_X <- function(j){
  # Calculer la projection de la colonne j de X
  x_j <- X[, j, drop = FALSE]  # S'assurer que x_j reste une matrice
  return(x_j %*% solve(t(x_j) %*% W %*% x_j) %*% t(x_j) %*% W)
}
#On calcule la trace du produit matriciel
Tr_1 <- sum(diag(P_X(25) %*% P_Y))


# Question 1 c)
#On pose R et on calcul la trace demandée
R <- X %*% M %*% t(X) %*% W
Tr_2 <- sum(diag(R %*% P_Y))

# Question 2 
# On programme Z et on calcul les traces demandé
P_Z <- Z %*% solve(t(Z)%*% W %*% Z) %*% t(Z) %*% W
Tr_3 <- sum(diag(P_X(25) %*% P_Z))
Tr_4 <- sum(diag(R %*% P_Z))