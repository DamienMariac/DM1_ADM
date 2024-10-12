rm(list = ls())

data <- read.csv("C:/Users/damie/Desktop/MASTER/ADM/tp/tp1/Datagenus.csv", sep=";")
data2 <- data[1 :1000,] #On enlève la ligne qui pose problème

##### Question 1 #### 

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
# Vérification des résultats, calcul des variances des colonnes après standardisation 
variances_standardisées <- apply(densités_standardisées, 2, var)
# Inertie totale (on doit avoir 27)
inertie_totale <- sum(variances_standardisées)

##### Question 2 ####
#Calcul des poids de chaque forest.
poid_forest <-table(data2$forest)/1000
types_forestiers <- sort(unique(data2$forest)) #On trie les types de forest par odre croissant
# Créer une matrice pour stocker les barycentres de chaque type forestier (7 x 27)
barycentres_forestiers <- matrix(0, nrow = 7, ncol = 27)

# Calculer le barycentre pour chaque type forestier
for (i in 1:length(types_forestiers)) {
  type <- types_forestiers[i]
  parcelles_type <- densités_standardisées[data2$forest == type, ]
  
  # Calcul du barycentre pour ce type (moyenne des lignes correspondant à ce type)
  barycentres_forestiers[i, ] <- colMeans(parcelles_type)
}
# Calcul des normes euclidiennes carrées des barycentres
normes_euclidiennes <- apply(barycentres_forestiers, 1, function(x) sum(x^2))

# Calcul des poids des types forestiers
poids_forestiers <- table(data2$forest) / nrow(data2)

# Calcul de l'inertie inter-types
inertie_inter_types <- sum(normes_euclidiennes * poids_forestiers)

# Calcul du R^2
R2 <- inertie_inter_types / 27

#### Question 3 ####

# Initialisation du vecteur pour stocker les R^2 de chaque espèce
R2_par_espece <- numeric(27)

# Calcul du R² pour chaque espèce individuellement
for (j in 1:27) {
  # Inertie totale pour l'espèce j (variance de la colonne j de densités_standardisées)
  inertie_totale_espece <- var(densités_standardisées[, j])
  # Inertie inter-types pour l'espèce j (lié aux barycentres des types forestiers pour cette espèce)
  inertie_inter_espece <- sum(poids_forestiers * (barycentres_forestiers[, j]^2))
  # Calcul du R² pour cette espèce
  R2_par_espece[j] <- inertie_inter_espece / inertie_totale_espece
}
R2_par_espece_df <- data.frame(Espece = espece, R2 = R2_par_espece)

# Trouver les espèces les plus et les moins liées aux types forestiers
especes_plus_liees <- R2_par_espece_df[which.max(R2_par_espece_df$R2), ]
especes_moins_liees <- R2_par_espece_df[which.min(R2_par_espece_df$R2), ]


# Derniere question

mean_R2 <- mean(R2_par_espece)
print(mean_R2)
print(R2)

