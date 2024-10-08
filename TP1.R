rm(list = ls())

data_forest <- read.csv("C:/Users/damie/Desktop/MASTER/ADM/tp/tp1/Datagenus.csv", sep = ";", encoding = "UTF-8")
columns_of_interest <- c(paste0("gen", 1:27), "surface", "forest", "geology")
data_cleaned <- data_forest[, columns_of_interest]



density_data <- data_cleaned[, paste0("gen", 1:27)] / data_cleaned$surface

density_centered_reduced <- density_data

for (i in 1:ncol(density_data)) {

  moyenne <- sum(density_data[, i]) / nrow(density_data)
  

  n <- nrow(density_data)
  somme_derivee <- sum((density_data[, i] - moyenne) ^ 2)
  ecart_type <- sqrt(somme_derivee / n)

  density_centered_reduced[, i] <- (density_data[, i] - moyenne) / ecart_type
}

moyennes_apres_centrage <- colMeans(density_centered_reduced)

barycentre_a_l_origine <- all(abs(moyennes_apres_centrage) < 1e-10)

variances_apres_centrage <- apply(density_centered_reduced, 2, var) 
inertie_totale <- sum(variances_apres_centrage)
print(inertie_totale)

