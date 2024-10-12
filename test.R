rm(list = ls())

tab <- read.csv("C:/Users/damie/Desktop/MASTER/ADM/tp/tp1/Datagenus.csv", sep = ";")
data <- tab[1 :1000,]
species_columns <- grep("gen", colnames(data), value = TRUE)

##QUESTION 1

density_data <- data[species_columns] / data$surface


W <- diag(1/1000, 1000, 1000)
I_n <- rep(1, 1000)

Q <- as.matrix(density_data)
x_b <- t(Q) %*% W %*% I_n
x_c <- Q - I_n %*% t(x_b)  


vec_norm <- sqrt(diag(t(x_c) %*% W %*% x_c))

if (any(is.na(vec_norm) | vec_norm == 0)) {
  stop("Erreur : certaines normes sont nulles ou non définies. Vérifie les données d'entrée.")
}

x_cr <- sweep(x_c, 2, vec_norm, FUN = "/")
tableau_cr <- as.data.frame(x_cr)


bar <- t(x_cr) %*% W %*% I_n
barycentre <- t(bar)
print(barycentre)  

inertie_totale <- sum(diag(t(x_cr) %*% W %*% x_cr))
print(inertie_totale)  

print(tableau_cr)

##Question 2


