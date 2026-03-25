library(tidyverse)
library(corrplot)
library(cluster)
library(factoextra)
library(psych)

nuts3 <- read.csv("nuts3.csv", header = TRUE, sep = ",", 
                  check.names = FALSE, row.names = 1)

dane_long <- nuts3 %>%
  pivot_longer(
    cols = c(1:21), 
    names_to = "zmienna",
    values_to = "wartosc"
  )

ggplot(dane_long, aes(x = wartosc)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~ zmienna, scales = "free") +
  theme_minimal() +
  labs(title = "Histogramy dla zmiennych",
       x = "Wartość",
       y = "Liczba")

danecor <- cor(nuts3)

corrplot(danecor)

summary(nuts3)



# Metoda standaryzwanych sum








# TOPSIS
n_alternatyw <- nrow(nuts3)
n_kryteriow <- ncol(nuts3)
wagi <- rep(1 / n_kryteriow, n_kryteriow)
typy_kryteriow <- c(-1, rep(1, 8), -1, 1, rep(-1, 2), rep(1, 5), rep(-1, 3))

Norm <- apply(nuts3, 2, function(col) col / sqrt(sum(col^2)))
Wazona <- Norm * matrix(wagi, nrow = n_alternatyw, ncol = n_kryteriow, byrow = TRUE)

A_plus <- numeric(n_kryteriow) 
A_minus <- numeric(n_kryteriow) 

for (j in 1:n_kryteriow) {
  if (typy_kryteriow[j] == 1) {
    A_plus[j] <- max(Wazona[, j])
    A_minus[j] <- min(Wazona[, j])
  } else {
    A_plus[j] <- min(Wazona[, j])
    A_minus[j] <- max(Wazona[, j])
  }
}

d_plus <- apply(Wazona, 1, function(row) sqrt(sum((row - A_plus)^2)))
d_minus <- apply(Wazona, 1, function(row) sqrt(sum((row - A_minus)^2)))

Ri <- d_minus / (d_plus + d_minus)
ranking_topsis <- data.frame(
  Nazwa = nazwy,
  Wspolczynnik_R = Ri
)

ranking_topsis <- ranking_topsis[order(ranking_topsis$Wspolczynnik_R, decreasing = TRUE), ]
ranking_topsis$Ranking <- 1:n_alternatyw

print("===================================")
print("Ostateczny Ranking TOPSIS:")
print(ranking_topsis)



# Metoda k-medoid
fviz_nbclust(x = nuts3, method = 'silhouette', FUNcluster = pam)
fviz_nbclust(x = nuts3, method = 'wss', FUNcluster = pam)

k_medoid_resluts <- pam(nuts3, k = 4)

k_medoid_resluts$clustering

fviz_cluster(k_medoid_resluts,
             data = df_scaled,
             geom = "point", 
             palette = c("blue", "orange", "red", "green"),
             ellipse.type = "convex",
             ggtheme = theme_minimal())

podregiony_clusters <- data.frame(nuts3, cluster = k_medoid_resluts$clustering)

wyniki <- describeBy(podregiony_clusters,
                     group = podregiony_clusters$cluster)
#write.csv(wyniki[['4']], file = 'wyniki4.csv', row.names = TRUE)







# Grupowanie hierarchiczne
