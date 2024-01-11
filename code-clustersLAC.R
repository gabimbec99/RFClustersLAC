##### Clusters - Transición energética y gobernanza regional en América Latina y el Caribe #####
##### Librerías ----
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(readr)
library(tidyverse)
library(cluster)
library(NbClust)
library(factoextra)
library(ggplot2)
library(ggalluvial)
library(ggsankey)
library(factoextra)
library(networkD3)
library(plotly)
library(ggfortify)
library(ggdist)
library(viridis)
library(ggthemes)
library(ggpubr)
library(ggridges)
library(openxlsx)
library(stats)
library(dendextend)
library(heatmap3)
library(clustertend)
library(fpc)
library(corrplot)
library(gridExtra)
library(tsibble)
library(gt)
library(gtExtras)
library(ggrepel)
library(scales)  
library(caret)
library(bruceR)
library(fmsb)
##### Directorio y base -----
# Cambiar directorio según corresponda
setwd("~/Desktop/CREE/ENERGÍA Y RRII")
# Se carga la base original
data_clusters <- read.xlsx("data_clusters.xlsx")
##### Elección de variables -----
# Cambiamos nombres de columnas para mayor facilidad
colnames(data_clusters) <- c("País", "ISO3", "Poblacion","Calidad_Politicas",
                             "Part_fosil", "Balance_neto", "Balance_consumo",
                             "Export", "Vulnerabilidad","Preparación","IDH","Emisiones","PIB_PC","PIB_PC_PPA","PElec","Globalización","Glob_DJ","Glob_DF","Econ_Glob","Pol_Glob","Interconexiones","Fronteras_I",
                             "IntensidadE","EmisionesPC","IntensidadGEI")

###### Correlación entre variables --------
# Eliminamos variables de país e identificador de país
data_clusters_corr <- data_clusters %>% select(-c("País", "ISO3"))
cmtx <- cor(data_clusters_corr)
set.seed(123)
png(filename="Corrplot.png")
corrplot(cmtx, order="hclust", addrect = 9)
dev.off()
#-- Tras distintas sensibilidades se optó por eliminar las variables de interconexión fronteriza,
#-- así como de precios de la electricidad y participación fósil pues introducen una distorsión.
column_names <- colnames(data_clusters)
print(column_names)
# Se crea una base final "X" con únicamente la información necesaria
X <- data_clusters %>% select(c("Balance_consumo","Vulnerabilidad","Preparación","IDH","Emisiones","Globalización","IntensidadE"))
rownames(X) <- data_clusters$ISO3
X_scale<-scale(X)
# Así se correlacionan las variables finalmente elegidas
cmtx <- cor(X)
png(filename="Correlación_var_finales.png")
corrplot(cmtx, order="hclust")
dev.off()
##### Clusters -----
###### K-means ######
# Algunos criterios de elección de número de clústers 
png(filename="Silueta_clusters.png")
fviz_nbclust(X_scale, kmeans, method = "silhouette")
dev.off()
png(filename="Codo_clusters.png")
fviz_nbclust(X_scale, kmeans, method = "wss")
dev.off()
#-- Atendiendo a las necesidades de una agenda de gobernanza, y el criterio del codo,
#-- se optó por llevar a cabo 4 clústers
# Clusters
set.seed(123)
res.k_means <- eclust(X_scale, "kmeans", k = 4, graph = TRUE)
fviz_cluster(res.k_means, data = X_scale, 
             ellipse.type = "convex",
             geom = "point",
             ggtheme = theme_bw(), ellipse.alpha = 0,main = "Kmeans- 4 clusters"
)
#Número de observaciones en cada clústers
num_obs <- table(res.k_means$cluster)
num_obs
#Resultado - Muestra a qué clúster corresponde cada país
resultado <- data_clusters %>%
  select(País,ISO3) %>%
  mutate(ISO3 = as.character(ISO3))%>%
  mutate(kmeans_option1_vars_result = res.k_means$cluster)
resultado$kmeans_option1_vars_result <- as.factor(resultado$kmeans_option1_vars_result)
rownames(resultado) <- resultado$ISO3
##### Interpretación clústers -----
# Plot
png(filename="Clusters.png")
fviz_cluster(
  res.k_means,
  data = X_scale,
  palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
  ggtheme = theme_minimal()
)
dev.off()
#Tabla de medias
clustbl <- cbind (aggregate(X, by=list(cluster=res.k_means$cluster), mean), res.k_means$size) #mean values by cluster for original data
colnames(clustbl) <- c("Clúster", "Balance (% consumo)", "Vulnerabilidad",
                       "Preparación", "HDI","Emisiones",
                       "Globalization Index","Intensidad Energética","Tamaño") #rename columns

gt(clustbl) %>%
  tab_header (title = md("**Valores medios por clúster**"), subtitle = "Países ALC") %>%
  fmt_number(columns=c(6), decimals=0) %>%
  fmt_number(columns=c(2,3,5,4, 7,8), decimals=3)
# Componentes principales - Interpretación de dimensiones 1 y 2
ACP <- prcomp(X, scale = TRUE)
ACP$rotation <- -1*ACP$rotation
summary(ACP)

desv_acp=ACP[[1]]
var_acp=desv_acp^2
pve <- var_acp / sum(var_acp)

## Obtener los PC 1 y 2 para cada observación y los vectores de las variables 

ACP$x <- -1*ACP$x
autoplot(ACP, data = data_clusters, loadings.colour = 'red',
         loadings = TRUE,
         loadings.label = TRUE, loadings.label.size = 4)  + theme_bw() 

# Obtenemos los "loadings"
loadings_matrix <- ACP$rotation
print(loadings_matrix)

##### Gráficas de densidad -----
# Creamos gráficas de densidad en donde se muestre la distribución de distintas variables
# en los clústers.
# ----- HDI ---
# Vector de colores
cluster_colors <- c("#2E9FDF", "#00AFBB", "#E7B800","#FC4E07")  # Match these colors to your first plot
# Plot
newPlot <- data_clusters |>
  left_join(y = resultado, by = "ISO3") |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = IDH, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'IDH',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))

ggsave(plot = newPlot, filename = 'hdi.png')

# Dado que el clúster 2 contiene una única observación (Haití) se debe modificar el gráfico
# Se crequea el número de observaciones del clúster 3
num_observations_cluster3 <- sum(resultado$kmeans_option1_vars_result == 3)

# Se crea la unión de las bases para obtener una única
data_con_resultados <- data_clusters |>
  left_join(y = resultado, by = "ISO3")
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = IDH, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'IDH',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))

# Añadir el punto para el clúster 3
if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = IDH, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = IDH, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}

#Guardar
ggsave(plot = newPlot, filename = 'hdi.png')
# ----- Vulnerabilidad ---
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = Vulnerabilidad, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'Vulnerabilidad',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))
if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Vulnerabilidad, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Vulnerabilidad, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}

ggsave(plot = newPlot, filename = 'vul.png')

# ----- Preparación ---
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = Preparación, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'Preparación',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))

if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Preparación, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Preparación, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}
ggsave(plot = newPlot, filename = 'prep.png')

# ----- Balance exportador como % del consumo ---
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = Balance_consumo, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'Balance comercio energía (% consumo total)',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))
if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Balance_consumo, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Balance_consumo, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}

ggsave(plot = newPlot, filename = 'balance.png')
# ----- Emisiones ---
# Load the scales package if not already loaded
mean_emisiones <- mean(data_con_resultados$Emisiones)
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = Emisiones, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x))+
  geom_hline(yintercept = '7') +
  labs(x = 'Emisiones',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))

if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Emisiones, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Emisiones, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}

# Formato con separador de miles
newPlot <- newPlot + scale_x_continuous(labels = comma, breaks = seq(0, max(data_con_resultados$Emisiones), by = 100000))

ggsave(plot = newPlot, filename = 'emisiones.png')

# ----- Globalización ---
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = Globalización, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'Índice de Globalización',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))

if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Globalización, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = Globalización, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}

ggsave(plot = newPlot, filename = 'glob_index.png')

# ----- Intensidad de Energía Final ---
newPlot <- data_con_resultados |>
  filter(!is.na(kmeans_option1_vars_result)) |>
  ggplot(aes(x = IntensidadE, y = kmeans_option1_vars_result, fill = factor(kmeans_option1_vars_result), alpha = 0.6)) +
  geom_density_ridges2() +
  stat_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_hline(yintercept = '7') +
  labs(x = 'Índice de Globalización',
       y = 'Clúster') +
  scale_fill_manual(values = cluster_colors) +  # Specify the colors
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) + 
  theme(legend.position = 'none',
        text            = element_text(family='Georgia'),
        legend.text     = element_text(family='Georgia'),
        strip.text      = element_text(family='Georgia'),
        plot.title      = element_text(face='bold', family='Georgia'),
        plot.subtitle   = element_text(family='Georgia'),
        panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', color=NA), 
        legend.background = element_rect(fill='transparent'))

if (num_observations_cluster3 == 1) {
  newPlot <- newPlot +
    geom_point(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = IntensidadE, y = 3), color = cluster_colors[3], size = 3)
} else {
  newPlot <- newPlot +
    geom_histogram(data = filter(data_con_resultados, kmeans_option1_vars_result == 3), aes(x = IntensidadE, y = ..density..), fill = cluster_colors[3], alpha = 0.6, bins = 10)
}

ggsave(plot = newPlot, filename = 'intensidadee.png')


##### Gráficas de radar ----

#Max scaling
#Buscamos el valor máximo de cada columna
X %>% summarise_if(is.numeric, max, na.rm=TRUE)
X_normalizada<-X
#Reescalamos cada columna por el máximo
X_normalizada$Vulnerabilidad=X_normalizada$Vulnerabilidad/0.5135382
X_normalizada$Preparación=X_normalizada$Preparación/0.5361268
X_normalizada$IDH=X_normalizada$IDH/0.855
X_normalizada$Emisiones=X_normalizada$Emisiones/434654.6
X_normalizada$Globalización=X_normalizada$Globalización/75.90446
X_normalizada$IntensidadE=X_normalizada$IntensidadE/0.34044
#Como el balance tiene datos negativos hacemos max min
X_normalizada$Balance_consumo=scaler(X_normalizada$Balance_consumo)

#Max-min en primera fila
max_min <- data.frame(
  Balance_consumo = c(1, 0), IntensidadE = c(1, 0), Vulnerabilidad = c(1, 0),
  Preparación = c(1, 0), IDH = c(1, 0), Emisiones = c(1, 0),Globalización = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")
X_normalizada <- rbind(max_min, X_normalizada)
X_normalizada

#Radar
radarchart(X_normalizada) #Aquí se muestran todos los 18 países

#Función para crear radar
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


#Debemos incluir un radar del promedio de los clusters, por lo que debemos agregar las nuevas filas
#Cluster 1
# Subset the data to select the specific rows
cluster1 <- X_normalizada[c("BLZ", "BOL","COL","CUB","ECU","SLV","GTM","HND","JAM","NIC","PRY","PER","DOM","SUR"), ]
# Calculate the mean of all columns for the selected rows
col_means <- colMeans(cluster1)
# Step 2: Create a new row with the means and a rowname
new_row <- rbind(col_means)
row.names(new_row) <- "media_cluster1"
# Step 3: Add the new row to the original dataset
X_normalizada <- rbind(X_normalizada, new_row)

#Cluster 2
# Subset the data to select the specific rows
cluster2 <- X_normalizada[c("ARG", "BRB","BRA","CHL","CRI","MEX","PAN","URY"), ]
# Calculate the mean of all columns for the selected rows
col_means <- colMeans(cluster2, na.rm = TRUE)
# Step 2: Create a new row with the means and a rowname
new_row <- rbind(col_means)
row.names(new_row) <- "media_cluster2"
# Step 3: Add the new row to the original dataset
X_normalizada <- rbind(X_normalizada, new_row)

#Cluster 4
# Subset the data to select the specific rows
cluster4 <- X_normalizada[c("TTO", "GUY","VEN"), ]
# Calculate the mean of all columns for the selected rows
col_means <- colMeans(cluster4, na.rm = TRUE)
# Step 2: Create a new row with the means and a rowname
new_row <- rbind(col_means)
row.names(new_row) <- "media_cluster4"
# Step 3: Add the new row to the original dataset
X_normalizada <- rbind(X_normalizada, new_row)

# Clusters Radares -----
dimensiones_cluster1<- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster1','Max','Min')) 
dimensiones_cluster2<- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster2','Max','Min')) 
dimensiones_cluster3<- subset(X_normalizada, rownames(X_normalizada) %in% c('HTI','Max','Min')) 
dimensiones_cluster4<- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster4','Max','Min')) 
dimensiones_clusters<- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster1', 'media_cluster2','media_cluster4','Max','Min')) 
par(mfrow = c(2,2))
#Cluster 1
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(dimensiones_cluster1,
                            #color
                            color = c("#2E9FDF"),
                            #custom labels
                            vlcex=0.8,
                            #labels
                            vlabels=c('Balance Exportaciones','Int_Energética', 'Vulnerabilidad', 'Preparación','HDI','Emisiones','Globalización'),
                            title = "Radar de Cluster 1",)
par(op)
#Cluster 2
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(dimensiones_cluster2,
                            #color
                            color = c("#00AFBB"),
                            #custom labels
                            vlcex=0.8,
                            #labels
                            vlabels=c('Balance Exportaciones','Int_Energética', 'Vulnerabilidad', 'Preparación','HDI','Emisiones','Globalización'),
                            title = "Radar de Cluster 2",)
par(op)
#Cluster 3
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(dimensiones_cluster3,
                            #color
                            color = c("#E7B800"),
                            #custom labels
                            vlcex=0.8,
                            #labels
                            vlabels=c('Balance Exportaciones','Int_Energética', 'Vulnerabilidad', 'Preparación','HDI','Emisiones','Globalización'),
                            title = "Radar de Cluster 3  - Haití",)
par(op)
#Cluster 4
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(dimensiones_cluster4,
                            #color
                            color = c("#FC4E07"),
                            #custom labels
                            vlcex=0.8,
                            #labels
                            vlabels=c('Balance Exportaciones','Int_Energética', 'Vulnerabilidad', 'Preparación','HDI','Emisiones','Globalización'),
                            title = "Radar de Cluster 4",)
par(op)
#Ambos clusters
# Reduce plot margin using par()
op <- par(mar = c(2, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = dimensiones_clusters,
  color = c("#2E9FDF","#00AFBB", "#FC4E07"),
  #custom labels
  vlcex=0.8,
  #labels
  vlabels=c('Balance Exportaciones','Int_Energética', 'Vulnerabilidad', 'Preparación','HDI','Emisiones','Globalización'),)
# Add an horizontal legend
legend("bottom", legend = c("Cluster 1", "Cluster 2","Cluster 4"), horiz = TRUE,
       bty = "o", pch = 20 , col = c("#2E9FDF","#00AFBB", "#FC4E07"),
       text.col = "black", cex = 1, pt.cex = 1.5,xpd=TRUE,inset=c(-0.3, -0.15),
)
par(op)

#Cada país y los clusters
#Chat GPT
par(mar = rep(0.8,4))
par(mfrow = c(5,4))

# loop through each country in the dataset
for (country in rownames(X)) {
  # filter rows for the current country and the profiles of interest
  df <- subset(X_normalizada, rownames(X_normalizada) %in% c('Max','Min',country)) 
  # set title for the radar chart
  chart_title <- paste(country)
  # create radar chart using the radarchart() function from fmsb package
  radarchart(
    df,
    pfcol = c("#99999980",NA,NA),
    pcol= c(NA,"#2E9FDF","#FC4E07"), plty = "dashed", plwd = 1,
    title = chart_title,
    #labels
    vlabels=c('Balance Exportaciones','Int_Energética', 'Vulnerabilidad', 'Preparación','HDI','Emisiones','Globalización'),
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
  )
}

# Radar todos países Cluster 1-----
par(mar = rep(0.8,4))
par(mfrow = c(4,4))

# loop through each country in the cluster 1
for (country in rownames(cluster1)) {
  # filter rows for the current country and the profiles of interest
  df <- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster1','Max','Min',country)) 
  # set title for the radar chart
  chart_title <- paste(country)
  # create radar chart using the radarchart() function from fmsb package
  radarchart(
    df,
    pfcol = c("#c0e2f5",NA,NA),
    pcol= c(NA,"#2E9FDF","#FC4E07"), plty = "dashed", plwd = 1,
    title = chart_title,
    #labels
    vlabels=c('Balance Exportaciones','Int_Energética', 'Vul', 'Preparación','HDI','Emisiones','Globalización'),
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
  )
}
# Radar todos países Cluster 4-----
par(mar = rep(0.8,4))
par(mfrow = c(3,3))

# loop through each country in the cluster 4
for (country in rownames(cluster4)) {
  # filter rows for the current country and the profiles of interest
  df <- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster1', 'media_cluster4','Max','Min',country)) 
  # set title for the radar chart
  chart_title <- paste(country)
  # create radar chart using the radarchart() function from fmsb package
  radarchart(
    df,
    pfcol = c("#fec9b4",NA,NA),
    pcol= c(NA,"#2E9FDF","#FC4E07"), plty = "dashed", plwd = 1,
    title = chart_title,
    #labels
    vlabels=c('Balance Exportaciones','Intensidad Energética', 'Precio Gasolina', 'Reservas Petróleo','Intensidad Emisiones','PIB Per cápita'),
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
  )
}

# Radar países Cluster 2 y 3 -----
clusters2y3 <- X_normalizada[c("Trinidad y Tobago", "Venezuela"), ]
par(mar = rep(0.8,4))
par(mfrow = c(2,1))

# loop through each country in the cluster 4
for (country in rownames(clusters2y3)) {
  # filter rows for the current country and the profiles of interest
  df <- subset(X_normalizada, rownames(X_normalizada) %in% c('media_cluster1', 'media_cluster4','Max','Min',country)) 
  # set title for the radar chart
  chart_title <- paste(country)
  # create radar chart using the radarchart() function from fmsb package
  radarchart(
    df,
    pfcol = c("#BCBCBC",NA,NA),
    pcol= c(NA,"#2E9FDF","#FC4E07"), plty = "dashed", plwd = 1,
    title = chart_title,
    #labels
    vlabels=c('Balance Exportaciones','Intensidad Energética', 'Precio Gasolina', 'Reservas Petróleo','Intensidad Emisiones','PIB Per cápita'),
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
  )
}

colnames(X) <- c("Balance_Exportador","Vulnerabilidad","Preparación","IDH","Emisiones","Globalización","IntensidadE") #rename columns
