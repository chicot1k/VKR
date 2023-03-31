library(sf)
library(sp)
library(rgdal)
library(spdep)
library(lattice)
library(RANN)
library(RColorBrewer)
library(dbscan)
reg.sf <- st_read('/Users/chicot1k/Desktop/gadm41_RUS_1_21_61.json')
#Reading layer `gadm41_RUS_1' from data source `/Users/chicot1k/Desktop/gadm41_RUS_1_21_61.json' using driver `GeoJSON'
#Simple feature collection with 81 features and 11 fields
#Geometry type: MULTIPOLYGON
#Dimension:     XY
#Bounding box:  xmin: -180 ymin: 41.1889 xmax: 180 ymax: 81.8562
#Geodetic CRS:  WGS 84
reg <- as(reg.sf, 'Spatial')
reg = st_geometry(reg.sf)
par(mar = c(1,1,1,1))
plot(reg, border = "gray50")
nb_queen = poly2nb(reg) # Соседство по правилу ферзя
nb_queen  # посмотрим сводную информацию
#Neighbour list object:
 # Number of regions: 81 
#Number of nonzero links: 376 
#Percentage nonzero weights: 5.730834 
#Average number of links: 4.641975 
class(nb_queen)  # проверим тип объекта
## [1] "nb"
coords = reg %>% 
st_centroid() %>% 
st_coordinates()
# Теперь рисуем граф:
plot(reg, border = "gray50")
plot(nb_queen, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по смежности (правило ферзя)")
nb_rook = poly2nb(reg, queen = FALSE) # Соседство по правилу ладьи
plot(reg, border = "grey70")
plot(nb_rook, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = "Соседи по смежности (правило ладьи)")
#Neighbour list object:
# Number of regions: 81 
#Number of nonzero links: 376 
#Percentage nonzero weights: 5.730834 
#Average number of links: 4.641975 
nb_tin = tri2nb(coords)
 plot(reg, border = "grey70")
 plot(nb_tin, coords, pch = 19, cex = 0.5, add = TRUE)
 title(main = "Соседи по триангуляции Делоне")
 nb_tin
 #Neighbour list object:
 #Number of regions: 81 
 #Number of nonzero links: 458 
 #Percentage nonzero weights: 6.980643 
 #Average number of links: 5.654321 
 if (require("dbscan", quietly=TRUE)) {
   col.soi.nb <- graph2nb(soi.graph(nb_tin,coords), sym=TRUE)
   plot(col.soi.nb, coords, add=TRUE)
   title(main="Sphere of Influence Graph", cex.main=0.6)
 }
 col.soi.nb <- graph2nb(soi.graph(nb_tin,coords), sym=TRUE)
 nb_gab = gabrielneigh(coords) %>% graph2nb()
 
 plot(reg, border = "grey70")
 plot(nb_gab, coords, pch = 19, cex = 0.5, add = TRUE)
 title(main = "Соседи по графу Гэбриела")
 nb_rel = relativeneigh(coords) %>% graph2nb()
 
 plot(reg, border = "grey70")
 plot(nb_rel, coords, pch = 19, cex = 0.5, add = TRUE)
 title(main = "Относительные соседи по графу")
 
  nb_knn = knearneigh(coords, k = 1) %>% knn2nb()
  plot(reg, border = "grey70")
  plot(nb_knn, coords, pch = 19, cex = 0.5, add = TRUE)
  title(main = "Ближайшие соседи (k = 1)")
  
  dnearnei = dnearneigh(coords, d1 = 0, d2 = 6)
  plot(reg, border = "grey70")
  plot(dnearnei, coords, pch = 19, cex = 0.5, add = TRUE)
  title(main = "Ближайшие соседи (d <= 6 )")
 
  nb_queen = poly2nb(reg)
  Wbin = nb2listw(nb_queen, style = "B")
  Wbin
  Wbin$neighbours
  Wbin$weights
  M = listw2mat(Wbin)
  levelplot(M, main = "Матрица весов (бинарная)")
