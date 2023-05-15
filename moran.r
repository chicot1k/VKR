library(sf)
library(sp)
library(spdep)
library(rgdal)
library(openxlsx)
library(openxlsx)
library (readxl)
library(spatialreg)
Output.Areas <- read_sf("/Users/chicot1k/Desktop/gadm41_RUS_1_21_61_3.json")
View(Output.Areas)
neighbours <- poly2nb(Output.Areas)
neighbours
df <- merge(Output.Areas, tab, by="NAME_1")
View(df)
W <- nb2listw(neighbours)
df_sp <- as(df, 'Spatial')
moran.test(df$income, W)
sim<-moran.mc(df$income, listw = W, nsim = 10000)
sim
hist(sim$res,
     freq = TRUE,
     breaks = 20,
     xlim = c(-1,1),
     main = "Перестановочный тест Морана",
     xlab = "Случайный индекс Морана",
     ylab = "Частота появления",
     col = "steelblue")
> abline(v = sim$statistic, col = "red")
> moran.plot(df$income, W)
df2 <- as(df, 'Spatial')
levels <- seq(0,100000,10000)
 nclasses <- length(levels)-1
spplot(df_sp, 'income', at = levels, col.regions = ramp(nclasses))

spplot(dff,
       zcol=c("mid_soul_income","fitted", "residuals"),
       names.attr = c("Фактические значения", "Модель", "Остатки"),
       at = levels,
       col.regions = ramp(nclasses))
