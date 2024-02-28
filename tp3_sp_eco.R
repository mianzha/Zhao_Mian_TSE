library("colorspace")
library("sf")
library("spData")
library("spdep")
library("spatialreg")
library("tidyverse")
library(ggplot2)

columbus <- sf::st_read(system.file("shapes/columbus.shp", package="spData")[1])

columbus_nb <- poly2nb(columbus)
columbus_listw <- nb2listw(columbus_nb, style = "W")

W = listw2mat(columbus_listw) 

res_w = eigen(W)
eigen_value = res_w$values
eigen_value
plot(1/eigen_value, rep(0, length(eigen_value)))
abline(v = 1/eigen_value)

abline(v = 1/max(eigen_value), col = "red")
abline(v = 1/min(eigen_value), col = "red")


#Emprirical part

INC_lag <- lag.listw(columbus_listw, columbus$INC)
HOVAL_lag <- lag.listw(columbus_listw, columbus$HOVAL)

OLS = lm(CRIME~INC + HOVAL, data =  columbus)
summary(OLS)


moran.plot(residuals(OLS), columbus_listw)

moran_scatter_plot(X = residuals(OLS),
                   geo = columbus,
                   X_name = "X")

lm.morantest(OLS, columbus_listw)


slx = lm(CRIME~INC + HOVAL +INC_lag + HOVAL_lag, data = columbus)
summary(slx)

slx_1 = spatialreg::lmSLX(CRIME~INC + HOVAL, data =  columbus, listw = columbus_listw)

impacts(slx_1, columbus_listw)



sdm = spatialreg::lagsarlm(CRIME~INC + HOVAL, data =  columbus, 
                           listw = columbus_listw,
                           Durbin = T)
summary(sdm)
impacts(sdm, listw = columbus_listw)


sem = spatialreg::errorsarlm(CRIME~INC + HOVAL, data =  columbus, 
                             listw = columbus_listw)
summary(sem) #interpretation is same as OLS 


AIC(slx_1, sdm, sem)

