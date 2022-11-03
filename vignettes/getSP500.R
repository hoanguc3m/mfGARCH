library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod
library(midasml)

SP500 <- tq_get('^GSPC',
                from = '1928-01-01',
                to = "2021-12-31",
               get = "stock.prices")

ret <- as.numeric( diff(log(SP500$adjusted)) * 100 )
date <- SP500$date[2:(length(ret)+1)]

library(dtsmcvol)
real_temp <- RealizedVol(data.x = ret, data.xdate = date)
plot(real_temp$data.x)

y = real_temp$data.x[2:1128]
x = mls(real_temp$data.x[1:1127], k = 0:11, m = 1)

y = tail(y, 1000)
x = tail(x, 1000)
library(midasr)

midas.ardl(y = y, x = x, z = rep(1, 1000), loss_choice = c("mse"),
           poly_choice = c("beta"),
           poly_spec = 3)

beta <- -1
# m = gamma(theta1 + theta2)/(gamma(theta1) * gamma(theta2))
# weights <- xx^(theta1 - 1) * (ii - xx)^(theta2 - 1) * m + theta3
weights <- rbeta_w(param = 10, dayLag = 12)
res = y - beta * x %*% weights
mse = t(res) %*% (res); as.numeric(mse)

