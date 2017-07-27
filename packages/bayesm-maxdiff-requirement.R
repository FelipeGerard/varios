
library(bayesm)
data("cheese")
str("cheese")

retailer <- levels(cheese$RETAILER)
nreg <- length(retailer); nreg

regdata <- NULL

for (i in 1:nreg) {
  filter <- cheese$RETAILER==retailer[i]
  y <- log(cheese$VOLUME[filter])
  X <- cbind(1,      # intercept placeholder
             cheese$DISP[filter],
             log(cheese$PRICE[filter]))
  regdata[[i]] <- list(y=y, X=X)
}

Data <- list(regdata=regdata)
Mcmc <- list(R=2000)

system.time(
  outcpu <- bayesm::rhierLinearModel(
    Data=Data,
    Mcmc=Mcmc))


