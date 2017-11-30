## ---- echo = FALSE-------------------------------------------------------
# Uncomment to enter DEBUG mode. The package vignette will compile, but code will not be evaluated for speed purposes.
# knitr::opts_chunk$set(eval = FALSE)

## ---- message = FALSE----------------------------------------------------
library(SUMMER)
if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
  install.packages('INLA', repos = 'https://www.math.ntnu.no/inla/R/stable')
}

data(Uganda)
data(UgandaMap)

## ---- warning=FALSE------------------------------------------------------
years <- levels(Uganda[[1]]$time)

data <- countrySummary_mult(births = Uganda, years = years, idVar = "id", regionVar = "region",
                           timeVar = "time", clusterVar = "~clustid+id", ageVar = "age",
                           weightsVar = "weights", geo.recode = NULL)

## ---- message = FALSE----------------------------------------------------
    geo <- UgandaMap$geo
    mat <- UgandaMap$Amat

## ------------------------------------------------------------------------
priors <- simhyper(R = 2, nsamp = 1e+05, nsamp.check = 5000, Amat = mat)

## ---- message = FALSE----------------------------------------------------
data <- data[data$region %in% c("central","eastern","northern","western"),]
inla_model <- fitINLA(data = data, geo = geo, Amat = mat, year_names = years, priors = priors)

## ---- message = FALSE----------------------------------------------------
surveylabel <- paste0("DHS ", unique(data$surveyYears))
    
results_rw2 <- projINLA(data = data, inla_mod = inla_model, years = years, geo = geo, 
                      newyear = "15-19", quantiles = c(0.025,0.5,0.975))

## ----fig.height=5,fig.width=7--------------------------------------------
mapPlot(countryname = "Uganda", results = results_rw2, geo = geo, 
            countrysum = data, inlamod = inla_model)

## ----fig.height=5,fig.width=7--------------------------------------------
spagPlot(countryname = "Uganda", results = results_rw2, geo = geo, 
            countrysum = data, inlamod = inla_model)

