library(readr)
library(evently)
require(tidyverse)
source("scripts/util/io-util.R")
source("scripts/evently_feature.R")

# netsense or nethealth
dataset <- "netsense"

datasetFolder <- paste0("data/", dataset, "/telcodata")

users <- read_all_users(datasetFolder)

fits <- list()
for(sender in users) {
  fit <- readRDS(file = sprintf("results/%sFits-PL/fits-PL-%s-20calls.dat", dataset, sender))
  if(length(fit) == 0) {
    next
  }
  
  namedList <- FALSE
  for(i in 1:length(fit)) {
    namedList <- namedList | "users" %in% names(fit[[i]])
  }
  
  if(namedList) {
    newFit <- list()
    i <- 1
    for(j in 1:length(fit)) {
      if(length(fit[[j]]) == 11) {
        newFit[[i]] <- fit[[j]]
        i <- i + 1
      }
    }
    fit <- newFit
  } else {
    # Convert the data we already have to the form returned by the group_fit_series() function
    names(fit) <- rep(c('model_type', 'data', 'init_par', 'par', 'value', 'lower_bound', 'upper_bound', 'observation_time', 'convergence', 'users', 'branching_factor'), length(fit)/11)
    fit <- split(fit, rep(1:(length(fit)/11), each=11))
  }

  fit <- list(fit)
  names(fit) <- sender

  fits <- append(fits, fit)
}

for (i in seq_along(fits)) {
  class(fits[[i]]) <- 'hawkes.group.fits'
  class(fits[[i]][[1]]) <- "hawkes_MULTI"
}

dfFeatures <- generate_features(fits, TRUE)

write_csv(dfFeatures, file = sprintf("results/hawkesEmbedding-%s.csv", dataset))

