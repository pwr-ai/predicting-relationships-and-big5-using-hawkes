library(readr)
library(evently)
library(parallel)
require(tidyverse)
library(future)
library(future.apply)
library(parallel)

#### FITTING CONFIG
# lowerBound <- 20; chunk_size <- 10; nCoresEvently <- 1; upperBound <- 4000 ## first fitting
# lowerBound <- 4000; chunk_size <- 1; nCoresEvently <- 5; upperBound <- 6000
# lowerBound <- 6000; chunk_size <- 1; nCoresEvently <- 5; upperBound <- 7000
# lowerBound <- 7000; chunk_size <- 1; nCoresEvently <- 5; upperBound <- 8000
lowerBound <- 20; chunk_size <- 1; nCoresEvently <- 5; upperBound <- 10000

kernel.type <- 'PL'
exo.type <- 'CONST'
nCoresCluster <- ceiling(detectCores() / nCoresEvently)
# netsense or nethealth
dataset <- "netsense"

switch (dataset,
  netsense = {
    dfRelationshipsTypes <- read.csv("data/netsense/dfRelationshipsTypes.csv")
    telcodataPath <- "data/netsense/telcodata"
    resultsPath <- "results/netsenseChangePointDetection.csv"
  },
  nethealth = {
    dfRelationshipsTypes <- read.csv2("data/nethealth/hawkes-networksurvey.csv")
    telcodataPath <- "data/nethealth/telcodata/"
    resultsPath <- "results/nethealthChangePointDetection.csv"
  },
  {
    stop("Don't know this dataset!")
  }
)

dfRelations <- dfRelationshipsTypes %>%
  filter(!is.na(TippingPoint) & TippingPoint > 2 & RelationshipOverTime %in% c("family-relaxing", "friendship-relaxing", "friendship-strengthening", "romantic-relaxing"))

## Setup a multisession plan with as many workers as we computed
## MAR: note that we use PSOCK slaves (multisession), to optimise memory usage, and allow internal parallelization
plan(multisession, workers = nCoresCluster) 
options(future.wait.interval=0L, future.wait.alpha = 1) ## <~~~ newly added line

res <- future_lapply(future.chunk.size = chunk_size, future.seed = T, X = 1:nrow(dfRelations), FUN = function(i) {
  sender <- dfRelations[i, 'EgoID']
  receiver <- dfRelations[i, 'AlterID']

  ## MAR edit: I have xzipped the telco files
  switch (dataset,
          netsense = {
            dfCommunication <- read_delim(file = sprintf("%s/telcodata-%s.csv", telcodataPath, sender), delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                          show_col_types = FALSE)
          },
          nethealth = {
            dfCommunication <- read_delim(file = sprintf("%s/telcodata-%s.csv", telcodataPath, sender), delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                          col_types = cols(
                                            SenderID = col_double(),
                                            ReceiverID = col_double(),
                                            EventTime = col_datetime(format = ""),
                                            EventType = col_double(),
                                            EventLength = col_double()
                                          ))
          },
          {
            stop("Don't know this dataset!")
          }
  )

  calls <- dfCommunication %>%
    filter(SenderID == sender & ReceiverID == receiver | SenderID == receiver & ReceiverID == sender)
  
  # compute time diffs and build the history data frame
  calls$timeDiff <- calls$EventTime - calls$EventTime[1]
  history <- data.frame(matrix(data = 1, nrow = nrow(calls), ncol = 2))
  names(history) <- c("magnitude", "time")
  # make time difference in hours
  history$time <- as.numeric(calls$timeDiff) / 3600

  tippingPoint <- dfRelations[i, 'TippingPoint']
  
  switch (dataset,
    netsense = {
      tippingPoint1 <- 0
      tippingPoint2 <- 0
      if(tippingPoint == 3) {
        tippingPoint1 <- 1
        tippingPoint2 <- 2
      } else if (tippingPoint == 5) {
        tippingPoint1 <- 2
        tippingPoint2 <- 3
      } else if (tippingPoint == 6) {
        tippingPoint1 <- 3
        tippingPoint2 <- 5
      }
    },
    nethealth = {
      tippingPoint1 <- tippingPoint - 2
      tippingPoint2 <- tippingPoint - 1
    },
    {
      stop("Don't know this dataset!")
    }
  )
  
  # get survey completion date
  completed1 <- as.numeric(as.POSIXct(dfRelations[i, paste('Completed', tippingPoint1, sep='_')], format='%Y-%m-%d', tz='UTC') - calls$EventTime[1]) * 24
  completed2 <- as.numeric(as.POSIXct(dfRelations[i, paste('Completed', tippingPoint2, sep='_')], format='%Y-%m-%d', tz='UTC') - calls$EventTime[1]) * 24
  completed3 <- as.numeric(as.POSIXct(dfRelations[i, paste('Completed', tippingPoint, sep='_')], format='%Y-%m-%d', tz='UTC') - calls$EventTime[1]) * 24
  
  # survey not completed
  if(is.na(completed1) | is.na(completed2) | is.na(completed3)) {
    return()
  }
  
  # lower bound of the number of events
  if(nrow(history[history$time < completed3, ]) < lowerBound) {
    return()
  }
  
  # upper bound of the number of events
  if(nrow(history[history$time < completed3, ]) > upperBound) {
    return()
  }
  
  # survey completed but first contact between users was later
  if(completed1 < 0) {
    return()
  }
  
  # use training set of events to fit process
  thetaBefore <- fit_series(list(history), 
                            model_type = c(exo.type, kernel.type),
                            observation_time = completed1, 
                            cores = nCoresEvently)
  
  thetaAfter <- fit_series(list(history), 
                           model_type = c(exo.type, kernel.type),
                           observation_time = completed2, 
                           cores = nCoresEvently)

  # compute total neg-log-likelihood
  totalLLBefore <- get_hawkes_neg_likelihood_value(par = thetaBefore$par, data = list(history), model_type = thetaBefore$model_type, observation_time = completed2)
  totalLLAfter <- get_hawkes_neg_likelihood_value(par = thetaAfter$par, data = list(history), model_type = thetaAfter$model_type, observation_time = completed3)
  
  # compute holdout neg-log-likelihood
  holdoutLLBefore <- (totalLLBefore - thetaBefore$value) / (nrow(history[history$time < completed2, ]) - nrow(history[history$time < completed1, ]) + 1)
  holdoutLLAfter <- (totalLLAfter - thetaAfter$value) / (nrow(history[history$time < completed3, ]) - nrow(history[history$time < completed2, ]) + 1)

  return(c(sender=sender, receiver=receiver, before.NLL=holdoutLLBefore, after.NLL=holdoutLLAfter, before=thetaBefore$par, after=thetaAfter$par))
})

dfRes <- bind_rows(lapply(res, as.data.frame.list))

dfRelations <- inner_join(dfRelations, dfRes, by=c(EgoID='sender', AlterID='receiver'))

write_csv(dfRelations, file = resultsPath)
