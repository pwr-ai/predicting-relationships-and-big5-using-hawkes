## how to install the packages requres for this script
# install.packages("devtools", repos = "cran.csiro.au")
# remotes::install_github("behavioral-ds/evently")
# install.packages(c("readr", "tidyverse", "dplyr", "future", "future.apply", "stringr"), repos = "cran.csiro.au")

## In order to run on the cluster, this script needs to run from the root of the repository.

## libraries and packages
library(readr)
library(evently)
library(tidyverse)
library(dplyr)
library(future)
library(future.apply)
library(parallel)

source("scripts/util/io-util.R")

########## make our fitting selection ######
kernel.type <- 'PL'
holdout <- F
dataset <- "netsense"
# dataset <- "nethealth"
#############################

ncores <- detectCores()
# ncores <- 54
lowerBound <- 20
exo.type <- 'CONST'
## make nCoresEvently = 1 for full usage of processors, and increase to 5 as only larger jobs are left
# nCoresEvently <- 1 ; chunk_size <- 10; upperBound <- 10000 ## use this at the beginning of the fitting, to better use processors
# nCoresEvently <- 2 ; chunk_size <- 5; upperBound <- 10000 ## use this at the beginning of the fitting, to better use processors
nCoresEvently <- 5 ; chunk_size <- 1; upperBound <- 10000 ## later fitting, for larger jobs
nCoresCluster <- ceiling(ncores / nCoresEvently)

## Setup a multisession plan with as many workers as we computed
## MAR: note that we use PSOCK slaves (multisession), to optimise memory usage, and allow internal parallelization
plan(multisession, workers = nCoresCluster)
options(future.wait.interval=0L, future.wait.alpha = 1) ## <~~~ newly added line
# plan(multicore, workers = nCoresCluster) 

## where are the dataset?
switch (dataset,
  netsense = {
    datasetFolder <- "data/netsense/telcodata/"
  },
  nethealth = {
    datasetFolder <- "data/nethealth/telcodata/"
  },
  {
    stop("Don't know this dataset!")
  }
)

## where are the intermediary atomic fits saved?
holdouttext <- ""
if (holdout)
  holdouttext <- "Holdout"
tmpFolder <- sprintf("~/tmp_nfs/%sFits%s-%s", dataset, holdouttext, kernel.type)
dir.create(path = tmpFolder, showWarnings = F, recursive = T)

## where are the fits saved?
resultsFolder <- sprintf("results/%sFits%s-%s", dataset, holdouttext, kernel.type)
dir.create(path = resultsFolder, showWarnings = F, recursive = T)

users <- read_all_users(datasetFolder)

peers <- bind_rows(future_lapply(future.seed = T, X = users, FUN = function(user_1) {
  ## load data
  switch (dataset,
          netsense = {
            data_1 <- read_delim(file = sprintf("%s/telcodata-%s.csv", datasetFolder, user_1), delim = ";", 
                                 escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
          },
          nethealth = {
            data_1 <- read_delim(file = sprintf("%s/telcodata-%s.csv", datasetFolder, user_1), delim = ";",
                                 escape_double = FALSE, trim_ws = TRUE, col_types = cols(
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
  
  data_1 <- data_1 %>%
    mutate(User_1 = user_1, User_2 = ifelse(SenderID == user_1, ReceiverID, SenderID))
  
  ## get all the users that the current user talked to more than 20 times
  prefered_users <- data_1 %>%
    filter(ReceiverID != SenderID) %>%
    group_by(User_1, User_2) %>%
    count() %>%
    filter(n >= lowerBound) %>%
    filter(n <= upperBound) %>% ## do not fit extremelly long processes -- out of memory.
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(User_1 = as.numeric(user_1)) %>%
    mutate(atomic.tmp.file = sprintf("%s/atomic-fit-%s-%s-%s-%s.dat", tmpFolder, kernel.type, exo.type, user_1, User_2)) %>%
    filter(! file.exists(atomic.tmp.file) )
}))

print(sprintf("Doing %d pairs", nrow(peers)))

################### fitting in parallel for all users ########################
if (nrow(peers) > 0) {
  foo <- future_apply(MARGIN = 1, future.chunk.size = chunk_size, future.seed = T, X = peers, FUN = function(line){ 
    user_1 <- line["User_1"]
    user_2 <- line["User_2"]
    
    ## load data
    switch (dataset,
            netsense = {
              data_1 <- read_delim(file = sprintf("%s/telcodata-%s.csv", datasetFolder, user_1), delim = ";", 
                                   escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
            },
            nethealth = {
              data_1 <- read_delim(file = sprintf("%s/telcodata-%s.csv", datasetFolder, user_1), delim = ";",
                                   escape_double = FALSE, trim_ws = TRUE, col_types = cols(
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
    
    atomic.tmp.file <- sprintf("%s/atomic-fit-%s-%s-%s-%s.dat", tmpFolder, kernel.type, exo.type, as.numeric(user_1), as.numeric(user_2))
    old.file.name <- sprintf("%s/atomic-fit-%s-%s-%s-%s-%dcalls.dat", tmpFolder, kernel.type, exo.type, as.numeric(user_1), as.numeric(user_2), lowerBound)
    ## if old file name exists, rename it to new convention
    if (file.exists(old.file.name)) {
      print(sprintf("Old atomic file \"%s\". Will be renamed to \"%s\"", old.file.name, atomic.tmp.file))
      file.rename(from = old.file.name, to = atomic.tmp.file)
    }
    
    ## check if fit was already done. If so, simply load and return
    if (file.exists(atomic.tmp.file)) {
      res <- readRDS(file = atomic.tmp.file)
      return(res)
    }
    
    ## if here, means nobody did this work. Marking our teritory
    res <- list()
    saveRDS(res, file = atomic.tmp.file, compress = "bzip2")
    
    ## get the communications between the two
    calls <- data_1 %>%
      filter(SenderID == as.numeric(user_1) & ReceiverID == as.numeric(user_2) | SenderID == as.numeric(user_2) & ReceiverID == as.numeric(user_1)) %>%
      arrange(EventTime)
    
    ## compute time diffs
    calls$timeDiff <- calls$EventTime - calls$EventTime[1]
    
    ## Now, let's try to fit a Hawkes to the data
    ## build the history data frame
    history <- data.frame(matrix(data = 1, nrow = nrow(calls), ncol = 2))
    names(history) <- c("magnitude", "time")
    ## make time difference in days, to avoid numerical issues
    history$time <- as.numeric(calls$timeDiff) / (3600 * 24)
    
    ## holdout neg likelihood
    if (holdout) {
      split_point <- max(history$time) * 0.8
      train_history <- history[with(history, time <= split_point),]
    } else {
      train_history <- history
    }
    
    ## fit the event series in an error catching env -- in case things go south, we don't loose the worker, and we move on.
    tryCatch(expr = {
      res <- fit_series(list(train_history), 
                        model_type = c(exo.type, kernel.type),
                        observation_time = max(train_history$time), 
                        cores = nCoresEvently)
      
      res$users <- c(user_1 = user_1, user_2 = user_2)
      
      ## holdout neg likelihood
      if (holdout) {
        totalLikelihood <- get_hawkes_neg_likelihood_value(par = res$par, data = list(history), model_type = res$model_type, observation_time = max(history$time))
        hll = (totalLikelihood - res$value) / (nrow(history) - nrow(train_history))
        res$hll <- hll
      }
      
      ## branching factor
      if (kernel.type == "PL") {
        bf <- evently:::get_branching_factor.hawkes_PL(res)
      } else {
        bf <- evently:::get_branching_factor.hawkes_EXP(res)
      }
      res$bf <- bf
      
      ## viral score
      # vs <- get_viral_score(res, mu = 1)
      # res$vs <- vs
      ## save the atomic fit, so that we can continue later
      saveRDS(res, file = atomic.tmp.file, compress = "bzip2")
    }, 
    error = function(e) {
      print(sprintf("Evently fitting failed! I'm deleting the semaphore for the pair %s->%s", user_1, user_2))
      print(e)
      ## remove atomic temporary file, maybe another worker has better luck
      file.remove(atomic.tmp.file)
      
      ## build a dummy construct
      res$users <- c(user_1 = user_1, user_2 = user_2)
      res$bf <- -1
      
      ## return this dummy construct
      return(res)
    })
    
    return(res)
  })
}

####################### results gathering ###################
foo <- future_lapply(future.seed = T, X = users, FUN = function(user_1) {
  ## load data
  switch (dataset,
          netsense = {
            data_1 <- read_delim(file = sprintf("%s/telcodata-%s.csv", datasetFolder, user_1), delim = ";", 
                                 escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
          },
          nethealth = {
            data_1 <- read_delim(file = sprintf("%s/telcodata-%s.csv", datasetFolder, user_1), delim = ";",
                                 escape_double = FALSE, trim_ws = TRUE, col_types = cols(
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
  
  data_1 <- data_1 %>%
    mutate(User_1 = user_1, User_2 = ifelse(SenderID == user_1, ReceiverID, SenderID))
  
  ## get all the users that the current user talked to more than 20 times
  prefered_users <- data_1 %>%
    filter(ReceiverID != SenderID) %>%
    group_by(User_1, User_2) %>%
    count() %>%
    filter(n >= lowerBound) %>%
    filter(n <= upperBound) %>% ## do not fit extremelly long processes -- out of memory.
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(User_1 = as.numeric(user_1))
  
  results <- lapply(X = prefered_users$User_2, FUN = function(user_2) {
    res <- list()
    atomic.tmp.file <- sprintf("%s/atomic-fit-%s-%s-%s-%s.dat", tmpFolder, kernel.type, exo.type, user_1, user_2)
    ## check if fit was already done. If so, simply load and return
    if (file.exists(atomic.tmp.file)) {
      tryCatch(expr = {
        res <- readRDS(file = atomic.tmp.file)
        return(res)
      },
      error = function(e) {
        print(sprintf("Failed loading file \"%s\". Deleting file...", atomic.tmp.file))
        ## remove atomic temporary file, maybe another worker has better luck
        file.remove(atomic.tmp.file)
      })

    } else {
      print(sprintf("The pair: %s->%s is not fitted.", user_1, user_2))
    }
    return(res)
  })
  saveRDS(results, file = sprintf("%s/fits-%s-%s-20calls.dat", resultsFolder, kernel.type, user_1), compress = "bzip2")
})
