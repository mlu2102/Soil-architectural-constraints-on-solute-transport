# *************************************************************************
# Statistical analysis for ACI metrics
#
# This script fits linear models relating deep and downslope ACI to
# soil texture, aggregate size, and antecedent precipitation metrics.
# It also performs leave-one-out (jackknife) sensitivity analyses for
# relationships reported as positive in the main text.
#
# Outputs are stored as structured lists to support figure generation
# and review of model behavior.
# *************************************************************************

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Define filepaths --------------------------------------------------------

filepaths <- list()
filepaths$raw$deepACI      <- "data/raw/deepACI.csv"
filepaths$raw$downslopeACI        <- "data/raw/downslopeACI.csv"

filepaths$intermediate$deepACI  <- "data/intermediate/deepACI.rds"
filepaths$intermediate$downslopeACI    <- "data/intermediate/downslopeACI.rds"

filepaths$output$deepACI$precip <- "output/data/deepACI_precip.csv"
filepaths$output$downslopeACI$precip <- "output/data/downslopeACI_precip.csv"

filepaths$output$deepACI$drynessIndex <- "output/data/deepACI_drynessIndex.csv"
filepaths$output$downslopeACI$drynessIndex <- "output/data/downslopeACI_drynessIndex.csv"

# Define functions --------------------------------------------------------

# jackknife()
#
# Performs leave-one-out (n−1) sensitivity analysis for a simple linear regression.
# For each observation in inputData, the function refits the model with that
# observation omitted and records the resulting slope and p-value for the
# specified predictor.
#
# This is used to evaluate whether the direction and magnitude of a relationship
# are robust to individual influential observations
#
# Arguments:
#   inputData          Tibble containing all variables
#   inputPredictorVar  Character string naming the predictor variable
#   inputDependentVar  Character string naming the response variable
#
# Returns:
#   A tibble with one row per omitted observation (identified by pitID),
#   containing the regression slope and p-value from the n−1 model.

jackknife <- function(inputData, inputPredictorVar, inputDependentVar){
  
  stopifnot(inputPredictorVar %in% names(inputData))
  stopifnot(inputDependentVar %in% names(inputData))
  
  outputData <- tibble()
  
  for(i in 1:nrow(inputData)){
    omit  <- inputData[i,]
    df    <- inputData %>%
      anti_join(omit, by = "pitID")
    
    frm   <- as.formula(paste0(inputDependentVar, " ~ ",inputPredictorVar))
    
    model <- lm(frm, df)

    s <- summary(model)
    outputData[i,"run"]     <- i
    outputData[i,"site"]    <- omit$site
    outputData[i,"subsite"] <- omit$subsite
    outputData[i,"pit"]     <- omit$pit
    outputData[i,"pitID"]   <- omit$pitID
    outputData[i,"slope"]   <- s$coefficients[inputPredictorVar, "Estimate"]
    outputData[i,"p"]       <- s$coefficients[inputPredictorVar, "Pr(>|t|)"]
  }
  
  return(outputData)
}

# constructSummary()
#
# Extracts key statistics from a linear model for a specified predictor.
# This helper function summarizes the slope (coefficient estimate) and
# associated p-value for a single predictor variable from an lm() object.
# It is used to standardize reporting of regression results across models.
#
# Arguments:
#   inputModel        An lm() object.
#   inputPredictorVar Character string giving the name of the predictor
#                     variable
#
# Returns:
#   A tibble with one row containing:
#     - predictor:  predictor variable name
#     - p:          p-value for the predictor coefficient
#     - m:          regression slope

constructSummary <- function(inputModel, inputPredictorVar){

  s <- summary(inputModel)
  
  p <- s$coefficients[inputPredictorVar, "Pr(>|t|)"]
  m <- s$coefficients[inputPredictorVar, "Estimate"]
  
  outputData <- tibble(predictor = inputPredictorVar,
                       p = p,
                       m = m)
  
  return(outputData)
}

# Deep ACI ----------------------------------------------------------------
# This section:
# 1) Fits full linear models for deep ACI vs predictors and stores slope + p
# 2) Runs leave-one-out jackknife for relationships presented in main text

deepACI <- list()

deepACI$raw <- read_csv(filepaths$raw$deepACI)

## Aggregate size ---------------------------------------------------------

deepACI$aggSize$fullModel$model <- lm(deepACI ~ aggSize, deepACI$raw)
deepACI$aggSize$fullModel$summary <- constructSummary(inputModel = deepACI$aggSize$fullModel$model,
                                                      inputPredictorVar = "aggSize")
deepACI$aggSize$jackknife <- jackknife(inputData = deepACI$raw, 
                                       inputPredictorVar = "aggSize",
                                       inputDependentVar = "deepACI")  %>%
  mutate(leverage = case_when(pitID == "LUQICM" ~ T,
                              TRUE ~ F))

## Areal clay content -----------------------------------------------------

deepACI$arealClayContent$fullModel$model <- lm(deepACI ~ arealClayContent, deepACI$raw)
deepACI$arealClayContent$fullModel$summary <- constructSummary(inputModel = deepACI$arealClayContent$fullModel$model,
                                                        inputPredictorVar = "arealClayContent")
deepACI$arealClayContent$jackknife <- jackknife(inputData = deepACI$raw,
                                         inputPredictorVar = "arealClayContent",
                                         inputDependentVar = "deepACI") %>%
  mutate(leverage = case_when(pitID %in% c("CASDT", "LUQICM") ~ T,
                              TRUE ~ F))

## Mean clay concentration ------------------------------------------------

deepACI$meanClayConcentration$fullModel$model <- lm(deepACI ~ meanClayConcentration, deepACI$raw)
deepACI$meanClayConcentration$fullModel$summary <- constructSummary(inputModel = deepACI$meanClayConcentration$fullModel$model,
                                                       inputPredictorVar = "meanClayConcentration")

## Antecedent precipitation ------------------------------------------------

precipVars <- c("precip7", "precip14", "precip30", "precip90")

for (i in precipVars){
  
  frm <- as.formula(paste0("deepACI ~ ",i))
  
  deepACI[[i]]$model <- lm(frm, deepACI$raw)
  deepACI[[i]]$summary <- constructSummary(inputModel = deepACI[[i]]$model,
                                           inputPredictorVar = i)
  
}

deepACI$precipAll <- bind_rows(lapply(precipVars, function(v) deepACI[[v]]$summary))

## Dryness indexes ---------------------------------------------------------

deepACI$pet_map$model <- lm(deepACI ~ pet_map, deepACI$raw)
deepACI$pet_map$summary <- constructSummary(inputModel = deepACI$pet_map$model,
                                            inputPredictorVar = "pet_map")

deepACI$aet_map$model <- lm(deepACI ~ aet_map, deepACI$raw)
deepACI$aet_map$summary <- constructSummary(inputModel = deepACI$aet_map$model,
                                            inputPredictorVar = "aet_map")

deepACI$drynessIndex$summary <- bind_rows(deepACI$pet_map$summary, deepACI$aet_map$summary)

# Downslope ACI -----------------------------------------------------------
# This section:
# 1) Fits full linear models for downslope ACI vs predictors and stores slope + p
# 2) Runs leave-one-out jackknife for relationships presented in main text

downslopeACI <- list()

downslopeACI$raw <- read_csv(filepaths$raw$downslopeACI)

## Aggregate size ---------------------------------------------------------

downslopeACI$aggSize$fullModel$model <- lm(downslopeACI ~ aggSize, downslopeACI$raw)
downslopeACI$aggSize$fullModel$summary <- constructSummary(inputModel = downslopeACI$aggSize$fullModel$model,
                                                    inputPredictorVar = "aggSize")
downslopeACI$aggSize$jackknife <- jackknife(inputData = downslopeACI$raw, 
                                     inputPredictorVar = "aggSize",
                                     inputDependentVar = "downslopeACI") %>%
  mutate(leverage = case_when(pitID %in% c("CAMCM", "CAMCT") ~ T,
                              TRUE ~ F))

## Areal clay content -----------------------------------------------------

downslopeACI$arealClayContent$fullModel$model <- lm(downslopeACI ~ arealClayContent, downslopeACI$raw)
downslopeACI$arealClayContent$fullModel$summary <- constructSummary(inputModel = downslopeACI$arealClayContent$fullModel$model,
                                                      inputPredictorVar = "arealClayContent")
downslopeACI$arealClayContent$jackknife <- jackknife(inputData = downslopeACI$raw,
                                       inputPredictorVar = "arealClayContent",
                                       inputDependentVar = "downslopeACI") %>%
  mutate(leverage = case_when(pitID %in% c("CAMCM") ~ T,
                              TRUE ~ F))

## Mean clay concentration ------------------------------------------------

downslopeACI$meanClayConcentration$fullModel$model <- lm(downslopeACI ~ meanClayConcentration, downslopeACI$raw)
downslopeACI$meanClayConcentration$fullModel$summary <- constructSummary(inputModel = downslopeACI$meanClayConcentration$fullModel$model,
                                                     inputPredictorVar = "meanClayConcentration")

## Antecedent precipitation ------------------------------------------------

precipVars <- c("precip7", "precip14", "precip30", "precip90")

for (i in precipVars){
  
  frm <- as.formula(paste0("downslopeACI ~ ",i))
  
  downslopeACI[[i]]$model <- lm(frm, downslopeACI$raw)
  downslopeACI[[i]]$summary <- constructSummary(inputModel = downslopeACI[[i]]$model,
                                         inputPredictorVar = i)
  
}

downslopeACI$precipAll <- bind_rows(lapply(precipVars, function(v) downslopeACI[[v]]$summary))

## Dryness index -----------------------------------------------------------

downslopeACI$pet_map$model <- lm(downslopeACI ~ pet_map, downslopeACI$raw)
downslopeACI$pet_map$summary <- constructSummary(inputModel = deepACI$pet_map$model,
                                            inputPredictorVar = "pet_map")

downslopeACI$aet_map$model <- lm(downslopeACI ~ aet_map, downslopeACI$raw)
downslopeACI$aet_map$summary <- constructSummary(inputModel = downslopeACI$aet_map$model,
                                            inputPredictorVar = "aet_map")

downslopeACI$drynessIndex$summary <- bind_rows(downslopeACI$pet_map$summary, downslopeACI$aet_map$summary)

# Save output -------------------------------------------------------------

## Dryness index -----------------------------------------------------------

write_csv(deepACI$drynessIndex$summary, filepaths$output$deepACI$drynessIndex)
write_csv(downslopeACI$drynessIndex$summary, filepaths$output$downslopeACI$drynessIndex)

## Antecedent precipitation ------------------------------------------------

write_csv(deepACI$precipAll, filepaths$output$deepACI$precip)
write_csv(downslopeACI$precipAll, filepaths$output$downslopeACI$precip)

## Plot data ---------------------------------------------------------------

saveRDS(deepACI, filepaths$intermediate$deepACI)
saveRDS(downslopeACI, filepaths$intermediate$downslopeACI)


