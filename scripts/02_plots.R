# *************************************************************************
# Figure generation for ACI metrics
#
# This script generates figures relating deep and downslope ACI to soil texture 
# and aggregate size metrics. Plots include full linear model fits and 
# leave-one-out (jackknife) sensitivity visualizations. 
#
# Figures are assembled into multi-panel layouts with shared legends
# and annotations using model outputs produced in 01_stats.R.
# *************************************************************************

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)

# Define filepaths --------------------------------------------------------

filepaths <- list()

filepaths$plotAesthetics <- "scripts/plotAesthetics.R"

filepaths$raw$deepACI <- "data/raw/deepACI.csv"
filepaths$raw$downslopeACI <- "data/raw/downslopeACI.csv"

filepaths$raw$asd <- "data/raw/asd.csv"
filepaths$raw$eoc <- "data/raw/eoc.csv"
filepaths$raw$soc <- "data/raw/soc.csv"
filepaths$raw$texture <- "data/raw/texture.csv"

filepaths$intermediate$deepACI <- "data/intermediate/deepACI.rds"
filepaths$intermediate$downslopeACI <- "data/intermediate/downslopeACI.rds"

filepaths$plots$arealClayContent <- "output/plots/Fig4_arealClayContent.png"
filepaths$plots$aggSize <- "output/plots/Fig5_aggSize.png"
filepaths$plots$meanClayConcentration <- "output/plots/S9_meanClayConcentration.png"

filepaths$plots$clayConcentration_unscaled <- "output/plots/S1_clayConcentration.png"
filepaths$plots$clayContent_unscaled <- "output/plots/S2_clayContent.png"
filepaths$plots$aggSize_unscaled <- "output/plots/S3_aggSize.png"
filepaths$plots$soc <- "output/plots/S4_soc.png"
filepaths$plots$eoc <- "output/plots/S6_eoc.png"

# Load data ---------------------------------------------------------------

deepACI <- list()
downslopeACI   <- list()
metricRanges <- list()

deepACI$raw <- read_csv(filepaths$raw$deepACI)
downslopeACI$raw <- read_csv(filepaths$raw$downslopeACI)

deepACI$intermediate <- readRDS(filepaths$intermediate$deepACI)
downslopeACI$intermediate <- readRDS(filepaths$intermediate$downslopeACI)

metricRanges$asd$raw <- read_csv(filepaths$raw$asd)
metricRanges$eoc$raw <- read_csv(filepaths$raw$eoc)
metricRanges$soc$raw <- read_csv(filepaths$raw$soc)
metricRanges$texture$raw <- read_csv(filepaths$raw$texture)


# Aesthetic definitions ---------------------------------------------------

theme_set(
  theme_bw()+
    theme(axis.title = element_text(size = 8,
                                face = "bold"),
      axis.text  = element_text(size = 7.5),
      plot.tag   = element_text(size = 10, face = "bold"),
      plot.tag.position = c(0, 1),
      panel.grid = element_blank(),
      plot.margin = margin(4, 6, 4, 4)))

diColors <-c(
  "EV"   = "#3300FF",
  "IC"   = "#0000FF",
  "P"    = "#6600CC",
  "H"    = "#6600CC",
  "NL"   = "#9900CC",
  "MC"   = "#990099",
  "SWL"  = "#CC0033",
  "SJER" = "#FF0066",
  "SD"   = "#FF0000")

ecosystemColors <- c(
  "Younger tropical rainforest"   = "#3C5DB3",
  "Older tropical rainforest"   = "#5A96D6",
  "Temperate pine forest"    = "#A5CC6B",
  "Temperate hardwood forest"    = "#5E9C2A",
  "Semi-arid shrubland"   = "#F49F0A",
  "Fir and pine forest"   = "#B86F00",
  "Aspen stand"  = "#88A2AA",
  "Oak savanna" = "#4F6F78",
  "Desert"   = "#F42C04")

# Define annotation function ----------------------------------------------

# annotatePlot()
#
# Annotates plots with p-values and slopes
#
# Arguments:
#   inputPlot          Base plot depicting a relationship of interest
#   inputData          List containing $raw (tibble used to set plot extents) and
#                      $intermediate (model summaries used for annotation)
#   inputXVar          Character string naming the x-axis variable
#   inputYVar          Character string naming the y-axis variable
#
# Returns:
#   The original plot, annotated with the associated model's p-value and
#   regression coefficient

annotatePlot <- function(inputPlot,
                         inputData,
                         inputXVar,
                         inputYVar) {
  
  #Identifies maximum x and y values for use in calculating annotation coordinates
  xmax <- max(inputData[["raw"]][[inputXVar]], na.rm = TRUE)
  ymax <- max(inputData[["raw"]][[inputYVar]], na.rm = TRUE)
  
  #Sets padding area around annotations
  pad_x <- 0.01 * (xmax)
  pad_y <- 0.01 * (ymax)
  
  p <- inputData[["intermediate"]][[inputXVar]]$fullModel$summary$p
  p <- round(p, digits = 2)
  
  
  #Append significance symbols to p-value
  pSuffix <- case_when(p > 0.05 ~ "", 
                       p <= 0.05 & p > 0.01 ~ "*",
                       p <= 0.01 & p > 0.001 ~ "**",
                       p <= 0.001 ~ "***")
  
  m <- inputData[["intermediate"]][[inputXVar]]$fullModel$summary$m
  m <- round(m, digits = 3)
  
  jLab <- ""
  
  if(!is.null(inputData[["intermediate"]][[inputXVar]]$jackknife)){
    jAll <- inputData[["intermediate"]][[inputXVar]]$jackknife %>%
      filter(leverage == TRUE) %>%
      pull(p)
    
    if(length(jAll) >1){
      minJ <- round(min(jAll), digits = 2)
      maxJ <- round(max(jAll), digits = 2)
      
      if(minJ < 0.001){
        minJ <- "< 0.001"
      }
      if(maxJ < 0.001){
        maxJ <- "< 0.001"
      }
      
      jMinSuffix <- case_when(minJ > 0.05 ~ "", 
                              minJ <= 0.05 & minJ > 0.01 ~ "*",
                              minJ <= 0.01 & minJ > 0.001 ~ "**",
                              minJ <= 0.001 ~ "***")

      
      jMaxSuffix <- case_when(maxJ > 0.05 ~ "", 
                              maxJ <= 0.05 & maxJ > 0.01 ~ "*",
                              maxJ <= 0.01 & maxJ > 0.001 ~ "**",
                              maxJ <= 0.001 ~ "***")

      
      jLab <- paste0(minJ,jMinSuffix,", ",maxJ,jMaxSuffix)
      
      
    }
    if(length(jAll) == 1){
      
      jSuffix <- case_when(jAll > 0.05 ~ "", 
                           jAll <= 0.05 & jAll > 0.01 ~ "*",
                           jAll <= 0.01 & jAll > 0.001 ~ "**",
                           jAll <= 0.001 ~ "***")
      
      jLab <- paste0(round(jAll, digits = 2),jSuffix)
      
  
    }
    
    jLab <- paste0("(",jLab,")")
  }
  
  
  
  pLab <- paste0("p = ", p, pSuffix," ", jLab)  
  mLab <- paste0("slope = ", m)
  
  outputPlot <- inputPlot +
    annotate("text",
      x = xmax - pad_x,
      y = ymax - pad_y,
      label = pLab,
      size = 3,
      hjust = 1,
      vjust = 1) +
    annotate("text",
      x = xmax - pad_x,
      y = (ymax * 0.90) - pad_y,
      label = mLab,
      size = 3,
      hjust = 1,
      vjust = 1)
  
  return(outputPlot)
  
}

# Aggregate size ----------------------------------------------------------

## Deep -------------------------------------------------------------------

deepACI$plots$aggSize$model <- deepACI$intermediate$aggSize$jackknife %>%
  select(pitID, leverage) %>%
  left_join(deepACI$raw, by = "pitID") %>%
  ggplot() +
  geom_smooth(aes(x = aggSize, y = deepACI),
    color = "black",
    method = "lm",
    se = FALSE,
    formula = y ~ x) +
  geom_point(aes(x = aggSize, y = deepACI,
      fill = subsite,
      alpha = leverage),
    shape = 21,
    size = 3) +
  scale_alpha_manual(values = c(`TRUE` = 0.3, `FALSE` = 1), guide = "none") +
  scale_fill_manual(values = diColors) +
  theme(legend.position = "none") +
  ylab("log(Deep ACI)") +
  xlab(expression(bold("Surface large aggregate fraction")))

deepACI$plots$aggSize$annotatedModel <- annotatePlot(inputPlot = deepACI$plots$aggSize$model,
  inputData = deepACI,
  inputXVar = "aggSize",
  inputYVar = "deepACI")

deepACI$plots$aggSize$jackknife <- ggplot(data = deepACI$intermediate$aggSize$jackknife) +
  geom_hline(aes(yintercept = deepACI$intermediate$aggSize$fullModel$summary$m),
             linetype = "dashed") +
  geom_point(aes(x = run, y = slope), size = 1.5, color = "black") +
  geom_text(data = deepACI$intermediate$aggSize$jackknife %>%
      filter(leverage),
    aes(x = run, y = slope),
    label = "†",
    hjust = -0.75,
    vjust = -0.5,
    size = 1.75) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) +
  xlab("Leave-one-out iteration") +
  ylab("Regression slope") +
  theme(axis.title = element_text(size = 8), axis.text  = element_text(size = 6))

## Downslope ---------------------------------------------------------------

downslopeACI$plots$aggSize$model <- downslopeACI$intermediate$aggSize$jackknife %>%
  select(pitID, leverage) %>%
  left_join(downslopeACI$raw, by = "pitID") %>%
  ggplot() +
  geom_smooth(
    aes(x = aggSize, y = downslopeACI),
    color = "black",
    method = "lm",
    se = FALSE,
    formula = y ~ x) +
  geom_point(aes(x = aggSize, y = downslopeACI,
      fill = subsite,
      alpha = leverage),
    shape = 24,
    size = 3) +
  scale_alpha_manual(values = c(`TRUE` = 0.3, `FALSE` = 1), guide = "none") +
  scale_fill_manual(values = diColors) +
  theme(legend.position = "none") +
  ylab("log(Downslope ACI)") +
  xlab(expression(bold("Upslope surface large aggregate fraction"))) +
  scale_x_continuous(breaks = c(-1, 0, 1))

downslopeACI$plots$aggSize$annotatedModel <- annotatePlot(inputPlot = downslopeACI$plots$aggSize$model,
  inputData = downslopeACI,
  inputXVar = "aggSize",
  inputYVar = "downslopeACI")

downslopeACI$plots$aggSize$jackknife <- ggplot(data = downslopeACI$intermediate$aggSize$jackknife) +
  geom_hline(aes(yintercept = downslopeACI$intermediate$aggSize$fullModel$summary$m),
    linetype = "dashed") +
  geom_point(aes(x = run, y = slope),
    size = 1.5,
    shape = 17,
    color = "black") +
  geom_text(data = downslopeACI$intermediate$aggSize$jackknife %>%
      filter(leverage),
    aes(x = run, y = slope),
    label = "†",
    hjust = -0.75,
    vjust = -0.5,
    size = 1.75) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  xlab("Leave-one-out iteration") +
  ylab("Regression slope") +
  theme(axis.title = element_text(size = 8), axis.text  = element_text(size = 6)) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.1)))

# Areal clay content ------------------------------------------------------

## Deep --------------------------------------------------------------------

deepACI$plots$arealClayContent$model <- deepACI$intermediate$arealClayContent$jackknife %>%
  select(pitID, leverage) %>%
  left_join(deepACI$raw, by = "pitID") %>%
  ggplot() +
  geom_smooth(aes(x = arealClayContent, y = deepACI),
    color = "black",
    method = "lm",
    se = FALSE,
    formula = y ~ x) +
  geom_point(aes(x = arealClayContent, y = deepACI,
      fill = subsite,
      alpha = leverage),
    shape = 21,
    size = 3) +
  scale_alpha_manual(values = c(`TRUE` = 0.3, `FALSE` = 1), guide = "none") +
  scale_fill_manual(values = diColors) +
  theme(legend.position = "none") +
  ylab("log(Deep ACI)") +
  xlab(expression(bold("Areal clay content (g cm"^-2 * ")")))

deepACI$plots$arealClayContent$annotatedModel <- annotatePlot(inputPlot = deepACI$plots$arealClayContent$model,
  inputData = deepACI,
  inputXVar = "arealClayContent",
  inputYVar = "deepACI")

deepACI$plots$arealClayContent$jackknife <- ggplot(data = deepACI$intermediate$arealClayContent$jackknife) +
  geom_hline(aes(yintercept = deepACI$intermediate$arealClayContent$fullModel$summary$m),
    linetype = "dashed") +
  geom_point(aes(x = run, y = slope), size = 1.5, color = "black") +
  geom_text(data = deepACI$intermediate$arealClayContent$jackknife %>%
      filter(leverage),
    aes(x = run, y = slope),
    label = "†",
    hjust = -0.75,
    vjust = -0.5,
    size = 1.75) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) +
  xlab("Leave-one-out iteration") +
  ylab("Regression slope") +
  theme(axis.title = element_text(size = 8), axis.text  = element_text(size = 6))

## Downslope ---------------------------------------------------------------

downslopeACI$plots$arealClayContent$model <- downslopeACI$intermediate$arealClayContent$jackknife %>%
  select(pitID, leverage) %>%
  left_join(downslopeACI$raw, by = "pitID") %>%
  ggplot() +
  geom_smooth(aes(x = arealClayContent, y = downslopeACI),
    color = "black",
    method = "lm",
    se = FALSE,
    formula = y ~ x) +
  geom_point( aes(x = arealClayContent, y = downslopeACI,
      fill = subsite,
      alpha = leverage),
    shape = 24,
    size = 3) +
  scale_alpha_manual(values = c(`TRUE` = 0.3, `FALSE` = 1), guide = "none") +
  scale_fill_manual(values = diColors) +
  theme(legend.position = "none") +
  ylab("log(Downslope ACI)") +
  xlab(expression(bold("Upslope areal clay content (g cm"^-2 * ")"))) +
  scale_x_continuous(breaks = c(-1, 0, 1))

downslopeACI$plots$arealClayContent$annotatedModel <- annotatePlot(inputPlot = downslopeACI$plots$arealClayContent$model,
  inputData = downslopeACI,
  inputXVar = "arealClayContent",
  inputYVar = "downslopeACI")

downslopeACI$plots$arealClayContent$jackknife <- ggplot(data = downslopeACI$intermediate$arealClayContent$jackknife) +
  geom_hline(aes(yintercept = downslopeACI$intermediate$arealClayContent$fullModel$summary$m),
    linetype = "dashed"  ) +
  geom_point(aes(x = run, y = slope),
    size = 1.5,
    shape = 17,
    color = "black") +
  geom_text(data = downslopeACI$intermediate$arealClayContent$jackknife %>%
      filter(leverage),
    aes(x = run, y = slope),
    label = "†",
    hjust = -0.75,
    vjust = -0.5,
    size = 1.75) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  xlab("Leave-one-out iteration") +
  ylab("Regression slope") +
  theme(axis.title = element_text(size = 8), axis.text  = element_text(size = 6)) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.1)))

# Mean clay concentration -------------------------------------------------

## Deep -------------------------------------------------------------------

deepACI$plots$meanClayConcentration$model <- ggplot(data = deepACI$raw) +
  geom_point(aes(x = meanClayConcentration, y = deepACI, fill = subsite),
    shape = 21,
    size = 3) +
  geom_smooth(aes(x = meanClayConcentration, y = deepACI),
              color = "black",
              method = "lm",
              se = FALSE,
              formula = y ~ x) +
  scale_fill_manual(values = diColors) +
  theme(legend.position = "none") +
  ylab("log(Deep ACI)") +
  xlab(expression(bold("Mean clay concentration (g g"^-1 * ")")))

deepACI$plots$meanClayConcentration$annotatedModel <- annotatePlot(inputPlot = deepACI$plots$meanClayConcentration$model,
  inputData = deepACI,
  inputXVar = "meanClayConcentration",
  inputYVar = "deepACI")

## Downslope --------------------------------------------------------------

downslopeACI$plots$meanClayConcentration$model <- ggplot(data = downslopeACI$raw) +
  geom_point(aes(x = meanClayConcentration, y = downslopeACI, fill = subsite),
    shape = 21,
    size = 3) +
  geom_smooth(aes(x = meanClayConcentration, y = downslopeACI),
              color = "black",
              method = "lm",
              se = FALSE,
              formula = y ~ x) +
  scale_fill_manual(values = diColors) +
  theme(legend.position = "none") +
  ylab("log(Downslope ACI)") +
  xlab(expression(bold("Upslope mean clay concentration (g g"^-1 * ")")))

downslopeACI$plots$meanClayConcentration$annotatedModel <- annotatePlot(inputPlot = downslopeACI$plots$meanClayConcentration$model,
  inputData = downslopeACI,
  inputXVar = "meanClayConcentration",
  inputYVar = "downslopeACI")

# Aridity index legent ----------------------------------------------------

legend <- list()

legend$full <- ggplot() +
  geom_point(data = deepACI$raw, aes(x = 1, y = pet_map, color = pet_map)) +
  scale_color_gradient(low = "blue", high = "red",
    limits = c(0.59, 5.74),
    name = "Aridity Index",
    guide = guide_colorbar(direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(7, "cm"),
      # <-- increase this
      barheight = unit(0.4, "cm"))) +
  theme_void() +
  theme(legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 12,
      face = "bold"
    ))

legend$legend <- get_legend(legend$full)


# Metric ranges -----------------------------------------------------------

## Clay concentration -----------------------------------------------------

metricRanges$plots$clayConcentration_unscaled <- metricRanges$texture$raw %>%
  mutate(pit = case_when(pit == "R" ~ "Ridge",
                         pit == "M" ~ "Midslope",
                         pit == "T" ~ "Toeslope")) %>%
  mutate(pit = factor(pit, levels = c("Ridge", "Midslope", "Toeslope"))) %>%
  ggplot()+
  geom_linerange(aes(x = pitID, ymin = clayMin, ymax = clayMax),
                 linewidth = 0.3)+
  geom_point(aes(y = clayMean, 
                 x = reorder(pitID, clayMean),
                 fill = ecosystem),
             shape = 21,
             size = 2.5) +
  scale_fill_manual(values = ecosystemColors, name = "Ecosystem")+
  xlab(NULL)+
  ylab(expression(bold("Clay (%)"))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8),
        legend.key.height = unit(0.6, "lines"),
        legend.spacing.y = unit(0.15, "lines"),
        panel.spacing = unit(1.2, "lines"))+
  scale_x_discrete(expand = expansion(add = 1))+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  facet_wrap(~pit)

ggsave(filepaths$plots$clayConcentration_unscaled, metricRanges$plots$clayConcentration_unscaled, 
       width = 6.5, height = 1.75,
       dpi = 600)

## Clay content ------------------------------------------------------------

metricRanges$plots$clayContent_unscaled <- metricRanges$texture$raw %>%
  mutate(pit = case_when(pit == "R" ~ "Ridge",
                         pit == "M" ~ "Midslope",
                         pit == "T" ~ "Toeslope")) %>%
  mutate(pit = factor(pit, levels = c("Ridge", "Midslope", "Toeslope"))) %>%
  ggplot()+
  geom_point(aes(y = clayContent_B, 
                 x = reorder(pitID, clayContent_B),
                 fill = ecosystem),
             size = 3,
             shape = 21) +
  scale_fill_manual(values = ecosystemColors, name = "Ecosystem")+
  xlab(NULL)+
  ylab(expression(bold("Areal clay content (g cm"^-2 * ")")))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 8),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8),
        legend.key.height = unit(0.6, "lines"),
        legend.spacing.y = unit(0.15, "lines"),
        panel.spacing = unit(1.2, "lines"))+
  scale_x_discrete(expand = expansion(add = 1))+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  facet_wrap(~pit)

ggsave(filepaths$plots$clayContent_unscaled, metricRanges$plots$clayContent_unscaled, 
       width = 6.5, height = 1.75,
       dpi = 600)


## Aggregate size ---------------------------------------------------------

metricRanges$plots$aggSize_unscaled <- metricRanges$asd$raw %>%
  mutate(pit = case_when(pit == "R" ~ "Ridge",
                         pit == "M" ~ "Midslope",
                         pit == "T" ~ "Toeslope")) %>%
  mutate(pit = factor(pit, levels = c("Ridge", "Midslope", "Toeslope"))) %>%
  ggplot()+
  geom_point(aes(y = f476, 
                 x = reorder(pitID, f476),
                 fill = ecosystem),
             size = 3,
             shape = 21) +
  scale_fill_manual(values = ecosystemColors, name = "Ecosystem")+
  xlab(NULL)+
  ylab(expression(bold("Large aggregate fraction"))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8),
        legend.key.height = unit(0.6, "lines"),
        legend.spacing.y = unit(0.15, "lines"),
        panel.spacing = unit(1.2, "lines"))+
  scale_x_discrete(expand = expansion(add = 1))+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  facet_wrap(~pit)

ggsave(filepaths$plots$aggSize_unscaled, metricRanges$plots$aggSize_unscaled, 
       width = 6.5, height = 1.75,
       dpi = 600)

## SOC --------------------------------------------------------------------

metricRanges$plots$soc <- metricRanges$soc$raw %>%
  mutate(pit = case_when(pit == "R" ~ "Ridge",
                         pit == "M" ~ "Midslope",
                         pit == "T" ~ "Toeslope")) %>%
  mutate(pit = factor(pit, levels = c("Ridge", "Midslope", "Toeslope"))) %>%
  ggplot()+
  geom_linerange(aes(x = pitID, ymin = socMin, ymax = socMax),
                 linewidth = 0.3)+
  geom_point(aes(y = socMean, 
                 x = reorder(pitID, socMean),
                 fill = ecosystem),
             size = 3,
             shape = 21) +
  scale_fill_manual(values = ecosystemColors, name = "Ecosystem")+
  xlab(NULL)+
  ylab(expression(bold("Soil organic C (%)"))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8),
        legend.key.height = unit(0.6, "lines"),
        legend.spacing.y = unit(0.15, "lines"),
        panel.spacing = unit(1.2, "lines"))+
  scale_x_discrete(expand = expansion(add = 1))+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  facet_wrap(~pit)

ggsave(filepaths$plots$soc, metricRanges$plots$soc, 
       width = 6.5, height = 1.75,
       dpi = 600)

## EOC ---------------------------------------------------------------------

metricRanges$plots$eoc <- metricRanges$eoc$raw %>%
  mutate(pit = case_when(pit == "R" ~ "Ridge",
                         pit == "M" ~ "Midslope",
                         pit == "T" ~ "Toeslope")) %>%
  mutate(pit = factor(pit, levels = c("Ridge", "Midslope", "Toeslope"))) %>%
  ggplot()+
  geom_linerange(aes(x = pitID, ymin = eocMin, ymax = eocMax),
                 linewidth = 0.3)+
  geom_point(aes(y = eocMean, 
                 x = reorder(pitID, eocMean),
                 fill = ecosystem),
             size = 3,
             shape = 21) +
  scale_fill_manual(values = ecosystemColors, name = "Ecosystem")+
  xlab(NULL)+
  ylab(expression(bold("Extractable organic carbon (μg g"^-1*")"))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 6.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8),
        legend.key.height = unit(0.6, "lines"),
        legend.spacing.y = unit(0.15, "lines"),
        panel.spacing = unit(1.2, "lines"))+
  scale_x_discrete(expand = expansion(add = 1))+
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black")))+
  facet_wrap(~pit)

ggsave(filepaths$plots$eoc, metricRanges$plots$eoc, 
       width = 6.5, height = 1.75,
       dpi = 600)

# Plot assembly -----------------------------------------------------------

plots <- list()

## Aggregate size ---------------------------------------------------------

plots$aggSize$merged <- (deepACI$plots$aggSize$annotatedModel |
    downslopeACI$plots$aggSize$annotatedModel |
    (deepACI$plots$aggSize$jackknife / downslopeACI$plots$aggSize$jackknife)) +
  plot_layout(widths = c(1.20, 1.20, 0.85)) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")

plots$aggSize$final <- ggdraw() +
  draw_plot(plots$aggSize$merged,
    x = 0,
    y = 0.15,
    width = 1,
    height = 0.85) +
  draw_plot(legend$legend,
    x = 0.05,
    y = 0.02,
    width = 0.9,
    height = .15)

## Areal clay content -----------------------------------------------------

plots$arealClayContent$merged <- (deepACI$plots$arealClayContent$annotatedModel |
    downslopeACI$plots$arealClayContent$annotatedModel |
    (deepACI$plots$arealClayContent$jackknife / downslopeACI$plots$arealClayContent$jackknife)) +
  plot_layout(widths = c(1.20, 1.20, 0.85)) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")

plots$arealClayContent$final <- ggdraw() +
  draw_plot(plots$arealClayContent$merged,
    x = 0,
    y = 0.15,
    width = 1,
    height = 0.85) +
  draw_plot(legend$legend,     
    x = 0.05,
    y = 0.02,
    width = 0.9,
    height = .15) 

## Mean clay concentration ------------------------------------------------

plots$meanClayConcentration$merged <- (deepACI$plots$meanClayConcentration$annotatedModel | downslopeACI$plots$meanClayConcentration$annotatedModel) +
  plot_layout(widths = c(1,1))+
  plot_annotation(tag_levels = 'A',
                  tag_prefix = "(",
                  tag_suffix = ")")

plots$meanClayConcentration$final <- ggdraw()+
  draw_plot(plots$meanClayConcentration$merged, 
            x = 0,
            y = 0.15,
            width = 1,
            height = 0.85) +
  draw_plot(legend$legend,
            x = 0.05,
            y = 0.02,
            width = 0.9,
            height = .15) 

# Save plots --------------------------------------------------------------

ggsave(filepaths$plots$aggSize, plots$aggSize$final, 
       width = 7.5, height = 4.2, 
       dpi = 300, bg = "white")

ggsave(filepaths$plots$arealClayContent, plots$arealClayContent$final, 
       width = 7.5, height = 4.2, 
       dpi = 300, bg = "white")

ggsave(filepaths$plots$meanClayConcentration, plots$meanClayConcentration$final, 
       width = 5.5, height = 4.2, 
       dpi = 300, bg = "white")
