# *************************************************************************
# Figure generation for depth distributions
#
# This script generates figures showing depth distributions of soil organic
# carbon (SOC), extractable organic carbon (EOC), root presence, and the
# allogenic carbon index (ACI) across soil profiles and ecosystems.
#
# Plots display individual profiles by hillslope position and include
# genetic horizon boundaries and horizon classifications for context.
#
# Figures are formatted for inclusion in the supplementary materials.
# *************************************************************************

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)
library(ggnewscale)
library(grid)

# Define filepaths --------------------------------------------------------

filepaths <- list()
df <- list()
temp <- list()
plots <- list()
labels <- list()
output <- list()

# Load data ---------------------------------------------------------------

df$root <- read_csv("data/raw/root.csv")

df$horizonization <- read_csv("data/raw/horizonization.csv") 

df$depthDistribution <- read_csv("data/raw/depthDistribution.csv")

# Define plot elements and aesthetics -------------------------------------

labelTextSize = 9.25 #Text size for site labels (e.g., Temperate Pine Forest)

plotWidth = 8.75
plotHeight = 10.25

labels$LUQEV <- wrap_elements(full = grid::textGrob(
  "Younger Tropical Rainforest (YTR)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$LUQIC <- wrap_elements(full = grid::textGrob(
  "Older Tropical Rainforest (OTR)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$R8H <- wrap_elements(full = grid::textGrob(
  "Temperate Hardwood Forest (THF)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$R8P <- wrap_elements(full = grid::textGrob(
  "Temperate Pine Forest (TPF)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$RCSWL <- wrap_elements(full = grid::textGrob(
  "Semi-arid Shrubland (SAS)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$RCNL <- wrap_elements(full = grid::textGrob(
  "Aspen Stand (AS)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$CAMC <- wrap_elements(full = grid::textGrob(
  "Fir/Pine Forest (FPF)",
  gp = gpar(fontsize = labelTextSize, fontface = "bold")
))
labels$CASD <- wrap_elements(full = grid::textGrob("Desert (DS)", gp = gpar(
  fontsize = labelTextSize, fontface = "bold"
)))
labels$SNSJER <- wrap_elements(full = grid::textGrob("Oak Savanna (OS)", gp = gpar(
  fontsize = labelTextSize, fontface = "bold"
)))

layout1 <- "
          AAABB
          CDEFG
          HHHII
          JKLMN
          "

layout2 <- "
          AAABB
          CDEFG
          HHHIJ
          KLMNO
          "

depthDistLabs <- c(
  "LUQEVR" = "(A)",
  "LUQEVM" = "(B)",
  "LUQEVT" = "(C)",
  "R8HR" = "(D)",
  "R8HM" = "(E)",
  "LUQICR" = "(F)",
  "LUQICM" = "(G)",
  "LUQICT" = "(H)",
  "R8PR" = "(I)",
  "R8PM" = "(J)",
  "SNSJERR" = "(K)",
  "SNSJERM" = "(L)",
  "SNSJERT" = "(M)",
  "RCSWLT" = "(N)",
  "RCNLT" = "(O)",
  "CAMCR" = "(P)",
  "CAMCM" = "(Q)",
  "CAMCT" = "(R)",
  "RCNLM" = "(S)",
  "CASDT" = "(T)"
)

geneticHorizonColors <- c(
  "O"	= "gray48",
  "A"	= "gray48",
  "B"	= "gray70",
  "R"	= "gray90",
  "C"	= "gray90",
  "E"	= "gray48"
)

theme_set(
  theme_bw() +
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,   
        face = "bold", 
        size = 10      
      ),
      plot.subtitle = element_text(
        hjust = 0.5,   
        size = 10
      ),
      axis.title = element_text(
        face = "bold",
        size = 14     
      ),
      axis.text = element_text(
        face = "italic",
        size = 9   
      ),
      legend.title = element_text(
        face = "bold",
        size = 16
      ),
      legend.text = element_text(
        size = 15
      ),
      plot.tag = element_text(
        size = 16,
        face = "bold"),
      panel.grid = element_blank()  
    )
)

# Generate pit labels -----------------------------------------------------

pitIDs <- df$depthDistribution %>%
  select(pitID) %>%
  distinct() %>%
  pull(pitID)

temp$defLabs <- tibble(
  "pitID" = names(depthDistLabs),
  "lab" = depthDistLabs
)

temp$defLabs %>%
  distinct() %>%
  pull(pitID)

# Blank profiles ----------------------------------------------------------

for (i in pitIDs){
  
  temp$h <- df$horizonization %>%
    filter(pitID == i)
  
  temp$t <- df$depthDistribution %>%
    filter(pitID == i) 
  
  plots$depthDist[[i]] <- temp$t %>%
    ggplot()+
    geom_blank(data = temp$t, aes(x = .001))+
    geom_rect(data = temp$h, aes(ymin = startDepth, ymax = endDepth, xmin = -Inf, xmax = Inf,
                                 fill = geneticHorizon), show.legend = FALSE)+
    geom_hline(data = temp$h, aes(yintercept = endDepth), linetype = "dashed")+
    geom_label(data = temp$h, aes(x = Inf, y = (((endDepth-startDepth)/2)+startDepth), label = horizon), 
               hjust = 1, size = 2) +
    scale_fill_manual(values = geneticHorizonColors)+
    ylab(NULL) +
    xlab(NULL) +
    scale_y_reverse(limits = c(200,0),
                    sec.axis = sec_axis(~ ., breaks = temp$h$plottingDepth,
                                        labels = temp$h$horizon, name = NULL))+
    guides(
      fill = "none",
      color = "none",
      linetype = "none"
    )
  
}

# ACI ---------------------------------------------------------------------

for(i in pitIDs){
  temp$df <- df$depthDistribution %>%
    filter(pitID == i) %>%
    filter(endDepth <= 200) %>%
    filter(!is.na(eoc))
  
  temp$lab <- temp$df %>%
    select(pitID, pit) %>%
    distinct() %>%
    left_join(temp$defLabs, by = "pitID") %>%
    mutate(lab = case_when(pit == "R" ~ paste0(lab," Ridge"),
                           pit == "M" ~ paste0(lab," Midslope"),
                           pit == "T" ~ paste0(lab," Toeslope"))) %>%
    pull(lab)
  
  xLabPos <- temp$df %>%
    mutate(maxVal = max(eoc, na.rm = TRUE)) %>%
    select(maxVal) %>%
    distinct() %>%
    pull(maxVal)
  
  plots$aci[[i]] <- plots$depthDist[[i]]+
    new_scale_fill()+
    geom_point(data = temp$df, aes(y = plottingDepth, x = aci), size = 2)+
    guides(fill = "none")+
    ggtitle(temp$lab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

temp$set1 <- labels$LUQEV + labels$R8H +
  plots$aci$LUQEVR + plots$aci$LUQEVM + plots$aci$LUQEVT + plots$aci$R8HR + plots$aci$R8HM +
  labels$LUQIC + labels$R8P +
  plots$aci$LUQICR + plots$aci$LUQICM + plots$aci$LUQICT + plots$aci$R8PR + plots$aci$R8PM +
  plot_layout(design = layout1,
              heights = c(0.2, 1),
              byrow = FALSE)

temp$set2 <- labels$SNSJER + labels$RCSWL +
  plots$aci$SNSJERR + plots$aci$SNSJERM + plots$aci$SNSJERT + plots$aci$RCSWLT  + plots$aci$RCNLT +
  labels$CAMC +  labels$RCNL + labels$CASD +
  plots$aci$CAMCR + plots$aci$CAMCM + plots$aci$CAMCT + plots$aci$RCNLM + plots$aci$CASDT +
  plot_layout(design = layout2,
              heights = c(0.2, 1),
              byrow = FALSE)

temp$merged <- temp$set1 / temp$set2

output$aci <- ggdraw() +
  draw_plot(
    temp$merged,
    x = 0.04,
    y = 0.04,
    width = 0.96,
    height = 0.96
  ) +
  draw_label(
    "Depth (cm)",
    x = 0.02,
    angle = 90,
    vjust = 0.5,
    size = 14,
    fontface = "bold"
  ) +
  draw_label(
    expression(bold("Allogenic carbon index (μg g"^-1*")")),
    y = 0.02,
    vjust = 0.5,
    angle = 0,
    hjust = 0.5,
    size = 14,
    fontface = "bold"
  ) +
  theme(plot.background = element_rect(fill = "white", color = NA))

save_plot(
  filename = "output/plots/S8_aciDepthDistributions.png",
  plot =  output$aci,
  base_width = plotWidth,
  base_height = plotHeight)

# EOC ---------------------------------------------------------------------

for(i in pitIDs){
  temp$df <- df$depthDistribution %>%
    filter(pitID == i) %>%
    filter(endDepth <= 200) %>%
    filter(!is.na(eoc))
  
  temp$lab <- temp$df %>%
    select(pitID, pit) %>%
    distinct() %>%
    left_join(temp$defLabs, by = "pitID") %>%
    mutate(lab = case_when(pit == "R" ~ paste0(lab," Ridge"),
                           pit == "M" ~ paste0(lab," Midslope"),
                           pit == "T" ~ paste0(lab," Toeslope"))) %>%
    pull(lab)
  
  plots$eoc[[i]] <- plots$depthDist[[i]]+
    new_scale_fill()+
    geom_point(data = temp$df, aes(y = plottingDepth, x = eoc), size = 2)+
    guides(fill = "none")+
    ggtitle(temp$lab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}


temp$set1 <- labels$LUQEV + labels$R8H +
  plots$eoc$LUQEVR + plots$eoc$LUQEVM + plots$eoc$LUQEVT + plots$eoc$R8HR + plots$eoc$R8HM +
  labels$LUQIC + labels$R8P +
  plots$eoc$LUQICR + plots$eoc$LUQICM + plots$eoc$LUQICT + plots$eoc$R8PR + plots$eoc$R8PM +
  plot_layout(design = layout1,
              heights = c(0.2, 1),
              byrow = FALSE)

temp$set2 <- labels$SNSJER + labels$RCSWL +
  plots$eoc$SNSJERR + plots$eoc$SNSJERM + plots$eoc$SNSJERT + plots$eoc$RCSWLT  + plots$eoc$RCNLT +
  labels$CAMC +  labels$RCNL + labels$CASD +
  plots$eoc$CAMCR + plots$eoc$CAMCM + plots$eoc$CAMCT + plots$eoc$RCNLM + plots$eoc$CASDT +
  plot_layout(design = layout2,
              heights = c(0.2, 1),
              byrow = FALSE)

temp$merged <- temp$set1 / temp$set2

output$eoc <- ggdraw() +
  draw_plot(
    temp$merged,
    x = 0.04,
    y = 0.04,
    width = 0.96,
    height = 0.96
  ) +
  draw_label(
    "Depth (cm)",
    x = 0.02,
    angle = 90,
    vjust = 0.5,
    size = 14,
    fontface = "bold"
  ) +
  draw_label(
    expression(bold("Extractable organic carbon (μg g"^-1*")")),
    y = 0.02,
    vjust = 0.5,
    angle = 0,
    hjust = 0.5,
    size = 14,
    fontface = "bold"
  ) +
  theme(plot.background = element_rect(fill = "white", color = NA))

save_plot(
  filename = "output/plots/S7_eocDepthDistributions.png",
  plot =  output$eoc,
  base_width = plotWidth,
  base_height = plotHeight)


# SOC ---------------------------------------------------------------------

for(i in pitIDs){
  temp$df <- df$depthDistribution %>%
    filter(pitID == i) %>%
    filter(endDepth <= 200) %>%
    filter(!is.na(soc))
  
  temp$lab <- temp$df %>%
    select(pitID, pit) %>%
    distinct() %>%
    left_join(temp$defLabs, by = "pitID") %>%
    mutate(lab = case_when(pit == "R" ~ paste0(lab," Ridge"),
                           pit == "M" ~ paste0(lab," Midslope"),
                           pit == "T" ~ paste0(lab," Toeslope"))) %>%
    pull(lab)
  
  plots$soc[[i]] <- plots$depthDist[[i]]+
    new_scale_fill()+
    geom_point(data = temp$df, aes(y = plottingDepth, x = soc), size = 2)+
    guides(fill = "none")+
    ggtitle(temp$lab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

temp$set1 <- labels$LUQEV + labels$R8H +
  plots$soc$LUQEVR + plots$soc$LUQEVM + plots$soc$LUQEVT + plots$soc$R8HR + plots$soc$R8HM +
  labels$LUQIC + labels$R8P +
  plots$soc$LUQICR + plots$soc$LUQICM + plots$soc$LUQICT + plots$soc$R8PR + plots$soc$R8PM +
  plot_layout(design = layout1,
              heights = c(0.2, 1),
              byrow = FALSE)

temp$set2 <- labels$SNSJER + labels$RCSWL +
  plots$soc$SNSJERR + plots$soc$SNSJERM + plots$soc$SNSJERT + plots$soc$RCSWLT  + plots$soc$RCNLT +
  labels$CAMC +  labels$RCNL + labels$CASD +
  plots$soc$CAMCR + plots$soc$CAMCM + plots$soc$CAMCT + plots$soc$RCNLM + plots$soc$CASDT +
  plot_layout(design = layout2,
              heights = c(0.2, 1),
              byrow = FALSE)

temp$merged <- temp$set1 / temp$set2

output$soc <- ggdraw() +
  draw_plot(
    temp$merged,
    x = 0.04,
    y = 0.04,
    width = 0.96,
    height = 0.96
  ) +
  draw_label(
    "Depth (cm)",
    x = 0.02,
    angle = 90,
    vjust = 0.5,
    size = 14,
    fontface = "bold"
  ) +
  draw_label(
    expression(bold("Soil organic carbon (%)")),
    y = 0.02,
    vjust = 0.5,
    angle = 0,
    hjust = 0.5,
    size = 14,
    fontface = "bold"
  ) +
  theme(plot.background = element_rect(fill = "white", color = NA))

save_plot(
  filename = "output/plots/S5_socDepthDistributions.png",
  plot =  output$soc,
  base_width = plotWidth,
  base_height = plotHeight)

