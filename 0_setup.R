# Libraries
library(DHARMa)
library(ggpubr)
library(glmmTMB)
library(magrittr)
library(MASS)
library(papaja)
library(patchwork)
library(stringr)
library(tidyverse)
library(xtable)
library(scales)

# Paths
processed.data.path <- "processed_data/"

#----

# Custom utilities

## Standard error of the mean (SEM)
sem <- function (x) {
  sd(x) / sqrt(length(x))
}
## Basic plotting theme
basic.theme <- theme(
  panel.background = element_rect(
    fill = "transparent",colour = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(
    fill = "transparent",colour = NA),
  legend.background = element_rect(
    fill="transparent"),
  legend.text = element_text(size=30),
  legend.title = element_text(size=30),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.key.height = unit(2, "lines"),
  axis.text.x = element_text(size=30),
  axis.title.x = element_text(size=30),
  axis.text.y = element_text(size=30),
  axis.title.y = element_text(size=30),
  strip.text = element_text(size=30),
  panel.spacing = unit(2, "lines"),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Aids for pval display
p.levels <- c(0, 0.001, 0.01)
pval.display <- function(p.est) {
  level <- findInterval(p.est, p.levels)
  p.string <- ifelse(level == 3, paste0("= ", round(p.est, 2)),
                     ifelse(level == 2, "< 0.01", "< 0.001"))
  return(p.string)
}

