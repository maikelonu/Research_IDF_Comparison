# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# INSTITUTO TECNOLOGICO DE COSTA RICA
# Construction Engineering School
# MSc.Eng. Maikel Mendez Morales
# https://www.tec.ac.cr
# Email: maikel.mendez@gmail.com; mamendez@itcr.ac.cr
# https://orcid.org/0000-0003-1919-141X
# https://www.scopus.com/authid/detail.uri?authorId=51665581300
# https://scholar.google.com/citations?user=JnmSVFYAAAAJ&hl=en
# https://www.youtube.com/c/maikelmendez
# https://twitter.com/MaikelMendezM
# https://github.com/maikelonu
# Skype: maikel.mendez
# ////////////////////////////////////////////////////////////////////////////////////////////////////////////

#-------------------------------------------------------------------------------------------------------------------
# INFO: This script is intended for the generation of Intensity-Duration-Frequency (IDFs) curves for all climatic 
# regions in Costa Rica. Certain Blocks are coded to take automatic rain-gauges data as inputs, whereas other Blocks
# will take only mechanical rain-gauges data as inputs
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# MANUSCRIPT TITLE:
# To be defined
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# INPUT FILES:
# TXT IMN raw data format
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# MANUSCRIPT FIGURES:
# None
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# OUTPUT FILES:
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# Workspace is cleared
gc()
rm(list = ls())

# Working directory is defined
setwd("/home/shoe/Dropbox/Academics/IDF_CC_tool_CANADA/R_scripts")

# Scientific notation is disabled
options(scipen=999)

# Start time is recorded
start.time <- Sys.time()

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: Libraries installation from Github
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Download IDFtool from Github
# https://github.com/dazamora/IDFtool/

# Library devtools MUST be previously installed
# devtools::install_github("dazamora/IDFtool")

# Library investr MUST be downloaded from CRAN and installed as source
# directly in RStudio Console as:
# install.packages("~/Downloads/investr_1.4.0.tar.gz", repos=NULL, type="source")
# devtools::install_github("ricardo-bion/ggradar")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: CRAN libraries are loaded
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
require(ggradar)
require(investr)
require(IDFtool)
require(DescTools)
require(dplyr)
require(ggplot2)
require(lubridate)
require(matrixStats)
require(pastecs)
require(tidyr)

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: Automatic rain-gauge IDFs development
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Sub-hourly observed data are loaded
df.base <- read.table("RADAR_Comando02.txt",header=T,sep="\t",quote="")

df.base.KS <- subset(df.base, Metric == "KS-norm")
df.base.KS <- df.base.KS[, -c(12)]
apply(df.base.KS[,-c(1)], 1, max, na.rm=TRUE)

df.base.AD <- subset(df.base, Metric == "AD-norm")
df.base.AD <- df.base.AD[, -c(12)]
apply(df.base.AD[,-c(1)], 1, max, na.rm=TRUE)

df.base.CVM <- subset(df.base, Metric == "CVM-norm")
df.base.CVM <- df.base.CVM[, -c(12)]
apply(df.base.CVM[,-c(1)], 1, max, na.rm=TRUE)

df.base.AIC <- subset(df.base, Metric == "AIC-norm")
df.base.AIC <- df.base.AIC[, -c(12)]
apply(df.base.AIC[,-c(1)], 1, max, na.rm=TRUE)

df.base.BIC <- subset(df.base, Metric == "BIC-norm")
df.base.BIC <- df.base.BIC[, -c(12)]
apply(df.base.BIC[,-c(1)], 1, max, na.rm=TRUE)

df.base.Ranking <- subset(df.base, Metric == "Total-Ranking")
df.base.Ranking <- df.base.Ranking[, -c(12)]
apply(df.base.Ranking[,-c(1)], 1, max, na.rm=TRUE)

# KS
ggradar(df.base.KS,
        values.radar = c(1.0, 2.0, 3.0),
        grid.min = 1.0,
        grid.mid = 2.0,
        grid.max = 3.0,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        gridline.min.linetype = 2,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        legend.title = "Kolmogorov-Smirnov",
        legend.position = "bottom",
        grid.line.width = 1.25,
        legend.text.size = 18,
        axis.label.size = 8)

# AD
ggradar(df.base.AD,
        values.radar = c(1.0, 2.0, 3.0),
        grid.min = 1.0,
        grid.mid = 2.0,
        grid.max = 3.0,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        gridline.min.linetype = 2,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        legend.title = "Anderson-Darling",
        legend.position = "bottom",
        grid.line.width = 1.25,
        legend.text.size = 18,
        axis.label.size = 8)

# CVM
ggradar(df.base.CVM,
        values.radar = c(1.0, 2.0, 3.0),
        grid.min = 1.0,
        grid.mid = 2.0,
        grid.max = 3.0,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        gridline.min.linetype = 2,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        legend.title = "Cramer-von Mises",
        legend.position = "bottom",
        grid.line.width = 1.25,
        legend.text.size = 18,
        axis.label.size = 8)

# AIC
ggradar(df.base.AIC,
        values.radar = c(1.0, 2.0, 3.0),
        grid.min = 1.0,
        grid.mid = 2.0,
        grid.max = 3.0,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        gridline.min.linetype = 2,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        legend.title = "Akaike's Inf.Crit.",
        legend.position = "bottom",
        grid.line.width = 1.25,
        legend.text.size = 18,
        axis.label.size = 8)

# BIC
ggradar(df.base.BIC,
        values.radar = c(1.0, 2.0, 3.0),
        grid.min = 1.0,
        grid.mid = 2.0,
        grid.max = 3.0,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        gridline.min.linetype = 2,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        legend.title = "Bayesian Inf.Crit.",
        legend.position = "bottom",
        grid.line.width = 1.25,
        legend.text.size = 18,
        axis.label.size = 8)

# BIC
ggradar(df.base.Ranking,
        values.radar = c(5.0, 10.0, 15.0),
        grid.min = 5.0,
        grid.mid = 10.0,
        grid.max = 15.0,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        gridline.min.linetype = 2,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        legend.title = "Ranking",
        legend.position = "bottom",
        grid.line.width = 1.25,
        legend.text.size = 18,
        axis.label.size = 8)

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# END OF CODE
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
