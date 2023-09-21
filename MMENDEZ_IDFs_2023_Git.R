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
# library(devtools)
# install_github("ClimDesign/fixIDF",ref="main")

# If NOT available in CRAN, library {investr} MUST then be downloaded from CRAN 
# and installed as source directly in RStudio Console as:
# install.packages("~/Downloads/investr_1.4.0.tar.gz", repos=NULL, type="source")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: CRAN libraries are loaded
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
require(devtools)
require(investr)
require(IDFtool)
require(DescTools)
require(dplyr)
require(ggplot2)
require(lubridate)
require(matrixStats)
require(pastecs)
require(tidyr)
require(fixIDF)

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: Automatic rain-gauge IDFs development
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Sub-hourly observed data are loaded
df.base.sub <- read.table("EST_69_633_SUB.txt",header=T,sep="\t",quote="")
df.base.hour <- read.table("EST_69_633_HOR.txt",header=T,sep="\t",quote="")

# Only complete days and complete years are included within hourly-data
# This MUST be done manually by inspection
# View(df.base.hour)
df.base.hour <- df.base.hour[904:225335, ]

# A head/tail length=5 is requested for verification
head(df.base.hour)
tail(df.base.hour)

# Only complete days and complete years are included within hourly-data
# This MUST be done manually by inspection
# View(df.base.hour)
df.base.sub <- df.base.sub[38:9380, ]

# A head/tail length=5 is requested for verification
head(df.base.sub)
tail(df.base.sub)

# Irrelevant variables are excluded
df.base.sub <- df.base.sub[, c(4,5,6,7,11)]
df.base.hour <- df.base.hour[, c(3,4,5)]

# A dummy variable is created
temp.date <- df.base.hour$FECHA

# "FECHA" class character is converted to class-date and added as a new column named "DATE2"
df.base.hour$DATE <- as.Date(temp.date, format = "%m/%d/%Y")

# A date-class query is requested (TRUE or FALSE)
is.Date(df.base.hour$DATE)

# lubridate Library functions are applied to df.base.hour to create new columns containing:
# YEAR, YEAR_CH, MONTH, MONTH_CH, WEEK, DAY, DAY_MONTH   
df.base.hour$YEAR <- lubridate::year(df.base.hour$DATE) # Years component of a date-time
df.base.hour$YEAR_CH <- as.character(year(df.base.hour$DATE)) # Years component of a date-time as character
df.base.hour$MONTH <- lubridate::month(df.base.hour$DATE, label = FALSE) # Months component of a date-time
df.base.hour$MONTH_CH <- lubridate::month(df.base.hour$DATE, label = TRUE) # Months component of a date-time as character
df.base.hour$WEEK <- lubridate::week(df.base.hour$DATE) # Weeks component of a date-time
df.base.hour$DAY <- lubridate::yday(df.base.hour$DATE) # Days component of a date-time
df.base.hour$DAY_MONTH <- lubridate::days_in_month(df.base.hour$DATE) # Number of days in the month of a date-time

# mutate_all {dplyr} function is called to replace NAs with 0.0's
# This should be consulted, as data quality analysis should be performed in ADVANCE!!
df.base.hour[3][df.base.hour[3] < 0] <- NA # NEGATIVE values !!!!!

# A summary is requested to check the total number of NAs
summary(df.base.hour[3])
summary(df.base.sub[-c(5)])

# Negative (e.g -9) values are replaced with 0.0's
# This should be consulted, as data quality analysis should be performed in ADVANCE!!
# df.base.hour[3][df.base.hour[3] < 0] <- 0

# mutate_all {dplyr} function is called to replace NAs with 0.0's
# This should be consulted, as data quality analysis should be performed in ADVANCE!!
#df.base.hour[3] <- df.base.hour[3] %>% mutate_all(funs(replace_na(.,0)))
df.base.sub[1][df.base.sub[1] < 0] <- NA
df.base.sub[2][df.base.sub[2] < 0] <- NA
df.base.sub[3][df.base.sub[3] < 0] <- NA
df.base.sub[4][df.base.sub[4] < 0] <- NA

# NAs rows are removed from data.frames
# We cannot continue calculations with NAs present !!!
df.base.hour <- na.omit(df.base.hour)
df.base.sub <- na.omit(df.base.sub)

# Hourly data are aggregated into 2-hour timestep
df.base.hour.two <- as.data.frame(rowsum(df.base.hour[,c(3,5)], as.integer(gl(nrow(df.base.hour), 2, nrow(df.base.hour)))))
df.base.hour.two$YEAR <- as.integer((df.base.hour.two$YEAR)/2)

# Hourly data are aggregated into 3-hour timestep
df.base.hour.three <- as.data.frame(rowsum(df.base.hour[,c(3,5)], as.integer(gl(nrow(df.base.hour), 3, nrow(df.base.hour)))))
df.base.hour.three$YEAR <- as.integer((df.base.hour.three$YEAR)/3)

# Hourly data are aggregated into 6-hour timestep
df.base.hour.six <- as.data.frame(rowsum(df.base.hour[,c(3,5)], as.integer(gl(nrow(df.base.hour), 6, nrow(df.base.hour)))))
df.base.hour.six$YEAR <- as.integer((df.base.hour.six$YEAR)/6)

# Hourly data are aggregated into 12-hour timestep
df.base.hour.twelve <- as.data.frame(rowsum(df.base.hour[,c(3,5)], as.integer(gl(nrow(df.base.hour), 12, nrow(df.base.hour)))))
df.base.hour.twelve$YEAR <- as.integer((df.base.hour.twelve$YEAR)/12)

# Hourly data are aggregated into 24-hour timestep
df.base.hour.daily <- as.data.frame(rowsum(df.base.hour[,c(3,5)], as.integer(gl(nrow(df.base.hour), 24, nrow(df.base.hour)))))
df.base.hour.daily$YEAR <- as.integer((df.base.hour.daily$YEAR)/24)

# Precipitation depths [mm] are transformed to intensities [mm/hour]
df.base.sub[1] <- df.base.sub[1]/(5/60)
df.base.sub[2] <- df.base.sub[2]/(10/60)
df.base.sub[3] <- df.base.sub[3]/(15/60)
df.base.sub[4] <- df.base.sub[4]/(30/60)

# Precipitation depths [mm] are transformed to intensities [mm/hour]
df.base.hour[3] <- df.base.hour[3]/(60/60)
df.base.hour.two[1] <- df.base.hour.two[1]/(120/60)
df.base.hour.three[1] <- df.base.hour.three[1]/(180/60)
df.base.hour.six[1] <- df.base.hour.six[1]/(360/60)
df.base.hour.twelve[1] <- df.base.hour.twelve[1]/(720/60)
df.base.hour.daily[1] <- df.base.hour.daily[1]/(1440/60)

# Sub-hourly observed data are aggregated by year
df.aggre.01 <- df.base.sub %>%
  group_by(ANNO) %>%
  summarise(LLUV_MX5= max(LLUV_MX5),
            LLUV_MX10= max(LLUV_MX10),
            LLUV_MX15= max(LLUV_MX15),
            LLUV_MX30= max(LLUV_MX30))

# Hourly observed data are aggregated by year
df.aggre.02 <- df.base.hour %>%
  group_by(YEAR) %>%
  summarise(LLUVIA = max(LLUVIA))

# Hourly observed data are aggregated into 2-hour timestep by year
df.aggre.two <- df.base.hour.two %>%
  group_by(YEAR) %>%
  summarise(LLUVIA = max(LLUVIA))

# Hourly observed data are aggregated into 3-hour timestep by year
df.aggre.three <- df.base.hour.three %>%
  group_by(YEAR) %>%
  summarise(LLUVIA = max(LLUVIA))

# Hourly observed data are aggregated into 6-hour timestep by year
df.aggre.six <- df.base.hour.six %>%
  group_by(YEAR) %>%
  summarise(LLUVIA = max(LLUVIA))

# Hourly observed data are aggregated into 12-hour timestep by year
df.aggre.twelve <- df.base.hour.twelve %>%
  group_by(YEAR) %>%
  summarise(LLUVIA = max(LLUVIA))

# Hourly observed data are aggregated into 24-hour timestep by year
df.aggre.daily <- df.base.hour.daily %>%
  group_by(YEAR) %>%
  summarise(LLUVIA = max(LLUVIA))

# Residual rows are excluded
#df.aggre.two <- df.aggre.two[-c(1) , ]
df.aggre.three <- df.aggre.three[-c(1) , ]
df.aggre.six <- df.aggre.six[-c(1) , ]
df.aggre.twelve <- df.aggre.twelve[-c(1) , ]
df.aggre.daily <- df.aggre.daily[-c(1) , ]

# Sub-hourly and hourly data are c-binded
df.aggre.01$LLUV_MX60 <- df.aggre.02$LLUVIA
df.aggre.01$LLUV_MX120 <- df.aggre.two$LLUVIA

df.aggre.01$LLUV_MX180 <- df.aggre.three$LLUVIA
df.aggre.01$LLUV_MX360 <- df.aggre.six$LLUVIA
df.aggre.01$LLUV_MX720 <- df.aggre.twelve$LLUVIA
df.aggre.01$LLUV_MX1440 <- df.aggre.daily$LLUVIA

# A data.frame is transformed to matrix object
matrix.base <- as.matrix(df.aggre.01)

# Names are assigned to matrix columns
colnames(matrix.base) <- c("year", "5",	"10",	"15",	"30", "60",
                           "120", "180", "360", "720", "1440")

# A duration vector is created [min]
v.duration <- c(5, 10, 15, 30, 60, 120, 180, 360, 720, 1440)

# A periods vector is created [years]
v.periods <- c(2, 3, 5, 10, 15, 20, 25, 30, 50, 75, 100, 200)

# stat.desc {pastecs} function is requested
df.stat.desc <- as.data.frame(round(stat.desc(matrix.base[ , ], norm = TRUE), 6))

# IDFCurve {IDFtool} function is called and an new object is created
IDF.Auto <- IDFCurve(Data = matrix.base, Station="IMN_69_633_Comando_Chiles",
                     Duration = v.duration, Periods = v.periods, 
                     Type = "gev", M.fit = "Lmoments", Plot = 13, Strategy = 1,
                     logaxe = "", CI = TRUE, CIpdf = TRUE, iter = 500,
                     goodtest = TRUE, Resolution = 600, 
                     SAVE = FALSE, name = TRUE)

# Object structure is requested
str(IDF.Auto)

# A summary of various objects is requested
IDF.Auto$Intensities
IDF.Auto$Models$HIDFUN$Predict
IDF.Auto$Models$HIDFUN$Coefficients
IDF.Auto$Models$HIDFUN$test.fit.reg
IDF.Auto$Models$HIDFUN$Prediction.Int
IDF.Auto$Models$HIDFUN$Confidence.Int
IDF.Auto$Models$HIDFUN$Modols
IDF.Auto$Test.fit
IDF.Auto$Distribution$`5 min`
IDF.Auto$Distribution$`1440 min`$Parameters$para

# -------------------------------------------------------------------------------------------------
#                        xi         alpha     kappa
# Series                 Location   Scale     Shape
# -------------------------------------------------------------------------------------------------

# fitDISTRI {IDFtool} is used for distribution fitting functions by L-moments
fit.AUTO <- fitDISTRI(Intensity = matrix.base[, 2], Type ="gev", Plot = 1, M.fit = "lmoments",
                     Periods = v.periods, Dura = "5-min", Station ="IMN_69_633_Comando_Chiles",
                     CI = F, iter = 500, goodtest = TRUE,Resolution = 300, SAVE = FALSE)

# fitDISTRI {IDFtool} parameters are requested
fit.AUTO$Parameters$para

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: Mechanical rain-gauge IDFs development
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Observed data are loaded
df.base <- read.table("pacayas02.txt",header=T,sep="\t",quote="") # Pacayas 73-22

# A data.frame is transformed to matrix object
matrix.base <- as.matrix(df.base)

# Names are assigned to matrix columns
#colnames(matrix.base) <- c("year", "5",	"15",	"30",	"60",	"240", "360")
colnames(matrix.base) <- c("year", "5",	"10",	"15",	"30", "60",
                           "120", "180", "360", "720", "1440")

# A duration vector is created [min]
#v.duration <- c(5,15,30,60,240,360)
v.duration <- c(5, 10, 15, 30, 60, 120, 180, 360, 720, 1440)

# A periods vector is created [years]
#v.periods <- c(2,3,5,10,15,20,25,30,50,75,100,200)
v.periods <- c(2, 3, 5, 10, 15, 20, 25, 30, 50, 75, 100, 200)

# IDFCurve {IDFtool} function is called and an new object is created
Test.idftool.pacayas <- IDFCurve(Data = matrix.base, Station="IMN_Pacayas", 
                                 Duration = v.duration,Periods = v.periods,
                                 Type = "gev", M.fit = "lmoments", Plot = 13, Strategy = 1,
                                 logaxe = "", CI = TRUE, CIpdf = TRUE, iter = 500, 
                                 goodtest = TRUE, Resolution = 600, 
                                 SAVE = FALSE, name = TRUE)  

# A summary of various objects is requested
Test.idftool.pacayas$Intensities
Test.idftool.pacayas$Test.fit
Test.idftool.pacayas$Models$HIDFUN$Coefficients

# A summary of various objects is requested
Test.idftool.pacayas

# -------------------------------------------------------------------------------------------------
#                        xi         alpha     kappa
# Series                 Location   Scale     Shape
# -------------------------------------------------------------------------------------------------

# fitDISTRI {IDFtool} is used for distribution fitting functions by L-moment
fit.AUTO <- fitDISTRI(Intensity = matrix.base[, 2], Type ="gev", Plot = 1, M.fit = "lmoments",
                      Periods = v.periods, Dura = "5-min", Station ="IMN_Pacayas",
                      CI = TRUE, iter = 500, goodtest = TRUE,Resolution = 300, SAVE = FALSE)

# fitDISTRI {IDFtool} parameters are requested
fit.AUTO$Parameters$para

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# END OF CODE
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
