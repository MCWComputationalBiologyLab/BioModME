library(shinydashboard)
library(bs4Dash)
library(shiny)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tidyverse)
library(dplyr)
library(rhandsontable)
library(data.table)
library(ggpmisc)
library(colourpicker)
library(shinyBS)
library(shinyjqui)
library(bsplus)
library(deSolve)
library(plotly)
library(Deriv)
library(viridis)
library(ggpubr)
library(shinycssloaders)
library(waiter)
library(fresh)
library(readxl)
library(minpack.lm)
library(measurements)
library(qdapRegex)
library(XML)
library(xml2)
library(katex)
library(reshape2)
library(clipr)
library(mathml)

UNIT_MAPPING <- c(
  nsec = "ns",
  usec = "us",
  msec = "ms",
  sec = "s",
  mins = "min",
  hour = "hr",
  hours = "hr",
  days = "day",
  week = "wk",
  mon = "month",
  year = "yr",
  dec = "decade",
  cen = "century",
  mil = "millenium",
  
  # Length
  meter = "m",
  metre = "m",
  inch = "in",
  foot = "ft",
  feet = "ft",
  yard = "yd",
  mile = "mi",
  AU = "au",
  light_yr = "light_year",
  
  # Mass
  pound = "lbs",
  pounds = "lbs",
  
  # Volume
  ul = "uL",
  ml = "mL",
  dl = "dL",
  l = "L",
  
  # Energy
  Cal = "cal",
  
  # Flow
  LPM = "l_per_min",
  LPH = "l_per_hour",
  GPM = "gal_per_min",
  GPH = "gal_per_hour"
  
  # Count (none)
)

DURATION_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$duration,
    names(UNIT_MAPPING)
  )

VOLUME_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$volume,
    names(UNIT_MAPPING)
  )

MASS_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$mass,
    names(UNIT_MAPPING)
  )

LENGTH_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$length,
    names(UNIT_MAPPING)
  )

ENERGY_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$energy,
    names(UNIT_MAPPING)
  )

FLOW_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$flow,
    names(UNIT_MAPPING)
  )

COUNT_CHOICES <- 
  setdiff(
    measurements::conv_unit_options$count,
    names(UNIT_MAPPING)
  )

