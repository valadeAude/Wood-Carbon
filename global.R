## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(DT)
  library(factoextra)
  library(fresh)
  library(logging)
  library(magrittr)
  library(plotly)
  library(RColorBrewer)
  library(shiny)
  library(shinyBS)
  library(shinybusy)
  library(shinyjs)
  library(shinyWidgets)
  library(viridis)
  library(waiter)
  library(shinydashboard)
  library(slider)
  library(bslib)
  library(htmltools)
  library(mathjaxr)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(metafor)
  library(stringr)
})
load("./initData/initData.Rdata")

source("functions.R")




my_theme <- create_theme(
  adminlte_color(
    light_blue = "#00a98e",#top banner
    blue="#ffa17a",#"#9eadc3",#button1
    aqua = "#f3eada",#button2

    maroon = "#344b47",#infobox1
    orange="#97b1ab",#infobox2
    red = "#ffa17a",
    green = "#ffa17a",
    yellow = "#ffa17a",
    navy = "#ffa17a",
    teal = "#ffa17a",
    olive = "#ffa17a",
    lime = "#ffa17a",
    fuchsia = "#ffa17a",
    purple = "#ffa17a",
    black = "#ffa17a",
    gray_lte = "#ffa17a"

  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#344b47",
    dark_hover_bg = "#97b1ab",
    dark_color = "#e8f3f1"
  ),
  adminlte_global(
    content_bg = "#e8f3f1",
    box_bg = "#FFFFFF",
    info_box_bg = "#FFFFFF"
  )
)
txt_size<-10
txt_angle<-45
