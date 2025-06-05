## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------
library(cluster)    # clustering algorithms
library(corrplot)
library(data.table)
library(dplyr)
library(DT)
library(EnvStats)
library(factoextra)
library(ggcorrplot)
library(ggplot2)
library(ggridges)
library(ggsignif)
library(ggthemes)
library(gridExtra)
library(heatmaply)
library(mapproj)
library(Matrix)
library(meta)
library(metafor)
library(multcompView)
library(plotly)
library(RColorBrewer)                           
library(readxl)
library(reshape2)
library(rmarkdown)
library(scales)
library(shinyBS)
library(stringr)
library(viridis)
library(waiter)
library(fresh)

load("./initData/initData.Rdata")


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
