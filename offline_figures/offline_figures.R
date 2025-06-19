library(factoextra)
library(stringr)
library(viridis)
library(readxl)
load("../initData/initData.Rdata")
offlineFiguresPath<-file.path(WoodCarbonPath,"offline_figures/")
source(file.path(WoodCarbonPath ,"functions.R"))

data_expt_approachResults<-assignApproach(data_expt)
create_dendrogram(data_expt_approachResults)
ggsave(paste0(offlineFiguresPath,"/fig3_dendrogram.png"),dpi=300,width=15,height=7)

create_forest_plot(plotData.approachC,forestPlotData.approachC,FALSE)
ggsave(paste0(offlineFiguresPath,"/fig4_forestPlot_approachC.png"),dpi=300,width=15,height=7)


plotData.driverC<-plotDataFunc(data_expt_approachResults, c("Whole sector approach"),NULL,"driver1")
forestPlotData.driverC<-forestPlotDataFunc(plotData.driverC,"driver1","driver1Cat")
create_forest_plot(plotData.driverC,forestPlotData.driverC,TRUE)
ggsave(paste0(offlineFiguresPath,"/fig5_forestPlot_driverC.png"),dpi=300,width=15,height=9)


plotCountryData(countryFreq(data_study,countryRefData),"Roundwood (m3)")#
ggsave(paste0(offlineFiguresPath,"/figS1a_barplot_countries.roundwood.png"),dpi=300,width=15,height=9)
plotCountryData(countryFreq(data_study,countryRefData),"Forest.area..1000.ha.")#Roundwood (m3)
ggsave(paste0(offlineFiguresPath,"/figS1b_barplot_countries.forestArea.png"),dpi=300,width=15,height=9)

create_processes_versus_flux_size(study_freq ,"Set1","wrap")
ggsave(paste0(offlineFiguresPath,"/figS2_scatterplot_processesFluxSize.png"),dpi=300,width=15,height=9)

know_dynamics(data_expt)
ggsave(paste0(offlineFiguresPath,"/figS3_errorbar_knowledgeDynamics.png"),dpi=300,width=15,height=9)
