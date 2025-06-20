library(factoextra)
library(stringr)
library(viridis)
library(readxl)


load(file.path(WoodCarbonPath,"initData/initData.Rdata"))
offlineFiguresPath<-file.path(WoodCarbonPath,"offline_figures/")

source(file.path(WoodCarbonPath ,"functions.R"))




data_expt_approachResults<-assignApproach(data_expt)
create_dendrogram(data_expt_approachResults)
ggsave(file.path(offlineFiguresPath,"/fig3_dendrogram.png"),dpi=300,width=15,height=7)

plotData.approachC<-read.csv(paste0(initDataPath,"plotData.approachC.csv"))
forestPlotData.approachC<-read.csv(paste0(initDataPath,"forestPlotData.approachC.csv"))
create_forest_plot(plotData.approachC,forestPlotData.approachC,FALSE)
ggsave(paste0(offlineFiguresPath,"/fig4_forestPlot_approachC.png"),dpi=300,width=15,height=7)

plotData.driverC<-read.csv(paste0(initDataPath,"plotData.driverC.csv"))
forestPlotData.driverC<-read.csv(paste0(initDataPath,"forestPlotData.driverC.csv"))
create_forest_plot(plotData.driverC,forestPlotData.driverC,TRUE)
ggsave(paste0(offlineFiguresPath,"/fig5_forestPlot_driverC.png"),dpi=300,width=15,height=9)


plotCountryData(countryFreq(data_study,countryRefData),"Roundwood (m3)")#
ggsave(paste0(offlineFiguresPath,"/figS1a_barplot_countries.roundwood.png"),dpi=300,width=15,height=9)
plotCountryData(countryFreq(data_study,countryRefData),"Forest.area..1000.ha.")#Roundwood (m3)
ggsave(paste0(offlineFiguresPath,"/figS1b_barplot_countries.forestArea.png"),dpi=300,width=15,height=9)

create_processes_versus_flux_size(study_freq ,"Set1","wrap")
ggsave(paste0(offlineFiguresPath,"/figS2_scatterplot_processesFluxSize.png"),dpi=300,width=15,height=9)

forestPlotData.approachC.dyn<-read.csv(file.path(initDataPath,"forestPlotData.approachC.dyn.csv"))
create_knowDynamicsPlot(forestPlotData.approachC.dyn)
ggsave(paste0(offlineFiguresPath,"/figS3_errorbar_knowledgeDynamics.png"),dpi=300,width=15,height=9)
