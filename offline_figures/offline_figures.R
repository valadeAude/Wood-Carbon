library(EnvStats)
library(factoextra)
library(RColorBrewer)                           
library(readxl)
library(stringr)
library(viridis)

load("./initData/initData.Rdata")
offlineFiguresPath<-"./offline_figures/"

source("./functions.R")

# -------------
# ------------- Gobal carbon cycle synthesis
refCProces<-GlobalFluxData(dataFlux.file)
CcylePlot<-create_C_synthesis_plot(refCProces)
ggsave(paste0(offlineFiguresPath,"Csynthesis.png"),width=10,bg='transparent')

# -------------
# ------------- Database exploration
plotBarplotYear(bibliom(data))
ggsave(file.path(offlineFiguresPath,"/barplotYear.png"),dpi=300,width=15,height=7)


# -------------
# ------------- Analysis results
data_expt_approachResults<-assignApproach(data_expt)
create_dendrogram(data_expt_approachResults)
ggsave(file.path(offlineFiguresPath,"/fig3_dendrogram.png"),dpi=300,width=15,height=7)


create_forest_plot(plotData.approachC,forestPlotData.approachC,FALSE)
ggsave(paste0(offlineFiguresPath,"/fig4_forestPlot_approachC.png"),dpi=300,width=15,height=7)


create_forest_plot(plotData.driverC,forestPlotData.driverC,TRUE)
ggsave(paste0(offlineFiguresPath,"/fig5_forestPlot_driverC.png"),dpi=300,width=15,height=9)


plotCountryData(countryFreq(data_study,countryRefData),"Roundwood (m3)")#
ggsave(paste0(offlineFiguresPath,"/figS1a_barplot_countries.roundwood.png"),dpi=300,width=15,height=9)
plotCountryData(countryFreq(data_study,countryRefData),"Forest.area..1000.ha.")#Roundwood (m3)
ggsave(paste0(offlineFiguresPath,"/figS1b_barplot_countries.forestArea.png"),dpi=300,width=15,height=9)

create_processes_versus_flux_size(study_freq ,"Set1")
ggsave(paste0(offlineFiguresPath,"/figS2_scatterplot_processesFluxSize.png"),dpi=300,width=15,height=9)
create_processes_versus_flux_size(study_freq ,"Set1","wrap")
ggsave(paste0(offlineFiguresPath,"/figS2_scatterplot_processesFluxSize.wrap.png"),dpi=300,width=15,height=9)

forestPlotData.approachC.dyn<-read.csv(file.path(initDataPath,"forestPlotData.approachC.dyn.csv"))
create_knowDynamicsPlot(forestPlotData.approachC.dyn)
ggsave(paste0(offlineFiguresPath,"/figS3_errorbar_knowledgeDynamics.png"),dpi=300,width=15,height=9)

flowchart_data(data_bibliom_all,dataWoS)

create_processes_frequency(study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),] )
create_processes_frequency(expt_freq[(expt_freq$cat1 =="Processes") & !is.na(expt_freq$cat1),] )


modelComponentsCBoxplots(data_expt_approach,compartmentList, "", "PaperID")

