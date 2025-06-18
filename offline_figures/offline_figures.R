
load("./initData/initData.Rdata")

data_expt_approachResults<-assignApproach(data_expt)
create_dendrogram(data_expt_approachResults)
ggsave("./offline_figures/fig3_dendrogram.png",dpi=300,width=15,height=7)

create_forest_plot(plotData.approachC,forestPlotData.approachC,FALSE)
ggsave("./offline_figures/fig4_forestPlot_approachC.png",dpi=300,width=15,height=7)

create_forest_plot(plotData.driverC,forestPlotData.driverC,TRUE)
ggsave("./offline_figures/fig5_forestPlot_driverC.png",dpi=300,width=15,height=9)


plotCountryData(country(data,countryCodes),"Roundwood (m3)")#
ggsave("./offline_figures/figS1a_barplot_countries.roundwood.png",dpi=300,width=15,height=9)
plotCountryData(country(data,countryCodes),"Forest.area..1000.ha.")#Roundwood (m3)
ggsave("./offline_figures/figS1b_barplot_countries.forestArea.png",dpi=300,width=15,height=9)

create_processes_versus_flux_size(study_freq ,"Set1","wrap")
ggsave("./offline_figures/figS2_scatterplot_processesFluxSize.png",dpi=300,width=15,height=9)

know_dynamics(data_expt)
ggsave("./offline_figures/figS3_errorbar_knowledgeDynamics.png",dpi=300,width=15,height=9)
