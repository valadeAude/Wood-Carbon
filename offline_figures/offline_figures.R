library(EnvStats)
library(factoextra)
library(RColorBrewer)                           
library(readxl)
library(stringr)
library(viridis)
library(kableExtra)

load("./initData/initData.Rdata")
offlineFiguresPath<-"./offline_figures/"

source("./functions.R")

txt_size<-10
txt_angle<-45

#Do not run while debugging

# -------------
# ------------- Gobal carbon cycle synthesis
refCProces<-GlobalFluxData(dataFlux.file)
CcylePlot<-create_C_synthesis_plot(refCProcess)
ggsave(paste0(offlineFiguresPath,"fig2_Csynthesis.png"),width=10,bg='transparent')

# -------------
# ------------- Database exploration
plotBarplotYear(bibliom(data))
ggsave(file.path(offlineFiguresPath,"/barplotYear.png"),dpi=300,width=15,height=7)

#Stop Do not run while debugging

# -------------
# ------------- Analysis results
#data_expt_approachResults<-assignApproach(data_expt)
#data_expt_assumptionResults<-assignAssumption(data_expt)
create_dendrogram(data_study,
                  k=3,
                  c( 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic','off_product_biogenic'))
ggsave(file.path(offlineFiguresPath,"/fig3_dendrogram_study_processes.png"),dpi=300,width=17,height=7)

# create_dendrogram(data_expt_approachResults,
#                   k=3,
#                   c( 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic','off_product_biogenic'))
# ggsave(file.path(offlineFiguresPath,"/fig3_dendrogram_expt_processes.png"),dpi=300,width=15,height=7)

create_dendrogram(data_expt_assumptionResults,
                  k=6,
                  c( 'modelAssumption', 'scaleAgg','singleProduct','time_horizon','driver1'))
ggsave(file.path(offlineFiguresPath,"/fig3_dendrogram_topic.png"),dpi=300,width=14,height=7)




# create_forest_plot_dynamics(forestPlotData.approachC.dyn)
# #create_knowDynamicsPlot(forestPlotData.approachC.dyn)
# ggsave(paste0(offlineFiguresPath,"/fig4_errorbar_knowledgeDynamics_approach.png"),dpi=300,width=15,height=9)

create_forest_plot_dynamics(forestPlotData.assumptionC.dyn)
#create_knowDynamicsPlot(forestPlotData.approachC.dyn)
ggsave(paste0(offlineFiguresPath,"/fig4_errorbar_knowledgeDynamics_assumption.png"),dpi=300,width=15,height=9)


# create_forest_plot(plotData.driverC.wholeSector,forestPlotData.driverC.wholeSector,TRUE)
# ggsave(paste0(offlineFiguresPath,"/fig5_forestPlot_driverC.wholeSector.png"),dpi=300,width=15,height=9)

create_forest_plot(plotData.driverC.AllCPools,forestPlotData.driverC.AllCPools,TRUE)
ggsave(paste0(offlineFiguresPath,"/fig5_forestPlot_driverC.AllCPools.png"),dpi=300,width=15,height=9)

#Do not run while debugging
# 
plotCountryData(countryFreq(data_study,countryRefData),"Roundwood (m3)")#
ggsave(paste0(offlineFiguresPath,"/figS1a_barplot_countries.roundwood.png"),dpi=300,width=15,height=9)
plotCountryData(countryFreq(data_study,countryRefData),"Forest.area..1000.ha.")#Area (Mha)
ggsave(paste0(offlineFiguresPath,"/figS1b_barplot_countries.forestArea.png"),dpi=300,width=15,height=9)
plotCountryData(countryFreq(data_study,countryRefData),"Forest area ratio (%)")#Area ratio (%)
ggsave(paste0(offlineFiguresPath,"/figS1c_barplot_countries.forestAreaRatio.png"),dpi=300,width=15,height=9)



create_processes_versus_flux_size(refCProcessMean,study_freq ,"Set1",wood_type_names)
ggsave(paste0(offlineFiguresPath,"/figS2a_scatterplot_processesFluxSize.png"),dpi=300,width=15,height=9)
create_processes_versus_flux_size(refCProcessMean,study_freq ,"Set1",wood_type_names,"wrap")
ggsave(paste0(offlineFiguresPath,"/figS2b_scatterplot_processesFluxSize.wrap.png"),dpi=300,width=15,height=9)
#Stop Do not run while debugging


# create_forest_plot(plotData.approachC,forestPlotData.approachC,FALSE)
# ggsave(paste0(offlineFiguresPath,"/figS3_forestPlot_approachC.png"),dpi=300,width=15,height=7)

create_forest_plot(plotData.assumptionC,forestPlotData.assumptionC,FALSE)
ggsave(paste0(offlineFiguresPath,"/figS3_forestPlot_assumptionC.png"),dpi=300,width=15,height=7)





#Do not run while debugging
tableModelComponentsC(tTestPairsSignifAggVarMelt)
#
flowchart_data(data_bibliom_all,dataWoS)
#
create_processes_frequency(study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),],percent="percent" )
ggsave(paste0(offlineFiguresPath,"/figSX_barPlot_processFrequencyStudyPercent.png"),dpi=300,width=15,height=7)
create_processes_frequency(expt_freq[(expt_freq$cat1 =="Processes") & !is.na(expt_freq$cat1),] ,percent="percent")
ggsave(paste0(offlineFiguresPath,"/figSX_barPlot_processFrequencyExptPercent.png"),dpi=300,width=15,height=7)

create_driver_frequency(expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),] )



modelComponentsCBoxplots(data_expt_approach,c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic"), "", "PaperID")
#Stop Do not run while debugging





outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


#Check all WoS papers have been processed
dataWoS[ !(dataWoS$`Article Title`%in%  data_bibliom_all$Article_Title), c("Authors","Publication Year","Article Title")]
print(dim(dataWoS[!(dataWoS$PaperID %in%  data_bibliom_all$PaperID),c("PaperID")])[1]) #-> should be 0

nData<-length(unique(data$PaperID))

# Number of papers identified from WoS
nWoS<-length(dataWoS$PaperID)

# Number of manual additions of papers that were not identified by WoS query
dataManual<-unique(data[!((data$PaperID) %in% dataWoS$PaperID),"PaperID"])
nManual<-length(dataManual$PaperID)

# Number of processed papers
nBibliom<-length(data_bibliom$PaperID)
data_included<-unique(data[data$Exclusion=="included",c("PaperID")])
#Describe exclusion criteria 
exclusionTable<-table(data_bibliom_all[data_bibliom_all$Exclusion!="included","Exclusion"])

# Final number of studies included
nBibliom_in<-length(unique(data_study$PaperID))
# Final number of carbon balance estimates recorded
nRecords<-length(data_expt$Cdelta_tCO2...158)

# number of wood carbon substitution values
nSubstitution<-dim(data_expt[!is.na(data_expt$substitution),"PaperID"])[1]

# 
# Records identified from WoS : `r nWoS`
# 
# Manual addition :`r nManual`
# 
# Screened for relevance :`r nBibliom`
# 
# Exclusion reasons : `r kable(exclusionTable)`
# 
# Included studies : `r nIncluded`
# 
# Number of records for C: `r nRecords`
# 
# Number of records for substitution: `r nSubstitution`
# 
# Kolmogorov-Smirnov test between global sequestration and global emission of the forestry sector:

n_expt<-dim(data_expt[,"PaperID"])[1]
n_expt_subs<-dim(data_expt[!is.na(data_expt$substitution),"PaperID"])

pGlobal_expt<-  round(nrow(data_expt[ data_expt$scaleAgg=="w","PaperID"])/n_expt*100,0)
pGlobal_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution)& data_expt$scaleAgg=="w","PaperID"])/n_expt_subs*100,0)
pGlobal_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scaleAgg=="w","PaperID"]))/nBibliom_in*100,1)
nGlobal_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scaleAgg=="w","PaperID"])),1)

pLocal_expt<-  round(nrow(data_expt[data_expt$scaleAgg=="loc","PaperID"])/n_expt*100,0)
pLocal_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution)&data_expt$scaleAgg=="loc","PaperID"])/n_expt_subs*100,0)
pLocal_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scaleAgg=="loc","PaperID"]))/nBibliom_in*100,1)
nLocal_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scaleAgg=="loc","PaperID"])),1)

pNational_expt<-  round(nrow(data_expt[data_expt$scale=="coy","PaperID"])/n_expt*100,0)
pNational_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution)&data_expt$scale=="coy","PaperID"])/n_expt_subs*100,0)
pNational_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scale=="coy","PaperID"]))/nBibliom_in*100,1)
nNational_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scale=="coy","PaperID"])),1)

pReg_expt<-  round(nrow(data_expt[data_expt$scaleAgg=="reg","PaperID"])/n_expt*100,0)
pReg_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution)&data_expt$scaleAgg=="reg","PaperID"])/n_expt_subs*100,0)
pReg_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scaleAgg=="reg","PaperID"]))/nBibliom_in*100,1)
nReg_expt_bibliom<-round(nrow(unique(data_expt[data_expt$scaleAgg=="reg","PaperID"])),1)

pReg_expt_bibliom+pLocal_expt_bibliom+pGlobal_expt_bibliom

nCountries<-dim(unique(data_expt[,"country"]))
nCountries_subs<-dim(unique(data_expt[!is.na(data_expt$substitution),"country"]))




# global perimeter Publications :`r nGlobal_expt_bibliom`, `r pGlobal_expt_bibliom` %. 
# global perimeter Records :`r pGlobal_expt`%. 
# 
# local perimeter Publications :`r nLocal_expt_bibliom`, `r pLocal_expt_bibliom`%. 
# local perimeter Records :`r pLocal_expt`%. 
# 
# national perimeter Publications :`r nNational_expt_bibliom`, `r pNational_expt_bibliom`%. 
# national perimeter Records :`r pNational_expt`%. 
# 
# regional perimeter Publications :`r nReg_expt_bibliom`, `r pReg_expt_bibliom`%. 
# regional perimeter Records :`r pReg_expt`%. 


pTimber_expt<- round(nrow(data_expt[data_expt$productsCheck==1 & data_expt$TimberInput==1,"PaperID"])/n_expt*100,0)
pTimber_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck==1 & data_expt$TimberInput==1,"PaperID"])/n_expt_subs*100,0)
pTimber_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck==1 & data_expt$TimberInput==1,"PaperID"]))/nBibliom_in*100,0)

pEnergy_expt<-  round(nrow(data_expt[ data_expt$productsCheck==1 & data_expt$EnergyInput==1,"PaperID"])/n_expt*100,0)
pEnergy_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck==1 &  data_expt$EnergyInput==1,"PaperID"])/n_expt_subs*100,0)
pEnergy_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck==1 & data_expt$EnergyInput==1,"PaperID"]))/nBibliom_in*100,0)

pPulpPaper_expt<-  round(nrow(data_expt[ data_expt$productsCheck==1 & data_expt$PulpPaperInput==1,"PaperID"])/n_expt*100,0)
pPulpPaper_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck==1 &  data_expt$PulpPaperInput==1,"PaperID"])/n_expt_subs*100,0)
pPulpPaper_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck==1 & data_expt$PulpPaperInput==1,"PaperID"]))/nBibliom_in*100,0)

pUpstream_expt<-round(nrow(data_expt[data_expt$productsCheck==1 & data_expt$UpstreamInput==1,"PaperID"])/n_expt*100,0)
pUpstream_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck==1 &  data_expt$UpstreamInput==1,"PaperID"])/n_expt_subs*100,0)
pUpstream_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck==1 & data_expt$UpstreamInput==1,"PaperID"]))/nBibliom_in*100,0)

pMixed_expt<-  round(nrow(data_expt[ data_expt$productsCheck!=1 ,"PaperID"])/n_expt*100,0)
pMixed_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck!=1 ,"PaperID"])/n_expt_subs*100,0)
pMixed_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck!=1 ,"PaperID"]))/nBibliom_in*100,0)

pSingle_expt<-  round(nrow(data_expt[ data_expt$productsCheck==1 & data_expt$UpstreamInput!=1,"PaperID"])/n_expt*100,0)
pSingle_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck==1 & data_expt$UpstreamInput!=1,"PaperID"])/n_expt_subs*100,0)
pSingle_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck==1 & data_expt$UpstreamInput!=1,"PaperID"]))/nBibliom_in*100,0)

pNotSingle_expt<-  round(nrow(data_expt[ data_expt$productsCheck!=1 | data_expt$UpstreamInput==1,"PaperID"])/n_expt*100,0)
pNotSingle_expt_subs<-  round(nrow(data_expt[!is.na(data_expt$substitution) & data_expt$productsCheck==1 |  data_expt$UpstreamInput==1,"PaperID"])/n_expt_subs*100,0)
pNotSingle_expt_bibliom<-round(nrow(unique(data_expt[data_expt$productsCheck!=1 | data_expt$UpstreamInput==1,"PaperID"]))/nBibliom_in*100,0)
nrow(unique(data_expt[data_expt$productsCheck!=1,"PaperID"]))
nrow(unique(data_expt[data_expt$UpstreamInput==1,"PaperID"]))


# 
# No single wood type Publications :`r pNotSingle_expt_bibliom`%    
# No single wood type Records :`r pNotSingle_expt`  %
# 
# single wood type Publications :`r pSingle_expt_bibliom` %  
# single wood type Records :`r pSingle_expt`%  
# 
# timber wood type Publications :`r pTimber_expt_bibliom`  %
# timber wood type Records :`r pTimber_expt`%  
# 
# energy wood type Publications :`r pEnergy_expt_bibliom`  %
# energy wood type Records :`r pEnergy_expt` % 
# 
# pulp & paper wood type Publications :`r pPulpPaper_expt_bibliom` % 
# pulp & paper wood type Records :`r pPulpPaper_expt` % 
# 
# upstream wood type Publications :`r pPulpPaper_expt_bibliom`  %
# upstream wood type Records :`r pPulpPaper_expt` % 
# 
# mixed wood type Publications :`r pMixed_expt_bibliom`  %
# mixed wood type Records :`r pMixed_expt` % 



###
mypalDriv <- colorRampPalette(brewer.pal(4, "Purples")[2:4])
mypalDriv2 <- colorRampPalette(brewer.pal(7, "Oranges")[2:7])
mypalDriv3 <- colorRampPalette(brewer.pal(5, "Greens")[2:5])
mypalDriv4 <- colorRampPalette(brewer.pal(5, "Blues")[2:5])


ggplot(expt_frq_dyn[expt_frq_dyn$cat1=="Change in practices",]
       ,aes(x=year,y=valuePercent,color=interaction(longName,cat2),linetype=cat2))+
  geom_line()+
  theme_bw()+
  scale_linetype_manual(values=c("solid", "dotted","dotdash","dashed"))+
  scale_colour_manual(values = c(mypalDriv4(4), mypalDriv3(4),mypalDriv2(6),mypalDriv(3))) +
  facet_wrap(variable~modelAssumption)
ggsave(paste0(offlineFiguresPath,"/figSX_barPlot_DriverEvolution_wrap.png"),dpi=300,width=15,height=7)

ggplot(expt_frq_dyn[expt_frq_dyn$cat1=="Change in practices"&expt_frq_dyn$variable=="All",]
       ,aes(x=year,y=valuePercent,color=interaction(longName,cat2),linetype=cat2))+
  geom_line()+  
  theme_bw()+
  scale_linetype_manual(values=c("solid", "longdash","dotdash","twodash"))+
  scale_colour_manual(values = c(mypalDriv4(4), mypalDriv3(4),mypalDriv2(6),mypalDriv(3))) +
  facet_wrap(~modelAssumption)
ggsave(paste0(offlineFiguresPath,"/figSX_barPlot_DriverEvolution.png"),dpi=300,width=15,height=7)

####

mypalProc <- colorRampPalette(brewer.pal(4, "PuRd")[2:4])#Biogenic ex-situ 3
mypalProc2 <- colorRampPalette(brewer.pal(4, "Greys")[2:4])#Biogenic in-situ 3
mypalProc3 <- colorRampPalette(brewer.pal(4, "Greens")[2:4])# C dynamics 2
mypalProc4 <- colorRampPalette(brewer.pal(4, "Oranges")[2:4])# Fossil exsitu 3
mypalProc5 <- colorRampPalette(brewer.pal(4, "Purples")[2:4])#Fossil insitu 1
mypalProc6 <- colorRampPalette(brewer.pal(4, "Reds")[2:4])#Market-based feedbacks 1
mypalProc7 <- colorRampPalette(brewer.pal(4, "Blues")[2:4])#Substitution shortcut 2

ggplot(expt_frq_dyn[expt_frq_dyn$cat1=="Processes",]
       ,aes(x=year,y=valuePercent,color=interaction(longName,cat3),linetype=cat3))+
  geom_line()+
  theme_bw()+
  scale_colour_manual(values = c(mypalProc(3), mypalProc2(3),mypalProc3(2),mypalProc4(3), mypalProc5(1),mypalProc6(2),mypalProc7(2))) +
  facet_wrap(variable~modelAssumption)
ggsave(paste0(offlineFiguresPath,"/figSX_barPlot_ProcessEvolution_wrap.png"),dpi=300,width=15,height=7)

ggplot(expt_frq_dyn[expt_frq_dyn$cat1=="Processes" &expt_frq_dyn$variable=="All" ,]
       ,aes(x=year,y=valuePercent,color=interaction(longName,cat3),linetype=cat3))+
  geom_line()+  
  theme_bw()+
  scale_colour_manual(values = c(mypalProc(3), mypalProc2(3),mypalProc3(2),mypalProc4(3), mypalProc5(1),mypalProc6(2),mypalProc7(2))) +
  facet_wrap(~modelAssumption)
ggsave(paste0(offlineFiguresPath,"/figSX_barPlot_ProcessEvolution.png"),dpi=300,width=15,height=7)



