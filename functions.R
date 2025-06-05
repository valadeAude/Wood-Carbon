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
#----------- Data processing ----------- 

# Function Read data synthesis of global carbon fluxes
GlobalFluxData<-function(dataFlux.file){
  refCProcess<-read_xlsx(dataFlux.file)
  #refCProces<-refCProces[refCProces$substitutionDatabaseVariable!="live_biomass_in",]
  refCProcess$substitutionDatabaseVariable<-factor(refCProcess$substitutionDatabaseVariable,levels=rev(c(
    "AgregationLevel",
    "live_biomass_out_check",
    "total_sector_emission",
    "total_sector_sequestration",
    "harvest",
    "harv_residues", 
    "soilC",
    "live_biomass_in",
    "forestry_emiss",
    "products_storage_C",
    "manufacturing_emiss",
    "eol_biogenic_energy",
    "eol_biogenic_disposal"
  )))
  
  
  return(refCProcess)
}
create_C_synthesis_plot<-function(refCProces){
  
  refCProcessMean<-aggregate(`value GtCO2/yr`~substitutionDatabaseVariable,data=refCProces,FUN=mean,na.rm=T)
  refCProcessMean$`value GtCO2/yr`<-as.numeric(refCProcessMean$`value GtCO2/yr`)
  refCProcessSd<-aggregate(`value GtCO2/yr`~substitutionDatabaseVariable,data=refCProces,FUN=sd,na.rm=T)
  
  
  # Calculate the sum of all "emission" processes to compare to sequestration estimate
  TotalEmissionsMean<-round(refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="harv_residues","value GtCO2/yr"]+                                      refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_energy","value GtCO2/yr"]+ 
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_disposal","value GtCO2/yr"]+
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="forestry_emiss","value GtCO2/yr"]+                            refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="manufacturing_emiss","value GtCO2/yr"]+                             #  refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="maintenance_emiss","value GtCO2/yr"]+ 
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="soilC","value GtCO2/yr"],2)
  
  TotalEmissionsSd<-  round(sqrt(refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="harv_residues","value GtCO2/yr"]^2+  
                                   refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_energy","value GtCO2/yr"]^2+                 refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_disposal","value GtCO2/yr"]^2+  
                                   refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="forestry_emiss","value GtCO2/yr"]^2+
                                   refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="manufacturing_emiss","value GtCO2/yr"]^2+ 
                                   #refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="maintenance_emiss","value GtCO2/yr"]^2+
                                   refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="soilC","value GtCO2/yr"]^2),3)
  # 
  ######################
  TotalSequestrationMean<-round( refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="live_biomass_in","value GtCO2/yr"]+
                                   refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="products_storage_C","value GtCO2/yr"]/2,2)
  
  TotalSequestrationSd<-  round(sqrt(refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="live_biomass_in","value GtCO2/yr"]^2+
                                       refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="products_storage_C","value GtCO2/yr"]^2),3)
  # 
  
  refCProces<-rbind(c("Summed from data synthesis","Balance","Balance","3","biogenic", "Extraction","total_sector_emission","sum of biogenic and fossil, in-situ and ex-situ emissions"," "," "," "," "," "," "," ",(TotalEmissionsMean-TotalEmissionsSd)), refCProces)
  refCProces<-rbind(c("Summed from data synthesis","Balance","Balance","3","biogenic", "Extraction","total_sector_emission","sum of biogenic and fossil, in-situ and ex-situ emissions"," "," "," "," "," "," "," ",(TotalEmissionsMean)), refCProces)
  
  refCProces<-rbind(c("Summed from data synthesis","Balance","Balance","3","biogenic", "Extraction","total_sector_emission","sum of biogenic and fossil, in-situ and ex-situ emissions"," "," "," "," "," "," "," ",(TotalEmissionsMean+TotalEmissionsSd)), refCProces)
  
  refCProces<-rbind(c("Summed from data synthesis","Balance","Balance","2","biogenic", "Forest growth","total_sector_sequestration","sum of C sequestered out of atmosphere"," "," "," "," "," "," "," ",(TotalSequestrationMean-TotalSequestrationSd)), refCProces)
  refCProces<-rbind(c("Summed from data synthesis","Balance","Balance","2","biogenic", "Forest growth","total_sector_sequestration","sum of C sequestered out of atmosphere"," "," "," "," "," "," "," ",(TotalSequestrationMean)), refCProces)
  refCProces<-rbind(c("Summed from data synthesis","Balance","Balance","2","biogenic", "Forest growth","total_sector_sequestration","sum of C sequestered out of atmosphere"," "," "," "," "," "," "," ",(TotalSequestrationMean+TotalSequestrationSd)), refCProces)
  
  
  refCProces$`value GtCO2/yr`<-as.numeric(refCProces$`value GtCO2/yr`)
  
  
  refCProces$substitutionDatabaseVariable<-
    factor(refCProces$substitutionDatabaseVariable,
           levels=rev(c(
             "harvest",
             "live_biomass_in",
             "forestry_emiss",
             "products_storage_C",
             "harv_residues", 
             "soilC",
             "manufacturing_emiss",
             "maintenance_emiss",
             "eol_biogenic_energy",
             "eol_biogenic_disposal",
             "total_sector_sequestration",
             "total_sector_emission"
           )),
           labels=rev(c(
             "Harvested wood",
             "Net ecosystem production",
             "Forestry emissions",
             "Carbon storage in products",
             "Emissions from harvest residues", 
             "Emissions from forest soil",
             "Emissions from manufacturing",
             "Emissions from products' maintenance",
             "Emissions from wood-based fuel",
             "Emissions from products' disposal",
             "Total sector sequestration",
             "Total sector emissions"
           ))
    )
  refCProces$FluxDirection<-factor(refCProces$FluxDirection,levels=c("Sequestration","Emission","Balance"),labels=c("Sequestration","Emission ","Balance"))
  p<-ggplot(refCProces,aes(x=reorder(substitutionDatabaseVariable,`value GtCO2/yr`,na.rm=TRUE),y=`value GtCO2/yr`,col=Compartment))+
    geom_boxplot() +  
    scale_fill_manual(values=c("white","grey"))+
    scale_color_manual(values=c(color_biogenic,color_fossil,color_sum))+
    stat_summary(fun.y=mean, geom="point", shape=5, size=2, color="black") +
    theme_bw()+
    # theme(text = element_text(size=txt_size_big),
    #        axis.text.x = element_text(hjust=1))+
    labs(x="",y="Carbon flux (GtCO2/yr)")+
    #scale_y_continuous(limits = c(-0.5, 5), breaks = c(0, 2, 4))+
    # scale_x_discrete(labels=c("Wood end-of-life disposal","Wood energy burning","Maintenance energy","Manufacturing machinery","Storage in HWP","Forestry machinery","Harvest impact on soil C", "Wood harvest losses", "Wood harvest", "C sequestration in wood"))+
    stat_n_text()+
    coord_flip()+
    #  facet_wrap(~FluxDirection,ncol=1,scales="free_y",labeller =labeller(AgregationLevel=level.labs))
    facet_grid(FluxDirection~.,scales="free_y",space="free",switch="both")+  
    theme(strip.placement = "outside",
          strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid" ))
  return(p)

}
# Function used in create_dendrogram
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  (p.mat)
}


# Function used in create_dendrogram
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x),
    colnames(x),
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}



# ---------------Read Article-level data -> output = data_bibliom ----------------------- 
# data_bibliom<-unique(data[,(colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c('Metadata'),'names']) & ! colnames(data)%in%c("StudyID","ExperimentID","Nstudy","Nexperiment","Reviewer")])
# 
# data_bibliom_in<-unique(data[data$Exclusion=='included',(colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c("Metadata","Protocol"),'names']) ])


# ------Read Study-level data (i.e. model configurations) -> output = data_study 

study<-function(data){
  # Study data are unique sets of study framework parameters
  data_study<-unique(data[data$Exclusion=='included',(colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c("Metadata", "Model"),'names'])])
  
  # Each study data item is assigned a study code
  data_study$study_code<-apply( data_study[,(colnames(data_study) %in% c("DOI",categoriesdf[categoriesdf$cat0 %in% c("Metadata", "Model"),'names']))] , 1 , paste , collapse = "" )
  data_study<-data_study[!duplicated(data_study$study_code),]
  # data_study$boundaries<-factor(data_study$boundaries, levels=c("biogenic_insitu_only","biogenic_all","biogenic_exsitu_only","bothC_bothSitu","biogenic_insitu_fossil_exsitu","exsitu_only","fossil_exsitu_only"))
  
  return(data_study)
}


# --------------------------------------------------------------------------- 
# ----------------Prepare Experiment-level data -> output = data_expt 
bibliom<-function(data){
  data<-data[!is.na(data$substitution),]
  dataMd<-data[data$Exclusion=='included',
               (colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c('Metadata'),'names'])  & 
               ! colnames(data)%in%c("StudyID","ExperimentID","Nstudy","Nexperiment","Reviewer" ) , ]#data with paper metadata only
  data_bibliom<-setDT(dataMd)[,list(count=.N),names(dataMd)]  #data_bibliom<-unique(data[,(colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c('Metadata'),'names']) & ! colnames(data)%in%c("StudyID","ExperimentID","Nstudy","Nexperiment","Reviewer")])
  #data_bibliom_in<-unique(data[data$Exclusion=='included',(colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c("Metadata","Protocol"),'names']) ])
  
}

expt<-function(data){
  print("expt")
  data_expt<-data[!is.na(data$Exclusion) & data$Exclusion=='included' & !is.na(data$Article_Title) &!is.na(data$substitution),]
 
  data_expt$scaleAgg<-factor(data_expt$scaleAgg, levels=c("loc","reg","w"))
  data_expt$singleProduct<-factor(data_expt$singleProduct, levels=c("UpstreamInput","TimberInput","EnergyInput","mixedProduct","PulpPaperInput"))
  #data_expt$bothC_bothSitu<-factor(data_expt$bothC_bothSitu,levels=c(0,1))
  # DRIVER : convert 0/1 into text 
  data_expt$driver<-apply(
    data_expt[,colnames(data_expt) %in% categoriesdf[categoriesdf$cat0 =='Drivers','names']] == 1, 
    1,
    function(x) { paste0(gsub(".*_", "", names(which(x))), collapse = ":") }
  )
  data_expt$driver2<-apply(
    data_expt[,colnames(data_expt) %in% categoriesdf[categoriesdf$cat0 =='Drivers','names']] == 2, 
    1,
    function(x) { paste0(gsub(".*_", "", names(which(x))), collapse = ":") }
  )
  return(data_expt)
}

country<-function(data){
  print("country")
  #Count number of study items per country
  data_study<-study(data)
  test<-unique(data_study[,c("PaperID","country")])
  nStudyCountry<-aggregate(PaperID~country,data=test,FUN=length)
  nStudyCountry<-merge(nStudyCountry,countryCodes,by.x='country',by.y="Country",all=TRUE)
  
  #Load Bais data
  BaisFiga<-read_xlsx(paste0(dataPath,"/Bais/Bais2015Data/BaisExtract.xlsx"),sheet=1)
  BaisFigc<-read_xlsx(paste0(dataPath,"/Bais/Bais2015Data/BaisExtract.xlsx"),sheet=3)
  BaisData<-merge(BaisFiga[,c('iso_a2','levelFig3a','NAME')],BaisFigc[,c('iso_a2','levelFig3c')])
  BaisData<-merge(BaisData,countryCodes,by.y='Alpha-2 code',by.x='iso_a2')
  BaisData<-BaisData[,c("levelFig3a","levelFig3c","Alpha-3 code")]
  
  # Load FAO Roundwod data
  faoData<-read.csv(paste0(dataPath,"/FAOWoodData/FAOSTAT_data_en_12-11-2023.csv"))
  
  faoData[faoData$Area=="United Kingdom of Great Britain and Northern Ireland","Area"]<-"United Kingdom"
  faoData$Area<-str_to_title(faoData$Area)
  
  faoData<-merge(faoData[,c("Area","Item","Year.Code","Unit","Value")],countryCodes,by.x="Area",by.y="Country")
  colnames(faoData)[colnames(faoData)=="Value"]<-"Roundwood (m3)"
  faoData<-faoData[order(-faoData$Roundwood),]
  faoData<-faoData[,c("Alpha-3 code","Roundwood (m3)")]
  
  # Load FAO ForestAreaPercentLand data
  faoForestData<-read_xlsx(paste0(dataPath,"/FAOForestAreaPercentLand/Forest area as a percent of land area.xlsx"))
  
  faoForestData[faoForestData$Country=="United Kingdom of Great Britain and Northern Ireland","Country"]<-"United Kingdom"
  faoForestData$Country<-str_to_title(faoForestData$Country)
  faoForestData<-faoForestData[faoForestData$Year==2015,]
  
  faoForestData<-merge(faoForestData[,c("Country","Year","Forest area ratio (%)")],countryCodes,by.x="Country",by.y="Country")
  faoForestData<-faoForestData[,c("Alpha-3 code","Forest area ratio (%)")]
  
  # Load FAO ForestArea data
  faoForestAData<-read.csv(paste0(dataPath,"/FAOForestArea/Forest area.csv"))
  
  faoForestAData[faoForestAData$Country=="United Kingdom of Great Britain and Northern Ireland","Country"]<-"United Kingdom"
  faoForestAData$Country<-str_to_title(faoForestAData$Country)
  faoForestAData<-faoForestAData[faoForestAData$Year==2015,]
  faoForestAData<-merge(faoForestAData[,c("Country","Year","Forest.area..1000.ha.")],countryCodes,by.x="Country",by.y="Country")
  faoForestAData<-faoForestAData[,c("Alpha-3 code","Forest.area..1000.ha.")]
  
  #Load OECD GDP spending for research
  gdpRD<-read.csv(paste0(dataPath,"/UnescoData/SCN_DS_14122023043909123.csv"))
  gdpRD<-gdpRD[gdpRD$TIME=="2015",c("LOCATION","TIME","Value")]
  colnames(gdpRD)<-c("Alpha-3 code","TIME","GDP_RD")
  gdpRD<-merge(gdpRD,countryCodes,by="Alpha-3 code")
  gdpRD<-gdpRD[,c("Alpha-3 code","GDP_RD")]
  
  #Merge datasets
  countryData<-merge(BaisData,gdpRD,by='Alpha-3 code',all=TRUE)
  countryData<-merge(countryData,faoForestData ,by="Alpha-3 code",all=TRUE)
  countryData<-merge(countryData,faoForestAData ,by="Alpha-3 code",all=TRUE)
  countryData<-merge(countryData,faoData,by="Alpha-3 code",all=TRUE)
  countryData<-merge(countryData,nStudyCountry,by='Alpha-3 code',all=TRUE)
  #countryData<-countryData[!is.na(countryData$PaperID),]
  countryData$PaperID<-as.numeric(countryData$PaperID)
  countryData<-countryData[!is.na(countryData$`Forest area ratio (%)`),]  
  print("exit country")
  
  return(countryData)
}


funcFreq<-function(df,categoriesdf){
  dfShort<-subset(df,select=-c(Exclusion,DOI))
  
  nMixedProduct<-dim(dfShort[dfShort$singleProduct=="mixedProduct","singleProduct"])[1]
  nEnergyInput<-dim(dfShort[dfShort$singleProduct=="EnergyInput","singleProduct"])[1]
  nTimberInput<-dim(dfShort[dfShort$singleProduct=="TimberInput","singleProduct"])[1]
  nUpstreamInput<-dim(dfShort[dfShort$singleProduct=="UpstreamInput","singleProduct"])[1]
  nPulpPaperInput<-dim(dfShort[dfShort$singleProduct=="PulpPaperInput","singleProduct"])[1]  
  Freq0<-aggregate(dfShort[,],by=list(dfShort$singleProduct), function(x) length(which(x==1)))
  Freq1<-data.frame(t(Freq0[,2:ncol(Freq0)]))
  colnames(Freq1)<-Freq0$Group.1
  Freq1[,"names"]<-colnames(dfShort)
  Freq<-melt(Freq1)
  Freq<-data.frame(merge(Freq,categoriesdf[,c('names','colcat2','cat2','cat1','colcat1')],by="names",all.x=T))
  Freq<-unique(Freq)
  Freq<-Freq[!is.na(Freq$cat2),]
  Freq$names<-factor(Freq$names,levels=unique(categoriesdf[order(categoriesdf$id),'names']))
  
  Freq$valuePercent<-NA
  Freq[Freq$variable=="UpstreamInput",'valuePercent']<-Freq[Freq$variable=="UpstreamInput",'value']/nUpstreamInput*100
  Freq[Freq$variable=="PulpPaperInput",'valuePercent']<-Freq[Freq$variable=="PulpPaperInput",'value']/nPulpPaperInput*100
  Freq[Freq$variable=="TimberInput",'valuePercent']<-Freq[Freq$variable=="TimberInput",'value']/nTimberInput*100
  Freq[Freq$variable=="EnergyInput",'valuePercent']<-Freq[Freq$variable=="EnergyInput",'value']/nEnergyInput*100
  Freq[Freq$variable=="mixedProduct",'valuePercent']<-Freq[Freq$variable=="mixedProduct",'value']/nMixedProduct*100
  
  Freq[Freq$variable=="UpstreamInput",'nSingleProduct']<-nUpstreamInput
  Freq[Freq$variable=="PulpPaperInput",'nSingleProduct']<-nPulpPaperInput
  Freq[Freq$variable=="TimberInput",'nSingleProduct']<-nTimberInput
  Freq[Freq$variable=="EnergyInput",'nSingleProduct']<-nEnergyInput
  Freq[Freq$variable=="mixedProduct",'nSingleProduct']<-nMixedProduct
  
  return(Freq)
}

assignApproach<-function(data_expt){
  
  nminTechno<-2
  nminEcos<-4
  
  data_expt_approach<-data_expt
  data_expt_approach$modelApproach<-"Hybrid approach"
  
  data_expt_approach[
    (data_expt_approach$manufacturing_emiss==1 | data_expt_approach$off_product_biogenic==1)&
      (data_expt_approach$live_biomass_C==0 & data_expt_approach$harv_residues==0) &
      (data_expt_approach$manufacturing_emiss+ data_expt_approach$maintenance_emiss +data_expt_approach$forestry_emiss+data_expt_approach$off_product_biogenic >=nminTechno) 
    ,
    "modelApproach"]<-"Technology approach"
  
  data_expt_approach[
    data_expt_approach$manufacturing_emiss==0 & data_expt_approach$off_product_biogenic==0&
      (data_expt_approach$live_biomass_C==1 | data_expt_approach$harv_residues==1)&
      ( data_expt_approach$biogenic_dyn +data_expt_approach$live_biomass_C +data_expt_approach$harv_residues + data_expt_approach$soilC + data_expt_approach$eol_biogenic + data_expt_approach$products_storage_C >=nminEcos)
    ,
    "modelApproach"]<-"Ecosystem approach"
  
  
  data_expt_approach[
    (data_expt_approach$manufacturing_emiss==1 | data_expt_approach$off_product_biogenic==1)&
      (data_expt_approach$live_biomass_C==1 | data_expt_approach$harv_residues==1)&
      ( data_expt_approach$biogenic_dyn +data_expt_approach$live_biomass_C +data_expt_approach$harv_residues + data_expt_approach$soilC + data_expt_approach$eol_biogenic + data_expt_approach$products_storage_C >=nminEcos)&
      (data_expt_approach$manufacturing_emiss+ data_expt_approach$maintenance_emiss +data_expt_approach$forestry_emiss+data_expt_approach$off_product_biogenic >=nminTechno) 
    ,"modelApproach"]<-"Whole sector approach"
  
  
  data_expt_approach$modelApproach<-factor(data_expt_approach$modelApproach,levels=c("Hybrid approach","Technology approach", "Ecosystem approach","Whole sector approach"))
  return(data_expt_approach)
}


findDuplicates<-function(data_expt,variable){
  
  test<-data_expt[!is.na(data_expt[,variable]),c("PaperID","time_horizon","singleProduct","soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")]
  test.sub<-data_expt[!is.na(data_expt[,variable]),c("PaperID","time_horizon","singleProduct","soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn",variable)]
  
  test$biogenic_dyn<-as.double(test$biogenic_dyn)
  test$fossil_dyn<-as.double(test$fossil_dyn)
  t.u<-data.frame(unique(test))
  t.u<-t.u[order(t.u$PaperID),]
  n<-unique(t.u[duplicated(t.u$PaperID) | duplicated(t.u$PaperID,fromLast=TRUE),'PaperID'])
  #Select only studies with several compartment setups
  t.u.d <- t.u[duplicated(t.u$PaperID)| duplicated(t.u$PaperID,fromLast=TRUE),]
  t.u.d$modelID<-with(t.u.d, ave(seq_along(PaperID),
                                 PaperID, FUN = seq_along))
  t.u.d.m<-merge(t.u.d,test.sub,all=TRUE)
  
  colnames(t.u.d)
  colnames(test.sub)
  return(t.u.d.m)
}

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- data.frame(t(rbind(m2-m1, se, t, 2*pt(-abs(t),df))    ))
  
  
  colnames(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}




#----------- Data modeling ----------- 


modelComponentsC<-function(data_expt, compartmentList, option, listCriteria){

  #xlistCriteria<-c("PaperID","singleProduct","time_horizon")
  formulaRHS<-paste0(paste(listCriteria, collapse="+"),"+get(compartment)")
  formulaShort<-""
  for(i in seq(length(listCriteria))){
    print("along listCriteria")
    print(listCriteria[i])
    formulaShort<-paste0(formulaShort,"get(listCriteria[",i,"])")
    if(i!=length(listCriteria)){
      formulaShort<-paste(formulaShort,"+")
    }
    if(i==length(listCriteria)){
      formulaShort<-paste0(formulaShort,"~get(compartment)")
    }
  }
  formulaShort<-as.formula(formulaShort)
  
  formula=as.formula(paste0("get(variable)~ ",formulaRHS,option))
  
  # WARNING : need to run it with only experiments done with whole system approach
  
  # ----Run calculations
  rm(list=c("tTestPairsSignifAggVar"))
  rm(tTestPairsSignifAggVarMelt,tTestPairsSignifAggVar)
  variable<-"substitution"
  for(variable in c("substitution")){
    # Select dat rows that have several model setups only changing one model parameter 
    t.u.d.m<-findDuplicates(data_expt,variable)
    
    # Figures for each process -> To supplementary information
    # for (compartment in compartmentList){
    #   boxplotCompartment(t.u.d.m,compartment,variable,listCriteria)
    # }
    # 
    rm(list=c("tTestPairs"))
    rm(tTestPairs)
    for (compartment in compartmentList){
      # For a given compartment model parameter, for each paper calculate mean value of substitution of each of the recorded values
      t.u.d.mMean<-setNames(aggregate(formula,data=t.u.d.m, mean),c(listCriteria,compartment,"substitution.mn"))
      
      # For a given compartment model parameter, for each paper calculate standard deviation value of substitution of each of the recorded values
      t.u.d.mSd<-setNames(aggregate(formula,data=t.u.d.m, sd),c(listCriteria,compartment,"substitution.sd"))
      t.u.d.mSd[is.na(t.u.d.mSd$substitution.sd),"substitution.sd"]<-0
      # For a given compartment model parameter, for each paper calculate number of recorded values of substitution 
      t.u.d.mN<-setNames(aggregate(formula,data=t.u.d.m, length),c(listCriteria,compartment,"substitution.N"))
      
      # For a given compartment model parameter, for each paper combine mean, std and number of recorded values of substitution 
      t.u.d.mMeanSdN<-merge(merge(t.u.d.mMean,t.u.d.mSd),t.u.d.mN)
      
      # For a given compartment model parameter, Reshape to have only one line for each paper
      data_expt_pairs<-data.table::dcast(setDT(t.u.d.mMeanSdN),
                                         formulaShort,
                                         value.var=c("substitution.mn","substitution.sd","substitution.N"))
      #Filter out papers that do not have mean values for both model parameter values
      data_expt_pairs<-data_expt_pairs[!is.na(data_expt_pairs$substitution.mn_0) & !is.na(data_expt_pairs$substitution.mn_1),]
      #!!!!!Matrix of Boxplot of the slopes
      #matrix is : rows Energy/Timber , columns model processes
      #for each subplot bootstrap (compare all pairs of same-paper)
      if( nrow(data_expt_pairs)>0){
        if(exists("tTestPairs")){
          print("trying to rbind")
          print(paste("tTestPairs  :",dim(tTestPairs)))
          print(paste("data_expt_pairs  :",dim(data_expt_pairs)))
          print(paste("t.test2", t.test2(data_expt_pairs$substitution.mn_0, data_expt_pairs$substitution.mn_1, data_expt_pairs$substitution.sd_0, data_expt_pairs$substitution.sd_1, data_expt_pairs$substitution.N_0,data_expt_pairs$substitution.N_1,m0=0)))
          
          tTestPairs<-rbind(tTestPairs,cbind(compartment,data_expt_pairs,t.test2(data_expt_pairs$substitution.mn_0, data_expt_pairs$substitution.mn_1, data_expt_pairs$substitution.sd_0, data_expt_pairs$substitution.sd_1, data_expt_pairs$substitution.N_0,data_expt_pairs$substitution.N_1,m0=0)))
          print(paste("tTestPairs:",dim(tTestPairs)))
          
        }else{
          tTestPairs<- cbind(compartment,data_expt_pairs,t.test2(data_expt_pairs$substitution.mn_0, data_expt_pairs$substitution.mn_1, data_expt_pairs$substitution.sd_0, data_expt_pairs$substitution.sd_1, data_expt_pairs$substitution.N_0,data_expt_pairs$substitution.N_1,m0=0))
          print(paste("tTestPairs:",dim(tTestPairs)))
          
        }
      }
    }
    
    tTestPairs$signif<-ifelse(tTestPairs$`p-value`<=0.05,"*","")
    tTestPairs[is.na(tTestPairs$signif),c("signif")]<-"1"
    tTestPairsSignif<-tTestPairs[(tTestPairs$signif=="*"),]
    #tTestPairsSignif<-tTestPairsSignif[order(singleProduct),]
    tTestPairsSignifAgg<-aggregate(tTestPairsSignif$`Difference of means`,
                                   by=list(tTestPairsSignif$compartment),
                                   function(x) mean(x))
    colnames(tTestPairsSignifAgg)<-c("process",paste("Difference of means for ",variable))
    tTestPairsSignifAgg<- tTestPairsSignifAgg[order(tTestPairsSignifAgg[,paste("Difference of means for ",variable)]),]
    rownames(tTestPairsSignifAgg)<-NULL
    
    #aggregate(data_modelShort[,],by=list(data_modelShort$singleProduct), function(x) length(which(x==1)))
    if(!exists("tTestPairsSignifAggVar")){
      tTestPairsSignifAggVar<-tTestPairsSignifAgg
      print(paste("tTestPairsSignifAggVar:",dim(tTestPairsSignifAggVar)))
    }else{
      tTestPairsSignifAggVar<-merge(tTestPairsSignifAggVar,tTestPairsSignifAgg,by="process",all=TRUE)
      print(paste("tTestPairsSignifAggVar:",dim(tTestPairsSignifAggVar)))
      
    }
  }
  
  #### Heatmap
  print("AggVar")
  print(dim(tTestPairsSignifAggVar))
  print(head(tTestPairsSignifAggVar))
  
  print(class(tTestPairsSignifAggVar))
  
  tTestPairsSignifAggVarMelt<-melt(as.data.frame(tTestPairsSignifAggVar),id="process")
  print("AggVarMelt")
  print(dim(tTestPairsSignifAggVarMelt))
  colnames(tTestPairsSignifAggVarMelt)<-c("process","variable","value")
  
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="live_biomass_C","process"]<-"Live biomass"
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="soilC","process"]<-"Soil carbon"
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="harv_residues","process"]<-"Harvest residues"
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="eol_biogenic","process"]<-"End-of-life emiss."
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="maintenance_emiss","process"]<-"Maintenance emiss."
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="manufacturing_emiss","process"]<-"Manufacturing emiss."
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="products_storage_C","process"]<-"C storage in products"
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="off_product_biogenic","process"]<-"Avoided emiss."
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="forestry_emiss","process"]<-"Forestry emiss."
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="biogenic_dyn","process"]<-"Dyn. of biogenic emiss."
  tTestPairsSignifAggVarMelt[tTestPairsSignifAggVarMelt$process=="fossil_dyn","process"]<-"Dyn. of fossil emiss."

    
    
  return(tTestPairsSignifAggVarMelt)
}

approachC<-function(data_expt_approach,filters){
  print("Entering function : approachC")
  # medianData <- aggregate(get(variable) ~  modelApproach, data_expt_approach, median)
  # colnames(medianData)<-c("modelApproach",variable)
  # medianData[,variable]<-round(medianData[,variable],2)
  
  if(missing(filters)){
    print('in global : loading forestPlotData results')
    forestPlotData<-read.csv("forestPlotData.approachC.v5.v6.v7.init.csv")
    dataMA<-read.csv("dataMA.approachC.v5.v6.v7.init.csv")
    
  }else{
    print('in global : calculating forestPlotData results')
    
    # dataMA<-data_expt_approach[data_expt_approach$modelApproach!="Hybrid approach",]
    # dataMA$sei<-0.01
    # dataMA$recordID<-rownames(dataMA)
    # dataMA$split<-dataMA$modelApproach
    # dataMA$split2<-dataMA$driver1Cat
    #  countStudySplit <- aggregate(substitution ~ split, aggregate(substitution~PaperID+split,dataMA,mean), length)
    # colnames(countStudySplit)<-c("split","substitution")
    # countStudySplit$nStud<-round(countStudySplit$substitution,2)
    # 
    # countRecordSplit <- aggregate(substitution ~  split, dataMA, length)
    # colnames(countRecordSplit)<-c("split","substitution")
    # countRecordSplit$nRec<-round(countRecordSplit$substitution,2)
    # listSplits<-countRecordSplit[,"split"]
    # print(which(is.na(dataMA)))
    
    dataMA<-plotDataFunc(data_expt_approach,filters=NULL, include_approaches=c("Whole sector approach","Ecosystem approach", "Technology approach"),outliers_out=NULL,split="modelApproach")#data_expt_approach,filters, include_approaches,outliers_out,split
    listSplits<-levels(dataMA$split)
   
    forestPlotData<-forestPlotDataFunc(dataMA)
    
    # mod.model <- rma.mv(yi = substitution, 
    #                     V = sei, 
    #                     slab = PaperID, 
    #                     data = dataMA,
    #                     random = ~ 1 | PaperID/recordID, 
    #                     test = "t", method = "REML",
    #                     mods = ~ modelApproach-1)
    # forestPlotData<-coef(summary((mod.model)))
    # 
    # forestPlotData$split<-substring(rownames(forestPlotData),14)
    # forestPlotData<-forestPlotData[forestPlotData$split %in% listSplits ,]
    # forestPlotData<-merge(forestPlotData,countRecordSplit[,c("nRec","split")],by=c("split"),all.x=TRUE)
    # forestPlotData<-merge(forestPlotData,countStudySplit[,c("nStud","split")],by=c("split"),all.x=TRUE)
    # forestPlotData$signif<-""
    # forestPlotData[forestPlotData$pval<=0.1,"signif"]<-",."
    # forestPlotData[forestPlotData$pval<=0.05,"signif"]<-",*"
    # forestPlotData[forestPlotData$pval<=0.01,"signif"]<-",**"
    # forestPlotData[forestPlotData$pval<=0.001,"signif"]<-",***"
    # forestPlotData$color<-"grey"
    # forestPlotData[forestPlotData$pval<=0.1,"color"]<-"black"
    # forestPlotData[forestPlotData$pval<=0.05,"color"]<-"black"
    # forestPlotData[forestPlotData$pval<=0.01,"color"]<-"black"
    # forestPlotData[forestPlotData$pval<=0.001,"color"]<-"black"
    # 
    # forestPlotData$substitution<-forestPlotData$estimate
    # 
    # forestPlotData$split<-factor(forestPlotData$split)
  }
}

#----------- Data plotting ----------- 



plotBarplotYear<-function(data_bibliom){
  print("Entering function : plotBarplotYear")
  ggplot(data_bibliom,aes(x=Publication_Year))+
    geom_histogram(stat='count')+  
    theme_bw()+
    # scale_y_continuous(breaks=seq(0,20,by=2))+
    theme(text = element_text(size=txt_size),
          axis.text.x = element_text(angle=txt_angle, hjust=1))
  
}

plotCountryData<-function(countryData, sortingCriteria){
  print("Entering function : plotCountryData")
  countryData<-unique(countryData[order(-countryData[,sortingCriteria]), ])
  countryData$country<-factor(countryData$country,levels=as.vector(countryData[order(countryData[,sortingCriteria]), 'country']))
  
  print("countryData:")
  
  print(head(countryData) )
  
  countryDataSubset<-rbind(countryData[1:10,],countryData[!is.na(countryData$PaperID),])
  print("countryDataSubset:")
  print(head(countryDataSubset))
  
  print("plotting left side")
  
  ggplot(countryDataSubset,aes(fill=PaperID,y=country,x=get(sortingCriteria),label=country))+
    geom_bar(stat='identity',position='dodge',colour="gray",size=0.05)+
    #geom_col(aes(x=-get(sortingCriteria)),stat='identity',position='dodge')+
    scale_fill_viridis(na.value="white")+
    # scale_fill_viridis(na.value="white")+
    scale_y_discrete(position="left")+
    labs(fill="Number of studies")+
    xlab(sortingCriteria)+
    theme_bw()+
    # scale_x_reverse()+
    theme( axis.ticks = element_blank(),text = element_text(size=14))  
  
}



create_processes_frequency <- function(study_freq, wrap){
  
  if(missing(wrap)){
    plotData<-study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),c("names","variable","value","cat2","colcat2")] 
    plotData<-aggregate(plotData$value ,by=list(plotData$names,plotData$cat2,plotData$colcat2),FUN=sum)
    colnames(plotData)<-c("names","cat2","colcat2","value")
    colorVect<-unique(plotData[,c("colcat2","cat2")])
    colordictProcesses<-setNames(as.character(colorVect$colcat2), 
                                 as.character(colorVect$cat2))
    plotData$wrap<-"All"
    
  }else{
    plotData<-study_freq[(study_freq$cat1 =="Processes") & !is.na(study_freq$cat1),c("names","variable","value","cat2","colcat2")] 
    
    colorVect<-unique(plotData[,c("colcat2","cat2")])
    colordictProcesses<-setNames(as.character(colorVect$colcat2), 
                                 as.character(colorVect$cat2))    
    plotData$wrap<-plotData$variable
    #Debug info
    print("found(wrap)")
    print(head(plotData))
    ##   
  }
  plotData$wrap<-factor(plotData$wrap,levels=sort(levels(factor(plotData$wrap))))
  ggplot(plotData,aes(x=names,y=value,fill=cat2))+ 
    coord_flip() + 
    geom_bar(stat="identity") +  
    #  scale_fill_manual(values=colorsLabels$colcat2,labels=waiver())+   
    scale_fill_manual(values=colordictProcesses)+   
    theme_bw()+ 
    theme( axis.ticks = element_blank(),text = element_text(size=txt_size_big),axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.))+ 
    ylab("Number of studies")+ 
    xlab("Forest sector description")+
    geom_text(aes(label = value, text = paste(names, value)), alpha = 0, hoverinfo = "text", show.legend = FALSE)+
    facet_wrap(~wrap)
  # 
  # return(gg)
  #ggsave("study_model_freq.pdf",width=7,height=5) 
}


# 
create_processes_versus_flux_size<-function(study_freq,palette, wrap){
  refCProcess<-GlobalFluxData(dataFlux.file)
  refCProcessMean<-aggregate(`value GtCO2/yr`~substitutionDatabaseVariable,data=refCProcess,FUN=mean,na.rm=T)
  refCProcessMean$`value GtCO2/yr`<-as.numeric(refCProcessMean$`value GtCO2/yr`)
  
  my.cols <- c(brewer.pal(5, palette),"#000000")
  print("1")
  #  plotData<-study_freq[(study_freq$cat1 =="Processes") & study_freq$cat2=="C fluxes"& study_freq$names!="eol_fossil_emiss"& !is.na(study_freq$cat1),c("names","variable","value","cat2","colcat2")]
  plotData<-study_freq[(study_freq$cat2=="C fluxes"),c("names","variable","value","valuePercent","cat2","colcat2","nSingleProduct")]
  if(missing(wrap)){
    print("missing wrap:")
    plotDataAgg<-aggregate(cbind(value,nSingleProduct)~names+cat2,data=plotData,sum)
    plotDataAgg$valuePercent<-plotDataAgg$value/plotDataAgg$nSingleProduct*100
    plotDataAgg$variable<-"All"
    plotDataAgg$wrap<-"All"
    plotData<-plotDataAgg
    print(paste("plotData$wrap:",plotData$wrap))
    
  }else{
    print(paste("found wrap:",wrap))
    print(plotData$variable)
    print(sort(levels(plotData[,"variable"])))
    
    plotData$wrap<-factor(plotData$variable,levels=sort(levels(plotData[,"variable"])))
    print(paste("plotData$wrap:",plotData$wrap))
    
  }
  print("out of if")
  print(paste("plotData$wrap:",plotData$wrap))
  
  #plotData<-rbind(plotData[,c('names','variable','valuePercent','value','nSingleProduct','cat2')],plotDataAgg[,c('names','variable','valuePercent','value','nSingleProduct','cat2')])
  maxValue<-max(plotData$value)+5
  maxValuePercent<-max(plotData$valuePercent)+5  
  
  plotDataRefCProcess<-merge(plotData,refCProcessMean,by.x="names",by.y="substitutionDatabaseVariable")
  print(paste("plotDataRefCProcess$wrap:",plotDataRefCProcess$wrap))
  
  if(dim(plotDataRefCProcess)[1]>0){
    ggplot(plotDataRefCProcess,aes(x=(`value GtCO2/yr`),y=(valuePercent)))+#,shape=names))+
      geom_point(aes(size=value))+
      geom_smooth(aes(group = variable)  ,method="glm",se=FALSE,show.legend = FALSE,col="black")+      #
      #scale_color_manual(name="Wood type",values = c("EnergyInput" = my.cols[1], "PulpPaperInput" = my.cols[2],"TimberInput" = my.cols[3],"mixedProduct" = my.cols[4],"UpstreamInput" = my.cols[5],"All"=my.cols[6]))+
      labs(size="Number of studies",col="Processes")+
      theme_bw()+
      #theme( text = element_text(size=12))+
      geom_vline(data=plotDataRefCProcess, mapping=aes(xintercept=`value GtCO2/yr`,col=names), linetype="longdash") +
      #geom_text(data=plotDataRefCProcess, mapping=aes(x=`value GtCO2/yr`, y=0.7, label=names), angle=90, vjust=-0.4, hjust=0,color='black',check_overlap = TRUE) +
      xlab("Global flux (GtCO2)")+
      ylab("Fraction of studies accounting for this process (%)")+
      facet_wrap(~wrap,ncol=3, labeller = as_labeller(wood_type_names))
  }
  
}



create_driver_frequency <- function(expt_freq,wrap){
  
  if(missing(wrap)){
    
    plotData<-expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),
                        c("names","variable","value","cat2","colcat2")] 
    plotData<-aggregate(plotData$value ,by=list(plotData$names,plotData$cat2,plotData$colcat2),FUN=sum)
    colnames(plotData)<-c("names","cat2","colcat2","value")
    colorVectDrivers<-unique(plotData[,c("colcat2","cat2")])
    colordictDrivers<-setNames(as.character(colorVectDrivers$colcat2), 
                               as.character(colorVectDrivers$cat2)) 
    
    plotData$wrap<-"All"
  }else{
    plotData<-expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),
                        c("names","variable","value","cat2","colcat2")] 
    
    colorVectDrivers<-unique(plotData[,c("colcat2","cat2")])
    colordictDrivers<-setNames(as.character(colorVectDrivers$colcat2), 
                               as.character(colorVectDrivers$cat2)) 
    
    plotData$wrap<-plotData$variable
    #Debug info
    print("found(wrap)")
    print(head(plotData))
    ##   
  }
  plotData$wrap<-factor(plotData$wrap,levels=sort(levels(factor(plotData$wrap))))
  
  ggplot(plotData,aes(x=names,y=value,fill=cat2))+ 
    coord_flip() + 
    geom_bar(stat="identity") +  
    scale_fill_manual(values=colordictDrivers)+   
    theme_bw()+ 
    theme( axis.ticks = element_blank(),text = element_text(size=txt_size_big),axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.))+ 
    ylab("Number of experiments")+ 
    xlab("Forest sector description") +
    facet_wrap(~wrap)
  #ggsave("expt_model_freq.pdf",width=7,height=5) 
}



create_dendrogram<-function(data_expt){
  print("Entering function : create_dendrogram")
  
  #dataCor<-data_expt[data_expt$Exclusion=="included",c('scaleAgg','singleProduct','time_horizon', 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic', 'biogenic_dyn')]
  #Add off_product_biogenic once it is filled for all studies
  dataCor<-data_expt[data_expt$Exclusion=="included",c('scaleAgg','singleProduct','time_horizon', 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic','off_product_biogenic', 'biogenic_dyn')]
  
  dataCor[is.na(dataCor)]<-0
  dataMat<-model.matrix(~0+., data=dataCor)
  p.matgg <- cor.mtest(dataMat)
  p <- cor.test.p(dataMat)
  
  dataPlotCorgg<-cor(dataMat,use="pairwise.complete.obs")
  
  excludeCor<-names(rowSums(is.na(dataPlotCorgg))[rowSums(is.na(dataPlotCorgg))==dim(dataPlotCorgg)[1]])
  dataPlotCorgg<-dataPlotCorgg[!(rownames(dataPlotCorgg) %in% excludeCor) ,][,!(colnames(dataPlotCorgg) %in% excludeCor)]
  
  rowCluster = hclust(dist(dataPlotCorgg))
  rowCluster$labels<-c("Local scale","Regional scale","Global scale","Timber product","Energy product","Mixed product","Pulp and paper product","0 yrs time horizon","1-30 yrs time horizon", "31-70 yrs time horizon","71-100 yrs time horizon", "Soil carbon","Harvest residues","Live biomass", "C storage in products","Forestry emiss.","Manufacturing emiss.","Maintenance emiss.","End-of-life emiss.", "Dyn. of biogenic emiss.", "Avoided emiss.","Dyn. of fossil emiss.")
  
  dendrogram<-
    fviz_dend(rowCluster,
              k=2 ,           # Cut in x groups
              cex = 0.9,                 # label size
             # rect = TRUE,
              k_colors = c("#2E9FDF", "#FC4E07"),# color labels by groups
              labels_track_height=0.5,
              horiz = TRUE,  # color labels by groups
              ggtheme = theme_bw()     # Change theme
              
    )
  return(dendrogram)
}#end function


create_forest_plot<-function(plotData,forestPlotData,wrapSplit2){
  p<-ggplot(plotData,aes(x=split,y=substitution))+
    geom_boxplot(data=plotData,aes(y=substitution ,x=reorder(split,substitution,mean,na.rm=TRUE)),outliers = FALSE,outlier.color=NULL,fatten = NULL,size=2,fill="lightgrey",color="lightgrey")+
    stat_summary(fun=median, color="darkgrey", geom="point",  shape=15, size=3, show.legend=FALSE) +
    
    geom_point( data=forestPlotData) +
    geom_errorbar( data=forestPlotData,aes(ymin = ci.lb, ymax = ci.ub),size=0.5,width=0.5) + #add CIs as error bars
    theme_bw()+ 
    theme( 
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      text = element_text(size=16),
      axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0)
      
      #       axis.ticks = element_blank(),text = element_text(size=12),
      #        axis.text.x = element_text(size=12,hjust=0.,vjust=0.5),
      #        axis.text.y = element_text(size=12,hjust=0.,vjust=0.5)
    )+
    labs(x="Carbon balance (tCO2/m3")+
    geom_text(data=forestPlotData,
              aes(label=paste0(round(substitution,2)," ( ",nStud,"|",nRec,signif," )" )),
              y=2.1,
              size=6,
              nudge_x=0.2) +
    # stat_n_text(
    #   y.pos=-2.9,
    #   size=6)+
    
    geom_hline(yintercept=0)+
    ylab("Carbon balance")+
    xlab("")+
    coord_flip(y = c(-6,6),clip="off")
  
  if(wrapSplit2){
    p<-p+facet_grid(driver1Cat~.,
                    switch="y",
                    labeller = labeller( driver1Cat = label_wrap_gen(width = 10),
                                         .multi_line = TRUE),
                    scales="free_y",
                    space="free_y",)
  }
  
  print(p) 
  
  
}

plotModelComponentsC<-function(tTestPairsSignifAggVarMelt){
  print("Entering function : plotModelComponentsC")

  p<-ggplot(  tTestPairsSignifAggVarMelt,aes(x=process,y=variable,fill=value))+
    geom_tile()+
    theme_bw()+
    scale_fill_gradient2()+
    # scale_fill_gradientn(
    #   colours = c(muted("orange"), "white", muted("green")),
    #   na.value = "grey98",
    #   name = "ratio",
    #   limits = c(-1,1 )
    # )+
    theme( axis.ticks = element_blank(),
           text = element_text(size=txt_size_big),
           axis.text.x = element_text(angle = 45,hjust=1,vjust=1)
    )
  print(p)
  
  #kable(dcast(tTestPairsSignifAggVarMelt,process~variable))
}

plotDriverC<-function(data_expt_approach,filters){
  print("Entering function : plotDriverC")
  
  # 
  # if(missing(filters)){
  #   print('in global driverC : loading model results')
  #   forestPlotData<-read.csv("forestPlotData.driverC.v5.v6.v7.init.csv")
  #   dataMA<-read.csv("plotData.driverC.v5.v6.v7.init.csv")
  #   print(head(forestPlotData))
  #   print(head(plotData))
  #   
  # }else{
  #   print('in global driverC : calculating model results')
  #   plotData<-plotDataFunc(data_expt_approach,filters, "Whole sector approach",NULL,"driver1")
  #   forestPlotData<-forestPlotDataFunc(plotDataFunc,filters)
  # }
  # # print("out of missing filters")
  # print("forestPlotData")
  # print(class(forestPlotData))
  # print(forestPlotData$split)
  # print(forestPlotData$substitution)
  # print(class(plotData))
  # 
  # forest_plot<-create_forest_plot(dataMA,forestPlotData,wrapSplit2=TRUE)
  # print(forest_plot)
 
}


plotDataFunc<-function(data_expt_approach, include_approaches,outliers_out,splitName){
  print("Entering function : plotDataFunc")
  
  print(splitName)
  plotData <-data.frame(data_expt_approach[data_expt_approach$modelApproach %in% include_approaches & !(data_expt_approach$driver1Cat)%in%c("Demand" ,"Environmental change" ),])
  plotData $sei<-0.001
  plotData$singleProduct<-factor(plotData$singleProduct)
  
  if(missing(outliers_out)){
    plotData<-plotData[!is.na(plotData $substitution)& plotData $driver1!="Temperature" ,]
    
    
  }else{
    outlierdist<-3*sd(plotData $substitution,na.rm=T) 
    outliermin<-mean(plotData $substitution,na.rm=T)-outlierdist
    outliermax<-mean(plotData $substitution,na.rm=T)+outlierdist
    plotData<-plotData[!is.na(plotData $substitution)& plotData $driver1!="Temperature" 
                       & plotData $substitution<outliermax
                       & plotData $substitution>outliermin,]
    outlierData<-plotData[!is.na(plotData $substitution)& plotData $driver1!="Temperature" 
                          &(plotData $substitution>=outliermax
                            | plotData $substitution<=outliermin),]
  }
  
  plotData$split<-as.character(plotData[,splitName])
  if(splitName=="driver1"){
    plotData[plotData$driver1=="technologies/design switch", "split"] <- as.character(plotData[ plotData$driver1=="technologies/design switch", "singleProduct"])
    plotData$split<-factor(plotData$split,
                           levels=c("area subject to harvest", "cutting intensity", "environmental driver","fertilisation","multiple supply","multiple silviculture","mixed drivers","plantation density","recycling" ,"rotation length" , "site fertility", "species" ,"supply chain organization","technologies/design switch" ,"unspecified harvest increase","EnergyInput","TimberInput","PulpPaperInput","mixedProduct","efficiency improvement","harvesting system", "location of industry", "end of life disposal","products lifespan", "Temperature"),
                           labels=c("Increased area subject to harvest", "Increased cutting intensity", "Environmental driver","Increased fertilisation","Multiple driver","Multiple driver","Multiple driver","Increased plantation density","Increased recycling" ,"Decreased rotation length" , "Increased site fertility", "Shifting to more productive species" ,"Better organizing supply chain","Shifting technology" ,"Unspecified harvest increase", "Energy","Timber","Pulp and paper","Mixed products","Efficiency improvement","harvesting system", "location of industry", "end of life disposal","products lifespan", "Temperature"),
    )
    
    
  }
  if(splitName=="driver1Cat"){
    plotData[plotData$driver1=="technologies/design switch", "split"] <- as.character(plotData[ plotData$driver1=="technologies/design switch", "singleProduct"])
    plotData$split<-factor(plotData$split,
                           levels=c("SilvicultureRemov","SilvicultureProd","Supply chain", "Technology","Multiple strategies"),
                           labels=c("Mobilize additional wood by increased removals","Mobilize additional wood by increased productivity", "Make better use of wood",  "Use wood instead of other ressource","Multiple strategies")
                           )  
    
    
    
  }
  if(splitName=="modelApproach"){
    plotData$split<-factor(plotData$split)
  }
  
  plotData$driver1Cat<-factor(plotData$driver1Cat,
                              levels=c("SilvicultureRemov","SilvicultureProd","Supply chain", "Technology","Multiple strategies"),
                              labels=c("Mobilize additional wood by increased removals","Mobilize additional wood by increased productivity", "Make better use of wood",  "Use wood instead of other ressource","Multiple strategies")
  )  
  plotData$split2<-plotData$driver1Cat
  
  print(data.frame(plotData)$split)
  return(data.frame(plotData))
  
}




forestPlotDataFunc<-function(plotData,split,includeSplit2){
  print("Entering function : forestPlotDataFunc")
  
  plotData$recordID<-rownames(plotData)
  # plotData$split<-factor(forestPlotData$split,
  #                                levels=c("area subject to harvest", "cutting intensity", "environmental driver","fertilisation","multiple silviculture","multiple supply","mixed drivers","plantation density","recycling" ,"rotation length" , "site fertility", "species" ,"supply chain organization","technologies/design switch" ,"unspecified harvest increase","EnergyInput","TimberInput","PulpPaperInput","mixedProduct"),
  #                                labels=c("Increased area subject to harvest", "Increased cutting intensity", "Environmental driver","Increased fertilisation","Multiple driver","Multiple driver","Multiple driver","Increased plantation density","Increased recycling" ,"Decreased rotation length" , "Increased site fertility", "Shifting to more productive species" ,"Better organizing supply chain","Shifting technology" ,"Unspecified harvest increase", "Energy","Timber","Pulp and paper","Mixed products"),
  #   )
  #  plotData<-plotData[plotData$split %in% listSplits ,]
  mod.model <- rma.mv(yi = substitution, 
                      V = sei, 
                      slab = PaperID, 
                      data = plotData,
                      random = ~ 1 | PaperID/recordID, 
                      test = "t", 
                      method = "REML",
                      mods = ~ split-1)
  
  
  forestPlotData<-coef(summary((mod.model)))
  forestPlotData$split<-substring(rownames(forestPlotData),6)
  print(paste0("forestPlotData$split",forestPlotData$split))
  
  if(includeSplit2){
    
    countStudySplit <- aggregate(substitution ~ split+split2, aggregate(substitution~PaperID+split+split2,plotData,mean), length)
    colnames(countStudySplit)<-c("split","driver1Cat","nStud")
    countRecordSplit <- aggregate(substitution ~  modelApproach+split+split2, plotData, length)
    colnames(countRecordSplit)<-c("modelApproach","split","driver1Cat","nRec")
    forestPlotData<-merge(forestPlotData,unique(plotData[,c("split","driver1Cat")]),by="split",all.x=T)
    forestPlotData<-forestPlotData[forestPlotData$split %in% levels(plotData$split) &
                                     !(forestPlotData$driver1Cat)%in%c("Demand" ,"Environmental change" ),]
    forestPlotData<-merge(forestPlotData,countRecordSplit[,c("nRec","split","driver1Cat")],by=c("split","driver1Cat"),all.x=TRUE)
    forestPlotData<-merge(forestPlotData,countStudySplit[,c("nStud","split","driver1Cat")],by=c("split","driver1Cat"),all.x=TRUE)
    forestPlotData$split2<-factor(forestPlotData$driver1Cat,
                                  levels=c("SilvicultureRemov","SilvicultureProd","Supply chain", "Technology","Multiple strategies"),
                                  labels=c("Mobilize additional wood by increased removals","Mobilize additional wood by increased productivity", "Make better use of wood",  "Use wood instead of other ressource","Multiple strategies")
    )  

    
  }else{
    countStudySplit <- aggregate(substitution ~ split, aggregate(substitution~PaperID+split,plotData,mean), length)
    colnames(countStudySplit)<-c("split","nStud")
    print(paste0("countStudySplit",countStudySplit))
    countRecordSplit <- aggregate(substitution ~  modelApproach+split, plotData, length)
    colnames(countRecordSplit)<-c("modelApproach","split","nRec")
    print(paste0("countStudySplit",countRecordSplit))
    print("----- going for the merge")
    
    print(forestPlotData)
    
    #forestPlotData<-merge(forestPlotData,unique(plotData[,c("split")]),by="split",all.x=T)
    forestPlotData<-forestPlotData[forestPlotData$split %in% levels(factor(plotData$split)) ,]
    forestPlotData<-merge(forestPlotData,countRecordSplit[,c("nRec","split")],by=c("split"),all.x=TRUE)
    forestPlotData<-merge(forestPlotData,countStudySplit[,c("nStud","split")],by=c("split"),all.x=TRUE)
    print(paste0("forestPlotData",forestPlotData))
    
  }
  
  # countStudySplit$nStud<-round(countStudySplit$substitution,2)
  #  countRecordSplit$nRec<-round(countRecordSplit$substitution,2)
  listSplits<-countRecordSplit[,"split"]
  print(paste0("listSplits",listSplits))
  
  forestPlotData$signif<-""
  forestPlotData[forestPlotData$pval<=0.1,"signif"]<-",."
  forestPlotData[forestPlotData$pval<=0.05,"signif"]<-",*"
  forestPlotData[forestPlotData$pval<=0.01,"signif"]<-",**"
  forestPlotData[forestPlotData$pval<=0.001,"signif"]<-",***"
  forestPlotData$color<-"grey"
  forestPlotData[forestPlotData$pval<=0.1,"color"]<-"black"
  forestPlotData[forestPlotData$pval<=0.05,"color"]<-"black"
  forestPlotData[forestPlotData$pval<=0.01,"color"]<-"black"
  forestPlotData[forestPlotData$pval<=0.001,"color"]<-"black"
  
  forestPlotData$substitution<-forestPlotData$estimate
  print(paste0("split",split))
  if(split=="driver1"){
    
    
  }
  
  
  
  print(forestPlotData$split)
  
  return(forestPlotData)
  
}

debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}