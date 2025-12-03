
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


create_C_synthesis_plot<-function(refCProcess){

  refCProcessMean<-aggregate(`value GtCO2/yr`~substitutionDatabaseVariable,data=refCProcess,FUN=mean,na.rm=T)
  refCProcessMean$`value GtCO2/yr`<-as.numeric(refCProcessMean$`value GtCO2/yr`)
  refCProcessSd<-aggregate(`value GtCO2/yr`~substitutionDatabaseVariable,data=refCProcess,FUN=sd,na.rm=T)


  # Calculate the sum of all "emission" processes to compare to sequestration estimate
  TotalEmissionsMean<-round(refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="harv_residues","value GtCO2/yr"]+
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_energy","value GtCO2/yr"]+
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_disposal","value GtCO2/yr"]+
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="forestry_emiss","value GtCO2/yr"]+
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="manufacturing_emiss","value GtCO2/yr"]+                             #  refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="maintenance_emiss","value GtCO2/yr"]+
                              refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="soilC","value GtCO2/yr"],2)

  TotalEmissionsSd<-  round(sqrt(refCProcessMean[refCProcessSd$substitutionDatabaseVariable=="harv_residues","value GtCO2/yr"]^2+
                                   refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_energy","value GtCO2/yr"]^2+
                                   refCProcessMean[refCProcessMean$substitutionDatabaseVariable=="eol_biogenic_disposal","value GtCO2/yr"]^2+
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

  refCProcess<-rbind(c("Summed from data synthesis","Balance","Balance","3","biogenic", "Extraction","total_sector_emission","sum of biogenic and fossil, in-situ and ex-situ emissions"," "," "," "," "," "," "," ",(TotalEmissionsMean-TotalEmissionsSd)), refCProcess)
  refCProcess<-rbind(c("Summed from data synthesis","Balance","Balance","3","biogenic", "Extraction","total_sector_emission","sum of biogenic and fossil, in-situ and ex-situ emissions"," "," "," "," "," "," "," ",(TotalEmissionsMean)), refCProcess)

  refCProcess<-rbind(c("Summed from data synthesis","Balance","Balance","3","biogenic", "Extraction","total_sector_emission","sum of biogenic and fossil, in-situ and ex-situ emissions"," "," "," "," "," "," "," ",(TotalEmissionsMean+TotalEmissionsSd)), refCProcess)

  refCProcess<-rbind(c("Summed from data synthesis","Balance","Balance","2","biogenic", "Forest growth","total_sector_sequestration","sum of C sequestered out of atmosphere"," "," "," "," "," "," "," ",(TotalSequestrationMean-TotalSequestrationSd)), refCProcess)
  refCProcess<-rbind(c("Summed from data synthesis","Balance","Balance","2","biogenic", "Forest growth","total_sector_sequestration","sum of C sequestered out of atmosphere"," "," "," "," "," "," "," ",(TotalSequestrationMean)), refCProcess)
  refCProcess<-rbind(c("Summed from data synthesis","Balance","Balance","2","biogenic", "Forest growth","total_sector_sequestration","sum of C sequestered out of atmosphere"," "," "," "," "," "," "," ",(TotalSequestrationMean+TotalSequestrationSd)), refCProcess)


  refCProcess$`value GtCO2/yr`<-as.numeric(refCProcess$`value GtCO2/yr`)


  refCProcess$substitutionDatabaseVariable<-
    factor(refCProcess$substitutionDatabaseVariable,
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
  refCProcess$FluxDirection<-factor(refCProcess$FluxDirection,levels=c("Sequestration","Emission","Balance"),labels=c("Sequestration","Emission ","Balance"))
  p<-ggplot(refCProcess,aes(x=reorder(substitutionDatabaseVariable,`value GtCO2/yr`,na.rm=TRUE),y=`value GtCO2/yr`,col=Compartment))+
    geom_boxplot() +
    scale_fill_manual(values=c("white","grey"))+
    scale_color_manual(values=c(color_biogenic,color_fossil,color_sum))+
    stat_summary(fun=mean, geom="point", shape=5, size=2, color="black") +
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
          strip.background = element_rect(color="black", fill="white", size=0.5, linetype="solid" ),
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
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


# For dendrogram only the maximum mode config should be looked at instead of all model replicates resulting from slicing during extraction.
study<-function(data){
  # Study data are unique sets of study framework parameters
  #data_study<-unique(data[!is.na(data$substitution) &
  #                          data$Exclusion=='included',(colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c("Metadata", "Model"),'names'])])
  
  data_study<-merge(
    aggregate(. ~ PaperID, data = data[!is.na(data$substitution) & data$Exclusion=='included',
                                       colnames(data) %in% c("PaperID",categoriesdf[categoriesdf$cat1 %in% c("Processes"),'names'])], FUN = function(x) as.integer(any(as.numeric(x) != 0))),
    data[!is.na(data$substitution) & data$Exclusion=='included',
         (colnames(data) %in% categoriesdf[categoriesdf$cat1 %in% c( "DataCorrection")&categoriesdf$cat0 %in% c( "Model"),'names'] |
            colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c("Metadata"),'names'])]
)
  # Each study data item is assigned a study code
  data_study$study_code<-apply( data_study[,(colnames(data_study) %in% c("DOI",categoriesdf[categoriesdf$cat0 %in% c("Metadata", "Model"),'names']))] , 1 , paste , collapse = "" )
  data_study<-data_study[!duplicated(data_study$study_code),]
  return(data_study)
}


# ---------------------------------------------------------------------------
# ----------------Prepare Experiment-level data -> output = data_expt
bibliom<-function(data){
  
  dataMd<-data[,
               (colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c('Metadata'),'names'])  &
                 ! colnames(data)%in%c("StudyID","ExperimentID","Nstudy","Nexperiment","Reviewer" ,"Reviewer2","Extraction round","ReviewerQCv7") , ]#data with paper metadata only
  data_bibliom<-unique(setDT(dataMd)[,list(count=.N),names(dataMd)])  
  return(data_bibliom)
}

bibliom_in<-function(data){
  dataMd<-data[data$Exclusion=='included' &
                 !is.na(data$substitution),
               (colnames(data) %in% categoriesdf[categoriesdf$cat0 %in% c('Metadata'),'names'])  &
               ! colnames(data)%in%c("StudyID","ExperimentID","Nstudy","Nexperiment","Reviewer","Reviewer2","Extraction round","ReviewerQCv7" )  ]#data with paper metadata only
  
  data_bibliom<-unique(setDT(dataMd)[,list(count=.N),names(dataMd)])  #
  return(data_bibliom)
}
expt<-function(data){
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

countryFreq<-function(data_study,countryData){

  #Count number of study items per country
  data_study[(data_study$country=="USA"|data_study$country=="Usa"),"country"]<-str_to_title("United States Of America")
  nStudyCountry<-aggregate(PaperID~country,data=unique(data_study[,c("PaperID","country")]),FUN=length)
  colnames(nStudyCountry)<-c("country","nPaperID")
  countryFreqData<-merge(nStudyCountry,countryData,by="country",all.y=TRUE)
  return(countryFreqData)
}

readCountryData<-function(rawDataPath){
  refYear<-2015

  countryCodes<-read.csv(paste0(rawDataPath,"/countryCodes/countryCodes.csv"))
  countryCodes$country<-str_to_title(countryCodes$country)

  #Load Bais data
  BaisFiga<-read_xlsx(paste0(rawDataPath,"/Bais/Bais2015Data/BaisExtract.xlsx"),sheet=1)
  BaisFigc<-read_xlsx(paste0(rawDataPath,"/Bais/Bais2015Data/BaisExtract.xlsx"),sheet=3)
  BaisData<-merge(BaisFiga[,c('iso_a2','levelFig3a','NAME')],BaisFigc[,c('iso_a2','levelFig3c')])
  BaisData<-merge(BaisData,countryCodes,by.y='Alpha.2.code',by.x='iso_a2',all=T)
  BaisData<-BaisData[,c("levelFig3a","levelFig3c","Alpha.3.code")]

  # Load FAO Roundwod data
  faoData<-read.csv(paste0(rawDataPath,"/FAOWoodData/FAOSTAT_data_en_6-18-2025.csv"))

  faoData[faoData$Area=="United Kingdom of Great Britain and Northern Ireland","Area"]<-"United Kingdom"
  faoData$Area<-str_to_title(faoData$Area)
  faoData<-faoData[faoData$Year==refYear,]

  faoData<-merge(faoData[,c("Area","Item","Year.Code","Unit","Value")],countryCodes,by.x="Area",by.y="country",all=T)
  colnames(faoData)[colnames(faoData)=="Value"]<-"Roundwood (m3)"
  faoData<-faoData[order(-faoData$Roundwood),]
  faoData<-faoData[,c("Alpha.3.code","Roundwood (m3)")]

  # Load FAO ForestAreaPercentLand data
  faoForestData<-read_xlsx(paste0(rawDataPath,"/FAOForestAreaPercentLand/Forest area as a percent of land area.xlsx"))

  faoForestData[faoForestData$country=="United Kingdom of Great Britain and Northern Ireland","country"]<-"United Kingdom"
  faoForestData$country<-str_to_title(faoForestData$country)
  faoForestData<-faoForestData[faoForestData$Year==refYear,]

  faoForestData<-merge(faoForestData[,c("country","Year","Forest area ratio (%)")],countryCodes,by.x="country",by.y="country")
  faoForestData<-faoForestData[,c("Alpha.3.code","Forest area ratio (%)")]

  # Load FAO ForestArea data
  faoForestAData<-read.csv(paste0(rawDataPath,"/FAOForestArea/Forest area.csv"))

  faoForestAData[faoForestAData$country=="United Kingdom of Great Britain and Northern Ireland","country"]<-"United Kingdom"
  faoForestAData$country<-str_to_title(faoForestAData$country)
  faoForestAData<-faoForestAData[faoForestAData$Year==refYear,]
  faoForestAData<-merge(faoForestAData[,c("country","Year","Forest.area..1000.ha.")],countryCodes,by.x="country",by.y="country")
  faoForestAData<-faoForestAData[,c("Alpha.3.code","Forest.area..1000.ha.")]

  #Load OECD GDP spending for research
  gdpRD<-read.csv(paste0(rawDataPath,"/UnescoData/SCN_DS_14122023043909123.csv"))
  gdpRD<-gdpRD[gdpRD$TIME==as.character(refYear),c("LOCATION","TIME","Value")]
  colnames(gdpRD)<-c("Alpha.3.code","TIME","GDP_RD")
  gdpRD<-merge(gdpRD,countryCodes,by="Alpha.3.code",all=T)
  gdpRD<-gdpRD[,c("Alpha.3.code","GDP_RD")]

  #Merge datasets
  countryData<-merge(BaisData,gdpRD,by='Alpha.3.code',all=TRUE)
  countryData<-merge(countryData,faoForestData ,by="Alpha.3.code",all=TRUE)
  countryData<-merge(countryData,faoForestAData ,by="Alpha.3.code",all=TRUE)
  countryData<-merge(countryData,faoData,by="Alpha.3.code",all=TRUE)
  countryData<-merge(countryData,countryCodes,by="Alpha.3.code",all=TRUE)


  countryData<-countryData[!is.na(countryData$`Forest area ratio (%)`),]
  countryData<-countryData[countryData$Alpha.3.code %in% faoForestAData$Alpha.3.code,]

  return(countryData)
}



df<-data_expt
funcFreq<-function(df,categoriesdf){
  dfShort<-data.frame(subset(df,select=-c(Exclusion,DOI)))
  dfNames<-colnames(df)[!(colnames(df))%in% c("Exclusion","DOI")]
  colnames(dfShort)<-dfNames

  # nMixedProduct<-dim(dfShort[dfShort$singleProduct=="mixedProduct","singleProduct"])[1]
  # nEnergyInput<-dim(dfShort[dfShort$singleProduct=="EnergyInput","singleProduct"])[1]
  # nTimberInput<-dim(dfShort[dfShort$singleProduct=="TimberInput","singleProduct"])[1]
  # nUpstreamInput<-dim(dfShort[dfShort$singleProduct=="UpstreamInput","singleProduct"])[1]
  # nPulpPaperInput<-dim(dfShort[dfShort$singleProduct=="PulpPaperInput","singleProduct"])[1]
  
  nMixedProduct<-length(dfShort[dfShort$singleProduct=="mixedProduct","singleProduct"])
  nEnergyInput<-length(dfShort[dfShort$singleProduct=="EnergyInput","singleProduct"])
  nTimberInput<-length(dfShort[dfShort$singleProduct=="TimberInput","singleProduct"])
  nUpstreamInput<-length(dfShort[dfShort$singleProduct=="UpstreamInput","singleProduct"])
  nPulpPaperInput<-length(dfShort[dfShort$singleProduct=="PulpPaperInput","singleProduct"])
  nAll<-length(dfShort[,"singleProduct"])

  Freq0<-aggregate(dfShort[,],by=list(dfShort$singleProduct), function(x) length(which(x==1)))
  Freq1<-data.frame(t(Freq0[,2:ncol(Freq0)]))
  colnames(Freq1)<-Freq0$Group.1
  Freq1[,"names"]<-colnames(dfShort)
  Freq<-melt(data.table(Freq1),id.vars="names")
  FreqAll<-aggregate(value~names,data=Freq, sum)
  FreqAll$variable<-"All"
  FreqAll<-data.frame(names=FreqAll$names, variable=FreqAll$variable, value=FreqAll$value)

  Freq<-data.frame(merge(rbind(Freq,FreqAll),
                         categoriesdf[!is.na(categoriesdf$cat2),c('names','colcat2','cat2','cat1','colcat1','cat3')],
                         by="names",
                         all.x=T))
  Freq<-unique(Freq)
  Freq<-Freq[!is.na(Freq$cat2),]
  Freq$names<-factor(Freq$names,levels=unique(categoriesdf[order(categoriesdf$id),'names']))

  Freq$valuePercent<-NA
  Freq[Freq$variable=="UpstreamInput",'valuePercent']<-Freq[Freq$variable=="UpstreamInput",'value']/nUpstreamInput*100
  Freq[Freq$variable=="PulpPaperInput",'valuePercent']<-Freq[Freq$variable=="PulpPaperInput",'value']/nPulpPaperInput*100
  Freq[Freq$variable=="TimberInput",'valuePercent']<-Freq[Freq$variable=="TimberInput",'value']/nTimberInput*100
  Freq[Freq$variable=="EnergyInput",'valuePercent']<-Freq[Freq$variable=="EnergyInput",'value']/nEnergyInput*100
  Freq[Freq$variable=="mixedProduct",'valuePercent']<-Freq[Freq$variable=="mixedProduct",'value']/nMixedProduct*100
  Freq[Freq$variable=="All",'valuePercent']<-Freq[Freq$variable=="All",'value']/nAll*100

  Freq[Freq$variable=="UpstreamInput",'nSingleProduct']<-nUpstreamInput
  Freq[Freq$variable=="PulpPaperInput",'nSingleProduct']<-nPulpPaperInput
  Freq[Freq$variable=="TimberInput",'nSingleProduct']<-nTimberInput
  Freq[Freq$variable=="EnergyInput",'nSingleProduct']<-nEnergyInput
  Freq[Freq$variable=="mixedProduct",'nSingleProduct']<-nMixedProduct
  Freq[Freq$variable=="All",'nSingleProduct']<-nAll

  Freq$longName<-str_to_title(Freq$names)
  Freq[Freq$names=="live_biomass_C","longName"]<-"Live biomass"
  Freq[Freq$names=="soilC","longName"]<-"Soil carbon"
  Freq[Freq$names=="harv_residues","longName"]<-"Harvest residues"
  Freq[Freq$names=="eol_biogenic","longName"]<-"End-of-life biogenic emiss."
  Freq[Freq$names=="eol_fossil","longName"]<-"End-of-life fossil emiss."
  Freq[Freq$names=="maintenance_emiss","longName"]<-"Maintenance emiss."
  Freq[Freq$names=="manufacturing_emiss","longName"]<-"Manufacturing emiss."
  Freq[Freq$names=="products_storage_C","longName"]<-"C storage in products"
  Freq[Freq$names=="off_product_biogenic","longName"]<-"Avoided emiss."
  Freq[Freq$names=="forestry_emiss","longName"]<-"Forestry emiss."
  Freq[Freq$names=="LUC_dyn","longName"]<-"Dyn. of LUC"
  Freq[Freq$names=="rebound_dyn","longName"]<-"Dyn. of econ. feedbacks"
  Freq[Freq$names=="biogenic_dyn","longName"]<-"Dyn. of biogenic emiss."
  Freq[Freq$names=="fossil_dyn","longName"]<-"Dyn. of fossil emiss."
  Freq[Freq$names=="displacement factor used","longName"]<-"Generic displacement factor"
  Freq[Freq$names=="self-calculated DF","longName"]<-"Self-calculated displacement factor"



  return(Freq)
}

longName<-function(names_vec){
  longNames_vec<-names_vec
  longNames_vec[longNames_vec=="scaleAggloc"]<-"Local scale"
  longNames_vec[longNames_vec=="scaleAggreg"]<-"Regional scale"
  longNames_vec[longNames_vec=="scaleAggw"]<-"Global scale"
  longNames_vec[longNames_vec=="singleProductTimberInput"]<-"Timber"
  longNames_vec[longNames_vec=="singleProductEnergyInput"]<-"Energy"
  longNames_vec[longNames_vec=="singleProductmixedProduct"]<-"Mixed Products"
  longNames_vec[longNames_vec=="singleProductPulpPaperInput"]<-"Pulp and Paper"
  longNames_vec[longNames_vec=="time_horizon0"]<-"0yrs time horizon"
  longNames_vec[longNames_vec=="time_horizon1-30"]<-"1-30yrs time horizon"
  longNames_vec[longNames_vec=="time_horizon31-70"]<-"31-70yrs time horizon"
  longNames_vec[longNames_vec=="time_horizon71-100"]<-"71-100yrs time horizon"
  
  
  longNames_vec[longNames_vec=="live_biomass_C"]<-"Live biomass"
  longNames_vec[longNames_vec=="soilC"]<-"Soil carbon"
  longNames_vec[longNames_vec=="harv_residues"]<-"Harvest residues"
  longNames_vec[longNames_vec=="eol_biogenic"]<-"End-of-life biogenic emiss."
  longNames_vec[longNames_vec=="eol_fossil"]<-"End-of-life fossil emiss."
  longNames_vec[longNames_vec=="maintenance_emiss"]<-"Maintenance emiss."
  longNames_vec[longNames_vec=="manufacturing_emiss"]<-"Manufacturing emiss."
  longNames_vec[longNames_vec=="products_storage_C"]<-"C storage in products"
  longNames_vec[longNames_vec=="off_product_biogenic"]<-"Avoided emiss."
  longNames_vec[longNames_vec=="forestry_emiss"]<-"Forestry emiss."
  longNames_vec[longNames_vec=="LUC_dyn"]<-"Dyn. of LUC"
  longNames_vec[longNames_vec=="rebound_dyn"]<-"Dyn. of econ. feedbacks"
  longNames_vec[longNames_vec=="biogenic_dyn"]<-"Dyn. of biogenic emiss."
  longNames_vec[longNames_vec=="fossil_dyn"]<-"Dyn. of fossil emiss."
  longNames_vec[longNames_vec=="displacement factor used"]<-"Generic displacement factor"
  longNames_vec[longNames_vec=="self-calculated DF"]<-"Self-calculated displacement factor"
  
  longNames_vec[longNames_vec=="modelAssumptionCarbon neutral forest assumption"]<-"Carbon neutral forest assumption"
  longNames_vec[longNames_vec=="modelAssumptionBiogenic only assumption"]<-"Biogenic only assumption"
  longNames_vec[longNames_vec=="modelAssumptionCarbon neutral forest and HWP assumption"]<-"Carbon neutral forest and HWP assumption"
  longNames_vec[longNames_vec=="modelAssumptionAll C pools"]<-"All C pools"
  longNames_vec[longNames_vec=="modelAssumptionHybrid assumption"]<-"Hybrid assumption"
  longNames_vec[longNames_vec=="driver1area subject to harvest"]<-"Increased area subject to harvest"
  longNames_vec[longNames_vec=="driver1cutting intensity"]<-"Increased cutting intensity"
  longNames_vec[longNames_vec=="driver1environmental driver"]<-"Environmental driver"
  longNames_vec[longNames_vec=="driver1fertilisation"]<-"Increased fertilisation"
  longNames_vec[longNames_vec=="driver1multiple supply"]<-"Multiple driver"
  longNames_vec[longNames_vec=="driver1multiple silviculture"]<-"Multiple driver"
  longNames_vec[longNames_vec=="driver1mixed drivers"]<-"Multiple driver"
  longNames_vec[longNames_vec=="driver1plantation density"]<-"Increased plantation density"
  longNames_vec[longNames_vec=="driver1recycling"]<-"Increased recycling"
  longNames_vec[longNames_vec=="driver1rotation length"]<-"Decreased rotation length"
  longNames_vec[longNames_vec=="driver1site fertility"]<-"Increased site fertility"
  longNames_vec[longNames_vec=="driver1species"]<-"Shifting to more productive species"
  longNames_vec[longNames_vec=="driver1supply chain organization"]<-"Better organizing supply chain"
  longNames_vec[longNames_vec=="driver1technologies/design switch"]<-"Shifting technology"
  longNames_vec[longNames_vec=="driver1unspecified harvest increase"]<-"Unspecified harvest increase"
  longNames_vec[longNames_vec=="driver1efficiency improvement"]<-"Efficiency improvement"
  longNames_vec[longNames_vec=="driver1harvesting system"]<-"Harvesting system"
  longNames_vec[longNames_vec=="driver1location of industry"]<-"Location of industry"
  longNames_vec[longNames_vec=="driver1end of life disposal"]<-"End of life disposal"
  longNames_vec[longNames_vec=="driver1products lifespan"]<-"Products lifespan"
  longNames_vec[longNames_vec=="driver1Temperature"]<-"Temperature"


  return(longNames_vec)
}

assignApproach<-function(data_expt){

  nminTechno<-2
  nminEcos<-2

  data_expt_approach<-data_expt
  data_expt_approach$modelApproach<-"Hybrid approach"

  data_expt_approach[
    (data_expt_approach$manufacturing_emiss==1 | data_expt_approach$off_product_biogenic==1)&
      (data_expt_approach$live_biomass_C==0 & data_expt_approach$harv_residues==0) &
      (data_expt_approach$manufacturing_emiss+ data_expt_approach$maintenance_emiss + data_expt_approach$forestry_emiss +data_expt_approach$off_product_biogenic >=nminTechno)
    ,
    "modelApproach"]<-"Technology approach"

  data_expt_approach[
    data_expt_approach$manufacturing_emiss==0 & data_expt_approach$off_product_biogenic==0&
      (data_expt_approach$live_biomass_C==1 | data_expt_approach$harv_residues==1)&
      ( data_expt_approach$live_biomass_C +data_expt_approach$harv_residues + data_expt_approach$soilC + data_expt_approach$eol_biogenic >=nminEcos)
    ,
    "modelApproach"]<-"Ecosystem approach"


  data_expt_approach[
    (data_expt_approach$manufacturing_emiss==1 | data_expt_approach$off_product_biogenic==1)&
      (data_expt_approach$live_biomass_C==1 | data_expt_approach$harv_residues==1)&
      ( data_expt_approach$live_biomass_C +data_expt_approach$harv_residues + data_expt_approach$soilC + data_expt_approach$eol_biogenic + data_expt_approach$products_storage_C >=nminEcos)&
      (data_expt_approach$manufacturing_emiss+ data_expt_approach$maintenance_emiss +data_expt_approach$forestry_emiss +data_expt_approach$off_product_biogenic >=nminTechno)
    ,"modelApproach"]<-"Whole sector approach"
  
  

#Following VB's comment email 16/09 : 
  #"sur l’exemple de Gustavsson_2006, je trouve les critères fautifs pour déterminer « Ecosystem approach ». Je ne pense pas que HWP devrait jouer un rôle, 
  # et je pense que la biomasse devrait suffire (éventuellement avec « résidus » que de toutes façons j’ai du mal à voir comment on peut avoir biomasse sans
  # résidus et que j’ai l’impression qu’on n’a pas tous compris pareil). En l’occurrence, on n’a pas biogenic_dyn mais les pertes en forêt à t0 du fait de la 
  # récolte sont bien prise en compte, donc ça va."
  
  # #+ update of dendrogram :
  # 
  # nminTechno<-2
  # nminEcos<-2
  # 
  # data_expt_approach<-data_expt
  # data_expt_approach$modelApproach<-"Hybrid approach"
  # 
  # data_expt_approach[
  #   (data_expt_approach$manufacturing_emiss==1 | data_expt_approach$products_storage_C==1)&
  #     (data_expt_approach$live_biomass_C==0 & data_expt_approach$harv_residues==0) &
  #     (data_expt_approach$manufacturing_emiss+ data_expt_approach$maintenance_emiss +data_expt_approach$off_product_biogenic + data_expt_approach$eol_biogenic >=nminTechno)
  #   ,
  #   "modelApproach"]<-"Technology approach"
  # 
  # data_expt_approach[
  #   data_expt_approach$manufacturing_emiss==0 & data_expt_approach$products_storage_C==0&
  #     (data_expt_approach$live_biomass_C==1 | data_expt_approach$harv_residues==1)&
  #     ( data_expt_approach$biogenic_dyn +data_expt_approach$live_biomass_C +data_expt_approach$harv_residues + data_expt_approach$soilC  >=nminEcos)
  #   ,
  #   "modelApproach"]<-"Ecosystem approach"
  # 
  # 
  # data_expt_approach[
  #   (data_expt_approach$manufacturing_emiss==1 | data_expt_approach$products_storage_C==1)&
  #     (data_expt_approach$live_biomass_C==1 | data_expt_approach$harv_residues==1)&
  #     ( data_expt_approach$biogenic_dyn +data_expt_approach$live_biomass_C +data_expt_approach$harv_residues + data_expt_approach$soilC  >=nminEcos)&
  #     (data_expt_approach$manufacturing_emiss+ data_expt_approach$maintenance_emiss +data_expt_approach$off_product_biogenic +data_expt_approach$eol_biogenic >=nminTechno)
  #   ,"modelApproach"]<-"Whole sector approach" 
  # 
  # # data_expt_approach[
  # #   (data_expt_approach$manufacturing_emiss==1 )&
  # #     (data_expt_approach$live_biomass_C==0 )
  # #        ,
  # #   "modelApproach"]<-"Technology approach"
  # # 
  # # data_expt_approach[
  # #   (data_expt_approach$manufacturing_emiss==0 )&
  # #     (data_expt_approach$live_biomass_C==1)
  # #     
  # #   ,
  # #   "modelApproach"]<-"Ecosystem approach"
  # # 
  # # 
  # # data_expt_approach[
  # #   (data_expt_approach$manufacturing_emiss==1 )&
  # #     (data_expt_approach$live_biomass_C==1 )
  # #      ,"modelApproach"]<-"Whole sector approach"
  # 

  data_expt_approach$modelApproach<-factor(data_expt_approach$modelApproach,levels=c("Hybrid approach","Technology approach", "Ecosystem approach","Whole sector approach"))
  return(data_expt_approach)
}


assignAssumption<-function(data_expt){

  data_expt_assumption<-data_expt
  data_expt_assumption$modelAssumption<-"Hybrid assumption"
  
  # data_expt_assumption[
  #   #  (data_expt_assumption$harv_residues==0 &data_expt_assumption$live_biomass_C==0 & (data_expt_assumption$eol_biogenic==1 | data_expt_assumption$off_product_biogenic==1))
  #   ( (data_expt_assumption$harv_residues + data_expt_assumption$live_biomass_C + data_expt_assumption$soilC+ data_expt_assumption$forestry_emiss) ==0) 
  #   #& ( data_expt_assumption$eol_biogenic + data_expt_assumption$products_storage_C >=1)
  #   ,
  #   "modelAssumption"]<-"Carbon neutral forest assumption"  
  
  data_expt_assumption[
#    ( data_expt_assumption$harv_residues==0 & data_expt_assumption$live_biomass_C==0 & data_expt_assumption$eol_biogenic==0  & data_expt_assumption$products_storage_C==0)
    ( (data_expt_assumption$harv_residues + data_expt_assumption$live_biomass_C + data_expt_assumption$soilC) ==0) &
    ( (data_expt_assumption$eol_biogenic + data_expt_assumption$products_storage_C) ==0)
    ,
    "modelAssumption"]<-"Carbon neutral forest and HWP assumption"
  list_C_neutral<-data_expt_assumption[data_expt_assumption$modelAssumption=="Carbon neutral forest and HWP assumption",c("PaperID","substitution")]

  
  data_expt_assumption[
  #  ((data_expt_assumption$manufacturing_emiss==0 &data_expt_assumption$maintenance_emiss==0  & data_expt_assumption$off_product_biogenic==0) & data_expt_assumption$products_storage_C==0 & data_expt_assumption$eol_biogenic==0)
    (data_expt_assumption$manufacturing_emiss + data_expt_assumption$maintenance_emiss + data_expt_assumption$forestry_emiss + data_expt_assumption$off_product_biogenic ==0)
    ,
    "modelAssumption"]<-"Biogenic only assumption"
  list_insitu<-data_expt_assumption[data_expt_assumption$modelAssumption=="Biogenic only assumption",c("PaperID","substitution")]
  
  data_expt_assumption[
    data_expt_assumption$live_biomass_C+data_expt_assumption$soilC>=1 &
      data_expt_assumption$harv_residues +   data_expt_assumption$eol_biogenic >=1 &
    data_expt_assumption$manufacturing_emiss+ data_expt_assumption$maintenance_emiss  >=1 &
      data_expt_assumption$forestry_emiss+ data_expt_assumption$off_product_biogenic  >=1
    ,
    "modelAssumption"]<-"All C pools"
  
  list_All_C<-data_expt_assumption[data_expt_assumption$modelAssumption=="All C pools",c("PaperID","substitution")]
  
  data_expt_assumption$modelAssumption<-factor(data_expt_assumption$modelAssumption,levels=c("Biogenic only assumption","Carbon neutral forest assumption","Carbon neutral forest and HWP assumption", "All C pools","Hybrid assumption"))
  log_print("---Assign assumptions---")
  log_print("C_neutral")
  log_print(list_C_neutral,n=nrow(list_C_neutral))
  log_print("Insitu only")
  log_print( list_insitu,n=nrow(list_insitu))
  log_print("All C")
  log_print(list_All_C,n=nrow(list_All_C))
  log_print("Intercepts C_neutral / AllC")
  log_print(generics::intersect(list_C_neutral, list_C_neutral) )
  
  log_print("Intercepts  Biogenic/AllC")
  log_print(generics::intersect(list_insitu, list_C_neutral) )
  data_expt_assumption<-data_expt_assumption[data_expt_assumption$modelAssumption!="Hybrid assumption",]
  return(data_expt_assumption)
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
#modelComponentsC(data_expt_approachResults,c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")
#, "",("PaperID"))

modelComponentsC<-function(data_expt, compartmentList, option, listCriteria){

  #xlistCriteria<-c("PaperID","singleProduct","time_horizon")
  formulaRHS<-paste0(paste(listCriteria, collapse="+"),"+get(compartment)")
  formulaShort<-""
  for(i in seq(length(listCriteria))){

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

  variable<-"substitution"
  for(variable in c("substitution")){
    # Select dat rows that have several model setups only changing one model parameter
    t.u.d.m<-findDuplicates(data_expt,variable)


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

          tTestPairs<-rbind(tTestPairs,cbind(compartment,data_expt_pairs,t.test2(data_expt_pairs$substitution.mn_0, data_expt_pairs$substitution.mn_1, data_expt_pairs$substitution.sd_0, data_expt_pairs$substitution.sd_1, data_expt_pairs$substitution.N_0,data_expt_pairs$substitution.N_1,m0=0)))

        }else{
          tTestPairs<- cbind(compartment,data_expt_pairs,t.test2(data_expt_pairs$substitution.mn_0, data_expt_pairs$substitution.mn_1, data_expt_pairs$substitution.sd_0, data_expt_pairs$substitution.sd_1, data_expt_pairs$substitution.N_0,data_expt_pairs$substitution.N_1,m0=0))

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
    colnames(tTestPairsSignifAgg)<-c("process",paste("Difference of means for Carbon Balance (tCO2/m3)"))
    tTestPairsSignifAgg<- tTestPairsSignifAgg[order(tTestPairsSignifAgg[,"Difference of means for Carbon Balance (tCO2/m3)"]),]
    rownames(tTestPairsSignifAgg)<-NULL

    #aggregate(data_modelShort[,],by=list(data_modelShort$singleProduct), function(x) length(which(x==1)))
    if(!exists("tTestPairsSignifAggVar")){
      tTestPairsSignifAggVar<-tTestPairsSignifAgg
    }else{
      tTestPairsSignifAggVar<-merge(tTestPairsSignifAggVar,tTestPairsSignifAgg,by="process",all=TRUE)

    }
  }

  tTestPairsSignifAggVarMelt<-melt(as.data.frame(tTestPairsSignifAggVar),id="process")
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

modelComponentsCBoxplots<-function(data, compartmentList, option, listCriteria){
    t.u.d.m<-findDuplicates(data,"substitution")
     for (compartment in compartmentList){
       boxplotCompartment(t.u.d.m,compartment,"substitution",listCriteria)
     }
}



boxplotCompartment<-function(t.u.d.m, compartment,variable,listCriteria){
  countDistinct<-aggregate(t.u.d.m[,compartment], by=list(t.u.d.m$PaperID) , function(x) length(unique(x)))
  list_studies<-countDistinct[countDistinct$x!=1,"Group.1"]
  if(length(list_studies)>=1){
    #    plotData<-t.u.d.m[t.u.d.m$PaperID %in% list_studies,c('PaperID','modelID','singleProduct','time_horizon',variable,compartment)]
    plotData<-t.u.d.m[t.u.d.m$PaperID %in% list_studies,c('modelID',listCriteria,variable,compartment)]
    if (compartment!="singleProduct" & compartment!="time_horizon"){
      plotData[,compartment]<-factor(as.character(plotData[,compartment]),levels=c("0","1"))
    }
    if("time_horizon" %in% listCriteria){
      plotData$time_horizon<-factor(plotData$time_horizon,levels=c("0","1-70","70-100","+100"))
    }
    if(compartment!="singleProduct"){
      Switch<-ifelse(length(listCriteria)>=2,"T","F")
      p<-ggplot(plotData[ !is.na(plotData$modelID) &!is.na(plotData$PaperID),],aes(x=get(compartment),get(variable),color=PaperID))+
        #    geom_boxplot(show.legend=FALSE, position = position_dodge(width = 0.9))+
        coord_flip()+
        theme_bw()+
        ###   geom_signif( map_signif_level=TRUE, y_position = c(12, 17))+ !!! not working but add significance
        geom_boxplot(show.legend=FALSE)+
        geom_hline(yintercept = 0,size=0.8,linestyle="dotted")+
        geom_point()+
        ylab(variable)+
        stat_summary(
          fun.y = median,
          geom = 'line',
          aes(group = PaperID, colour = PaperID),show.legend=FALSE )+
        {if(Switch)facet_wrap(as.formula(paste("~", (listCriteria[2]))))}+
        # facet_wrap(~singleProduct,scales="free")+
        ggtitle(compartment)
      # print(p)
    }else{#singleProduct
      p<-ggplot(plotData[ !is.na(plotData$modelID) &!is.na(plotData$PaperID),],aes(x=get(compartment),get(variable),color=PaperID))+
        #    geom_boxplot(show.legend=FALSE, position = position_dodge(width = 0.9))+
        coord_flip()+
        theme_bw()+
        geom_boxplot(show.legend=FALSE)+
        geom_hline(yintercept = 0,size=0.8,linestyle="dotted")+
        geom_point()+
        ylab(variable)+
        stat_summary(
          fun.y = median,
          geom = 'line',
          aes(group = PaperID, colour = PaperID),show.legend=FALSE #this has to be added
          #    position = position_dodge(width = 0.9),show.legend=FALSE #this has to be added
        )
      # geom_line(aes(group=PaperID),show.legend=FALSE, position = position_dodge(width = 0.9))+
      ggtitle(compartment)
      # print(p)
    }   
    print(p)
    ggsave(file.path("./offline_figures/",paste0("boxplotCompartment_",compartment,".pdf")))
  }
}

approachC<-function(data_expt_approach,filters){

  if(missing(filters)){
    print('in global : loading forestPlotData results')
    forestPlotData<-read.csv("forestPlotData.approachC.v5.v6.v7.init.csv")
    dataMA<-read.csv("dataMA.approachC.v5.v6.v7.init.csv")

  }else{
    print('in global : calculating forestPlotData results')

    dataMA<-plotDataFunc(data_expt_approach,
                         filters=NULL, 
                         outliers_out=NULL,
                         split="modelApproach",
                         omitSplit2="omitSplit2",
                         omitSplit3="omitSplit3") #data_expt_approach,filters, include_approaches,outliers_out,split, omitSplit2 (default for split2=driver1Cat)
    listSplits<-levels(dataMA$split)

    forestPlotData<-forestPlotDataFunc(dataMA)


  }
}

#----------- Data plotting -----------



plotBarplotYear<-function(data_bibliom){
  ggplot(data_bibliom,aes(x=Publication_Year))+
    geom_bar()+
    theme_bw()+
    # scale_y_continuous(breaks=seq(0,20,by=2))+
    theme(text = element_text(size=txt_size),
          axis.text.x = element_text(angle=txt_angle, hjust=1))

}




plotCountryData<-function(countryFreqData, sortingCriteria){
  countryFreqData<-countryFreqData[!is.na(countryFreqData[,sortingCriteria]),]
  countryFreqData<-unique(countryFreqData[order(-countryFreqData[,sortingCriteria]), ])
  countryFreqData$country<-factor(countryFreqData$country,
                                  levels=as.vector(countryFreqData[order(countryFreqData[,sortingCriteria]), 'country']))
  
  colnames(countryFreqData)[colnames(countryFreqData)=="Forest area ratio (%)"]<-"Forest area ratio (%, source: FAO)"
  colnames(countryFreqData)[colnames(countryFreqData)=="Roundwood (m3)"]<-"Roundwood (m3, source: FAO)"
  colnames(countryFreqData)[colnames(countryFreqData)=="Forest.area..1000.ha."]<-"Forest area (x1000 ha, source: FAO)"
  colnames(countryFreqData)[colnames(countryFreqData)=="GDP_RD"]<-"Part of R&D in GDP (%, source: Unesco)"
  countryDataSubset<-rbind(countryFreqData[1:10,],countryFreqData[!is.na(countryFreqData$nPaperID),])
  if(sortingCriteria=="Forest area ratio (%)"){sortingCriteria<-"Forest area ratio (%, source: FAO)"}
  if(sortingCriteria=="Roundwood (m3)"){sortingCriteria<-"Roundwood (m3, source: FAO)"}
  if(sortingCriteria=="Forest.area..1000.ha."){sortingCriteria<-"Forest area (x1000 ha, source: FAO)"}
  if(sortingCriteria=="GDP_RD"){sortingCriteria<-"Part of R&D in GDP (%, source: Unesco)"}


  ggplot(countryDataSubset,aes(fill=nPaperID,y=country,x=get(sortingCriteria),label=country))+
    geom_bar(stat='identity',position='dodge',colour="gray",linewidth=0.05)+
    scale_fill_viridis(na.value="white")+
    scale_y_discrete(position="left")+
    labs(fill="Number of records")+
    xlab(sortingCriteria)+
    theme_bw()+
    theme( axis.ticks = element_blank(),
           text = element_text(size=14),
           axis.text.x = element_text(angle = 45))

}


create_processes_frequency <- function(data_freq,percent,wrap){
if(percent=="percent"){
  data_freq$frequency<-data_freq$valuePercent
  legend<-"Percent of records"
}else{
  data_freq$frequency<-data_freq$value
  legend<-"Number of records"
  
}
  if(missing(wrap)){
    plotData<-data_freq[(data_freq$cat1 =="Processes" & data_freq$variable=="All") & !is.na(data_freq$cat1),c("longName","variable","frequency","cat2","colcat2")]
   
    #colnames(plotData)<-c("longName","cat2","colcat2","frequency")
    colorVect<-unique(plotData[,c("colcat2","cat2")])
    colordictProcesses<-setNames(as.character(colorVect$colcat2),
                                 as.character(colorVect$cat2))
    plotData$wrap<-"All"

  }else{
    plotData<-data_freq[(data_freq$cat1 =="Processes") & !is.na(data_freq$cat1),c("longName","variable","frequency","cat2","colcat2")]
    plotData$cat2<-factor(plotData$cat2,levels=c("Dynamics","SubstitutionShortcut","C fluxes"), labels=c("Dynamics","Substitution shortcut","C fluxes"))

    colorVect<-unique(plotData[,c("colcat2","cat2")])
    colordictProcesses<-setNames(as.character(colorVect$colcat2),
                                 as.character(colorVect$cat2))
    plotData$wrap<-plotData$variable
    plotData$wrap<-gsub("([a-z])([A-Z])","\\1 \\2",str_remove(plotData$wrap,'Input'))

  }
  plotData$wrap<-factor(plotData$wrap,levels=sort(levels(factor(plotData$wrap))))
  ggplot(plotData,aes(x=reorder(longName,frequency),y=frequency,fill=cat2))+
    coord_flip() +
    geom_bar(stat="identity") +
    scale_fill_manual(values=colordictProcesses)+
    theme_bw()+
    theme( axis.ticks = element_blank(),
           text = element_text(size=txt_size),
           axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.),
           axis.title.y=element_blank(),
           legend.title = element_blank())+
    ylab(legend)+
    labs(colour = NULL)+
    geom_text(aes(label = frequency), alpha = 0,show.legend = FALSE)+
    facet_wrap(~wrap,scales="free_x")

}


#
create_processes_versus_flux_size<-function(refCProcessMean,study_freq,palette,wood_type_names, wrap){

  my.cols <- c(brewer.pal(5, palette),"#000000")
  plotData<-study_freq[(study_freq$cat2=="C fluxes"),c("names","longName","variable","value","valuePercent","cat2","colcat2","nSingleProduct")]
  if(missing(wrap)){
    plotDataAgg<-aggregate(cbind(value,nSingleProduct)~names+cat2+longName,data=plotData,sum)
    plotDataAgg$valuePercent<-plotDataAgg$value/plotDataAgg$nSingleProduct*100
    plotDataAgg$variable<-"All"
    plotDataAgg$wrap<-"All"
    plotData<-plotDataAgg

  }else{
    plotData$wrap<-factor(plotData$variable,levels=sort(levels(plotData[,"variable"])))
  }

  #plotData<-rbind(plotData[,c('names','variable','valuePercent','value','nSingleProduct','cat2')],plotDataAgg[,c('names','variable','valuePercent','value','nSingleProduct','cat2')])
  maxValue<-max(plotData$value)+5
  maxValuePercent<-max(plotData$valuePercent)+5

  plotDataRefCProcess<-merge(plotData,refCProcessMean,by.x="names",by.y="substitutionDatabaseVariable")

  if(dim(plotDataRefCProcess)[1]>0){
    ggplot(plotDataRefCProcess,aes(x=(`value GtCO2/yr`),y=(valuePercent)))+#,shape=names))+
      geom_point(aes(size=value))+
      geom_smooth(aes(group = variable),formula = y ~ x  ,method="glm",se=FALSE,show.legend = FALSE,col="black")+      #
      #scale_color_manual(name="Wood type",values = c("EnergyInput" = my.cols[1], "PulpPaperInput" = my.cols[2],"TimberInput" = my.cols[3],"mixedProduct" = my.cols[4],"UpstreamInput" = my.cols[5],"All"=my.cols[6]))+
      labs(size="Number of records",col="Processes")+
      theme_bw()+
      #theme( text = element_text(size=12))+
      geom_vline(data=plotDataRefCProcess, mapping=aes(xintercept=`value GtCO2/yr`,col=longName), linetype="longdash") +
      #geom_text(data=plotDataRefCProcess, mapping=aes(x=`value GtCO2/yr`, y=0.7, label=names), angle=90, vjust=-0.4, hjust=0,color='black',check_overlap = TRUE) +
      xlab("Global flux (GtCO2)")+
      ylab("Fraction of records accounting for this process (%)")+
      facet_wrap(~wrap,ncol=3, labeller = as_labeller(wood_type_names))
  }

}



create_driver_frequency <- function(expt_freq,wrap){

  if(missing(wrap)){

    plotData<-expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),
                        c("longName","variable","value","cat2","colcat2")]
    plotData$cat2<-factor(plotData$cat2,levels=c("Technology","Silviculture for removals","Silviculture for productivity"," Supply chain","Demand","Environmental change"),labels=c("Technology","Forest harvest","Forest growth"," Supply chain","Demand","Environmental change"))
    plotData<-aggregate(plotData$value ,by=list(plotData$longName,plotData$cat2,plotData$colcat2),FUN=sum)
    colnames(plotData)<-c("longName","cat2","colcat2","value")
    colorVectDrivers<-unique(plotData[,c("colcat2","cat2")])
    colordictDrivers<-setNames(as.character(colorVectDrivers$colcat2),
                               as.character(colorVectDrivers$cat2))

    plotData$wrap<-"All"
  }else{
    plotData<-expt_freq[(expt_freq$cat1 =="Change in practices"|expt_freq$cat1 =="Environmental change") & !is.na(expt_freq$cat1),
                        c("longName","variable","value","cat2","colcat2")]

    colorVectDrivers<-unique(plotData[,c("colcat2","cat2")])
    colordictDrivers<-setNames(as.character(colorVectDrivers$colcat2),
                               as.character(colorVectDrivers$cat2))

    plotData$wrap<-plotData$variable
    plotData$wrap<-gsub("([a-z])([A-Z])","\\1 \\2",str_remove(plotData$wrap,'Input'))


  }
  plotData$wrap<-factor(plotData$wrap,levels=sort(levels(factor(plotData$wrap))))

  ggplot(plotData,aes(x=reorder(longName,value),y=value,fill=cat2))+
    coord_flip() +
    geom_bar(stat="identity") +
    scale_fill_manual(values=colordictDrivers)+
    theme_bw()+
    theme( axis.ticks = element_blank(),
           text = element_text(size=txt_size),
           axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.),
           axis.title.y=element_blank(),
           legend.title = element_blank())+
    ylab("Number of experiments")+
    labs(colour = NULL)+
    facet_wrap(~wrap,scales="free_x")
  #ggsave("expt_model_freq.pdf",width=7,height=5)
}


# data_expt<-data_expt_assumptionResults
# ksplit<-5
# factors<-c( 'modelAssumption', 'scaleAgg','singleProduct','time_horizon')
create_dendrogram<-function(data_expt,ksplit,factors){

  #dataCor<-data_expt[data_expt$Exclusion=="included",c('scaleAgg','singleProduct','time_horizon', 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic', 'biogenic_dyn')]
  #Add off_product_biogenic once it is filled for all studies
#dataCor<-data_expt[data_expt$Exclusion=="included",c('scaleAgg','singleProduct','time_horizon', 'soilC', 'harv_residues', 'live_biomass_C', 'products_storage_C', 'forestry_emiss', 'manufacturing_emiss', 'maintenance_emiss','eol_biogenic','off_product_biogenic', 'biogenic_dyn')]
  dataCor<-data_expt[data_expt$Exclusion=="included",factors]

  dataCor[is.na(dataCor)]<-0
  dataMat<-model.matrix(~0+., data=dataCor)
  p.matgg <- cor.mtest(dataMat)
  p <- cor.test.p(dataMat)

  dataPlotCorgg<-cor(dataMat,use="pairwise.complete.obs")

  excludeCor<-names(rowSums(is.na(dataPlotCorgg))[rowSums(is.na(dataPlotCorgg))==dim(dataPlotCorgg)[1]])
  dataPlotCorgg<-dataPlotCorgg[!(rownames(dataPlotCorgg) %in% excludeCor) ,][,!(colnames(dataPlotCorgg) %in% excludeCor)]

  rowCluster = hclust(dist(dataPlotCorgg))
  rowCluster$labels<-longName(rowCluster$labels)
  #c("Local scale","Regional scale","Global scale","Timber product","Energy product","Mixed product","Pulp and paper product","0 yrs time horizon","1-30 yrs time horizon", "31-70 yrs time horizon","71-100 yrs time horizon", "Soil carbon","Harvest residues","Live biomass", "C storage in products","Forestry emiss.","Manufacturing emiss.","Maintenance emiss.","End-of-life emiss.", "Dyn. of biogenic emiss.", "Avoided emiss."")

  if(ksplit==3){
    kcolors<-c("#0072B2","#E69F00", "#D55E00")
  }else if(ksplit==5){
    kcolors<-c( "#D55E00","#0072B2","#E69F00","#999999","#000000",) # "#009E73",
  }else if(ksplit==6){
    kcolors<-c( "#D55E00","#E69F00","#009E73","#999999","#000000","#0072B2") # "#009E73",
  }
  par(mfrow=c(2,1), mar=c(4,4,0.9,14))
  dendrogram<-
    fviz_dend(rowCluster,
          #  k=2 ,           # Cut in x groups
              k=ksplit ,           # Cut in x groups
              cex = 0.7,                 # label size
              linewidth=0.1,
              # rect = TRUE,
          repel=TRUE,
              k_colors = kcolors,# color labels by groups
              labels_track_height=0.5,
              horiz = TRUE,  # color labels by groups
              ggtheme = theme_bw()     # Change theme

    )
  return(dendrogram)
}#end function
# create_forest_plot(plotData.driverC,forestPlotData.driverC,TRUE)
# create_forest_plot(plotData.assumptionC,forestPlotData.assumptionC,FALSE)

# plotData<-plotData.assumptionC
# forestPlotData<-forestPlotData.assumptionC
#  wrapSplit2<-FALSE
create_forest_plot<-function(plotData,forestPlotData,wrapSplit2){
  p<-ggplot(plotData,aes(x=split,y=substitution))+
    geom_boxplot(data=plotData,aes(y=substitution ,x=reorder(split,substitution,mean,na.rm=TRUE)),
                 outliers = TRUE,outlier.color=NULL,size=2,fill="lightgrey",color="lightgrey")+
    stat_summary(fun=median, color="darkgrey", geom="point",  shape=15, size=3, show.legend=FALSE) +

    geom_point( data=forestPlotData) +
    geom_errorbar( data=forestPlotData,aes(ymin = ci.lb, ymax = ci.ub),size=0.5,width=0.5) + #add CIs as error bars
    theme_bw()+
    theme(
      axis.ticks = element_blank(),
      #axis.title.x = element_blank(),
      #axis.title.y = element_blank(),
      text = element_text(size=16),
      axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0)
    )+
    geom_text(data=forestPlotData,
              aes(label=paste0(round(substitution,2)," ( ",nStud,"|",nRec,signif," )" )),
              y=2.1,
              size=6,
              nudge_x=0.2) +

    geom_hline(yintercept=0)+
    coord_flip(y = c(-6,6),clip="off")

  if(wrapSplit2){

    p<-p+facet_grid(split2~.,
                    switch="y",
  labeller = labeller( split2 = label_wrap_gen(width = 10),
                       #labeller = labeller( driver1Cat = label_wrap_gen(width = 10),
                                         .multi_line = TRUE),
                    scales="free_y",
                    space="free_y",)
  }
  p<-p+labs(x="",y="Carbon balance (tCO2/m3)")


  print(p)


}

tableModelComponentsC<-function(tTestPairsSignifAggVarMelt){
  write.csv(kable(reshape2::dcast(tTestPairsSignifAggVarMelt,process~variable)),paste0("./offline_figures/","table_S3_model_components.csv"))
}

plotModelComponentsC<-function(tTestPairsSignifAggVarMelt){
  tTestPairsSignifAggVarMelt$process <- factor(tTestPairsSignifAggVarMelt$process, levels=(
c("Live biomass","Soil carbon","Harvest residues","Forestry emiss.","Manufacturing emiss.","C storage in products","Maintenance emiss.","End-of-life emiss.")
    #    tTestPairsSignifAggVarMelt$process)[order(tTestPairsSignifAggVarMelt$value)]
    ))
  p<-ggplot(  tTestPairsSignifAggVarMelt,aes(x=process,y=variable,fill=value))+
    geom_tile(color="white")+
    scale_y_discrete(labels = function(x) str_wrap(x, width = 15))+
    theme_bw()+
    scale_fill_gradient2()+
    # scale_fill_gradientn(
    #   colours = c(muted("orange"), "white", muted("green")),
    #   na.value = "grey98",
    #   name = "ratio",
    #   limits = c(-1,1 )
    # )+
    theme( axis.ticks = element_blank(),
           text = element_text(size=txt_size),
           axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
           axis.text.y = element_text(hjust=1),
           axis.title=element_blank()
    )

   #print(p)

}
# 
# #DEBUG
# plotDataFunc(data_expt_approachResults, c("Whole sector approach","Technology approach","Ecosystem approach"),NULL,"modelApproach","omitSplit2","omitSplit3")
# plotData.driverC<-plotDataFunc(data_expt_approachResults, c("Whole sector approach"),NULL,"modelApproach","driver1","omitSplit3")
# plotData.split.yr<-plotDataFunc(data_expt_split_yr, NULL,splitName,"omitSplit2","omitSplit3")

# data_expt_split<- data_expt_split_yr
# include_split_levels<- c("Whole sector approach","Technology approach","Ecosystem approach")
# outliers<- "outliers_in"
# splitName<- "modelAssumption"
# split2Name<- "omitSplit2"
# split3Name<-"omitSplit3"

plotDataFunc<-function(data_expt_split, outliers,splitName,split2Name,split3Name){
 # if (splitName =="modelApproach"){
 #   print(splitName)
 #   data_expt_split$split <- data_expt_split$modelApproach
 # }else if (splitName == "modelAssumption" ){
 #   print(splitName)
 #   data_expt_split$split <- data_expt_split$modelAssumption
 # }else if (splitName == "driver1" ){
 #   print(splitName)
 #   data_expt_split$split <- data_expt_split$driver1
 # }
  print(splitName)
  data_expt_split$split <- as.vector(data_expt_split[[c(splitName)]])
  
  plotData <-data.frame(data_expt_split[ !(data_expt_split$driver1Cat)%in%c("Demand" ,"Environmental change" ),])
  plotData$sei<-0.001
  plotData$singleProduct<-factor(plotData$singleProduct)

  if(outliers=="outliers_in"){
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

  plotData$split<-as.character(plotData$split)
  if(splitName=="modelApproach" |splitName=="modelAssumption"){
    plotData$split<-factor(plotData$split)
  }else if(splitName=="driver1"){
    plotData[plotData$driver1=="technologies/design switch", "split"] <- as.character(plotData[ plotData$driver1=="technologies/design switch", "singleProduct"])
    
  }
  if(split2Name!="omitSplit2"){ 
    plotData$split2 <- as.vector(plotData[[c(split2Name)]])
    if(split2Name=="driver1"){
    plotData[plotData$driver1=="technologies/design switch", "split2"] <- as.character(plotData[ plotData$driver1=="technologies/design switch", "singleProduct"])
    plotData$split2<-factor(plotData$split2,
                           levels=c("area subject to harvest", "cutting intensity", "environmental driver","fertilisation","multiple supply","multiple silviculture","mixed drivers","plantation density","recycling" ,"rotation length" , "site fertility", "species" ,"supply chain organization","technologies/design switch" ,"unspecified harvest increase","EnergyInput","TimberInput","PulpPaperInput","mixedProduct","efficiency improvement","harvesting system", "location of industry", "end of life disposal","products lifespan", "Temperature"),
                           labels=c("Increased area subject to harvest", "Increased cutting intensity", "Environmental driver","Increased fertilisation","Multiple driver","Multiple driver","Multiple driver","Increased plantation density","Increased recycling" ,"Decreased rotation length" , "Increased site fertility", "Shifting to more productive species" ,"Better organizing supply chain","Shifting technology" ,"Unspecified harvest increase", "Energy","Timber","Pulp and paper","Mixed products","Efficiency improvement","harvesting system", "location of industry", "end of life disposal","products lifespan", "Temperature"),
    )
    } else if(split2Name=="driver1Cat"){
      
      plotData$split2<-factor(plotData$split2,
                              levels=c("Silviculture for productivity","Silviculture for removals","Supply chain", "Technology","Multiple strategies"),
                              labels=c( "Mobilize additional wood by increased forest growth","Mobilize additional wood by increased forest harvest","Make better use of wood",  "Use wood instead of other ressource","Multiple strategies")
      )
    }
  }else{
    plotData$split2<-"dummySplit2"
  }
  
  if(split3Name!="omitSplit3"){ 
    plotData$split3 <- as.vector(plotData[[c(split3Name)]])
    if(split3Name=="driver1Cat"){
      
    plotData$split3<-factor(plotData$split3,
                          levels=c("Silviculture for productivity","Silviculture for removals","Supply chain", "Technology","Multiple strategies"),
                          labels=c( "Mobilize additional wood by increased forest growth","Mobilize additional wood by increased forest harvest","Make better use of wood",  "Use wood instead of other ressource","Multiple strategies")
  )
    }
  }else{
  plotData$split3<-"dummySplit3"
}
  return(data.frame(plotData))

}

# forestPlotData.assumptionC<-forestPlotDataFunc(plotData.assumptionC,"modelAssumption")
# forestPlotData.driverC<-forestPlotDataFunc(plotData.driverC,"driver1","driver1Cat") #set includeSplit2 to TRUE
# forestPlotDataFunc(plotData.split.yr,splitName,"omitSplit2")

# plotData<-plotData.assumptionC
# split<-"modelAssumption"
# split2<-"driver1Cat"



forestPlotDataFunc<-function(plotData,split,split2){
  plotData$recordID<-rownames(plotData)
  log_print(tibble(data.frame(PaperID=plotData$PaperID,split=plotData$split,substitution=plotData$substitution),n=nrow(plotData$PaperID)))
  if(length(table(plotData[,split])[table(plotData[,split])!=0]) > 1){
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
  }else{
    print("only 1 split level")
    mod.model <- rma.mv(yi = substitution,
                        V = sei,
                        slab = PaperID,
                        data = plotData,
                        random = ~ 1 | PaperID/recordID,
                        test = "t",
                        method = "REML")
    forestPlotData<-coef(summary((mod.model)))
    if(length(levels(plotData$split))==1){
      forestPlotData$split<-levels(plotData$split)
    }else{
      stop("in forestPlotDataFunc, incorrect number of levels for split")
    }
  }
   

  log_print(tibble(forestPlotData,n=nrow(forestPlotData)))

  if(split2=="omitSplit2"){
    countStudySplit <- aggregate(substitution ~ split, aggregate(substitution~PaperID+split,plotData,mean), length)
    colnames(countStudySplit)<-c("split","nStud")
    countRecordSplit <- aggregate(substitution ~  split+split2, plotData, length)
    colnames(countRecordSplit)<-c("split","split2","nRec")


    forestPlotData<-forestPlotData[forestPlotData$split %in% levels(factor(plotData$split)) ,]
    forestPlotData<-merge(forestPlotData,countRecordSplit[,c("nRec","split")],by=c("split"),all.x=TRUE)
    forestPlotData<-merge(forestPlotData,countStudySplit[,c("nStud","split")],by=c("split"),all.x=TRUE)

  }else{

    countStudySplit <- aggregate(substitution ~ split+split2, aggregate(substitution~PaperID+split+split2,plotData,mean), length)
    colnames(countStudySplit)<-c("split","split2","nStud")
    countRecordSplit <- aggregate(substitution ~  split+split2, plotData, length)
    colnames(countRecordSplit)<-c("split","split2","nRec")
    forestPlotData<-merge(forestPlotData,unique(plotData[,c("split","split2")]),by="split",all.x=T)
    forestPlotData<-forestPlotData[forestPlotData$split %in% levels(factor(plotData$split)) &
                                     !(forestPlotData$split2)%in%c("Demand" ,"Environmental change" ),]
    forestPlotData<-merge(forestPlotData,countRecordSplit[,c("nRec","split","split2")],by=c("split","split2"),all.x=TRUE)
    forestPlotData<-merge(forestPlotData,countStudySplit[,c("nStud","split","split2")],by=c("split","split2"),all.x=TRUE)



  }

  # countStudySplit$nStud<-round(countStudySplit$substitution,2)
  #  countRecordSplit$nRec<-round(countRecordSplit$substitution,2)
  listSplits<-countRecordSplit[,"split"]
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
  

  return(forestPlotData)

}


#knowDynamicsData(data_expt_approach,c("Whole sector approach","Technology approach","Ecosystem approach"),"modelApproach","Hybrid approach")
#data_expt_split<-data_expt_approach
#include_split_levels<-c("Whole sector approach","Technology approach","Ecosystem approach")
#splitName<-"modelApproach"
#splitExclude<-"Hybrid approach"

# forestPlotData.assumptionC.dyn<-knowDynamicsData(data_expt_assumption,,"modelAssumption","Hybrid assumption")
# forestPlotData.approachC.dyn<-knowDynamicsData(data_expt_approach[data_expt_approach$modelApproach!="Hybrid approach",],"modelApproach")
# knowDynamicsData(data_expt_assumption[data_expt_assumption$modelAssumption != "Hybrid assumption",],"modelAssumption")

# data_expt_split<- data_expt_assumption[data_expt_assumption$modelAssumption != "Hybrid assumption",]
# splitName<-"modelAssumption"


knowDynamicsData<-function(data_expt_split,splitName){
  if(splitName=="modelApproach"){
    data_expt_split$split<-data_expt_split$modelApproach
  }else if(splitName=="modelAssumption"){
    data_expt_split$split<-data_expt_split$modelAssumption
  }else if(splitName=="driver1"){
    data_expt_split$split<-data_expt_split$driver1
    
  }
  
  forestPlotData.split.dyn<-data.frame()
  for(i in seq(2002,2022,1)){
    log_print(i)
    print(i)
    data_expt_split_yr<-data_expt_split[data_expt_split$Publication_Year<=i  &!is.na(data_expt_split$split),]
    if( length(data_expt_split_yr$split) >= 1){
      plotData.split.yr<-plotDataFunc(data_expt_split_yr, "outliers_in",splitName,"omitSplit2","omitSplit3")
      forestPlotData.split.yr<-forestPlotDataFunc(plotData.split.yr,splitName,"omitSplit2")
      forestPlotData.split.yr$year<-as.character(i)
      forestPlotData.split.dyn <-rbind(forestPlotData.split.dyn,forestPlotData.split.yr)      
    }
   
  }
  return(forestPlotData.split.dyn)
}

FreqDynamics<-function(data_expt_assumption,categoriesdf){
  
  for(i in 2001:2022){
    for(assumption in c("All C pools","Biogenic only assumption","Carbon neutral forest and HWP assumption")){
      
      if(nrow(data_expt_assumption[data_expt_assumption$Publication_Year<=i &
                                   data_expt_assumption$modelAssumption==assumption
                                   ,colnames(data_expt_assumption) %in% categoriesdf[categoriesdf$cat0 %in% c("Drivers","Metadata","Model"),'names']])>0){
        
        
        expt_freqY<-funcFreq(data_expt_assumption[data_expt_assumption$Publication_Year<=i &
                                                    data_expt_assumption$modelAssumption==assumption
                                                  ,colnames(data_expt_assumption) %in% categoriesdf[categoriesdf$cat0 %in% c("Drivers","Metadata","Model"),'names']],categoriesdf)
        
        expt_freqY$year<-i
        expt_freqY$modelAssumption<-assumption
        if(exists("expt_frq_dyn")){
          expt_frq_dyn<-rbind(expt_frq_dyn,expt_freqY)
        }else{
          expt_frq_dyn<-expt_freqY
        }
      }
    }
  }
  return(expt_frq_dyn)
}

# 
# create_knowDynamicsPlot<-function(forestPlotData.approachC.dyn){
#   p<-ggplot(forestPlotData.approachC.dyn,aes(x=year,y=substitution,color=split))+
#        scale_color_manual(values=c("Ecosystem approach"="#0072B2", "Technology approach"="#D55E00","Whole sector approach"="black",
#                                    "Biogenic only assumption"="#0072B2","Carbon neutral forest assumption"="#009E73", "Carbon neutral forest and HWP assumption"="#D55E00","All C pools"="black"))+
# 
#     geom_point( data=forestPlotData.approachC.dyn,
#                 size=3,
#                 position=position_dodge(width = 0.9)) +
#     geom_errorbar( data=forestPlotData.approachC.dyn,
#                    aes(ymin = ci.lb, ymax = ci.ub),
#                    linewidth=1.,
#                    width=1,
#                    position=position_dodge(width = 0.9))+  #add CIs as error bars
#     theme_bw()+
# 
#     theme( axis.ticks = element_blank(),
#            text = element_text(size=txt_size),
#            axis.text.x = element_text(angle = 45,hjust=1,vjust=1)
#     )+
#     stat_summary(aes(group = 1), fun.data = mean_se, geom = "ribbon", 
#                  fill = "pink", alpha = 0.6)+
#     labs( y = "Carbon balance (tCO2/m3)",x="")+
# 
#     geom_hline(yintercept=0)
#   print(p)
# 
# }

# forestPlotData.assumptionC.dyn
# forestPlotData.dyn<-forestPlotData.assumptionC.dyn
create_forest_plot_dynamics<-function(forestPlotData.dyn){
  p<-ggplot(forestPlotData.dyn,aes(x=year,y=substitution,color=split))+
    scale_color_manual(values=c("Ecosystem approach"="#0072B2", "Technology approach"="#D55E00","Whole sector approach"="black",
                                "Biogenic only assumption"="#0072B2","Carbon neutral forest assumption"="#009E73", "Carbon neutral forest and HWP assumption"="#D55E00","All C pools"="black"))+
  
#    stat_summary(fun=median, aes(color=split), geom="point",  shape=15, size=3, show.legend=FALSE) +
 
    geom_point( data=forestPlotData.dyn,
                size=3,
                position=position_dodge(width = 0.9)) +
    geom_errorbar( data=forestPlotData.dyn,aes(ymin = ci.lb, ymax = ci.ub),
                   linewidth=1.,
                   width=1,
                   position=position_dodge(width = 0.9)) + #add CIs as error bars
    theme_bw()+
    theme(
      axis.ticks = element_blank(),
      #axis.title.x = element_blank(),
      #axis.title.y = element_blank(),
      text = element_text(size=16),
      axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0)
    )+
    # geom_text(data=forestPlotData.dyn,
    #           aes(label=paste0(round(substitution,2)," ( ",nStud,"|",nRec,signif," )" )),
    #           hjust=1.5,
    #           size=4,
    #           angle = 90,
    #           position=position_dodge(width = 0.9),
    #           #nudge_x=0.2
    #           ) +
    
    geom_hline(yintercept=0)
  #+
   # coord_flip(y = c(-6,6),clip="off")
  
  p<-p+labs(x="",y="Carbon balance (tCO2/m3)")
  
  print(p)
  
}



flowchart_data<-function(data_bibliom_all,dataWoS){
  # Number of papers identified from WoS
  nWoS<-length(unique(dataWoS$PaperID))
  # Number of manual additions of papers that were not identified by WoS query
  dataManual<-data_bibliom_all[!(data_bibliom_all$PaperID %in% dataWoS$PaperID),]
  nManual<-length(unique(dataManual$PaperID))
  inclusionTable<-data.frame(
    Var1=c("nWoS","nManual"),
    Freq=c(nWoS,nManual),
    Cat=c("Web Of Science","Manual addition")
    )
  
  # Number of processed papers
  nBibliom<-length(unique(data_bibliom_all$PaperID))
  data_bibliom_all$Cat <- factor(data_bibliom_all$Exclusion,
                                    levels=c("no journal","not english","duplicate data",    "review",     "different perimeters" ,"no driver",   "no change in wood use","C var cannot be cumulated","no C output",    "no delta wood",    "included"),
                                    labels=c("Screening" ,"Screening",  "Data originality","Data originality","Study design",      "Study design","Study design",            "Results displayed",   "Results displayed","Results displayed","Included")
  )
  #Describe exclusion criteria
  exclusionTable<-data.frame(table(data_bibliom_all$Exclusion))
  exclusionTable<-merge(exclusionTable,unique(data_bibliom_all[,c("Exclusion","Cat")]),by.x="Var1",by.y="Exclusion")
  
  return(rbind(inclusionTable,exclusionTable[order(exclusionTable$Cat),]))
}

debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}


