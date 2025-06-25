library(readxl)
source(file.path("./functions.R"))

rawDataPath<-file.path("rawData/")
initDataPath<-file.path("initData/")
wwwDataPath<-file.path("www/")
database.file <-file.path(rawDataPath,"/database_substitution_metaanalysis.v5.v6.QC7.ALL.xlsx")
wosQueryResults.file<-file.path(rawDataPath,"wosQueryResults/savedrecs_2000-2022.xls")

dataFlux.file<-paste0(rawDataPath,"TableForestCCycleSynthesis.3.xlsx")



color_fossil<- as.character("#332288ff")
color_biogenic<-as.character("#cc6677ff")
color_sum<-"black"
palette_C<-c(color_fossil,"grey",color_biogenic)
color_insitu<-as.character("#117733ff")
color_exsitu<-as.character("#ddcc77ff")
palette_situ<-c(color_insitu,"grey",color_exsitu)
# ==========================Process data
palette<-read_excel(database.file,sheet=3,col_names=TRUE)


wood_type_names<-c(
  `UpstreamInput` = "Upstream",
  `mixedProduct` = "Mixed products",
  `TimberInput` = "Timber",
  `PulpPaperInput` = "Pulp and paper",
  `EnergyInput` = "Energy",
  `All`='All types of wood use'
)



refCProcess<-GlobalFluxData(dataFlux.file)
refCProcessMean<-aggregate(`value GtCO2/yr`~substitutionDatabaseVariable,data=refCProcess,FUN=mean,na.rm=T)
refCProcessMean$`value GtCO2/yr`<-as.numeric(refCProcessMean$`value GtCO2/yr`)

# -------------
# ------------- Read header of data and palette to have a lookup table for category of variables along with their colors
categories<-t(read_excel(database.file,n_max=7,col_names=FALSE))
categoriesdf<-as.data.frame((categories),row.names=NULL )
colnames(categoriesdf)<-c("cat0","cat1","colcat1","cat2","colcat2","cat3","names")
categoriesdf$id<-1:nrow(categoriesdf)

# -------------
# ------------- Read corpus of data -> output = data
dataWoS<-read_excel(wosQueryResults.file)
data<-read_excel(database.file,skip=6)

## Make country index Titlecase and prepare country list
data$country<-str_to_title(data$country)


data_bibliom_all<-bibliom(data)
data_bibliom<-bibliom_in(data)
data_study<-study(data)
data_expt<-expt(data)
data_expt_approach<-assignApproach(data_expt)
study_freq<-funcFreq(data_study,categoriesdf)
expt_freq<-funcFreq(data_expt,categoriesdf)
countryRefData<-readCountryData(rawDataPath)
countryFreqData<-countryFreq(data_study,countryRefData)

singleProductVect<-c('UpstreamInput','TimberInput','PulpPaperInput','EnergyInput','mixedProduct') 

map.db<-NULL 
map.world<-NULL 

map.world <- map_data('world') 
map.world<-map.world[map.world$lat>-58,] 
nInit<-dim(map.world)[1] 

map.world<-map.world %>% slice(rep(1:n(), each = 5)) 
map.world$singleProduct<-rep(singleProductVect,len=nInit) 
map.world$region<-str_to_title(map.world$region) 


countriesList <- sort(unique(data_study$country))
countriesEurope <- c("Austria", "Denmark","Finland", "France", "Germany", "Ireland", "Lithuania", "Norway", "Portugal", "Sweden", "Switzerland", "Uk", "Ukraine")

timeHorizon <- sort(unique(data$time_horizon))
timeHorizonFrstElmt <- timeHorizon[1]
timeHorizon <- timeHorizon[-1]
timeHorizonList <- c(timeHorizon, timeHorizonFrstElmt)

scaleAgg <- unique(data$scaleAgg)
productsList <- sort(unique(data$singleProduct))
productsLabels<-gsub("([a-z])([A-Z])","\\1 \\2",str_remove(productsList,'Input'))

dict = list(  loc="Local scale",
              reg="Regional scale",
              w= "Global scale",
              TimberInput="Timber product",
              EnergyInput="Energy product",
              mixedProduct="Mixed product",
              PulpPaperInput="Pulp and paper product",
              soilC="Soil carbon",
              harv_residues="Harvest residues",
              live_biomass_C="Live biomass", 
              products_storage_C="C storage in products",
              forestry_emiss="Forestry emiss.",
              manufacturing_emiss="Manufacturing emiss.",
              maintenance_emiss="Maintenance emiss.",
              eol_biogenic="End-of-life biogenic emiss.",
              eol_fossil_emiss="End-of-life fossil emiss.",
              off_product_biogenic="Avoided emiss.",
              biogenic_dyn="Dyn. of biogenic emiss.",
              fossil_dyn="Dyn. of fossil emiss.",
              LUC_dyn="Land use change dyn.",
              rebound_dyn="Market-based dyn.",
              Demand="Demand",
              Environment="Environmental change",
              MultipleStrategies="Multiple strategies",
              Silviculture="Silviculture for productivity",
              SupplyChain="Supply chain",
              Technology="Technology"
              
)
processes <- sort(unique(colnames(data_expt)[colnames(data_expt) %in% categoriesdf[categoriesdf$cat2 %in% c( "C fluxes"),'names']]))
processesLabels<-as.vector(unlist(dict[match(processes,(names(dict)))]))
processesList<-setNames(processes,processesLabels)

dynamics <- sort(unique(colnames(data_expt)[colnames(data_expt) %in% categoriesdf[categoriesdf$cat2 %in% c( "Dynamics"),'names']]))
dynamicsLabels<-as.vector(unlist(dict[match(dynamics,(names(dict)))]))
dynamicsList<-setNames(dynamics,dynamicsLabels)

driver1 <-sort(unique(data$driver1))
driver1Labels <-str_to_title(driver1)
driver1List<-setNames(driver1,driver1Labels)

driver1Cat <-sort(unique(data$driver1Cat))
driver1CatLabels<-driver1Cat
driver1CatList<-setNames(driver1Cat,driver1Cat)

valSub <- round(sort(unique(data$substitution)), digits = 1)
minValSub <- min(valSub, na.rm = TRUE)
maxValSub <- max(valSub, na.rm = TRUE)

data_expt_approachResults<-assignApproach(data_expt)

plotData.approachC<-plotDataFunc(data_expt_approachResults, c("Whole sector approach","Technology approach","Ecosystem approach"),NULL,"modelApproach")
forestPlotData.approachC<-forestPlotDataFunc(plotData.approachC,"modelApproach")


plotData.driverC<-plotDataFunc(data_expt_approachResults, c("Whole sector approach"),NULL,"driver1")
forestPlotData.driverC<-forestPlotDataFunc(plotData.driverC,"driver1","driver1Cat") #set includeSplit2 to TRUE

tTestPairsSignifAggVarMelt<-modelComponentsC(data_expt_approachResults,c("soilC","harv_residues","live_biomass_C","products_storage_C","forestry_emiss","manufacturing_emiss","maintenance_emiss","eol_biogenic","off_product_biogenic","biogenic_dyn","fossil_dyn")
                                             , "",("PaperID"))


forestPlotData.approachC.dyn<-knowDynamicsData(data_expt_approach)


save(refCProcessMean,
     countriesList,
     productsLabels,
     productsList,
     timeHorizonList,
     processesList,
     dynamicsList,
     driver1CatList,
     driver1List,
     wood_type_names,
     minValSub,
     maxValSub,
     data,
     categoriesdf,
     countryRefData,
     data_expt,
     data_expt_approach,
     data_bibliom,
     plotData.approachC,
     forestPlotData.approachC,
     tTestPairsSignifAggVarMelt,
     forestPlotData.approachC.dyn,
     plotData.driverC,
     forestPlotData.driverC,
     file=paste0(initDataPath,"initData.Rdata"))
###





