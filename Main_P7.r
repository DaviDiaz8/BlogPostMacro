rm(list=ls()) #clear workspace

#################################
#Proyecto - Macroeconomía - Comparativa P7 y P8 
#Script para correr el modelo en escenarios P7 y P8
#
#Autor: Manuel David Ventosa Gregorio
#Fecha: Julio 2025 
#
# Basado en:
# - Script original elaborado por el Dr. Edmundo Molina-Pérez
# - Adecuaciones realizadas por Fabián Fuentes (compañero de la maestría)
#
# "This script runs the International Green Tech Change Model in parallel across a set of experimental design scenarios"
############################################

#load libraries

# If necessary, install packages
#install.packages(c('parallel', 'snow', 'deSolve', 'optimx', 'reshape2', 'data.table'))
library(parallel)
library(snow)
library(deSolve)
library(optimx)
library(reshape2)
library(data.table)

## Set project root dynamically based on the current working directory
root<-"/Users/daviddiazherrera/Desktop/Ediam_model/Main/"
# Set number of cores for parallel processing
Number.Cores <- parallel::detectCores()

## ==================================================================================================================================================
## This section runs the model across the experimental design and prints an output file for each run
## ==================================================================================================================================================

#Source Model
dir.model <- file.path(root, "model")
model.version<-"InternationalGreenTechChangeModel_10_22_2015.r"
source(file.path(dir.model, model.version))
  
#Source Experimental Design
dir.exp.inputs <- file.path(root, "RDM Inputs//")

# Load experimental design

# Define the version of the experimental design to be used
experiment.version <- "Exp.design_P10.csv"
Exp.design <- read.csv(file.path(dir.exp.inputs, experiment.version))

#check policy names
table(Exp.design$policy.name)


#Define directory to print output files
dir.harness <- file.path(root, "RDM Harness//")

#Clean output folder
do.call(file.remove, list(file.path(dir.harness, list.files(dir.harness, pattern = "*.csv", full.names = FALSE))))

#Set up parallel environment

#Run Model in Parallel
nCore<-Number.Cores

cl <- makeSOCKcluster(names = rep('localhost',nCore))

global.elements<-list("Exp.design","TechChangeMod","dir.harness","dede","lagderiv","lagvalue","optimx") # dede, lagderiv are functions od deSolve

clusterExport(cl,global.elements,envir=environment())
 
 #Execute code
  parApply(cl,Exp.design,1,function(x) {
                        params<-c(
                        S.0 = as.numeric(x['S.0']),
                        TimeStep = as.numeric(x['TimeStep']),
                        EndTime = as.numeric(x['EndTime']),
                        alfa = as.numeric(x['alfa']),
                        epsilon = as.numeric(x['epsilon']),
                        Gamma.re = as.numeric(x['Gamma.re']),
                        k.re = as.numeric(x['k.re']),
                        Gamma.ce = as.numeric(x['Gamma.ce']),
                        k.ce = as.numeric(x['k.ce']),
                        Eta.re = as.numeric(x['Eta.re']),
                        Eta.ce = as.numeric(x['Eta.ce']),
                        Nu.re = as.numeric(x['Nu.re']),
                        Nu.ce = as.numeric(x['Nu.ce']),
                        qsi = as.numeric(x['qsi']),
                        Delta.S = as.numeric(x['Delta.S']),
                        Delta.Temp.Disaster = as.numeric(x['Delta.Temp.Disaster']),
                        Beta.Delta.Temp = as.numeric(x['Beta.Delta.Temp']),
                        CO2.base = as.numeric(x['CO2.base']),
                        CO2.Disaster = as.numeric(x['CO2.Disaster']),
                        labor.growth_N = as.numeric(x['labor.growth_N']),
                        labor.growth_S = as.numeric(x['labor.growth_S']),
                        lambda.S = as.numeric(x['lambda.S']),
                        sigma.utility = as.numeric(x['sigma.utility']),
                        rho = as.numeric(x['rho']),
                        Yre.0_N = as.numeric(x['Yre.0_N']),
                        Yce.0_N = as.numeric(x['Yce.0_N']),
                        Yre.0_S = as.numeric(x['Yre.0_S']),
                        Yce.0_S = as.numeric(x['Yce.0_S']),
                        size.factor = as.numeric(x['size.factor']),
                        Run.ID = as.numeric(x['Run.ID']),
                        policy.name = as.character(x['policy.name']),
                        dir.harness = dir.harness);

#Policy dimensions

#tax north
 ceN.0<-ifelse(x['Climate.Model']%in%c("GFDL-ESM2G","GFDL-ESM2M")==TRUE,0.30,
        ifelse(as.numeric(x['epsilon'])<8,0.25,
 		ifelse(as.numeric(x['epsilon'])<9,0.20,
 		ifelse(as.numeric(x['epsilon'])<10,0.15,0.10))));#initial tax north
 ceN.m<-0.05 ;#min tax north
 ceN.M<-0.5 ;#max tax north
 #ceN.M<-1.0 ;#max tax north

#tax south
 ceS.0<-ifelse(x['Climate.Model']%in%c("GFDL-ESM2G","GFDL-ESM2M")==TRUE,0.30,
        ifelse(as.numeric(x['epsilon'])<8,0.25,
		ifelse(as.numeric(x['epsilon'])<9,0.20,
 		ifelse(as.numeric(x['epsilon'])<10,0.15,0.10)))) ;#initial tax south

 ceS.m<-0.05 ;#min tax south
 ceS.M<-0.5 ;#max tax south
 #ceS.M<-1.0 ;#max tax south

#Tech Subsidy North
 tN.0<-0.10 ;#initial Tech Subsidy North
 tN.m<-0.01 ;#min tax Tech Subsidy North
 tN.M<-0.15 ;#max tax Tech Subsidy North

#RD Subsidy North
 sN.0<-2.0 ;#initial RD Subsidy North
 sN.m<-0.5 ;#min tax RD Subsidy North
 sN.M<-3.0 ;#max tax RD Subsidy North
 
#Tech Subsidy South
 tS.0<-0.05 ;#initial Tech Subsidy South
 tS.m<-0.01 ;#min tax Tech Subsidy South
 tS.M<-0.15 ;#max tax Tech Subsidy South
 
#RD Subsidy South
 sS.0<-0.50 ;#initial RD Subsidy South
 sS.m<-0.01 ;#min tax RD Subsidy South
 sS.M<-3.0 ;#max tax RD Subsidy South

if (x['policy.name']=="FWA")
 {
 TechChangeMod(c(0.0,0.0,0.0,0.0),params)

 } else{
       if (x['policy.name']=="Nordhaus")
        {
          optimx(c(ceN.0,ceS.0), TechChangeMod, lower=c(ceN.m,ceS.m), upper=c(ceN.M,ceS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01),parscale=c(10,10),maxit=200000),params=params)
         } else {
		 if (x['policy.name']=="Nordhauds+TechnologyPolicy")
		 {
		  optimx(c(ceN.0,ceS.0,tN.0,sN.0), TechChangeMod, lower=c(ceN.m,ceS.m,tN.m,sN.m), upper=c(ceN.M,ceS.M,tN.M,sN.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(10,10,3,3),maxit=200000),params=params)
		 } else {
		   if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund")
		  {
		   	optimx(c(ceN.0,tN.0,sN.0,tS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m), upper=c(ceN.M,tN.M,sN.M,tS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(10,3,3,3),maxit=200000),params=params)
		  } else {
		    if (x['policy.name']=="Nordhaus+R&DGreenClimateFund")
		     {
			  optimx(c(ceN.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01),parscale=c(10,3,3,3,3),maxit=200000),params=params)
			 } else {
			  if (x['policy.name']=="Nordhaus+TechnologyPolicy.Both")
		      {
			   optimx(c(ceN.0,ceS.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,ceS.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,ceS.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(10,10,3,3,3,3),maxit=200000),params=params)
			  } else {
			     if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund+R&DS")
		        {
			      optimx(c(ceN.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01),parscale=c(10,3,3,3,3),maxit=200000),params=params)
				} else {
			      if (x['policy.name']=="Nordhaus+CoR&DGreenClimateFund")
		           {
			         optimx(c(ceN.0,tN.0,sN.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,sS.m), upper=c(ceN.M,tN.M,sN.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(10,3,3,3),maxit=200000),params=params)
				   } else {
				     if (x['policy.name']=="Nordhaus+CoR&DGreenClimateFund+TecS")
		              {
			            optimx(c(ceN.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(ceN.m,tN.m,sN.m,tS.m,sS.m), upper=c(ceN.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01),parscale=c(10,3,3,3,3),maxit=200000),params=params)
				      } else { "NA" }}}}}}}}}
	})
   #Stop cluster
   stopCluster(cl)


## =====================================================================================================
## This section reads the output of simulations and reshapes into a format ready for scenario discovery
## =====================================================================================================

Number.Cores <- parallel::detectCores()

#Define directory parameters
root<-"/Users/daviddiazherrera/Desktop/Ediam_model/Main/"
dir.inputs <- file.path(root, "RDM Inputs//")
dir.harness <- file.path(root, "RDM Harness//")
dir.output <- file.path(root, "RDM Outputs//")

#create vector with file names
filenames <- list.files(dir.harness, pattern="*.csv", full.names = FALSE)

#source function to process harnessed output data
source(file.path(dir.inputs, "harness_processing.r"))

#run post-processing in parallel
nCore <- Number.Cores

cl <- makeSOCKcluster(names = rep('localhost',nCore))
global.elements<-list("dir.harness","process.prim.data","data.table")
clusterExport(cl,global.elements,envir=environment())

  prim.data <- parLapply(cl,filenames, function(x){data.table(process.prim.data(x,dir.harness))} )
  stopCluster(cl)
  prim.data<-rbindlist(prim.data)

#merge data with experimental design
 
  prim.data <- merge.exp.design(dir.inputs,experiment.version,prim.data)

  table(prim.data$policy.name)

#create future without action consumption
  prim.data.fwa<-subset(prim.data,prim.data$policy.name=="FWA")
  table(prim.data.fwa$policy.name)

  prim.data.fwa <- prim.data.fwa[,c("Future.ID","Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Consumption.Total_N.300","Consumption.Total_S.300"),with=FALSE]
  
  setnames( prim.data.fwa,
  c("Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Consumption.Total_N.300","Consumption.Total_S.300"),
  c("Y.Total_N.fwa","Y.Total_S.fwa","Consumption.Total_N.fwa","Consumption.Total_S.fwa","Consumption.Total_N.300.fwa","Consumption.Total_S.300.fwa"))
  prim.data<-merge(prim.data,prim.data.fwa,by="Future.ID")

#standarize relative values
  prim.data$Z.Relative.Gamma<-prim.data$Gamma.re/prim.data$Gamma.ce
  prim.data$Z.Relative.Gamma<-scale(prim.data$Z.Relative.Gamma, center=TRUE, scale=TRUE)
  prim.data$Z.Relative.Eta<-prim.data$Eta.re/prim.data$Eta.ce
  prim.data$Z.Relative.Eta<-scale(prim.data$Z.Relative.Eta, center=TRUE, scale=TRUE)
  prim.data$Z.Relative.Nu<-prim.data$Nu.re/prim.data$Nu.ce
  prim.data$Z.Relative.Nu<-scale(prim.data$Z.Relative.Nu, center=TRUE, scale=TRUE)
  prim.data$Z.epsilon<-scale(prim.data$epsilon, center=TRUE, scale=TRUE)

  #write.csv(prim.data, file.path(dir.output, "prim.data_7_06_2015.csv"), row.names = FALSE)
  write.csv(prim.data, file.path(dir.output, "prim.data_extras_P10.csv"), row.names = FALSE)

## =====================================================================================================
## This section reads the output of simulations and reshapes it into time series split by region,
## =====================================================================================================
Number.Cores <- parallel::detectCores()
#Define directory parameters
root<-"/Users/daviddiazherrera/Desktop/Ediam_model/Main/"
dir.inputs <- file.path(root, "RDM Inputs//")
dir.harness <- file.path(root, "RDM Harness//")
dir.output <- file.path(root, "RDM Outputs//")

#crate vector with file names
 filenames <- list.files(dir.harness, pattern="*.csv", full.names = FALSE)

#source function to process harnessed output data
  source(file.path(dir.inputs, "harness_processing.r"))

#run post-processing in parallel
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("dir.inputs","experiment.version","dir.harness","process.harness.data")
  clusterExport(cl,global.elements,envir=environment())
  
  modelruns <- parLapply(cl,filenames, function(x){process.harness.data(x,dir.inputs,experiment.version,dir.harness)} )
  stopCluster(cl)
  modelruns<-rbindlist(modelruns)

#print time series for model
  write.csv(modelruns, file.path(dir.output, "model.runs_P10.csv"), row.names = FALSE)

  ## =====================================================================================================
  ## This section of the code is dedicated to create graphs in order to analize the data
  ## =====================================================================================================  

  install.packages(c('tidyverse', 'readr'))
  # 📦 load necesary packages
  library(tidyverse)
  library(readr)
  
  # 🔍 Load datasets
  model_runs <- read_csv("/Users/daviddiazherrera/Desktop/Ediam_model/Main/RDM Outputs/model.runs_P10.csv")
  prim_data <- read_csv("/Users/daviddiazherrera/Desktop/Ediam_model/Main/RDM Outputs/prim.data_extras_P10.csv")
  exp_design <- read_csv("/Users/daviddiazherrera/Desktop/Ediam_model/Main/RDM Inputs/Exp.design_P10.csv")
  
  # 📝 Cast Run.ID
  model_runs <- model_runs %>%
    mutate(Run.ID = as.character(Run.ID))
  
  exp_design <- exp_design %>%
    mutate(Run.ID = as.character(Run.ID))
  
  # 🔗 Join datasets
  full_data <- model_runs %>%
    left_join(exp_design, by = "Run.ID")
  
  # ✅ VVerify coolumns
  colnames(full_data)
  
  library(tidyverse)
  
  # Evolution of Temp
  full_data %>%
    ggplot(aes(x = time, y = Delta.Temp)) +
    geom_line(color = "steelblue") +
    facet_grid(rho.y ~ policy.name.y, labeller = labeller(policy.name.y = label_wrap_gen(50))) +
    labs(title = "Evolución de Temperatura por rho y política",
         x = "Año", y = "Delta.Temp (°C)") +
    theme_minimal()
  
  #Evolution of consumption
  full_data %>%
    ggplot(aes(x = time, y = Consumption)) +
    geom_line(color = "goldenrod") +
    facet_grid(rho.y ~ policy.name.y, labeller = labeller(policy.name.y = label_wrap_gen(50))) +
    labs(title = "Figura 1. Evolución del consumo por rho y estrategia",
         x = "Año", y = "Consumo") +
    theme_minimal()
  
  # Evolution of Are
  full_data %>%
    ggplot(aes(x = time, y = Are)) +
    geom_line(color = "royalblue") +
    facet_grid(rho.y ~ policy.name.y, labeller = labeller(policy.name.y = label_wrap_gen(50))) +
    labs(title = "Figura 2. Evolución de la productividad (energías limpias) por rho y estrategia",
         x = "Año", y = "Are") +
    theme_minimal()
  
  #EC02 emisions by time
  full_data %>%
    ggplot(aes(x = time, y = CO2.Concentration, color = policy.name.y)) +
    geom_line() +
    facet_wrap(~rho.y) +
    labs(title = "Figura 3. Concentración de CO2 por rho y estrategia",
         x = "Año", y = "Concentración de CO2 (ppm)", color = "Política") +
    theme_minimal() +
    theme(legend.position = "bottom")
  #Growth and strategy
  full_data %>%
    ggplot(aes(x = time, y = Y, color = as.factor(rho.y))) +
    geom_line() +
    facet_wrap(~policy.name.y, labeller = label_wrap_gen(50)) +
    labs(title = "Evolución del PIB (Y) por política y rho",
         x = "Año", y = "PIB total (Y)", color = "rho") +
    theme_minimal() +
    theme(legend.position = "bottom")
  #Investment
  full_data %>%
    ggplot(aes(x = time, y = Agg.demand.re.tech, color = as.factor(rho.y))) +
    geom_line() +
    facet_wrap(~policy.name.y, labeller = label_wrap_gen(50)) +
    labs(title = "Inversión en tecnologías limpias (Agg.demand.re.tech) por política y rho",
         x = "Año", y = "Inversión (Agg.demand.re.tech)", color = "rho") +
    theme_minimal() +
    theme(legend.position = "bottom")
  #Profits
  full_data %>%
    ggplot(aes(x = time, y = Profits.re, color = as.factor(rho.y))) +
    geom_line() +
    facet_wrap(~policy.name.y, labeller = label_wrap_gen(50)) +
    labs(title = "Ganancias en tecnologías limpias (Profits.re) por política y rho",
         x = "Año", y = "Profits.re", color = "rho") +
    theme_minimal() +
    theme(legend.position = "bottom")
  #SRE
  full_data %>%
    ggplot(aes(x = time, y = sre, color = as.factor(rho.y))) +
    geom_line() +
    facet_wrap(~policy.name.y, labeller = label_wrap_gen(50)) +
    labs(title = "Proporción de científicos en tecnologías limpias (sre) por política y rho",
         x = "Año", y = "sre", color = "rho") +
    theme_minimal() +
    theme(legend.position = "bottom")
  