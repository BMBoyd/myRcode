#' Read in Genie report files and calculate activities
#'
#' @param type type of efficiency to use
#' @param folder name of folder where reports are located. If NULL (default) user prompted for location
#' @param csv logical; write a csv of the output to the working directory
#' @param raw logical; returns raw data
#' @param filename filename of output csv. Defaults to folder name.
#'
#' @export DataMine

DataMine <- function( type = c("half jar", "full jar", "regression"), folder = NULL, csv = TRUE, raw = FALSE, filename = "DEFAULT"){
  Radionuclides <- function(){
    Pb210 <-   c(47,"Pb210")
    Am241 <-   c(60,"Am241")
    Th234 <-   c(63,"Th234")
    Ra226 <-   c(186,"Ra226")
    Pb212 <-   c(239,"Pb212")
    Pb214.1 <- c(295, "Pb214")
    Ac228 <-   c(338, "Ac228")
    Pb214.2 <- c(352, "Pb214")
    Be7  <-    c(477, "Be7")
    Bi214 <-   c(609, "Bi214")
    Cs137 <-   c(661, "Cs137")
    K40<-      c(1460, "K40")
    Nuclides<-list(Pb210,Am241 ,Th234,Ra226,Pb212,Pb214.1,Ac228,Pb214.2,Be7,Bi214,Cs137,K40)
    Library<-do.call(rbind.data.frame, Nuclides)
    names(Library) <- c("energy","nuclide")
    Library$energy<-as.numeric(as.character(Library$energy))
    Library$nuclide<-as.character(Library$nuclide)
    Library
  }


    ### Check arguments and look for calibration efficiencies ----
    if(is.null(type)) (stop("No efficiency type selected"))
    if(!(type %in% c("half jar", "full jar", "regression"))) (stop("Efficiency must be \"half jar\", \"full jar\", or \"regression\""))
    if(!file.exists("../EffCalib.csv"))("EffCalib.csv is missing from parent directory")
    if(is.null(folder)){
      folder <-readline("What is the name of the folder? \n ")
      while(!file.exists(folder)){
        message("Directory does not exist. Please try again.")
        folder <-readline("What is the name of the folder? \n ")
      }
    }
    ### Read in Genie Reports  ----
    RPTfiles<-as.list(list.files(path=folder, pattern="\\.RPT$", recursive = T, include.dirs = F)) # read in list of Genie2K RPT files
    ## searches subdirectories
    if(length(RPTfiles)==0)(stop("No report files present in directory."))
    for(i in RPTfiles){ # find empty rpt files
      if(file.info(paste(folder,i,sep="/"))$size==0){
        message(paste(i,"is empty"))
      }
    }
    DATA <- {} # create empty object to populate
    for(i in RPTfiles){ # for every report in the directory
      DATA[[i]] <- read.csv(paste(folder,i,sep="/"),header=T, strip.white=TRUE)} # import and bind the DF to a list
    DATA <- rbind.fill(DATA) # bind DFs in list into a single DF
    for(i in 5:10){ # class numbers as numeric for calculation
      DATA[,i]<-as.numeric(DATA[,i])
    }
    ## Add in uniform detector name
    detector<-sapply(DATA$Detector,
                     function(x)
                       if( grepl("1", x)) {"LEGe1"}
                     else if( grepl("2", x)) {"LEGe2"}
                     else if(x=="BEGE") {"BEGe"}
                     else if(x=="WELL") {"WELL"})
    DATA <- cbind(DATA, detector=detector)
    # DATA <- cbind(DATA, detector)
    ## efficiency type
    if(length(type)==1){
      EffType<-type
    } else {
      EffType<-{}
      type<-data.frame(type=type,sample=as.numeric(1:length(type)))
      for(i in levels(as.factor(DATA$depth))){ # i <-levels(as.factor(DATA$depth))[3]
        sample.number<-which(levels(as.factor(DATA$depth))==i)
        # this returns the level number ie sample number
        eff.type<-as.character(type[type$sample==sample.number,1])
        # what is the efficiency to be used for this sample ?
        eff.type<-rep(eff.type, length(DATA$depth[DATA$depth==i]))
        # repeat the efficiency type as many times as there are peaks for the sample
        # eff.type<- as.data.frame(eff.type)
        # create a df
        EffType<-rbind(EffType, as.data.frame(eff.type))
        # rbind these vectors
      }}
    # bind efficiency type to data
    DATA <- data.frame(DATA[,1:3], eff.type=EffType, DATA[,4:length(DATA)])
    ## normalize peak centroid energies from GENIE to match energies in efficiency file
    Library<-Radionuclides() # (above) create df of energies names(Library) <- c("Energy","Nuclide")
    EffEnergy<-lapply(DATA$Energy, # GENIE Peak Centroid Energy
                      function(e)
                        if(abs(Library$energy[which.min(abs(Library$e - e))]-e)<4)
                          # what efficiency energy is closest to the Sample energy ?
                          (Library[which.min(abs(Library$energy - e)), 1:2])
                      else (c(NA,NA)))
    # if no match, return NA
    EffEnergy<-do.call(rbind.data.frame, EffEnergy)
    # create df of energies and nuclide names
    significant<- CountSig(DATA$NetPeakArea, DATA$NetAreaUncert, DATA$ContinuumCounts,DATA$ROILength,4,1.645)
    # run CountSig on all peaks
    DATA<-cbind(DATA,EffEnergy,significant)
    DATA$eff.type<-as.character(DATA$eff.type)
    # cbind efficiency energies and significant logical trigger to DATA df
    ## add in energy placeholder for nuclides undefined in EffCalib
    DATA$energy[is.na(DATA$energy)] <- DATA$Energy[is.na(DATA$energy)]
    ## Add in efficienies
    EffCalib <- read.csv("../EffCalib.csv")
    EffCalib$Detector<-as.character(EffCalib$Detector)
    EffCalib$Type<-as.character(EffCalib$Type)
    EffCalib$Standard <- as.character(  EffCalib$Standard)
    # check for multiples efficiencies
    if(length(unique(DATA$eff.type))>1){
      message("More than one detector efficiency type detected")
      print(unique(DATA$eff.type))
    }
    ## If more than one standard is available
    if( nrow(unique(subset(EffCalib, Detector==unique(DATA$detector) & Type == unique(DATA$eff.type), select = Standard))) > 1) {
      message("Multiple efficiencies available")
      STANDARD <- as.character(select.list( unique(EffCalib[ EffCalib$Detector == unique(DATA$detector) & EffCalib$Type == unique(DATA$eff.type), "Standard"]), title = "Select Standard"))
    } else {
      STANDARD <- unique(subset(EffCalib, Detector==unique(DATA$detector) & Type == unique(DATA$eff.type), select = Standard))
    }
    EffCalib <- EffCalib[ EffCalib$Detector %in% DATA$detector & EffCalib$Standard %in% STANDARD & EffCalib$Type %in% DATA$eff.type, ]
    ## merge EffCalib into Data preserving all DATA rows
    DATA <- merge( DATA, EffCalib, by.x = c( "detector", "eff.type", "energy"), by.y = c("Detector", "Type", "Energy"), all.x = T, sort = F)
    ## Get efficiencies for regression samples
    need <- is.na(DATA$Efficiency)
    DATA$Efficiency[need] <- DATA$Mass[need] * DATA$Slope[need] + DATA$Intercept[need]
    ## Calculate Activities and errors
    ## get activities
    need <- DATA$significant == "Significant"
    DATA$activity[need] <- round(1000 * (DATA$NetPeakArea[need]/DATA$CountTime[need])*1/(DATA$Mass[need] * DATA$Efficiency[need]), digits = 2)
    # get upper limit activity
    DATA$activity[!need] <- round(1000 * (as.numeric(as.character(DATA$significant[!need]))/DATA$CountTime[!need])*1/(DATA$Mass[!need] * DATA$Efficiency[!need]), digits = 2)
    ## multiple radium/uranium doublet by ratio to get radium
    need2 <- which(DATA$energy==186)
    DATA$activity[need2] <- round(DATA$activity[need2]*0.571, digits = 2)
    ## get error
    DATA$error[need] <- round(DATA$activity[need] * (DATA$NetAreaUncert[need]/DATA$NetPeakArea[need]), digits=2)
    ## MDA
    Ld <- 2*CriticalLimit(DATA$ContinuumCounts, DATA$ROILength, 4, 1.645)
    DATA$MDA <- round(1000 * (Ld/DATA$CountTime)*1/(DATA$Mass * DATA$Efficiency), digits = 2)

    # return df of raw data
    if(raw==T) assign(paste(folder,"_raw",sep=""),DATA, envir=.GlobalEnv)

    # DATA<-DATA[!is.na(DATA$efficiency),] # rm rows with no activity
    ## Create a df for each energy
    acts.errs<-{} # null object
    for(i in unique(DATA$energy)){ # for each depth in DATA
      Sample <- DATA[ DATA$energy == i,]
      activity.error <- Sample[ , c("SampleID", "NetPeakArea", "NetAreaUncert", "ContinuumCounts","ROILength", "activity", "error", "MDA")]
      ## Add in nuclide and energy to name for column titles
      names(activity.error)[2:8] <- paste(Sample$energy[1],Sample$nuclide[1], names(activity.error)[2:8], sep="_")
      ## add df to list
      acts.errs[[which(unique(DATA$energy) ==i)]]<-activity.error#[,-8]
    }
    ## Merge list of energy df into one gamma df
    merge.all <- function(x, y) {
      merge(x, y, all=TRUE, by= "SampleID")
    }
    Gamma <- Reduce( merge.all, acts.errs)
    front <- unique(DATA[ ,c("SampleID", "Mass", "detector", "eff.type", "CountDate", "CountTime")])
    Gamma <- merge( front, Gamma, by = "SampleID")

    ## Add in sample identifiers with desired columns
    Core <- str_extract(Gamma$SampleID, "[a-zA-Z]+[- \\S][0-9]")
    depths <-str_match(Gamma$SampleID, "([0-9]{1,3})[-]([0-9]{1,3})")
    Interval <- depths[,1]
    depth <- apply(cbind(as.numeric(depths[ ,3]),as.numeric(depths[ ,2])), 1, mean)
    Gamma <- data.frame(Core, Interval, depth, Gamma[, 2:length(Gamma)])
    Gamma <- Gamma[ order(Gamma$Core, Gamma$depth), ]
    ###############################
    # File Saving and Output
    ###############################
    if( filename == "DEFAULT"){
      #     filename <-  data.frame(strsplit(folder,split="\\\\"))[max(length(data.frame(strsplit(folder,split="\\\\"))[,1])),]
      filename<-folder
    }
    if(csv == T) {
      Gamma.2<-Gamma
      Gamma.2$Interval<- paste("'", Gamma.2$Interval, sep="") # keep interval from changing to date in excel
      new.names<-gsub("_"," ",colnames(Gamma.2))
      new.names<-gsub("X","",new.names)
      colnames(Gamma.2) <- new.names
      write.csv(Gamma.2, paste(filename,".csv", sep=""))
      Gamma
    } else {
      Gamma
    }
  }
