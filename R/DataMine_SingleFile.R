#' Read in Genie report files and calculate activities
#'
#' @param type type of efficiency to use
#' @param folder name of folder where reports are located. If NULL (default) user prompted for location
#' @param csv logical; write a csv of the output to the working directory
#' @param raw logical; returns raw data
#' @param filename filename of output csv. Defaults to folder name.
#'

DataMine_SingleFile <- function(RPT = NULL, standard = NULL, efftype = NULL, CSV = T, FILENAME = "DEFAULT") {
  # RPT file name, presekct standard, preselect efficiency type, output csv

  if (is.null(RPT)) {
    message("Choose .RPT file")
    RPT <- file.choose()
  }

  if (file.info(RPT)$size == 0)
    stop(paste("Check empty report files",RPT))

  # Assing column classes
  cls <- c(SampleID = "character", Detector = "character", Mass = "numeric", CountDate = "character", CountTime =
             "numeric", Energy = "integer", NetPeakArea = "integer", NetAreaUncert = "integer", ContinuumCounts =
             "integer", ROILength = "integer")
  # import and bind the DF to a list
  DATA <- read.csv(RPT, header = T, colClasses = cls)

  DATA[, 1:2] <- lapply(DATA[, 1:2], FUN = function(x) gsub("[[:space:]]*$", "", x))

  # Add the detector type to the sheet
  detector <- sapply(DATA$Detector, function(x) {
    if (x == "DET01") {
      "LEGe1"
    } else if (x == "DET_1") {
      "LEGe1"
    } else if (x == "DET02") {
      "LEGe2"
    } else if (x == "DET_2") {
      "LEGe2"
    } else if (x == "BEGE") {
      "BEGe"
    }
  })

  ## Efficienies
  if (!file.exists("../EffCalib.csv"))
    ("EffCalib.csv is missing from parent directory")
  EffCalib <- read.csv("../EffCalib.csv")
  # subset for detector
  EffCalib <- EffCalib[EffCalib$Detector == unique(detector), ]
  # subset for standard
  if (is.null(standard)) {
    standard <- select.list(unique(EffCalib$Standard), title = paste("\nEnter number to pick standard for",
                                                                     unique(detector)))
  }
  EffCalib <- EffCalib[EffCalib$Standard == unique(standard), ]
  # subset for efficiency type
  if (is.null(efftype)) {
    efftype <- select.list(unique(EffCalib$Type), title = paste("\nEnter number to pick efficiency type for",
                                                                unique(standard)))
  }
  EffCalib <- EffCalib[EffCalib$Type == unique(efftype), ]


  ## normalize peak centroid energies from GENIE to match energies in efficiency file what efficiency energy
  ## is closest to the Sample energy ?  if no match, return NA create df of energies and nuclide names


  Library <- Radionuclides()
  EffEnergy <- lapply(DATA$Energy, function(e) if (abs(Library$energy[which.min(abs(Library$energy - e))] -
                                                       e) < 4)
    (Library[which.min(abs(Library$energy - e)), 1:2]) else (c(NA, NA)))
  EffEnergy <- do.call(rbind.data.frame, EffEnergy)
  row.names(EffEnergy) <- NULL
  # run CountSig on all peaks
  significant <- CountSig(DATA$NetPeakArea, DATA$NetAreaUncert, DATA$ContinuumCounts, DATA$ROILength, 4, 1.645)
  # cbind efficiency energies and significant logical trigger to DATA df
  DATA <- data.frame(SampleID = DATA[, 1], detector = detector, standard = standard, efftype = efftype, EffEnergy
                     , significant, DATA[, 2:length(DATA)])


  ## Add in efficienies
  EffCalib$Detector <- as.character(EffCalib$Detector)
  DATA$detector <- as.character(DATA$detector)
  EffCalib$Type <- as.character(EffCalib$Type)
  DATA$efftype <- as.character(DATA$efftype)

  # logical switch to assign NA where there is no efficiency list in EffCalib calculate efficiency based on
  # regression return half or full jar efficiency

  efficiency <- do.call(rbind, lapply(1:nrow(DATA), function(i) if (nrow(subset(EffCalib, Energy == DATA[i,
                                                                                                         ]$energy)) == 0) {
    return(NA)
  } else {
    if (DATA[i, "efftype"] == "regression") {
      DATA[i, "Mass"] * subset(EffCalib, Energy == DATA[i, ]$energy)$Slope + subset(EffCalib, Energy ==
                                                                                      DATA[i, ]$energy)$Intercept
    } else if (DATA[i, "efftype"] == "half jar" | DATA[i, "efftype"] == "full jar") {
      subset(EffCalib, Energy == DATA[i, ]$energy)$Efficiency
    }
  }))
  DATA <- cbind(DATA, efficiency)
  # cbind efficiencies with DATA Activities and Errors 1000 conversion constant for mass g to kg if peak is
  # U-235/U-238, use 57.1% ratio of activity from Gilmore (2008) text
  activity <- do.call(rbind, lapply(1:nrow(DATA), function(i) if (DATA[i, "significant"] == "Significant") {
    1000 * ifelse(DATA[i, "energy"] == 186, 0.571, 1) * (DATA[i, "NetPeakArea"]/DATA[i, "CountTime"]) *
      1/(DATA[i, "Mass"] * DATA[i, "efficiency"])
  } else {
    1000 * ifelse(DATA[i, "energy"] == 186, 0.571, 1) * (as.numeric(DATA[i, ]$significant)/DATA[i, "CountTime"]
    ) * 1/(DATA[i, "Mass"] * DATA[i, "efficiency"])
  }))
  activity <- round(as.numeric(as.character(activity)), digits = 2)
  DATA <- cbind(DATA, activity)

  error <- do.call(rbind, lapply(1:nrow(DATA), function(i) if (DATA[i, "significant"] == "Significant") {
    error <- DATA[i, "activity"] * (DATA[i, "NetAreaUncert"]/DATA[i, "NetPeakArea"])
  } else if (DATA[i, "significant"] != "Significant") {
    NA
  }))
  error <- round(as.numeric(as.character(error)), digits = 2)
  DATA <- cbind(DATA, error)

  # Add the nuclide names to use as columns
  DATA <- cbind(DATA, Headers = paste(ifelse(is.na(DATA$energy), DATA$Energy, DATA$energy), DATA$nuclide,
                                      sep = "_"))
  # Add the nuclide names to use as columns
  Sample <- DATA[1, which(colnames(DATA) %in% c("SampleID", "Mass", "detector", "CountDate", "CountTime",
                                                "efficiency"))]


  ## change rows of activities into columns for each sample subset into sample, headers, activity, error and
  ## raw count info

  for (ii in DATA$Headers) {
    activity.error <- DATA[DATA$Headers == ii, which(colnames(DATA) %in% c("SampleID", "activity",
                                                                           "error", "NetPeakArea", "NetAreaUncert", "ContinuumCounts", "ROILength"))]

    names(activity.error) <- c("SampleID", paste(as.character(ii), "Net_Peak_Area", sep = "_"),
                               paste(as.character(ii), "Net_Area_Uncert", sep = "_"), paste(as.character(ii),
                                                                                            "Continuum_Counts", sep = "_"), paste(as.character(ii),
                                                                                                                                  "ROI_Length", sep = "_"), paste(as.character(ii), "activity", sep = "_"),
                               paste(as.character(ii), "error", sep = "_"))
    Sample <- merge(Sample, activity.error, by = "SampleID")
  }
  ############################### File Saving and Output

  if (CSV == T) {
    if (FILENAME == "DEFAULT") {
      FILENAME <- Sample[, "SampleID"]
    }
    new.names <- gsub("_", " ", colnames(Sample))
    SampleOut <- Sample
    colnames(SampleOut) <- new.names
    write.csv(SampleOut, paste(FILENAME, ".csv", sep = ""), row.names = FALSE)
  }
  # add leading "X" in front of numeric columnames for easier indexing
  colnames(Sample)[6:length(names(Sample))] <- paste("X", colnames(Sample)[6:length(names(Sample))], sep="")
  Sample
}
