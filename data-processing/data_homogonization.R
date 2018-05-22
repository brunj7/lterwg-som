# libraries

library(googledrive)
library(googlesheets)
library(tidyverse)
library(readxl)
if(!grepl("Darwin", Sys.info()["sysname"])) { 
  library(readODS) }
library(tools)


# batch load synced googlesheets file ingestion function
batch_load <- function(fileName, skipRows, missingValueCode) {
  if (file_ext(fileName) == 'ods') {
    dataFile <- read_ods(fileName, skip = skipRows, na = missingValueCode)
  } else if (file_ext(fileName) == 'csv') {
    dataFile <- read_csv(fileName, skip = skipRows, na = missingValueCode)
  } else if (grepl('xls', file_ext(fileName))) {
    dataFile <- read_excel(fileName, skip = skipRows, na = missingValueCode)
  } else { print(" --- data file type compatability error ---")}
}

# access units table from google sheets for units conversion
unitsConversionFile <- gs_title('units_translation_table')

# units conversion table from google - location
unitsConversionLocation <- gs_read(unitsConversionFile, ws = 1) %>% 
  filter(!is.na(Unit)) %>% 
  select(unit_levels = Unit, Var_long, var, givenUnit, unitConversionFactor)

# units conversion table from google - profile
unitsConversionProfile <- gs_read(unitsConversionFile, ws = 2,
                                  range = 'A1:H546') %>% # ignore fractions for now 
  filter(!is.na(unit_levels)) %>% 
  select(unit_levels, Var_long, var, givenUnit, unitConversionFactor)


data_homogonization <- function(directoryName, temporaryDirectory) {
  
  # set the file reference to the identified directory
  setwd(directoryName)
  
  
  # OUTPUT DIRECTORY ----
  
  # create a temporary, local workspace to receive script output use a default
  # directory path is one is not provided
  if(missing(temporaryDirectory)) {
    temporaryDirectory <- '~/Desktop/temp_som_outspace/'
  }
  
  # create the receiving directory if it does not exist; delete the contents if
  # it does exist (may want to revisit whether this is desired behavior)
  if(!dir.exists(temporaryDirectory)) {
    dir.create(temporaryDirectory)
  } else {
    file.remove(file.path(temporaryDirectory, list.files(temporaryDirectory))) 
  }
  
  
  # GOOGLE DRIVE DIRECTORY ----
  
  # access Google directory id for reference
  # not avaialable locally, must be pulled from Google API
  googleID <- drive_get(basename(directoryName)) %>% 
    pull(id)
  
  # list files in Google directory 
  dirFileNames <- list.files(directoryName) 
  
  
  # ACCESS KEY FILE
  
  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", dirFileNames, ignore.case = T, value = T)
  
  # location-level data
  locationData <- read_ods(keyFileName, sheet = 1) %>% 
    filter(!is.na(Value)) %>% 
    add_row(Value = googleID, var = 'google_id', .before = 1)
  
  # profile-level data
  profileData <- read_ods(keyFileName, sheet = 2) %>% 
    filter(!is.na(header_name))
  
  # GENERATE NOTE FILE (FROM THE KEY FILE)
  
  # create a note name with path to output directory, name of key file + _HMGZD_NOTES.csv
  notesFileName <- paste0(temporaryDirectory, file_path_sans_ext(keyFileName), "_HMGZD_NOTES.csv")
  
  # capture notes from location and profile key-file tabs
  notes <- bind_rows(
    locationData %>% 
      filter(!is.na(var_notes)) %>% 
      mutate(source = "location") %>% 
      select(source, Var_long, var, var_notes),
    profileData %>% 
      filter(!is.na(Notes) | !is.na(Comment)) %>% 
      unite(col = var_notes, Notes, Comment, sep = ";") %>% 
      mutate(source = "profile") %>% 
      select(source, Var_long, var, var_notes)
  )
  
  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN STANDARDIZE UNITS::location data
  # source('~/Dropbox/development/standardize_units_location.R')
  
  # location tab DATA containing units only
  locationDataUnits <- locationData %>%
    dplyr::filter(!is.na(Unit)) %>%
    dplyr::select(Value, unit_levels = Unit, Var_long, var)
  
  # join location DATA with units and corresponding vars in conversion table
  LDU_UCL <- dplyr::left_join(locationDataUnits, unitsConversionLocation,
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1
    )
  
  # standardize location data units
  for (varValue in c(LDU_UCL$var)) {
    
    # standardize values per the units_conversion_table
    locationData[locationData$var == varValue,]['Value'] <- as.numeric(locationData[locationData$var == varValue,]['Value']) * LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor']
    
    # add mention of conversions to notes
    if (nrow(notes[notes$var == varValue,]) >= 1) {
      notes <- notes %>%
        dplyr::mutate(var_notes = replace(var_notes,
                                          var == varValue,
                                          paste0(var_notes, "; UNITS CONVERSION APPLIED: ", LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor'])))
    } else {
      notes <- notes %>%
        tibble::add_row(source = 'location',
                        var = varValue,
                        var_notes = paste0("UNITS CONVERSION APPLIED: ", LDU_UCL[LDU_UCL$var == varValue,]['unitConversionFactor']))
    }
  }
  
  # END STANDARDIZE UNITS::location data
  # +++++++++++++++++++++++++++++++++++++++
  
  
  # Isolate rows to skip from locationData for data import. This was originally
  # intended to be an input as to the number of rows to skip but it seems to
  # have been interpreted as the row number of the header.
  if(length(locationData[locationData$var == 'header_row',]$var) == 1) { 
    skipRows = as.numeric(locationData[locationData$var == 'header_row',]$Value) - 1
  } else {
    skipRows = 0
  }
  
  # isolate missing value codes from locationData for data import
  if (length(locationData[locationData$var == 'NA_1',]$var) == 1) {
    mvc1 = locationData[locationData$var == 'NA_1',]$Value }
  if (length(locationData[locationData$var == 'NA_2',]$var) == 1) {
    mvc2 = locationData[locationData$var == 'NA_2',]$Value }
  
  missingValueCode = "NA"
  if (exists('mvc1')) { missingValueCode = mvc1}
  if (exists('mvc2')) { missingValueCode = mvc2}
  if (exists('mvc1') && exists('mvc2')) { missingValueCode = c(paste(mvc1, mvc2))}
  
  
  # DATA FILE(S)
  
  # import all (data + key) files from the google dir
  googleDirData <- lapply(dirFileNames, 
                          batch_load, 
                          missingValueCode = missingValueCode,
                          skipRows = skipRows)
  
  # add names to Google dir files
  names(googleDirData) <- dirFileNames
  
  # as the key file is already loaded at this point in the workflow, remove it
  # from the list of data frames
  googleDirData <- googleDirData[!grepl("key", names(googleDirData), ignore.case = T)]
  
  # print header rows to stdout as a quality-control step
  lapply(googleDirData, function(frame) {
    print(head(frame, n = 1L)) })
  
  # generate a vector of dataframe columns to keep from key file input to
  # header_name
  varsToKeep <- profileData %>% 
    select(header_name) %>% 
    pull()
  
  # pull targeted vars from each data frame based on header_names in key file
  googleDirData <- map(googleDirData, select, one_of(varsToKeep))
  
  # rename investigator-supplied names to key-file (standardized) names
  googleDirData <- lapply(googleDirData, function(frame) { 
    setNames(frame, profileData$var[match(names(frame), profileData$header_name)]) })
  
  # +++++++++++++++++++++++++++++++++++++++
  # BEGIN STANDARDIZE UNITS::profile data
  # source('~/Dropbox/development/standardize_units_profile.R')
  
  # profile tab DATA containing units only
  profileDataUnits <- profileData %>%
    dplyr::filter(!is.na(unit_levels)) %>%
    dplyr::select(header_name, unit_levels, Var_long, var)
  
  # join profile DATA with units and corresponding vars in conversion table
  PDU_UCP <- dplyr::left_join(profileDataUnits, unitsConversionProfile,
                              by = c("var", "unit_levels"),
                              suffix = c(".PD", ".UT")) %>%
    dplyr::filter(
      !is.na(unitConversionFactor),
      unitConversionFactor != 1
    )
  
  # loop through all data frames in google dir
  for (i in 1:length(googleDirData)) {
    
    for (dataCol in c(PDU_UCP$var)) {
      
      if (!is.null(googleDirData[[i]][[dataCol]])) {
        
        googleDirData[[i]][[dataCol]] <- googleDirData[[i]][[dataCol]] * PDU_UCP[PDU_UCP$var == dataCol,][['unitConversionFactor']]
        
        # add mention of conversions to notes
        if (nrow(notes[notes$var == dataCol,]) >= 1) {
          print(paste("mutate: ", dataCol))
          
          baseNote <- notes %>%
            dplyr::filter(var == dataCol) %>%
            dplyr::select(var_notes)
          
          notes <- notes %>%
            dplyr::mutate(var_notes = replace(var_notes,
                                              var == dataCol,
                                              paste0(baseNote, "; UNITS CONVERSION APPLIED: ", PDU_UCP[PDU_UCP$var == dataCol,]['unitConversionFactor'])))
        } else {
          print(paste("add_row: ", dataCol))
          notes <- notes %>%
            tibble::add_row(source = 'profile',
                            var = dataCol,
                            var_notes = paste0("UNITS CONVERSION APPLIED: ", PDU_UCP[PDU_UCP$var == dataCol,]['unitConversionFactor']))
        }
      }
    }
  }
  
  # END STANDARDIZE UNITS::profile data
  # +++++++++++++++++++++++++++++++++++++++
  
  # generate wide data frame of location data
  locationDataWide <- locationData %>% 
    select(var, Value) %>% 
    spread(key = var, value = Value)
  
  # merge location data with each data frame
  googleDirData <- lapply(googleDirData, function(frame) { 
    merge(locationDataWide, frame, all = T) })
  
  # rename files to include base name + indication of homogenization 
  names(googleDirData) <- paste0(str_extract(names(googleDirData), "^[^\\.]*"), "_HMGZD")
  
  
  # FILE OUTPUT
  
  # We can write the output directly to the working/target Google Directory. The
  # problem with that approach is that, even with a csv or otherwise file
  # extension, Google sees each added file as a text file. These files open
  # with Google Sheets so are fully functional. The problem is that opening the
  # file (with Google Sheets for example) creates a copy of the file so you end
  # up with a Google Sheets version and the original version. To avoid confusion
  # or processing headaches created by these dual files, we will write the
  # output to a temporary, local directory then use drive_upload to add the
  # files to the working/target Google Directory. drive_upload has the option to
  # declare a file type so we can make explicit to Google Drive that the added
  # files are of type spreadsheet, for example, which allows us to avoid the
  # duplicate file problem.
  
  # Approach to write output csv files directly to target Google Directory - not
  # used!
  # googleDirData %>%
  #   names(.) %>%
  #   map(~ write_csv(googleDirData[[.]], paste0(directoryName, "/", ., ".csv")))
  
  # write processed files to the temporary directory
  
  # notes
  write_csv(notes, notesFileName)
  
  # data
  googleDirData %>%
    names(.) %>%
    map(~ write_csv(googleDirData[[.]], paste0(temporaryDirectory, ., ".csv")))
  
  
  # UPLOAD OUTPUT TO GOOGLE DRIVE
  
  # list the files added to the temporary output space
  filesToUpload <- list.files(path=temporaryDirectory,
                              full.names=F,
                              recursive=FALSE)
  
  # upload these files to the target Google directory
  lapply(filesToUpload, function(frame) {
    drive_upload(paste0(temporaryDirectory, frame),
                 path = drive_get(as_id(googleID)),
                 type = "spreadsheet")
  })
  
  
}


# linux
directoryName <- '/home/srearl/googleDrive/LTER-SOM/Data_downloads/cap.557.9_151026011609313008/'

# mac
directoryName <- '~/Google Drive/LTER-SOM/Data_downloads/UMBS_DIRT/SOIL_CN/UMBS_DIRT_C_N_by_Plot_2004_2014/'
