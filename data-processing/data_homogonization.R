# libraries

library(googledrive)
library(googlesheets)
library(tidyverse)
library(readxl)
if(!grep("Darwin", Sys.info()["sysname"])) { 
  library(readODS) }
library(tools)


# todos and notes ----

# check header_row, 1 if NA - done!
# for header_name to var consider rename_at, rename_all, or purrr::set_names - done!
# check out select(matches) if select_one poses a problem - done!
# location data are being sorted alphabetically, not a huge problem but need to correct
# warnings to file
# on-run warns for: 
  # multiple header_names of same name
# need to clear temporaryWorkspace between runs - done!
# Run Sarah's CRD E133 first

#' @title data_homogonization
#'
#' @description data_homogonization standardizes data files in a Google Drive
#'   directory according to a user-supplied template loosely based on the Powell
#'   Center Template
#'
#' @param directoryName Path to a Google Drive directory where data and a key
#'   file (the template are housed)
#'
#' @import tidyverse
#' @import googlesheets
#' @import googledrive
#'
#' @return Returns a homogonized data file for each input data file
#'
#' @examples
#' \dontrun{
#' data_homogonization('CAP_LTER')
#' }
#'
#' @export

# helper functions

# batch load googlesheets file ingestion function
# batch_load <- function(fileName, skipRows, missingValueCode) {
#   token <- gs_title(fileName)
#   dataFile <- gs_read(token, skip = skipRows, na = missingValueCode)
# }

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

# load synced googlesheets that will have varying file type depending on OS
# connection to drive
file_load <- function(fileName, skipRows, missingValueCode) {
  if (file_ext(fileName) == 'ods') {
    dataFile <- read_ods(fileName, skip = skipRows, na = missingValueCode)
  } else if (file_ext(fileName) == 'csv') {
    dataFile <- read_csv(fileName, skip = skipRows, na = missingValueCode)
  } else if (grepl('xls', file_ext(fileName))) {
    dataFile <- read_excel(fileName, skip = skipRows, na = missingValueCode)
  } else { print(" --- data file type compatability error ---")}
}

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
  
  
  # KEY FILE ----
  
  # isolate key-key and extract details in location and profile tabs
  keyFileName <- grep("key", dirFileNames, ignore.case = T, value = T)
  
  # location-level data
  locationData <- read_ods(keyFileName, sheet = 1) %>% 
    filter(!is.na(Value)) %>% 
    add_row(Value = googleID, var = 'google_id', .before = 1)
  
  # profile-level data
  profileData <- read_ods(keyFileName, sheet = 2) %>% 
    filter(!is.na(header_name))
  
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
  
  
  # GENERATE A NOTE FILE FROM THE KEY FILE ----
  
  # create a note name with path to output directory, name of key file + _HMGZD_NOTES.csv
  notesFileName <- paste0(temporaryDirectory, file_path_sans_ext(keyFileName), "_HMGZD_NOTES.csv")
  
  # capture notes from location and profile key-file tabs and write to file
  write_csv( 
    bind_rows(
      locationData %>% 
        filter(!is.na(var_notes)) %>% 
        mutate(source = "location") %>% 
        select(source, Var_long, var, var_notes),
      profileData %>% 
        filter(!is.na(Notes) | !is.na(Comment)) %>% 
        unite(col = var_notes, Notes, Comment, sep = ";") %>% 
        mutate(source = "profile") %>% 
        select(source, Var_long, var, var_notes)
    ), notesFileName 
  )
  
  
  # DATA FILE(S) ----
  
  # import all (data + key) files from the Google dir
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
  
  # generate wide data frame of location data
  locationDataWide <- locationData %>% 
    select(var, Value) %>% 
    spread(key = var, value = Value)

  # merge location data with each data frame
  googleDirData <- lapply(googleDirData, function(frame) { 
    merge(locationDataWide, frame, all = T) })
  
  # rename files to include base name + indication of homogenization 
  names(googleDirData) <- paste0(str_extract(names(googleDirData), "^[^\\.]*"), "_HMGZD")
  
  
  # DATA FILE OUTPUT ----
  
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
  googleDirData %>%
    names(.) %>%
    map(~ write_csv(googleDirData[[.]], paste0(temporaryDirectory, ., ".csv")))
  
  
  # UPLOAD TO GOOGLE DRIVE ----
  
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
directoryName <- '/home/srearl/googleDrive/LTER-SOM/Data_downloads/UMBS_DIRT/SOIL_CN/UMBS_DIRT_C_N_by_Plot_2004_2014'

# mac
directoryName <- '~/Google Drive/LTER-SOM/Data_downloads/UMBS_DIRT/SOIL_CN/UMBS_DIRT_C_N_by_Plot_2004_2014/'

# tempToGoogleDrive <- function() {
#   
#   # identify directory with files (not full.names=T)
#   filesToUpload <- list.files(path=temporaryDirectory,
#                               full.names=F,
#                               recursive=FALSE)
#   
#   googleDirData %>%
#     names(.) %>%
#     map(~ write_csv(googleDirData[[.]], paste0(temporaryDirectory, ., ".csv")))
#   
#   filesToUpload %>% 
#     names(.) %>%
#     map(~ drive_upload(filesToUpload[[.]], path = drive_get(as_id(googleID)), type = "spreadsheet"))
#   
#   lapply(filesToUpload, function(frame) {
#     drive_upload(paste0(temporaryDirectory, frame), path = drive_get(as_id(googleID)), type = "spreadsheet")
#   })
#   
# }