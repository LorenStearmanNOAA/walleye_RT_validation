#==============================================================================#
#   LOWER GRANITE WALLEYE RADIO TELEMETRY DATA MANAGEMENT SCRIPT:              #
#   USER FRONT END FOR DATA QAQC                                               #
#==============================================================================#
#______________________________________________________________________________#
#                                                                              #

# Copyright 2024 U.S. Federal Government (in countries where
# recognized)

# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software is
# distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
# either express or implied. See the License for the specific language governing
# permissions and limitations under the License.





#==============================================================================#
#   FUNCTIONS                                                                  #
#==============================================================================#

OneOrMany <- function() {
    tk_select.list(title = file_count_text,
        choices = c("All files", "A subset of files", "Just one file"))
}
    

ReAnalyze <- function() {
    tk_select.list(title = analysis_status_text,
            choices = c("Analyze only new files", "Analyze all files"))    
}    


SingleFile <- function() {
    tk_select.list(title = "Please select a single file to analyze",
        choices = c(data_files, "A different file"))
    
    
}


SubsetFiles <- function() {
    tk_select.list(title = "Please select one or more files to analyze",
        choices = data_files, multiple = TRUE)
}





#==============================================================================#
#   LIBRARIES                                                                  #
#==============================================================================#

# Libraries
    library(khroma)
    library(viridis)
    library(tcltk)





#==============================================================================#
#   DATA FILE DETECTION AND SELECTION                                          #
#==============================================================================#

# Identifies possible files
    text_files <- list.files("raw_data", pattern = ".txt", recursive = TRUE,
        full.names = TRUE)

# Scrubs the possible files of filenames with $
    text_files <- text_files[-grep("\\$", text_files)]
    
# Separates data files and processing receipts
    proc_count <- length(text_files[-grep("receipt", text_files)])
    if (proc_count > 0) {
        data_files <- text_files[-grep("receipt", text_files)]
        proc_repts <- text_files[grep("receipt", text_files)]
    } else {
        data_files <- text_files
        proc_repts <- NULL
    }

# Removes any write history files
    write_files <- grep("write_history", data_files)
    if (length(write_files) > 0) {
        data_files <- data_files[-write_files]
    }
        
# Determines which files have been previously processed
    data_id <- gsub(".txt", "", data_files)
    proc_id <- gsub("_processing_receipt.txt", "", proc_repts)
    previous_files <- which(data_id %in% proc_id)

# Builds text for the number of files found and which have been analyzed
    analysis_status_text <- paste0("I found ", length(data_files),
        " files, of which ", length(previous_files),
        " have been analyzed.\nWould you like to exclude previously analyzed files?")

# First assesses whether to reanalyze old files
# Were there previously analyzed files?
    if (length(previous_files > 0)) {
    
    # User prompt: what to do with previously analyzed files
        reanalysis <- ReAnalyze()
        
    # Logical step: if user said exclude old files, excludes them
        if (reanalysis == "Analyze only new files") {
            data_files <- data_files[!data_id %in% proc_id]
        }
    } 

# Stores the text with the number of files remaining
    if (length(proc_id) > 0) {
        file_count_text <- paste0("Ok, that leaves ", length(data_files),
            " files.\nWhich files do you want to analyze?")
    } else {
        file_count_text <- paste0("There are ", length(data_files),
            " files for analysis.\nWhich files do you want to analyze?")
    }

# Prompts user to select broadly which files to analyze (all, subset, one)
    one_or_many <- OneOrMany()
    
# Conditional approach to file selection
# Case if the user only wants to select one file
    if (one_or_many == "Just one file") {
        analysis_files <- SingleFile()
        
    # If the user selects "a different file", this allows the input of a unique
    # user-selected file
        if (analysis_files == "A different file") {
            analysis_files <- file.choose()
            if (length(grep("txt", analysis_files)) == 0) {
                stop("Selected file must end in .txt!")
            }
        }
    } else {
        if (one_or_many == "A subset of files") {
            analysis_files <- SubsetFiles()
        } else {
            analysis_files <- data_files
        }
    }

    
    
    
    
#==============================================================================#
#   INCIDENT DETECTION AND SUGGESTED OBSERVATIONS                              #
#==============================================================================#
    
# Lets the user know that the ball is rolling
    cat("\nPing and incident detection in progress!\n")
    cat("If this is the first run, or the first run in a while,\n")
    cat("the process could take an hour or two.\n\n")
    
# Iteratively runs the incident detection script over the list of selected
# files for analysis
    for (k in 1:length(analysis_files)) {
        focal_file <- analysis_files[k]
        source("qaqc_back_end.R")
        cat("\nFile ", k, "of ", length(analysis_files), " completed!\n")
        cat("\n\n")
        flush.console()
    }

   
    
    
    
#==============================================================================#
#   EXIT TEXT                                                                  #
#==============================================================================#
    
# R terminal output information
    cat("\n\n\nAll files successfully analyzed!")
    cat("\nStop time at ")
    cat(as.character(Sys.time()))
    cat("\n\nSTOP BEFORE YOU PROCEED!")
    cat("\nYou need to review the output files in the folders of /interm_data!!!")
    cat("\nCheck for edge cases where you disagree with R!!!")
	cat("\nThere are multiple plots and a csv to help you review each case.")
    cat("\nIf you disagree, change the value (0 = no, 1 = yes) in the 'Recommend' column\nof the incident csv file in the /interm_data folder.\n\n")
    
    
    
    
    
#______________________________________________________________________________#
#                                                                              #    
#==============================================================================#
#   END OF SCRIPT                                                              #
#==============================================================================#