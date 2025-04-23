#==============================================================================#
#   LOWER GRANITE WALLEYE RADIO TELEMETRY DATA MANAGEMENT SCRIPT:              #
#   USER FRONT END FOR DATA ASSEMBLY                                           #
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

Inquisitor <- function() {
    tk_select.list(choices = c("Yes, I verified the suggestions that R made.",
        "Nope, I like to live dangerously."),
        title = "STOP! Did you manually verify the suggestions that R made in the previous script?")
}


DoubleCheck <- function() {
    tk_select.list(choices = c("Yes, and I accept responsibility.",
        "No, I'll go back and do the QAQC."),
        title = "SIGH. Are you sure you want to do this?")
}





#==============================================================================#
#   USER CHOICE QUERYING                                                       #
#==============================================================================#

# Libraries
    library(khroma)
    library(viridis)
    library(tcltk)
    library(plotrix)

# Acquires the sensor file
    sites <- read.csv("metadata/receivers.csv")

# Asks the user whether they did manual QAQC
    user_confessions <- Inquisitor()
    
# If the user says no, tries to encourage them to go back and qaqc
    if (user_confessions == "Nope, I like to live dangerously.") {
        user_call <- DoubleCheck()
        if (user_call == "No, I'll go back and do the QAQC.") {
            cat("\n\nGood call! I'll be here ready to run again when you are finished!\n\n")
        }
    }
    

# Scrapes the filenames of the incident files
    incident_files <- list.files("interm_data", pattern = "incidents.csv",
        full.names = TRUE, recursive = TRUE)
    
# Reads the files
    incident_list <- lapply(incident_files, read.csv)
    
# Removes any bigfeet or jackalopes
    incident_list <- lapply(incident_list, function(x) {
        x <- x[x$Recommend == 1, ]
    })

# Combines the output
    incidents <- do.call("rbind", incident_list)
    
# Does some reordering
    observations <- incidents[order(incidents$Tag_Code, incidents$Time_Start,
        incidents$Sensor), ]
    
# Checks for and creates an output directory
    if (!dir.exists("assembled_data")) {
        dir.create("assembled_data")
    }
    
# Stores the output filename for the whole dataset
    assemb_fn <- "assembled_data/assembled_all.csv"
    
# Outputs the assembled file
# NOTE THAT THIS OVERWRITES PREVIOUS ASSEMBLY FILES!!!
    write.csv(observations, assemb_fn, row.names = FALSE)
    
# Stores output filenames for individual tags
    ind_tags <- sort(unique(observations$Tag_Code))
    ind_fns <- paste0("assembled_data/assembled_", ind_tags, ".csv")
    
    for (i in 1:length(ind_tags)) {
        obs_sub <- observations[observations$Tag_Code %in% ind_tags[i], ,
            drop = FALSE]
   
    # Converts the date/times to date/times
        obs_sub$Time_Start <- strptime(obs_sub$Time_Start,
            format = "%Y-%m-%d %H:%M:%S")
        obs_sub$Time_Stop <- strptime(obs_sub$Time_Stop,
            format = "%Y-%m-%d %H:%M:%S")        
        
    # Sets the X axis range
        xmin <- min(obs_sub$Time_Start, na.rm = TRUE) - 43200
        xmax <- max(obs_sub$Time_Stop, na.rm = TRUE) + 43200
    
    # Generates the vertical grid values for plotting
    # Determines the total date range 
        grid_time_range <- c(min(obs_sub$Time_Start), max(obs_sub$Time_Stop))
        
    # Extracts the start and stop days
        grid_bookends <- format(grid_time_range, "%Y-%m-%d")
        
    # Generates date objects for all days in the range
        grid_days <- seq.Date(from = as.Date(grid_bookends[1]) - 1,
            to = as.Date(grid_bookends[2]) + 1, by = 1)
        
    # Defines the hours where gridlines will occur
        hour_marker <- seq(0, 23, by = 12)
        hour_formatted <- formatC(hour_marker, width = 2, flag = "0")
        time_formatted <- paste0(hour_formatted, ":00:01")
        
    # Combines the days and the hour markers
        grid_times <- lapply(grid_days, function(x) paste(x, time_formatted))
        grid_times <- unlist(grid_times)
        
    # Converts the grid times to positct objects
        grid_times <- strptime(grid_times, format = "%Y-%m-%d %H:%M:%S",
            tz = "Etc/GMT+8")

        # Initializes a png file
            png(gsub("csv", "png", ind_fns[i]), width = 8, height = 6,
                units = "in", res = 300)
        
        # Sets the framing parameters
            par(mar = c(4.5, 6, 3, 1))
            
        # Initializes a plot
            plot(obs_sub$Time_Start, obs_sub$Recommend, xlim = c(xmin, xmax),
                 ylim = c(1, 19), xlab = "Date", ylab = "", yaxt = "n",
                 cex = 0)
            
        # Adds horizontal gridlines
            abline(h = sites$Y_Axis_Order, col = "grey80")
            abline(v = as.numeric(grid_times)[seq(1, length(grid_times), by = 2)],
                col = "grey80")
            abline(v = as.numeric(grid_times)[seq(2, length(grid_times), by = 2)],
                col = "grey80", lty = 3, lwd = 0.7)
            
        # Adds a bounding box
            box()
            
            
        # Adds the receiver axis
            axis(side = 2, at = sites$Y_Axis_Order, labels = sites$Site_Code,
                 las = 1)
            
        # Adds margin text
            mtext("Sensor", side = 2, line = 4)
            mtext(paste0("Tag: ", obs_sub$Tag_Code[1]), side = 3, line = 0.5,
                font = 2, cex = 1.25)
            
        # Adds the connecting lines
            xvals <- (as.numeric(obs_sub$Time_Start) +
                as.numeric(obs_sub$Time_Stop)) / 2
            yvals <- sapply(obs_sub$Sensor, function(x) {
                sites[sites$Site_Code %in% x, ]$Y_Axis_Order
                })
            
            
            lines(xvals, yvals, lty = 3)
            points(xvals, yvals, pch = 19, cex = 4, col = "white")
                
                
        # Plots the observations
            for (j in 1:nrow(obs_sub)) {
                y <- sites$Y_Axis_Order[which(sites$Site_Code == obs_sub$Sensor[j])]
                points(obs_sub$Time_Start[j], y, pch = 4)
                points(obs_sub$Time_Stop[j], y, pch = 1)
                segments(as.numeric(obs_sub$Time_Start[j]), y,
                    as.numeric(obs_sub$Time_Stop[j]), y)
            # Adds counts
                thigmophobe.labels(mean(c(obs_sub$Time_Start[j],
                    obs_sub$Time_Stop[j])),
                    y, obs_sub$Tag_Pings[j], cex = 0.7)
            
                
            }
            
        dev.off()
 
        write.csv(obs_sub, ind_fns[i], row.names = FALSE)

    }

    
    
    
    

#______________________________________________________________________________#
#                                                                              #    
#==============================================================================#
#   END OF SCRIPT                                                              #
#==============================================================================#