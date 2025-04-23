#==============================================================================#
#   LOWER GRANITE WALLEYE RADIO TELEMETRY DATA QAQC SCRIPT I:                  #
#   PING AND INCIDENT IDENTIFICATION AND OBSERVATION RECOMMENDATIONS           #
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
#   ACQUISITIONS                                                               #
#==============================================================================#

    
# Stores a color-friendly palette
  #  hicon_pal <- color("high contrast")
   # hicon <- hicon_pal(3)
    hicon <- viridis(10)[c(10, 5, 1)]

# Grabs the tag code file
    tag_file <- read.csv("metadata/Radio_Tags_2024.csv")
	
# Grabs the filter file
    filters <- read.csv("metadata/filter_values.csv")
    
# Identifies the text file
# This is set up so that the whole script can be run from a batch approach with
# a list of files, or one file at a time, selected by the user
    if (!exists("focal_file")) {
        text_files <- list.files("raw_data", pattern = ".txt",
            full.names = TRUE, recursive = TRUE)
        text_files <- text_files[-grep("\\$", text_files)]
        focal_file <- select.list(choices = text_files,
            title = "Please select a file")
    }

# Queries the underlying file structure to figure out where the data starts
# and stops
    raw_data <- readLines(focal_file)
    char_struc <- sapply(raw_data, nchar)
    char_table <- table(char_struc)
    char_max <- as.numeric(names(which.max(char_table)))
    row_inds <- range(which(char_struc == char_max))
    start_row <- row_inds[1] - 1
    #nrows <- length(row_inds[1]:row_inds[2])
    end_row <- which(sapply(raw_data, function(x) grep("END", x, ignore.case = FALSE)) == 1)
    nrows <- length(start_row:(end_row -1)) - 1
 
# Acquires the data as a table
    raw_table <- read.table(focal_file, skip = start_row, nrows = nrows)

# Stores the table dimensions (used later down in the receipt)
    dim_x <- nrow(raw_table)
    dim_y <- ncol(raw_table)
    
    
    
    
#==============================================================================#
#   DATA FORMATTING                                                            #
#==============================================================================#

# Assigns column names
    colnames(raw_table) <- c("Date", "Time", "Sensor", "Hits", "Channel", "Code",
        "Strength")
    
# Formats date and time
    raw_table$DateTime <- apply(raw_table[, 1:2], 1, paste, collapse = " ")
    raw_table$DateTime <- strptime(raw_table$DateTime,
        format = "%d/%m/%y %H:%M:%S", tz = "Etc/GMT+8")
    raw_table$DateTimeNum <- as.numeric(raw_table$DateTime)

# Rounds the tag code to the nearest 5 value
    raw_table$CodeRound <- round(raw_table$Code/5) * 5
    
# Generates a tag code variable (actually matches with known codes)
    raw_table$Tag_Code <- formatC(raw_table$CodeRound, width = 3, flag = "0")
    raw_table$Tag_Code <- paste0(raw_table$Channel, raw_table$Tag_Code)
    
# Removes tag code values which are not in the tag code list
    raw_table$Tag_Code <- ifelse(raw_table$Tag_Code %in%
        tag_file$Radio.Tag.Number, raw_table$Tag_Code, NA)

# Determines the number of records with bad dates or times
    bad_dt <- which(is.na(raw_table$DateTime))
    
# Removes the records with bad dates or times
    raw_table <- raw_table[!is.na(raw_table$DateTime), ]
    
    
    
#==============================================================================#
#   SUMMARIZED DATA FRAME CONSTRUCTION                                         #
#==============================================================================#
    
# Creates a vector of putative tag files in the data
    tags_present <- sort(unique(raw_table$Tag_Code))

# Builds the crunched incident data frame   
    incident_list <- lapply(tags_present, function(tag) {
       
    # Extracts just data with the tag of interest
        sub_table <- raw_table[raw_table$Tag_Code %in% tag, ]
    
    # Handles what to do if it was just one ping
    # Give me a ping, Vasily. One ping only, please.  
        if (nrow(sub_table) == 1) {
          sub_table <- rbind(sub_table, sub_table)
        }

    # Calculates the distance between each ping    
        time_diffs <- diff(sub_table$DateTime, units = "secs")

    # Stores the start and stop times for each incident
        #time_stop_ind <- c(which(time_diffs > (5 * 60)), length(time_diffs))
        if (max(time_diffs) == 0) {
            time_stop_ind <- 2
            time_start_ind <- 1
            ping_add <- 0
        } else {
            time_stop_ind <- c(which(time_diffs > 300), nrow(sub_table))
            time_start_ind <- c(1, (time_stop_ind[-length(time_stop_ind)] + 1))
          
            ping_add <- 1
        }

    # Stores the number of detection in the incident
        npings <- time_stop_ind - time_start_ind + ping_add
        
    # Stores the start and stop times
        time_stops <- sub_table$DateTime[time_stop_ind]
        time_starts <- sub_table$DateTime[time_start_ind]
    
    # Calculates the elapsed intervals
        time_elapsed <- difftime(time_stops, time_starts, units = "secs")
        time_elapsed <- ifelse(time_elapsed == 0, 1, time_elapsed)
        
        
    # Calculates ping density
        tag_ping_density <- npings / as.numeric(time_elapsed)

    # Stores the numeric time starts and stops
        time_stop_n <- sub_table$DateTimeNum[time_stop_ind]
        time_start_n <- sub_table$DateTimeNum[time_start_ind]
 
    # Calculates a background ping density from the whole data
        raw_time_n <- raw_table$DateTimeNum[is.na(raw_table$Tag_Code) &
            raw_table$CodeRound != 575]
        back_pings <- sapply(1:length(time_stop_n), function(rt, x) {
            length(rt[rt <= time_stop_n[x] & rt >= time_start_n[x]])
        }, rt = raw_time_n)
        back_ping_density <- back_pings / as.numeric(time_elapsed)
        
    # Calculates the mean and max tag ping strength detected
        tag_strengths <- sapply(1:length(time_stops), function(x) {
            mean(sub_table$Strength[time_start_ind[x]:time_stop_ind[x]])
        })
        tag_max_strengths <- sapply(1:length(time_stops), function(x) {
            max(sub_table$Strength[time_start_ind[x]:time_stop_ind[x]])
        })
        
    # Calculates the mean and max background ping strength
        back_strengths <- sapply(1:length(time_stop_n), function(x) {
            sub_table_x <- raw_table[raw_table$DateTimeNum <= time_stop_n[x] &
                raw_table$DateTimeNum >= time_start_n[x], ]
            sub_table_x <- sub_table_x[is.na(sub_table_x$Tag_Code), ]
            sub_table_x <- sub_table_x[!sub_table_x$CodeRound == 575, ]
            mean(sub_table_x$Strength)
        })
        back_strengths <- ifelse(is.na(back_strengths), 0, back_strengths)
    
    # Stores the recommendation variable
        sufficient_detects <- npings >= filters[1, 2]
        good_noise_rate <- back_ping_density < filters[2, 2]
        good_tag_noise_n <- npings/back_pings > filters[3, 2]
        good_tag_noise_vol <- tag_strengths - back_strengths > filters[4, 2]
        good_mat <- cbind(good_noise_rate, good_tag_noise_n,
            good_tag_noise_vol)
        filter_pass_init <- apply(good_mat, 1, sum)
        filter_pass <- filter_pass_init + sufficient_detects
        
    # Determines recommendations as those which pass trhee of the four filters
    # in good_mat
        recommends <- as.numeric(filter_pass_init > 2)
        
    # Checks ping count - if too low, it's an automatic fail
        recommends <- ifelse(sufficient_detects == FALSE, 0, recommends)

    # Creates an output data frame
        out_df <- data.frame(Sensor = raw_table$Sensor[1],
            Tag_Code = tag,
            Time_Start = time_starts,
            Time_Stop = time_stops,
            Elapsed_Seconds = time_elapsed,
            Tag_Pings = npings,
            Noise_Pings = back_pings,
            Noise_Rate = back_ping_density,
            Tag_Noise_Ratio = npings / back_pings,
            Max_Tag_Strength = tag_max_strengths,
            Mean_Tag_Strength = tag_strengths,
            Mean_Noise_Strength = back_strengths,
            Diff_dB = tag_strengths - back_strengths,
            Filter_Pass = filter_pass,
            Recommend = recommends)
        
    # Holds the output df
        out_df
    })
 
# Combines the incidents into a single data frame
  incident_df <- do.call("rbind", incident_list)
  
# Organizes the incident df chronologically by first detection
  if (!is.null(dim(incident_df))) {
    incident_df <- incident_df[order(incident_df$Time_Start), ]
  } else {
    incident_df <- data.frame(Sensor = NA,
            Tag_Code = NA,
            Time_Start = NA,
            Time_Stop = NA,
            Elapsed_Seconds = NA,
            Tag_Pings = NA,
            Noise_Pings = NA,
            Noise_Rate = NA,
            Tag_Noise_Ratio = NA,
            Max_Tag_Strength = NA,
            Mean_Tag_Strength = NA,
            Mean_Noise_Strength = NA,
            Diff_dB = NA,
            Filter_Pass = NA,
            Recommend = NA)
  }

# Sorts the incident df by tag pings, strength difference, etc
    incident_df <- incident_df[order(incident_df$Tag_Pings,
        incident_df$Noise_Rate, incident_df$Tag_Noise_Ratio,
        incident_df$Diff_dB, decreasing = TRUE), ]
  

  
    
    
#==============================================================================#
# Plotting parameter definition                                                #
#==============================================================================#
    
# Stores the channel pch values
    channel_pch <- ifelse(raw_table$Channel == 2, 0, 
        ifelse(raw_table$Channel == 4, 2,
        ifelse(raw_table$Channel == 9, 6, 4)))
    
# Stores the ping tag colors
    ping_cols <- ifelse(is.na(raw_table$Tag_Code), "grey", hicon[1])

# Generates the vertical grid values for plotting
# Determines the total date range 
    grid_time_range <- range(raw_table$DateTime, na.rm = TRUE)
    
# Extracts the start and stop days
    grid_bookends <- format(grid_time_range, "%Y-%m-%d")
    
# Generates date objects for all days in the range
    grid_days <- seq.Date(from = as.Date(grid_bookends[1]),
        to = as.Date(grid_bookends[2]), by = 1)
    
# Defines the hours where gridlines will occur
    hour_marker <- seq(0, 23, by = 3)
    hour_formatted <- formatC(hour_marker, width = 2, flag = "0")
    time_formatted <- paste0(hour_formatted, ":00:01")
    
# Combines the days and the hour markers
    grid_times <- lapply(grid_days, function(x) paste(x, time_formatted))
    grid_times <- unlist(grid_times)
    
# Converts the grid times to positct objects
    grid_times <- strptime(grid_times, format = "%Y-%m-%d %H:%M:%S",
        tz = "Etc/GMT+8")




    
#==============================================================================#
#   OUTPUT DIRECTORY CREATION                                                  #
#==============================================================================#

# Stores the directories
    raw_dir <- paste0("raw_data/", raw_table$Sensor[1], "/")
    int_dir <- paste0("interm_data/", raw_table$Sensor[1], "/",
        raw_table$Sensor[1], "_", grid_bookends[1], "_", grid_bookends[2], "/")
    
# Checks to see if the intermediate data directory exists and if not then
# creates it
    if (!dir.exists(int_dir)) {
        dir.create(int_dir, recursive = TRUE)
    }

    

    
        
#==============================================================================#
#   INCIDENT TABLE CREATION                                                    #
#==============================================================================#
    
# This section does something kind of interesting. If no version of this
# table exists in the interim_data folder, it writes one there. If one exists,
# it skips writing that file. Either way, it also writes one to the raw data
# folder. 
    
    
# Creates the filenames
    core_fn <- paste(raw_table$Sensor[1], grid_bookends[1], grid_bookends[2],
        "incidents.csv", sep = "_")
    raw_fn <- paste0(raw_dir, core_fn)
    int_fn <- paste0(int_dir, core_fn)
    
# Writes the incident data frame to file in the raw folder
# THIS HAPPENS EVERY TIME NO MATTER WHAT
# The idea is to preserve R's output unaltered somewhere
    write.csv(incident_df, raw_fn, row.names = FALSE)

# Conditionally writes the incident data frame to file in the interm folder
    if (!file.exists(int_fn)) {
        write.csv(incident_df, int_fn, row.names = FALSE)
        int_file_status <- "No incident csv file "
        int_write_status <- " "
    } else {
        int_file_status <- "An existing incident csv file "
        int_write_status <- " NOT "
    }
    
# Stores the status line for the processing receipt
    int_write_line <- paste0(int_file_status, "was detected in the folder: ",
        int_dir, ". A new incident data frame was therefore", int_write_status,
        "written to the folder: ", int_dir, ".")
    

    
    
    
        
#==============================================================================#
#   Primary diagnostic plot                                                    #
#==============================================================================#
  
# Creates a filename
    core_fn <- paste(raw_table$Sensor[1], grid_bookends[1], grid_bookends[2],
        "initial_diagnostic_plot.png", sep = "_")
    fn <- paste(int_dir, core_fn)

# Opens the png    
    png(fn, width = 13.333, height = 7.5, units = "in", res = 200)
    
# Opens the first figure panel
    par(fig = c(0, 1, 0, 0.8))
    
# Sets the framing parameters
    par(mar = c(4.5, 4.5, 1, 8))

# Plots the core plot
    plot(raw_table$DateTime, raw_table$CodeRound, cex = 0,
         ylim = c(0, 540), xlab = "Date", ylab = "Code")
    
# Plots the horizontal ping gap lines (used to help ID tags)    
    abline(h = seq(0, 800, by = 20), lty = 3, lwd = 0.5, col = "grey")
    
# Plots the vertical time lines (used to help infer time)
    abline(v = as.numeric(grid_times), lty = 3, lwd = 0.5, col = "grey")
    abline(v = as.numeric(grid_times[format(grid_times, "%H") == "00"]))
    abline(v = as.numeric(grid_times[format(grid_times, "%H") == "12"]),
       col = "grey")
    
# Plots a bounding box (cleans up after gridlines)
    box()
    
# Plots the pings
# First does the background noise pings
    points(raw_table$DateTime[is.na(raw_table$Tag_Code)], 
        raw_table$CodeRound[is.na(raw_table$Tag_Code)], col = "grey",
        pch = channel_pch[is.na(raw_table$Tag_Code)], cex = 1.25)
    points(raw_table$DateTime[!is.na(raw_table$Tag_Code)], 
        raw_table$CodeRound[!is.na(raw_table$Tag_Code)], col = hicon[3],
        pch = channel_pch[!is.na(raw_table$Tag_Code)], cex = 1.25)

# Plots the legends 
    legend("topright", title = "Channel", legend = c("2", "4", "9", "Other"), 
        pch = c(0, 2, 6, 4), bty = "n", inset = c(-0.1, 0), xpd = TRUE)
    legend("right", title = "Tag Code?", legend = c("Tag code", "Not tag code"),
        fill = c(hicon[3], "grey"), bty = "n", inset = c(-0.14, 0),
        xpd = TRUE)
    legend("bottomright", title = "Recommend\nfor further\ninvestigation?",
        legend = c("Recommend", "Not Recommend"), pch = c(1, 1),
        pt.cex = c(3, 0), col = hicon[1], pt.lwd = 2, bty = "n",
        inset = c(-0.15, 0), xpd = TRUE)

# Sequentially plots circles for each recommended incident
    r_incidents <- incident_df[incident_df$Recommend == 1, , drop = FALSE]
    if (nrow(r_incidents) > 1) {
        for (i in 1:nrow(r_incidents)) {
            x1 <- as.numeric(r_incidents[i, 3])
            x2 <- as.numeric(r_incidents[i, 4])
            x_mid <- mean(c(x1, x2))
            y <- as.numeric(substr(r_incidents[i, 2], 2, 4))
            points(x_mid, y, pch = 1, cex = 3, col = hicon[1], lwd = 2)
        }
    }

# Opens the next panel (noise)
    par(fig = c(0, 1, 0.8, 0.9), new = TRUE)
    
# Sets the framing parameters
    par(mar = c(1, 4.5, 1, 8))

# Plots the core plot
    plot(raw_table$DateTime, raw_table$CodeRound, cex = 0,
         ylim = c(990, 1000), axes = FALSE, ann = FALSE)

# Adds the y axis label
    mtext("Noise\n(995)", side = 2, line = 1)
    
# Plots the vertical time lines (used to help infer time)
    abline(v = as.numeric(grid_times), lty = 3, lwd = 0.5, col = "grey")
    abline(v = as.numeric(grid_times[format(grid_times, "%H") == "00"]))
    abline(v = as.numeric(grid_times[format(grid_times, "%H") == "12"]),
       col = "grey")
    
# Plots a bounding box (cleans up after gridlines)
    box()
    
# Plots the noise points
    points(raw_table$DateTime, raw_table$CodeRound, pch = 4)
 
# Opens the last panel (timer tag)
    par(fig = c(0, 1, 0.9, 1), new = TRUE)
    
# Sets the framing parameters
    par(mar = c(1, 4.5, 1, 8))

# Plots the core plot
    plot(raw_table$DateTime, raw_table$CodeRound, cex = 0,
         ylim = c(570, 580), axes = FALSE, ann = FALSE)

# Adds the y axis label
    mtext("Timer\nTag\n(575)", side = 2, line = 1)
    
# Plots the vertical time lines (used to help infer time)
    abline(v = as.numeric(grid_times), lty = 3, lwd = 0.5, col = "grey")
    abline(v = as.numeric(grid_times[format(grid_times, "%H") == "00"]))
    abline(v = as.numeric(grid_times[format(grid_times, "%H") == "12"]),
       col = "grey")
    
# Plots a bounding box (cleans up after gridlines)
    box()
    
# Plots the noise points
    points(raw_table$DateTime, raw_table$CodeRound, pch = 4)
        
    
    
    
   # Adds the site metadata label
    # Opens the panel for the incident metadata
        par(fig = c(0.9, 1, 0.8, 1), new = TRUE)
        
    # Resets the margins to 0
        par(mar = rep(0, 4))
        
    # Plots an empty plot
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        
    # Stores the sensor + tag code, the start time, and the stop time text
        text1 <- incident_df$Sensor[1]
        text2 <- paste0("Start:\n ", incident_df$Time_Start[1])
        text3 <- paste0("Stop:\n", incident_df$Time_Stop[nrow(incident_df)])
        
    # Adds the sensor + tag code, start time, and stop time text
        text(0.5, 0.75, text1, font = 2)
        text(0.5, 0.5, text2, cex = 0.8)
        text(0.5, 0.25, text3, cex = 0.8)
     
    
    
    
    
    
# Closes the device
    dev.off()
    
    
    
#==============================================================================#
#   INDIVIDUAL DIAGNOSTIC PLOTS                                                #
#==============================================================================#
    
# Handles what to do when there are a lot of pings
#    if (sum(incident_df$Tag_Pings >= 1) > 100) {
#        ping_counts <- sapply(1:5, function(x) {
#            inc_x <- ifelse(x == 1, "ping", "pings")
#            n <- sum(incident_df$Tag_Pings >= x)
#           paste0(x, " ", inc_x, " (", n, " incidences)")
#        })
#        user_choice <- tk_select.list(choices = ping_counts,
#            title = "There are a LOT of incidences!\nThis could take a while to\nWould you like to plot only incidences\nwith more pings?")
#        multi_val <- as.numeric(substr(user_choice, 1, 1))
#        multi_ping <- which(incident_df$Tag_Pings > 1)
#    } else {
#      multi_ping <- 1:nrow(incident_df)
#    }

    if (is.na(incident_df$Tag_Pings[1])) {
        multi_ping <- NULL
    } else {
      if (sum(incident_df$Tag_Pings >= 1) > 500) {
          multi_ping <- which(incident_df$Tag_Pings > 5)
      } else {
          if (sum(incident_df$Tag_Pings >= 1) > 100) {
              multi_ping <- which(incident_df$Tag_Pings > 2)
          } else {
              multi_ping <- 1:nrow(incident_df)
          }
      }
    }
    
# Iteratively plots each incident in the incident df
    for (i in multi_ping) {
        
    # Stores the individual incident   
        inc_i <- incident_df[i, , drop = FALSE]
        
    # Creates a timing buffer of 5 minutes on either end of the incident
    # This just makes plotting prettier
        i_ind <- raw_table$DateTime >= inc_i$Time_Start - (5 * 60) &
            raw_table$DateTime <= inc_i$Time_Stop + (5 * 60)
        
    # Stores the raw data associated with the incident timing    
        raw_i <- raw_table[i_ind, , drop = FALSE]
        
    # Stores a new filename
        fn_i <- apply(inc_i[, 1:4], 1, paste, collapse = "_")
        fn_i <- gsub(" ", "_", fn_i)
        fn_i <- gsub(":", "-", fn_i)
        fn_i <- paste0(fn_i, ".png")
        fn_i <- paste(int_dir, fn_i)
        
    # Opens a png device
        png(fn_i, width = 8, height = 6, units = "in", res = 200)

  # Stores the tag color
  # This is hopefully a sufficiently high contrast color to be seen across a
  # range of conditions
      test_col <- viridis(10)[1]

    # Opens the first figure panel
        par(fig = c(0, 1, 0, 0.5))
        
    # Sets the framing parameters
        par(mar = c(4.5, 4.5, 1, 10))
    
    # Stores some Y limit values
        ylim_i <- range(raw_i$CodeRound[!is.na(raw_i$Date) &raw_i$CodeRound < 545])
        if (ylim_i[1] == ylim_i[2]) {
            ylim_i[1] <- ylim_i[1] - 0.5
            ylim_i[2] <- ylim_i[2] + 0.5
            
        }
        
    # Plots the core plot
        plot(raw_i$DateTime, raw_i$CodeRound, cex = 0, ylim = ylim_i,
          xlab = "Time", ylab = "Code")
        
    # Plots the horizontal ping gap lines (used to help ID tags)    
        abline(h = seq(0, 800, by = 20), lty = 3, lwd = 0.5, col = "grey")
        
    # Plots the vertical time lines (used to help infer time)
        abline(v = as.numeric(grid_times), lty = 3, lwd = 0.5, col = "grey")
        abline(v = as.numeric(grid_times[format(grid_times, "%H") == "00"]))
        abline(v = as.numeric(grid_times[format(grid_times, "%H") == "12"]),
           col = "grey")
        
    # Plots a bounding box (cleans up after gridlines)
        box()
        
    # Plots the pings
        points(raw_i$DateTime, raw_i$CodeRound, pch = channel_pch[i_ind],
            col = ifelse(raw_i$Tag_Code %in% inc_i$Tag_Code, test_col, "grey"),
            cex = 1.25)
    
    # Plots the legends 
        legend("topright", title = "Channel", legend = c("2", "4", "9",
            "Other"), pch = c(0, 2, 6, 4), bty = "n", inset = c(-0.35, 0),
            xpd = TRUE, ncol = 2)
        legend("bottomright", title = "Focal tag?", legend = c("Focal tag",
            "Not focal tag"), fill = c(test_col, "grey"), bty = "n",
            inset = c(-0.35, 0), xpd = TRUE)
    
    # Opens the next panel (noise)
        par(fig = c(0, 1, 0.8, 0.9), new = TRUE)
        
    # Sets the framing parameters
        par(mar = c(1, 4.5, 1, 10))
    
    # Plots the core plot
        plot(raw_i$DateTime, raw_i$CodeRound, cex = 0,
             ylim = c(990, 1000), axes = FALSE, ann = FALSE)
    
    # Adds the y axis label
        mtext("Noise\n(995)", side = 2, line = 1)
        
    # Plots the vertical time lines (used to help infer time)
        abline(v = as.numeric(grid_times), lty = 3, lwd = 0.5, col = "grey")
        abline(v = as.numeric(grid_times[format(grid_times, "%H") == "00"]))
        abline(v = as.numeric(grid_times[format(grid_times, "%H") == "12"]),
           col = "grey")
        
    # Plots a bounding box (cleans up after gridlines)
        box()
        
    # Plots the noise points
        points(raw_i$DateTime, raw_i$CodeRound, pch = 4)
     
    # Opens the last panel (timer tag)
        par(fig = c(0, 1, 0.9, 1), new = TRUE)
        
    
    # Plots the core plot
        plot(raw_i$DateTime, raw_i$CodeRound, cex = 0,
             ylim = c(570, 580), axes = FALSE, ann = FALSE)
    
    # Adds the y axis label
        mtext("Timer\nTag\n(575)", side = 2, line = 1)
        
    # Plots the vertical time lines (used to help infer time)
        abline(v = as.numeric(grid_times), lty = 3, lwd = 0.5, col = "grey")
        abline(v = as.numeric(grid_times[format(grid_times, "%H") == "00"]))
        abline(v = as.numeric(grid_times[format(grid_times, "%H") == "12"]),
           col = "grey")
        
    # Plots a bounding box (cleans up after gridlines)
        box()
        
    # Plots the noise points
        points(raw_i$DateTime, raw_i$CodeRound, pch = 4)
                
    # Sets up for the signal strength panel
    # Extracts the target tag rows, the timer tag rows, and noise rows
        all_rows <- 1:nrow(raw_i)
        tag_rows <- all_rows[raw_i$Tag_Code == inc_i$Tag_Code[1]]
        timer_rows <- all_rows[raw_i$Code == 575]
        noise_rows <- all_rows[!all_rows %in% c(tag_rows, timer_rows)]
        
    # Opens the fourth panel: signal strength
        par(fig = c(0, 1, 0.5, 0.8), new = TRUE)
    
    # Plots the basic empty plot
        plot(raw_i$DateTime, raw_i$Strength, ylim = c(-130, -70), cex = 0,
          xaxt = "n", ylab = "Strength")
    
    # Adds tag strength, noise strength, and timer strength, in that order
       segments(as.numeric(raw_i$DateTime[tag_rows]),
            rep(-130, length(tag_rows)),
            as.numeric(raw_i$DateTime[tag_rows]),
            raw_i$Strength[tag_rows], col = test_col)
         
        segments(as.numeric(raw_i$DateTime[noise_rows]),
            rep(-130, length(noise_rows)),
            as.numeric(raw_i$DateTime[noise_rows]),
            raw_i$Strength[noise_rows], col = "grey60")

#        segments(as.numeric(raw_i$DateTime[timer_rows]),
##            rep(-130, length(timer_rows)),
#            as.numeric(raw_i$DateTime[timer_rows]),
#            raw_i$Strength[timer_rows], col = "black")

    # Adds an average tag ping strength line
   #     abline(h = mean(raw_i$Strength[tag_rows], na.rm = TRUE), lwd = 2,
    #           col = test_col, lty = 2)
        
    # Adds a bounding box
        box()
        
    # Adds a legend
        legend("right", title = "Signal type", legend = c(inc_i$Tag_Code[1],
            "Noise (all)"), lty = 1, col = c(test_col, "grey"),
            bty = "n", inset = c(-0.3, 0), xpd = TRUE)

    # Adds the incident metadata label
    # Opens the panel for the incident metadata
        par(fig = c(0.75, 1, 0.75, 1), new = TRUE)
        
    # Resets the margins to 0
        par(mar = rep(0, 4))
        
    # Plots an empty plot
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        
    # Stores the sensor + tag code, the start time, and the stop time text
        text1 <- paste(inc_i$Sensor, inc_i$Tag_Code, sep = ": ")
        ping_text <- ifelse(inc_i$Tag_Pings == 1, "ping", "pings")
        text1 <- paste0(text1, " (", inc_i$Tag_Pings, " ", ping_text, ")")
        text2 <- paste0("Start: ", inc_i$Time_Start)
        text3 <- paste0("Stop: ", inc_i$Time_Stop)
        
    # Adds the sensor + tag code, start time, and stop time text
        text(0.5, 0.75, text1, font = 2)
        text(0.5, 0.5, text2, cex = 0.8)
        text(0.5, 0.25, text3, cex = 0.8)
    
        
    # Closes the device
        dev.off()
        
# Exits the loop
    }
    
    
    
    
    
#==============================================================================#
#   PROCESSING RECEIPT GENERATION                                              #
#==============================================================================#
    
  
# Stores the processing receipt file name
    receipt_fn <- gsub(".txt", "_processing_receipt.txt", focal_file)

# Opens the file connection
    sink(receipt_fn)
    
# Stores and prints the opening line
    open_line <- paste0("Processing receipt for ", focal_file)
    cat(open_line)
    cat("\n")
    cat("Processed on ")
    cat(as.character(Sys.time()))
    cat("\n\n")

# Input file structure
    cat("Input File Structure\n--------------------\n")
    cat("Rows: ")
    cat(dim_x)
    cat("\n")
    cat("Columns: ")
    cat(dim_y)
    cat("\n\n\n")

# Stores and prints the site
    cat("Site\n----\n")
    cat(raw_table$Sensor[1])
    cat("\n\n\n")

# Stores and prints the timing of observations
    cat("Date and Time\n-------------\n")
    cat("Start time of observations: ")
    cat(as.character(min(raw_table$DateTime, na.rm = TRUE)))
    cat("\n")
    cat("Stop time of observations: ")
    cat(as.character(max(raw_table$DateTime, na.rm = TRUE)))
    cat("\n")
    cat("Number of invalid dates/times: ")
    cat(length(bad_dt))
    cat("\n\n\n")
    
# Filter settings
    cat("Filter settings\n-------------\n")
    cat("Minimum tag ping count: ")
    cat(as.character(filters[1, 2]))
    cat("\n")
    cat("Maximum noise ping rate: ")
    cat(as.character(filters[2, 2]))
    cat("\n")
    cat("Minimum tag ping count: noise ping count ratio: ")
    cat(as.character(filters[3, 2]))
    cat("\n")
    cat("Minimum tag strength - noise strength difference (dB): ")
    cat(as.character(filters[4, 2]))
    cat("\n\n\n")

# Incidents
    cat("Incidents and Tags\n------------------\n")
    cat("Number of recommended incidents (potential observations): ")
    cat(as.character(sum(incident_df$Recommend == 1)))
    cat("\n")
    cat("Number of not recommended incidents (unlikely observations): ")
    cat(as.character(sum(incident_df$Recommend == 0)))
    cat("\n\n")
        
# Tags
    cat("Potential tags detected (passed filters): ")
    poss_tags <- sort(unique(incident_df$Tag_Code[incident_df$Recommend == 1]))
    imposs_tags <- sort(unique(incident_df$Tag_Code[incident_df$Recommend == 0]))
    imposs_tags <- imposs_tags[!imposs_tags %in% poss_tags]
    poss_tags <- paste(poss_tags, collapse = ", ")
    imposs_tags <- paste(imposs_tags, collapse = ", ")
    cat(poss_tags)
    cat("\n")
    cat("Other tags (did not pass filters): ")
    cat(imposs_tags)
    cat("\n\n\n")
    
    
# Output files
    cat("File outputs\n------------\n")
    cat("The incident data frame was saved in the raw folder as the file: ")
    cat(raw_fn)
    cat(".\nIf this file existed previously it may have been overwritten!!\n\n")
    cat(int_write_line)
    cat(" This is to protect any changes you have made to the file. If you'd like to replace it with the original, copy it over from: ")
    cat(raw_fn)
    cat(".\n\n")
    cat("Diagnostic plots were printed to: ")
    cat(int_dir)
    cat(".\n")
    
# Closes the file connection
    sink()
    
    
    
# R terminal output information
    cat(paste0("Processing completed for ", focal_file, " at ", Sys.time()), "!")
    
    cat("\n\nFile outputs:")
    cat(paste0("\n", raw_dir, ": incident data frame (original) and processing receipt"))
    cat(paste0("\n", int_dir, ": incident data frame (to be edited) and diagnostic plots"))

    if (length(bad_dt) > 0) {
        cat("\n\nYou had nonsensical date/times. You might wanna check on that.\n")
    }    
    
    
    
    
    
    
#==============================================================================#
#   WRITE HISTORY FILE GENERATION                                              #
#==============================================================================#
    
  
# Stores the processing receipt file name
    history_fn <- gsub(".txt", "_write_history.txt", focal_file)

# Opens the file connection
    sink(history_fn, append = TRUE)
   
# Writes text
    cat("The file was analyzed on ")
    cat(as.character(Sys.time()))
    cat(" with the following filter settings:\n")
    
# Filter settings
    cat("Minimum tag ping count: ")
    cat(as.character(filters[1, 2]))
    cat("\n")
    cat("Maximum noise ping rate: ")
    cat(as.character(filters[2, 2]))
    cat("\n")
    cat("Minimum tag ping count: noise ping count ratio: ")
    cat(as.character(filters[3, 2]))
    cat("\n")
    cat("Minimum tag strength - noise strength difference (dB): ")
    cat(as.character(filters[4, 2]))
    cat("\n\n\n")

# Closes the file
    sink()
    
    
    
        
    
#______________________________________________________________________________#
#                                                                              #    
#==============================================================================#
#   END OF SCRIPT                                                              #
#==============================================================================#