#==============================================================================#
#   LOWER GRANITE WALLEYE RADIO TELEMETRY DATA MANAGEMENT SCRIPT:              #
#   MAIN USER SCRIPT (RUN THIS SCRIPT AND NOT THE OTHERS)                      #
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
#   RUN FROM HERE                                                              #
#==============================================================================#

# Run this set of lines the first time you run this script, and any time that
# you update R. This set of lines checks for and installs necessary packages.
# It doesn't hurt to re-run it, but you don't need to once they are installed.
# THE CODE WILL NOT RUN WITHOUT THESE PACKAGES
    all_packages <- installed.packages()
    if (!"khroma" %in% all_packages) {
        install.packages("khroma")
    }
    if (!"viridis" %in% all_packages) {
        install.packages("viridis")
    }
    if (!"plotrix" %in% all_packages) {
        install.packages("plotrix")
    }

    
    

# Run the line below to conduct data management and QAQC.
# Highlight the line below and select "Run" from above -----------------------^
    source("qaqc_front_end.R")
    
    
    
# Run the line below to conduct data assembly.
# THIS SHOULD ONLY BE DONE AFTER HUMAN QAQC OF INCIDENT CALLS FROM R!!!
# GO BACK TO RESULTS IN /interm_data/ AND VERIFY THE CALLS ARE GOOD!!!
# DANGER DO NOT PROCEED WITHOUT HUMAN QAQC!!!
# Highlight the line below and select "Run" from above -----------------------^
    source("assembly_front_end.R")
    
    
    
#==============================================================================#
#   RUN TO HERE                                                                #
#==============================================================================#
    
    
    
#______________________________________________________________________________#
#                                                                              #    
#==============================================================================#
#   END OF SCRIPT                                                              #
#==============================================================================#



