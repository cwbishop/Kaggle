# Import necessary libraries
require(ggplot2)
require(signal)

# Create a dataset class to make things simpler and more organized.
#       This will be an S4 class, as described here ...
#       http://www.pitt.edu/~njc23/Lecture4.pdf
#
# The eeg_dataset will require the following data fields:
#
#       subject_id: subject identifier
#
#       filenames: a data frame containing file information used to load channel
#                  locations, event data, etc.
#
#               data_files: a list of data files from which data were read. 
#               
#               channel_locations: file from which channel locations are read
#
#               train_labels: file from which training labels are read
#
#       channel_location: path to channel locations file
#       
#       train_labels: path to file containing training labels 
#
#       
setClass("eeg_dataset", representation(subject_id = "character", continuous_data = "data.frame"))


import_training_dataset <- function(data_file, coord_file = "../ChannelLocation.csv", train_labels = "../TrainLabel.csv"){

        "Function imports a training dataset as a dataframe"
        
        # Get some information from the file name
        #       subject_id: subject identifier
        #       session_number: session number
        file_specs <- unlist(strsplit(basename(data_file), "_"))
        subject_id <- file_specs[2]
        session_number <- as.numeric(substr(file_specs[3], 5, 6))
        
        # Open the data file (CSV format)
        raw_data <- read.csv(data_file, header = TRUE, sep=",", colClasses = "numeric")
        
        # Open Coordinates
        coords <- read.csv(coord_file, header = TRUE, sep=",")
        
        # Read predicted labels
        #       These will be used below to create a "correct" and "incorrect"
        #       event label
        #
        #       Event codes: 
        #
        #               1: correct feedback
        #               2: incorrect feedback (wrong selection made)
        predicted_labels <- read.csv(train_labels)
        
        # Now we need to identify the predicted labels that match the data_file
        #       This will be done using some basic string matching of subject
        #       subject label and session number
        
        
        return(raw_data)
}