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
setClass("eeg_dataset", representation(subject_id = "character",
                                       time_stamps = "numeric", 
                                       sampling_rate = "numeric",
                                       continuous_data = "numeric"))
#######
# Generic functions (used as methods below)
#######
setGeneric("plot_channel", function(object, channel_number) standardGeneric("plot_channel"))

#######
# Methods
#######

# Plot a channel
setMethod(f = "plot_channel", signature = "eeg_dataset", 
          definition = function(object, channel_number){
                  plot(object@time_stamps, object@continuous_data)
          })
