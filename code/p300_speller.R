# Import necessary libraries
require(ggplot2)
require(tools)
require(stringr)
require(fastICA) # this will be required for some data cleaning later

# Set working directory
setwd("D:/GitHub/Kaggle/code")

# We have 56 electrodes in this recording montage. This can be hard coded. 
number_of_channels <- 56

# We are using a constant erp_label
#   This is used to load and write data (generally)
erp_label <- '-erp_filtered_0.05to20_epoched_-50to1800'

# Subject information
subjects.train = c('S02', 'S06', 'S07', 'S11', 'S12', 'S13', 'S14', 'S16', 'S17', 'S18', 'S20', 'S21', 'S22', 'S23', 'S24', 'S26')
subjects.test = c('S01', 'S03', 'S04', 'S05', 'S08', 'S09', 'S10', 'S15', 'S19', 'S25')

import_eeg.csv.group <- function(subjects){
    "
    Function to convert subject CSV files into RData files. This is essentially a wrapper for import_eeg.csv.

    subjects: list of subject IDs
    "
    for(subject in subjects){
        
        # Name of CSV file for this subject
        file_name <- paste(getwd(), '..', 'P300_Speller', subject, 'analysis', paste0(subject, erp_label, '.csv'), sep = .Platform$file.sep)
        
        # Call import function
        import_eeg.csv(file_name, save_as_robj = TRUE)
    }
}

import_eeg.csv <- function(file_name, save_as_robj = TRUE){
        
        "
        Import EEG datasets exported from MATLAB as csv. 

        Data are multiplexed, with each row containing 56 channels and N time 
        points. 

        First row contains time_stamps (useful for plotting).

        First column contains class labels
                1: correct feedback
                2: incorrect feedback
                3: Unknown (unlabeled). Should only be present in test subjects. 

        Remaining data are the EEG data (in microVolts)

        INPUT:

                file_name: path to exporte CSV file

        RETURN:

                data_matrix: raw EEG data

                time_stamps: you guessed it. Time stamps

                class_labels: yeah. class labels. rocket science
        "
        
        # Read in raw data from csv
        data_matrix <- as.matrix(read.csv(file_name, header = FALSE, sep=",", colClasses = "numeric"))
        
        # Data dimensions
        data_dims <- dim(data_matrix)        
        
        # Get time stamps (first row)
        time_stamps <- data_matrix[1,2:data_dims[2]]
                
        # Remove class labels (first column)
        #   Skip first row since this is the time stamps
        class_labels <- data_matrix[2:data_dims[1],1]
        
        # Remove time_stamps and class_labels from data_matrix
        data_matrix <- data_matrix[2:data_dims[1],2:data_dims[2]]
        
        # Data dimensions
        data_dims <- dim(data_matrix)   
        
        # Create ERPs
        #   The time averaged for each condition (1 and 2) are computed 
        make_erp <- function(label){
            
            # Get the trial by trial traces (sweeps) for this condition
            erp <- data_matrix[class_labels == label, ]
            
            # Average over trials
            erp <- as.matrix(colMeans(erp))
            
            # Reshape to channel x time 
            dim(erp) <- c(data_dims[2]/number_of_channels, number_of_channels)
            
            # Transpose erp so it's channels x time points
            erp <- t(erp)
            
            return(erp)
        }
        
        # Now, calculate correct/incorrect feedback ERPs
        erp_correct <- make_erp(1)
        erp_incorrect <- make_erp(2)
        erp_unknown <- make_erp(3)
        
        data <- list("time_stamps" = time_stamps, "class_labels" = class_labels, "sweeps" = data_matrix, 
                            "erp_correct" = erp_correct, "erp_incorrect" = erp_incorrect, "erp_unknown" = erp_unknown)
        
        # Save the data as an R file        
        if(save_as_robj){
            
            # Replace extension of filename with .Rdata
            pathstr = dirname(file_name)
            base = basename(file_name)
            ext = file_ext(file_name)
            
            # Output name
            output_file = paste(pathstr, base, sep = .Platform$file.sep)
            output_file = str_replace(output_file, ext, 'RData')
            
            # Write data to file
            save(data, file = output_file)
            
        }
        
        return(data)
        
}

"
bychan2row changes a channel x time matrix to a 1 x (channel x time) matrix.

This proved useful when converting channel x time erps to by row sweeps
"
erpbychan2row <- function(data){
    
    # Only do this if the data are valid.
    if(class(data) != "NULL"){
        # Get dimensions so we can resize the data properly
        data <- t(data)
        dim(data) <- c(1, dim(data)[1]*dim(data)[2])
    }
    
    
    return(data)
}

"
Import group data and sort it into a list

"
import_eeg.rdata <- function(subjects, erp_label){
    
    group_data <- list() 
    for(i in 1:length(subjects)){
        
        # Get current subject
        subject = subjects[i]
        
        # Get the RData file
        input_file = paste(getwd(), '..', 'P300_Speller', subject, 'analysis', paste0(subject, erp_label, '.RData'), sep = .Platform$file.sep)
        
        # Load the RData
        load(input_file)
        
        # Redo ERP calculations to reflect whatever format we currently what them in. Call make.erp to do this.
        data$erp_correct = erpbychan2row(data$erp_correct)
        data$erp_incorrect = erpbychan2row(data$erp_incorrect)
        data$erp_unknown = erpbychan2row(data$erp_unknown)
        
        # Extend time stamps
        
        # Assign to return list        
        group_data[[length(group_data) + 1]] <- data
        
    }
    
    return(group_data)
}

"
Apply t-test filter to difference wave
"


"
Average data at group level. Assumes the data are in precisely the same order
and dimensions.

This can also be used to create single-subject ERPs by passing in a dataset with
only a single element (i.e., 1 subject's data)

This should work for time or frequency, probably not time-frequency plots. 
Need to think about the latter more, if we need to go that route. Still need
to look at single trial sweeps to see if there's consistent information at any
time-frequency bin.
"
make.erp <- function(group_data){
    
    # Initialize return variables
    erp_dims <- dim(group_data[[1]]$erp_correct)
    erp_correct <- matrix(0, erp_dims[1], erp_dims[2])
    erp_incorrect <- matrix(0, erp_dims[1], erp_dims[2])
    erp_unknown <- matrix(0, erp_dims[1], erp_dims[2])
    
    # Get time stamps for the ERP
    time_stamps <- group_data[[1]]$time_stamps[1:erp_dims[2]]
    
    # Loop through all list elements (one per subject) and compute the group average
    for(i in 1:length(group_data)){
        
        # Need a way to test for conditional existence of these fields. Not all 
        # subjects will have all ERP types (recall that test subjects only have erp_unknown)
        if(class(group_data[[i]]$erp_correct) != "NULL"){
            erp_correct = erp_correct + group_data[[i]]$erp_correct    
        }
        
        if(class(group_data[[i]]$erp_incorrect) != "NULL"){
            erp_incorrect = erp_incorrect + group_data[[i]]$erp_incorrect    
        }
        
        if(class(group_data[[i]]$erp_unknown) != "NULL"){
            erp_unknown = erp_unknown + group_data[[i]]$erp_unknown    
        }        
        
    }
    
    # Divide my number of subjects to compute mean
    erp_correct = erp_correct/length(group_data)
    erp_incorrect = erp_incorrect/length(group_data)
    erp_unknown = erp_unknown/length(group_data)
    
    return_list <- list("time_stamps" = time_stamps, "erp_correct" = erp_correct, "erp_incorrect" = erp_incorrect, "erp_unknown" = erp_unknown)

    return(return_list)
    
}

"
This function performs a leave-one out maximum likelihood classification based 
on the Pearson's correlation coefficient. 

This was originally written for use with spatio-temporal brain data (that is, 
ERPs), but should scale to time-frequency representations without issue, 
provided the scale is linear (or log transformed to BE linear in the case of power)

Note: This function was modified to accept a training and test data set, which 
should (theoretically) be more flexible.

data.train: data set from training subjects. Format follows return from 
import_eeg.rdata

data.test: data set from test subjects.

evaluate_train: Returns predictions from training set using an N-1 analysis.

evaluate_test: returns predictions from the test set using the training set as a 
template.
"
classify.pcc <- function(data.train, data.test, cross_validate = TRUE){
     
    # Initialize prediction variables
    prediction.test <- matrix(nrow=dim(data.test[[1]]$sweeps)[1], ncol = length(data.test))
    prediction.cross_val <- matrix(nrow=dim(data.train[[1]]$sweeps)[1], ncol = length(data.train))
    
    # Initialize label variables
    #   Massaging these into a similar format will make scoring much easier later.
    label.test <- matrix(nrow = dim(prediction.test)[1], ncol = dim(prediction.test)[2])
    label.cross_val <- matrix(nrow = dim(prediction.cross_val)[1], ncol = dim(prediction.cross_val)[2])
    
    # Run cross-validation?
    #   The cross-validation procedure uses an N-1 procedure to validate the performance of the algorithm.
    if(cross_validate){
        
        for(i in 1:length(data.train)){
            
            # Use one subject as the cross-validation set
            cv.test <- data.train[i]
            
            # Use all remaining subjects as the training set
            cv.train <- data.train[1:length(data.train) != i]
            
            # Sommersault
            cv.pred <- classify.pcc(cv.train, cv.test, cross_validate = FALSE)
            
            # Assign to return variables
            prediction.cross_val[,i] <- cv.pred$prediction.test
            label.cross_val[,i] <- cv.test[[1]]$class_labels
            
        } # for(i in ...)
        
    } # if(cross_validate)
    
    
    # Prediction matrix will be used to evaluate classifier
    #prediction <- matrix()
    
    # Compute the template data. 
    #   These data are used as the templates to which single-trial sweeps are compared.
    #   We have a template for correct and incorrect feedback events.
    erp.train <- make.erp(data.train)
    erp.correct <- t(erp.train$erp_correct)
    erp.incorrect <- t(erp.train$erp_incorrect)
    
    # Concatenate channels
    #   Doing this will force templates to match the spatio-temporal samples of the individual sweeps below. 
    erp.dims <- dim(erp.correct)
    dim(erp.correct) <- c(erp.dims[1] * erp.dims[2], 1)
    dim(erp.incorrect) <- c(erp.dims[1] * erp.dims[2], 1)   
    
    for(i in 1:length(data.test)){
        
        for(t in 1:dim(data.test[[i]]$sweeps)[1]){
            
            # Make single-sweep prediction
            sweep <- data.test[[i]]$sweeps[t,]
            class.label <- data.test[[i]]$class_labels[t]
            
            # Compute PCC against each template (correct/incorrect feedback)            
            pcc <- (c( cor(x = sweep, y=erp.correct), cor(x = sweep, y = erp.incorrect)))
            
            # Make prediction
            prediction.test[t,i] <- which.max(pcc)
            
        } # for(t in 1:dim ...)
        
        # Assign class labels
        label.test[,i] <- data.test[[i]]$class_labels
        
    } # for(i in 1:length(data.test))
    
    # Return predictions for cross validation and test sets. 
    return(list("prediction.test" = prediction.test, "prediction.cross_val" = prediction.cross_val, "label.test" = label.test, "label.cross_val" = label.cross_val))
    
}

"
Here we evaluate classifier performance by calculating sensitivity, specificity,
and accuracy.

Returns a list with these fields. For formulas and discussion, see:

Margaux, P., et al. (2012). Objective and Subjective Evaluation of Online Error Correction during P300-Based Spelling. Advances in Human-Computer Interaction 2012: 13.

labels and predictions must be NxS matrices, where N is the number of observations (sweeps) and S is the number of subjects

Will return a list containing 1xS measures of accuracy, sensitivity, and specificity.

"
classifier.eval <- function(labels, predictions){
    
    # Initialize return variables
    accuracy <- matrix(ncol=1, nrow=dim(labels)[2])
    specificity <- matrix(ncol=1, nrow=dim(labels)[2])
    sensitivity <- matrix(ncol=1, nrow=dim(labels)[2])
    
    # Loop through all subjects
    for(i in 1:dim(labels)[2]){
        # A positive case will be when the feedback is INCORRECT. This might get
        # confusing.    
        mask_positive = labels[,i] == 2; # incorrect feedback
        mask_negative = labels[,i] == 1; # correct feedback
        
        # Calculate number of true_positives, false_positives, true_negatives, and false_negatives
        true_positives = length(which(labels[mask_positive,i] == predictions[mask_positive,i]))
        true_negatives = length(which(labels[mask_negative,i] == predictions[mask_negative,i]))
        
        false_positives = length(which(predictions[mask_positive,i] != labels[mask_positive,i] ))
        false_negatives = length(which(predictions[mask_negative,i] != labels[mask_negative,i] ))
        
        # Accuracy
        accuracy[i] <- (true_positives + true_negatives)/(true_positives + true_negatives + false_positives + false_negatives)
        
        # Sensitivity
        sensitivity[i] <- (true_positives)/(true_positives + false_negatives)
        
        # Specificity
        specificity[i] <- (true_negatives)/(true_negatives + false_positives)
        
    }
        
    return(list("accuracy" = accuracy, "sensitivity" = sensitivity, "specificity" = specificity))
    
}

"
This function builds submissions from prediction data. 

At the moment, this makes a lot of super stupid assumptions about the data layout. Needs to be made much smarter before it will be usable. 
"
submission.csv <- function(predictions, template = "D:/GitHub/Kaggle/P300_Speller/SampleSubmission.csv", filename){
    
    # Read in the template file
    template <- read.csv(template)
    
    
    # Do stuff with template
    for(i in 1:dim(predictions)[2]){
        
        if(i == 1){
            vpred <- as.matrix(predictions[,i])
        }
        else{
            vpred <- rbind(vpred, as.matrix(predictions[,i]))
        }
        
    }
    
    # Overwrite data in prediction column
    template$Prediction <- as.numeric(!(vpred - 1))
    
    # Write to file
    write.csv(template, file = filename, row.names = FALSE)
}

data.trim <- function(data, time_range = c(-Inf, Inf)){
    
    for(i in 1:length(data)){
        
        # Mask based on time
        data[[i]] <- mask.df.time(data[[i]], time_range)
        
    }
    
    return(data)
}
"
Create a logical mask for a data frame.

Accepts key/value pairs, where each key is the name of a data frame field and each
value specifies the lower and upper bounds of that 

"
mask.df.time <- function(mylist, time_range){
    
    mask <- mylist$time_stamps >= time_range[1] & mylist$time_stamps <= time_range[2]    
    
    # Get all names of dataframe, then apply logical mask to all fields.
    df.names <- names(mylist)
    mylist.mask <- list(mylist[['time_stamps']][mask], mylist[['class_labels']], mylist[['sweeps']][,mask], as.matrix(mylist[['erp_correct']][mask]), as.matrix(mylist[['erp_incorrect']][mask]), as.matrix(mylist[['erp_unknown']][mask]))
    
    names(mylist.mask) <- names(mylist)
    
    return(mylist.mask)
    
}


"
This function performs a leave-one out maximum likelihood classification based 
on the Pearson's correlation coefficient. 

This was originally written for use with spatio-temporal brain data (that is, 
ERPs), but should scale to time-frequency representations without issue, 
provided the scale is linear (or log transformed to BE linear in the case of power)
"

# classify.pcc <- function(group_data){
#     
#     percentage_correct = numeric()
#     for(i in 1:length(group_data)){
#         
#         # Test data
#         #   We'll test each sweep individually.
#         test_data <- group_data[[i]]        
#         
#         # Template data to which 
#         template_data <- group_data[1:length(group_data) != i]
#         
#         # Compute group ERP
#         erp <- make.erp(template_data)
#         
#         # Assign to easier to use variables
#         erp_correct <- t(erp$erp_correct)
#         erp_incorrect <- t(erp$erp_incorrect)
#         
#         # Reshape to concatenate channels
#         erp_dims <- dim(erp$erp_correct)
#         dim(erp_correct) <- c(erp_dims[1] * erp_dims[2], 1)
#         dim(erp_incorrect) <- c(erp_dims[1] * erp_dims[2], 1)        
#         
#         # Now, classify each sweep based on PCC        
#         n_correct = 0
#         for(t in 1:dim(test_data$sweeps)[1]){
#             
#             # Get the sweep
#             sweep <- test_data$sweeps[t,]
#             
#             # class_label
#             class_label <- test_data$class_labels[t]
#             # Compute correlation with correct/incorrect erp
#             pcc <- (c( cor(x=sweep, y=erp_correct), cor(x=sweep, y=erp_incorrect)))
#             
#             # Class based on max
#             #   Take absolute value because negative correlations are fine. 
#             prediction <- which.max(pcc)
#             
#             if(prediction == class_label){
#                 n_correct = n_correct + 1
#             }
#         }
#         
#         # Compute % correct
#         
#         percentage_correct[i] = (n_correct / t)*100
#         
#     }
#     
#     # Return performance
#     return(percentage_correct)
# }