# Import necessary libraries
require(ggplot2)
require(tools)
require(stringr)
require(fastICA) # this will be required for some data cleaning later
require(caret)

# Set working directory
setwd("D:/GitHub/Kaggle/code")

# We have 56 electrodes in this recording montage. This can be hard coded. 
number_of_channels <- 56

# We are using a constant erp_label
#   This is used to load and write data (generally)
erp_label <- '-erp_filtered_1to20_epoched_-50to1800_NEB'

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

erpbyrow2chan changes a (channel * time points) vector into a channels x time points matrix. 

"
erpbyrow2chan <- function(data, number_of_channels = 56){
    
    # Coerce data into a matrix
    data = matrix(data = data, nrow = 1)
    
    if(class(data) != "NULL"){
        
        ntimepoints = dim(data)[2] / number_of_channels
        
        dim(data) = c(ntimepoints, number_of_channels)        
        
        data = t(data)        
        
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
        
        # Let's also add a channel grouping variable.
        #  Need to know the number of time points per channel first.
        ntimepoints = dim(data$sweeps)[2]/number_of_channels
        
        for(i in 1:number_of_channels){
            
            if(i == 1){
                channel_label = matrix(data = i, nrow = 1, ncol = ntimepoints)
            }
            else{
                channel_label = cbind(channel_label, matrix(data = i, nrow = 1, ncol = ntimepoints))
            }
            
        }
        
        # Redo ERP calculations to reflect whatever format we currently what them in. Call make.erp to do this.
        data$erp_correct = erpbychan2row(data$erp_correct)
        data$erp_incorrect = erpbychan2row(data$erp_incorrect)
        data$erp_unknown = erpbychan2row(data$erp_unknown)
        
        # Append channel_label variable to data list
        data$channel_label =  channel_label
        
        # Assign to return list        
        group_data[[length(group_data) + 1]] = data
        
    }
    
    return(group_data)
}

"
Apply t-test filter to difference wave
"

"
Create subject specific ERPs from from sweeps.

    sweeps: N x T data matrix, where N is the number of sweeps and T is the number of time points. 

    sweep.labels: N-element vector of class labels. One per sweep.

    event.label: a grouping class variable. All sweeps that match the event label
                  are used in the average. 
"

make.erp.sweeps <- function(sweeps, sweep.labels, event.label){    
    
    # Get the trial by trial traces (sweeps) for this condition
    erp = sweeps[sweep.labels == event.label, ]
    
    # Average over the relevant trials
    erp = as.matrix(colMeans(erp))
    
    # Reshape to channel x time
    # dim(erp) = c(dim(sweeps)[2]/number_of_channels, number_of_channels)
    
    # Transpose erp so it's channels x time points
    # erp = t(erp)
    
    return(erp)
}

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
make.erp.group <- function(group_data, pflag = FALSE){
    
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
    
    # Create descriptive plots
    if(pflag){
        
        # Create the data frame with the data in it
        data2plot = data.frame("time_stamps" = group_data[[1]]$time_stamps, 
                               "Correct" = erp_correct, 
                               "Incorrect" = erp_incorrect, 
                               "Difference" = erp_incorrect - erp_correct)
        
        # ERP plot
        gg <- ggplot(data2plot, aes(x = time_stamps)) +
            geom_line(aes(y = Correct, colour = "Black")) +
            geom_line(aes(y = Incorrect, colour = "Red")) + 
            geom_line(aes(y = Difference, size = 2), colour = 'Blue') + 
            ggtitle(paste0("PC", as.character(i))) +
            scale_x_continuous(breaks = seq(0, 1500, 100))
        
        # Generate the plot
        print(gg)
        
    }
    
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
    erp.train = make.erp.group(data.train)
    erp.correct = erp.train$erp_correct
    erp.incorrect = erp.train$erp_incorrect
    
    # erp.train <- make.erp.group(data.train)
    # erp.correct <- t(erp.train$erp_correct)
    # erp.incorrect <- t(erp.train$erp_incorrect)
    
    # Concatenate channels
    #   Doing this will force templates to match the spatio-temporal samples of the individual sweeps below. 
    #erp.dims <- dim(erp.correct)
    #dim(erp.correct) <- c(erp.dims[1] * erp.dims[2], 1)
    #dim(erp.incorrect) <- c(erp.dims[1] * erp.dims[2], 1)   
    
    for(i in 1:length(data.test)){
        
        for(t in 1:dim(data.test[[i]]$sweeps)[1]){
            
            # Make single-sweep prediction
            sweep <- data.test[[i]]$sweeps[t,]
            class.label <- data.test[[i]]$class_labels[t]
            
            # Compute PCC against each template (correct/incorrect feedback)            
            pcc <- (c( cor(x = sweep, y = as.numeric(erp.correct)), cor(x = sweep, y = as.numeric(erp.incorrect))))
            
            # Make prediction
            prediction.test[t,i] <- which.max(pcc)
            
        } # for(t in 1:dim ...)
        
        # Assign class labels
        label.test[,i] <- data.test[[i]]$class_labels
        
    } # for(i in 1:length(data.test))
    
    # Return predictions for cross validation and test sets. 
    return(list("prediction.test" = prediction.test, "prediction.cross_val" = prediction.cross_val, "label.test" = label.test, "label.cross_val" = label.cross_val))
    
}

#########
" 
DESCRIPTION:

classify.logit applies logistic regression to data. 

INPUT:

    data:   subject data structure, as returned from import_eeg.rdata or similar.




"
classify.logit <- function(data.train, data.test, cross_validate = TRUE, pflag = TRUE, ...){
    
    # Run Cross-validation    
    if(cross_validate){
        
        # Remove one subject at at a time and run the analysis
        for(i in 1:length(data.train)){
            
            # Keep user updated of cross-validation progress
            # message()
            message(paste('Cross validation:', 'subject', as.character(i), 'of', as.character(length(data.train))))
            
            # Run the model withholding one subject
            cv.pred = classify.logit(data.train = data.train[-i], data.test = data.train[i], cross_validate = FALSE)
            
            # Assign to probability and label cross val
            tprobability.cross_val = matrix(cv.pred$probability.test, nrow = 1)
            tlabel.cross_val = matrix(cv.pred$label.test, nrow = 1)
            
            if(i == 1){
                probability.cross_val = tprobability.cross_val
                label.cross_val = tlabel.cross_val
            }
            else{
                probability.cross_val = rbind(probability.cross_val, tprobability.cross_val)
                label.cross_val = rbind(label.cross_val, tlabel.cross_val)
            }
            
            # Create a plot for this subject
            if(pflag){                
                
                data2plot = (data.frame(p = as.numeric(tprobability.cross_val), type = as.numeric(tlabel.cross_val)))                
                print(ggplot(data2plot, aes(x=1:340, y = p, colour = type)) + geom_point(size = 3))
                
            }
        }
        
    }
    else{
        
        # Assign empty values to these if cross validation is skipped
        probability.cross_val = matrix()
        label.cross_val = matrix()
        
    }
    
    # Concatenate sweeps and class labels into a data frame for modeling
    #   The last column contains class labels
    df.train = concat.sweeps.trial.group(data = data.train, make.dataframe = TRUE)
    df.test = concat.sweeps.trial.group(data = data.test, make.dataframe = TRUE)
    
    # Class column name
    #   we'll grab the name dynamically for 
    # col.name = names(df.train)[ncol(df.train)]
    
    # Fit training data with logistic regression
    #   We'll use glm for now, with no regularization. We can fix that later.
    model <- glm(Type ~ ., data = df.train, family = "binomial")
    
    # Estimate probability for each sweep
    for(i in 1:length(data.test)){
        
        # Convert sweeps to data frame
        df = concat.sweeps.trial.group(data = data.test[i], make.dataframe = TRUE)
        
        # Create predictions
        tprobability.test = matrix(data = predict(model, newdata = df[, 1:ncol(df)-1], type = "response"), nrow = 1)
                                  
        # Assign label.test
        tlabel.test = matrix(data = df[,ncol(df)], nrow = 1)
        if(i == 1){
            label.test = tlabel.test
            probability.test = tprobability.test
        }
        else{
            label.test = rbind(label.test, tlabel.test)
            probability.test = rbind(probability.test, tprobability.test)
        }
        
    }
    
    # Return probabilities and label information in a data structure. 
    return(list( "probability.cross_val" = probability.cross_val, 
                       "label.cross_val" = label.cross_val, 
                       "probability.test" = probability.test, 
                       "label.test" = label.test))
    
    
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
       
    # Create summary plots
    
    # Accuracy plot
    data2plot <- as.data.frame(cbind((subjects.train), (pcc.eval$accuracy)))
    names(data2plot) <- c("subject", "accuracy")
    print(ggplot(data2plot, aes(subject, accuracy)) + geom_point(size = 5))
    # summary(t(pcc.eval$accuracy))
    
    # Sensitivity plot
    data2plot <- as.data.frame(cbind((subjects.train), (pcc.eval$sensitivity)))
    names(data2plot) <- c("subject", "sensitivity")
    print(ggplot(data2plot, aes(subject, sensitivity)) + geom_point(size = 5))
    # summary(t(pcc.eval$sensitivity))
    
    # Specificity plot
    data2plot <- as.data.frame(cbind((subjects.train), (pcc.eval$specificity)))
    names(data2plot) <- c("subject", "specificity")
    print(ggplot(data2plot, aes(subject, specificity)) + geom_point(size = 5))
    # summary(t(pcc.eval$specificity))
    
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

data.trim <- function(data, time_range = c(-Inf, Inf), channels = 1:56){
    
    for(i in 1:length(data)){
        
        # Mask based on time
        data[[i]] <- mask.time(data[[i]], time_range)
        
        # Mask based on channels
        data[[i]] <- mask.channels(data = data[[i]], channels = channels)
        
    }
    
    return(data)
}

mask.channels <- function(data, channels){
    
    # Create a mask
    mask = is.element(data$channel_label, channels)
    
    # Apply the mask
    data.mask = mask.apply(data = data, mask = mask)
    
    # Return masked data
    return(data.mask)
    
}

mask.apply <- function(data, mask){
    
    # Apply the mask
    data.mask = list(time_stamps = data[['time_stamps']][mask], 
                     class_labels = data[['class_labels']], 
                     sweeps = data[['sweeps']][,mask], 
                     erp_correct = as.matrix(data[['erp_correct']][mask]), 
                     erp_incorrect = as.matrix(data[['erp_incorrect']][mask]), 
                     erp_unknown = as.matrix(data[['erp_unknown']][mask]), 
                     channel_label = as.matrix(data[['channel_label']][mask]))
}
"
Create a logical mask for a data frame.

Accepts key/value pairs, where each key is the name of a data frame field and each
value specifies the lower and upper bounds of that 

"
mask.time <- function(data, time_range){
    
    mask <- data$time_stamps >= time_range[1] & data$time_stamps <= time_range[2]        
        
    return(mask.apply(data = data, mask = mask))
    
}

" 
mean.chan creates a data list comprised of an average time waveform over the listed channels. 
This proved useful when collapsing across channels. When CWB looked at data traces
across electrodes within an individual, he found that they tended to be highly 
correlated across time. So an average over all the channels should capture most
(perhaps all?) the discriminating information for a given sweep or event. 

This function can be used to average over all channels. This is done by averaging
over channels for each sweep, then recomputing the ERPs for each event type (1 or 2).
This way there is tight agreement between sweeps and trial averages. 

    subject.data: this is the working list that includes class labels, sweeps, and erp information. 


"
mean.chan <- function(subject.data, channels = 1:number_of_channels){    
    
    # Initialize return list
    return.data = list()
    
    # For each subject data set, mask the sweeps by channels, then reshape, then average
    for(s in 1:length(subject.data)){
        
        # Initialize mean.data
        mean.data = list(); 
        
        # Create a channel mask
        #   We assume that the data are laid out in an identical fashion for all subjects
        #   This is a reasonably safe assumption for these data (cuz I designed it that way)
        #   Could get me in some trouble in the future if I do something stupid like change 
        #   the data structure. Here's hoping I'm not too sleep deprived ... oh wait. 
        channel_mask = is.element(subject.data[[1]]$channel_label, channels)
        
        # Reassign the channel labels
        # mean.data$channel_labels = subject.data[[s]]$channel_label[channel_mask]
        
        # Reassign time stamsp
        # mean.data$time_stamps = subject.data[[s]]$time_stamps[channel_mask]
        
        # Only include the specified channels
        sweeps = subject.data[[s]]$sweeps[,channel_mask]
        
        # Here we compute the average across channels. This is done by:
        #   1. Reshaping the 1 x (channel x time) sweep into a channel x time matrix (see erpbyrow2chan)
        #   2. Computing the average over all rows in each column. In other words, we 
        #      average over all remaining channels.
        #
        # The user will recall that the sweep data have been masked to include 
        # only the specified channels above. 
        for(i in 1:nrow(sweeps)){
            
            if(i==1){
                mean.sweeps = colMeans(erpbyrow2chan(sweeps[i,]))
            }
            else{
                mean.sweeps = rbind(mean.sweeps, colMeans(erpbyrow2chan(sweeps[i,])))
            }            
        }
        
        # Assign sweeps to mean.data
        mean.data$sweeps = mean.sweeps 
        
        # Reset time stamps
        #   We'll only have one data trace coming out of this, so grab the first N points of the time_stamps list
        mean.data$time_stamps = subject.data[[s]]$time_stamps[1:ncol(mean.sweeps)]
        
        # Assign bogus channel label to channel labels
        #   We'll return a single channel, so assign channel 1 to it all. 
        mean.data$channel_label = matrix(data = 1, nrow = 1, ncol = ncol(mean.sweeps))
        
        # Class labels remain unchanged
        mean.data$class_labels = subject.data[[s]]$class_labels
        
        # Recompute ERPs for this subject   sweeps, sweep.labels, event.label     
        mean.data$erp_correct = make.erp.sweeps(sweeps = mean.sweeps, sweep.labels = mean.data$class_labels, event.label = 1)
        mean.data$erp_incorrect = make.erp.sweeps(mean.sweeps, mean.data$class_labels, 2)
        mean.data$erp_unknown = make.erp.sweeps(mean.sweeps, mean.data$class_labels, 3)                
        
        # Append to (growing) return data list (of lists)
        return.data[[s]] = mean.data
    }
    
    # Return the (modified) data
    return(return.data)
    
    
    
    
}


##########
" 
This block of code deals with PCA based processing. Functions include ...

"

##########
"
DESCRIPTION:

concat.sweeps.chan converts an M x N matrix of sweeps (observations x time points)
into a channel (usually 56) x (M*N) matrix. This data massaging proved useful when
creating concatenated data sets for PCA estimation (noise removal and dimension reduction)

INPUT:

    sweeps: M x N matrix, where M is the number of sweeps and N is the number of 
            time points

    number_of_channels: number of channels (or alternatively components) in each 
                        sweep. This number is used to determine empirically the
                        number of time points within the sweep. (default = 56) 

OUTPUT:

    sweeps.concat:  C x T*M matrix, where C is the number of channels, T is the 
                    number of time points per observation and M is the number of 
                    observations.
"
concat.sweeps.chan <- function(sweeps, number_of_channels = 56){
    
    for(i in 1:nrow(sweeps)){
           
        tmp = erpbyrow2chan(sweeps[i,], number_of_channels = number_of_channels)
        
        if(i == 1){
            sweeps.concat = tmp
        }
        else{
            sweeps.concat = cbind(sweeps.concat, tmp)
        }
    } # for i=1:nrow(sweeps)
    
    return(sweeps.concat)
}

##########
"
DESCRIPTION:

concat.sweeps.chan.group is a group-level wrapper for concat.sweeps.chan. It 
returns the C x (T*M*S) data matrix, where C is the number of channels, T is the
number of time points per sweep, M is the number of observations, and S is the
number of subjects

This proved useful when concatenating data across subjects for PC Estimation.

INPUT:

    data:   list of length S, where S is the number of subjects. Each element must
            contain a sweeps field. 

OUTPUT:

    data.concat:    concatenated data, as described in description above.

"
concat.sweeps.chan.group <- function(data, number_of_channels = 56){
    
    
    # Loop through each subject's data
    for(s in 1:length(data)){
        
        # Update user on progress
        message(paste('Concatenating subject', as.character(s), 'of', as.character(length(data))))
        tmp = concat.sweeps.chan(data[[s]]$sweeps, number_of_channels = number_of_channels)
        # Get sweeps field and concatenate.
        if(s == 1){
            data.concat = tmp
        }
        else{
            data.concat = cbind(data.concat, tmp)
        }        
        
    }
    
    # Return concatenated sweeps
    return(data.concat)
}

##########
"
DESCRIPTION:

concat.sweeps.trial.group combines all sweeps and class_labels into a single 
subject data structure. This can then be manipulated to return 

INPUT:

    data:   subject data structure, as returned from import_eeg.rdata or similar

    make.dataframe: returns the data as a set of sweeps with appended class labels.
                    This proved useful when making models.
    
"
concat.sweeps.trial.group <- function(data, make.dataframe = FALSE){
    
    # Loop through all subjects, concatenate sweeps and channel labels.
    for(i in 1:length(data)){
        
        tsweeps = data[[i]]$sweeps
        tclass_labels = as.matrix(data[[i]]$class_labels)
        
        if(i == 1){
            sweeps = tsweeps
            class_labels = tclass_labels
        }
        else{
            sweeps = rbind(sweeps, tsweeps)            
            class_labels = rbind(class_labels, tclass_labels)
        }
        
    }
    
    # Create a return data structure
    data.concat = data[1]
    
    # Replace sweeps and class_labels
    data.concat[[1]]$sweeps = sweeps
    data.concat[[1]]$class_labels = as.numeric(class_labels)
    
    # Update ERPs and the like
    #   Probably not necessary for our applications, but better to keep things 
    #   consistent as much as possible. This will also update time stamps and
    #   the like. 
    data.concat = update.data.subject(data.concat)
    
    # Return as a data frame or as a data structure?
    if(make.dataframe){
        
        # Make the data frame
        df = data.frame(data.concat[[1]]$sweeps, data.concat[[1]]$class_labels - 1)
        
        # Add in the class labels name (Type). Makes modeling easier
        names(df)[ncol(df)] = 'Type'
        
        # Return the data frame
        return(df)
        
    }
    else{
        return(data.concat)
    }
}
##########
"
DESCRIPTION:

pca.svd.estimate pre-processes features (in our case, channels) and returns SVD
decomposition of the data.

Within the context of the P300 speller competition, CWB intended to feed in a 
concatenated time course (across sweeps, timepoints, and subjects) to get a 
group average PCA decomposition. The PCs and estimated scaling/centering can then
be used to project individual test sweeps.

Note that PCA (via SVD) is done on the covariance matrix here.

INPUT:

    data:   a C x (T * M) matrix, where C is the number of channels, T is the 
            number of time points and M is the number of trials.

OUTPUT:
    
    pc: a list with the following attributes
            $u: left singular values
            $v: right singular values
            $d: singular values (eigen value)
            $cmeans:    channel means (used for scaling)
            $cstd:  channel std (used for scaling)

Christopher W. Bishop
"
pca.svd.estimate <- function(data, center = TRUE, scale = TRUE, number_of_channels = 56, ...){
    
    # Sanity check to make sure channels is our rows.
    if(nrow(data) != number_of_channels){
        stop('Number of channels does not match the number of rows. Transpose?')
    }
    
    # Get the mean and standard deviation of each row (channel)
    #   To do this, we need to transpose the matrix
    data.scale = scale(t(data), scale = scale, center = center)    
    
    # Transpose the data so channels are rows.
    data.scale = t(data.scale)
    
    # Estimate covariance matrix
    #   Scale by the number of samples
    data.cov = (1/ncol(data.scale)) * ((data.scale) %*% t(data.scale))
    
    # Singular Value Decomposition (SVD)
    s = svd(data.cov)
    
    # Add scaling information to SVD
    #   This might be useful if we need to recover the data or scale test 
    #   sweeps. 
    s$cmeans = attributes(data.scale)$`scaled:center`
    s$cstd = attributes(data.scale)$`scaled:scale`
    
    # Return the SVD results
    return(s)
    
}

##########
"
DESCRIPTION:

pca.svd.project projects data onto the corresponding component. To do this, we 
center/scale the data using the mean/std of the training set. These data are then
projected onto the left singular values, which gives us a time course for each 
requested component.

INPUT:

    data:   a C x T data matrix, where C is the number of channels and T is the 
            number of samples

    pc:     pc is a list returned by pca.svd.estimate

    pc.index:   vector with the PCs to project the data onto.

    number_of_channels: You guessed it. The number of channels. Spooky. 

OUTPUT:

    data.project:   PC x T matrix, where PC is the number of components 
                    specified in pc.index

"
pca.svd.project <- function(data, pc, pc.index, number_of_channels = 56){
    
    # Sanity check to make sure channels is our rows.
    if(nrow(data) != number_of_channels){
        stop('Number of channels does not match the number of rows. Transpose?')
    }
    
    # Transpose data for scaling/centering purposes
    data = t(data)
    
    # Center and scale
    #   We can only center and scale if we have column means and cstd. 
    #   Statement below returns false if those are not present.
    if(!is.null(pc$cmeans)){
        data = scale(data, center = pc$cmeans, scale = pc$cstd)
    }
    
    # Subset U so we only project data onto specified PCs
    U = pc$u[,pc.index]
    
    data.project = data %*% U
    
    # Transpose so components are rows again
    data.project = t(data.project)
    
    # Return the projected data
    return(data.project)
        
}

##########
"
DESCRIPTION:

    pca.svd.project.sweeps is used to project a sweeps matrix (M x (TxC), where
    M is the number of Trials, T is the number of time points, and C is the 
    number of channels, onto one or more principle components. 

INPUT:
    
    data:   sweeps matrix with dimensions M x (T*C), where M is the number of 
            trials, T is the number of time points per trial, and C is the number
            of data channels (electrodes)

    pc:     pc is a list returned from pca.svd.estimate.

    pc.index:   vector, the PC indices to project the sweep onto. 

    number_of_channels: Gasp! The number of channels!

Christopher W Bishop
1/15
"
pca.svd.project.sweeps <- function(data, pc, pc.index, number_of_channels = 56){
    
    # Loop through each sweep (row) and project it onto the requested PCs
    for(i in 1:nrow(data)){
        
        # Get the concatenated sweep and break it into a channel x time point matrix
        # This is necessary for data project in pca.svd.project
        sweep = erpbyrow2chan(data[i,])
        
        sweep.project = erpbychan2row(pca.svd.project(data = sweep, pc = pc, pc.index = pc.index, number_of_channels = number_of_channels))
                                      
        if(i == 1){
            
            # We'll project the data onto the requested PCs, then reshape it into
            # a single row. That is, concatenate the compoents into a single
            # time course. 
            data.project = sweep.project
        }
        else{
            data.project = rbind(data.project, sweep.project)
        }
        
    }
    
    # Return the projected sweeps 
    return(data.project)
    
}
##########
"
DESCRIPTION:

    pca.svd.project.subjects alters the subject lists such that the sweeps and 
    ERPs are projected onto the specified PCs. Sweeps is replaced with the data
    projection, ERPs are recalculated, etc. 

INPUT
"
pca.svd.project.subjects <- function(data, pc, pc.index, number_of_channels = 56){
    
    # Loop through each subject and reset the sweeps, then update the ERPs/time
    # stamps/channel labels.
    for(i in 1:length(data)){
        
        # Give user some feedback so she knows where we are in the projection process
        message(paste('Projecting subject ', as.character(i), '/', as.character(length(data))))
        
        # Project sweeps
        data[[i]]$sweeps = pca.svd.project.sweeps(data = data[[i]]$sweeps, pc = pc, pc.index = pc.index, number_of_channels = number_of_channels)
        
        # Update the subject data structure
        #   This should update the ERPs, time stamps, etc.
        data[i] = update.data.subject(data = data[i])
        
    }
    
    # Return the updated data structure
    return(data)
    
}

##########
"
DESCRIPTION:

    pca.svd.erp creates an ERP based on PCA projections. More concretely, each 
    sweep is projected onto a specified number of components. The sweeps that 
    match the corresponding event label are then averaged to create an ERP. 

INPUT:

    data:   sweeps matrix, N x (T*C) where N is the number of sweeps, T is the 
            number of time points per epoch (sweep), and C is the number of 
            channels.

    sweep.labels:   N-element vector of class labels. 

    event.label:    integer, the class label we want to use to create an ERP

    pc:     this is the pc list returned from pca.svd.estimate. 

    pc.index:   vector specifying which PCs to project the data onto.

    number_of_channels: number of channels (Default = 56)

Christopher W. Bishop
1/15
"
pca.svd.erp <- function(data, sweep.labels, event.label, pc, pc.index, number_of_channels = 56){
    
    # Number of components
    number_of_components = length(pc.index)
    
    # Project the sweeps onto the requested principle components    
    data.project = pca.svd.project.sweeps(data = data, pc = pc, pc.index = pc.index, number_of_channels = number_of_channels)
    
    # Now make an ERP
    erp = make.erp.sweeps(sweeps = data.project, sweep.labels = sweep.labels, event.label = event.label)
    
    # Return ther ERP
    return(erp)
}
    
##########
"
DESCRIPTION:

    pca.svd.screen is used to create group level ERPs based on PCs. This hope here
    is to identify PCs that can discriminate between correct and incorrect feedback
    trials. 

INPUT:

    data:   this is the data.train/data.test lists (of lists) returned from 
            import_eeg.rdata. 

    pc:     this is the list returned from pca.svd.estimate

    number_of_channels: number of channels!
    
    plot.flat:  logical, if TRUE then plots are generated. If False then no plots
                generated (default = TRUE). If set to FALSE, then there's not much
                to be learned from this function, CWB doesn't think.

OUTPUT:

    Some plots ... not sure if it will return a variable yet.

Christopher W Bishop
1/15
"
pca.svd.screen <- function(data, pc, pc.index, number_of_channels = 56, plot.flag = TRUE){
    
    # Number of components
    #   This will be used below to reshape the data for plotting purposes
    number_of_components = length(pc.index)
    
    # Copy data
    data.project = data

    # Project each subject's data onto the specified PCs. Replace the
    for(i in 1:length(data.project)){        
        
        # Give user some feedback so we know where we are. 
        message(paste('Projecting subject', as.character(i), 'of', as.character(length(data.project))))
        data.project[[i]]$sweeps = pca.svd.project.sweeps(data = data.project[[i]]$sweeps, pc = pc, pc.index = pc.index, number_of_channels = number_of_channels)
         
        # Replace correct/incorrect/unknown ERPs
        #   We also need to reshape the resulting ERP such that we have a component x time point matrix
        data.project[i] = update.data.subject(data = data.project[i], number_of_channels = number_of_channels)
        #data.project[[i]]$erp_correct = make.erp.sweeps(sweeps = data.project[[i]]$sweeps, sweep.labels = data.project[[i]]$class_labels, event.label = 1)
        #data.project[[i]]$erp_incorrect = make.erp.sweeps(sweeps = data.project[[i]]$sweeps, sweep.labels = data.project[[i]]$class_labels, event.label = 2)                
        #data.project[[i]]$erp_unknown = make.erp.sweeps(sweeps = data.project[[i]]$sweeps, sweep.labels = data.project[[i]]$class_labels, event.label = 3)
        
    }
    
    # Now make the group ERP
    group.project = make.erp.group(data.project)
    
    # Reshape erp_correct, erp_incorrect, and erp_unknown into a component x time matrix
    #   Actually, we'll omit erp_unknown for now
    erp_correct = erpbyrow2chan(group.project$erp_correct, number_of_channels = number_of_components)
    erp_incorrect = erpbyrow2chan(group.project$erp_incorrect, number_of_channels = number_of_components)
    
    # Plot the proportion of variance explained. Helps determine which components 
    # should be kept. Typically use a 95% cut off criterion.
    print(qplot(x = 1:length(pc$d), y = cumsum(pc$d)/sum(pc$d) * 100))
    
    # Now, create a plot with the ERPs and difference waves for each component
    for(i in 1:nrow(erp_correct)){
        
        # Create the data frame with the data in it
        data2plot = data.frame("time_stamps" = data.project[[1]]$time_stamps, 
                               "Correct" = erp_correct[i,], 
                               "Incorrect" = erp_incorrect[i,], 
                               "Difference" = erp_incorrect[i,] - erp_correct[i,])
        
        # ERP plot
        gg <- ggplot(data2plot, aes(x = time_stamps)) +
            geom_line(aes(y = Correct, colour = "Black")) +
            geom_line(aes(y = Incorrect, colour = "Red")) + 
            geom_line(aes(y = Difference, size = 2), colour = 'Blue') + 
            ggtitle(paste0("PC", as.character(i))) +
            scale_x_continuous(breaks = seq(0, 1500, 100))
        
        # Make the plot
        print(gg)
    }
}

##########
"
DESCRIPTION:

    update.data.subject updates a data structure

"
update.data.subject <- function(data, number_of_channels = 56){
    
    # Recalculate the ERPs
    data = update.data.erp(data = data, number_of_channels = number_of_channels)
    
    # Match the time stamps the the SWEEPS matrix
    data = update.data.time_stamps(data = data, number_of_channels = number_of_channels)
    
}

##########
"
DESCRIPTION:

    updates the ERPs in a subject data list. This is often necessary if the contents
    of the 'sweeps' field is altered in some way (e.g., projected on to PCs or 
    similar)

INPUT:

    data:   subject data list returned from import_eeg.rdata. This can be data 
            for one or more subjects simultaneously. 

    numberof_channels:  number of channels (or components)

OUTPUT:

    data:   updated data list with recomputed ERPs. 

Christopher W Bishop
1/15
"
update.data.erp <- function(data, number_of_channels = 56){
        
    # 
    for(i in 1:length(data)){        
        
        # Correct ERP
        data[[i]]$erp_correct = make.erp.sweeps(sweeps = data[[i]]$sweeps, sweep.labels = data[[i]]$class_labels, event.label = 1)
        
        # Incorrect ERP
        data[[i]]$erp_incorrect = make.erp.sweeps(sweeps = data[[i]]$sweeps, sweep.labels = data[[i]]$class_labels, event.label = 2)
        
        # Unknown ERP (only relevant for test subjects)
        data[[i]]$erp_unknown = make.erp.sweeps(sweeps = data[[i]]$sweeps, sweep.labels = data[[i]]$class_labels, event.label = 3)
        
    }
    
    # Return the updated data structure
    return(data)
}

##########
"
DESCRIPTION:

    update.data.time_stamps updates the time stamps associated with a data structure

INPUT:

    data:   subject data list returned from import_eeg.rdata

    number_of_channels:     number of channels

OUTPUT:
    
    data:   data structure with updated time stamps

Christopher W Bishop
1/15
"
update.data.time_stamps <- function(data, number_of_channels = 56){
    
    for(i in 1:length(data)){
        
        data[[i]]$time_stamps = data[[i]]$time_stamps[1:ncol(data[[i]]$sweeps)]
        
    }
    
    # Return the updated data structure
    return(data)
}

##########
"
DESCRIPTION:

gfp.sweeps calculates the global field power (GFP). CWB wanted to look at GFP
since it may be a more robust and computationally efficient data reduction 
routine. I also think it should capture the discriminative information well. 

Let's give it a whirl!

INPUT:

    data:   sweeps matrix, N x (T*C) where N is the number of sweeps, T is the 
            number of time points per epoch (sweep), and C is the number of 
            channels.  

    number_of_channels: number of channels!

OUTPUT:

    gfp:    N x T matrix, where N is the number of sweeps and is the number of
            time points. Each value is the GFP (spatial standad deviation) at 
            the corresponding time point.

Christopher W Bishop
2/15
"

gfp.sweeps <- function(data, number_of_channels = 56){
    
    # Loop through all sweeps and calculate the GFP.
    for(i in 1:nrow(data)){
        
        # Convert sweep into a channel x time point matrix
        sweep = erpbyrow2chan(data = data[i,], number_of_channels = number_of_channels )
        
        # compute standard deviation for each time point
        tgfp = apply(sweep, 2, sd)
        
        # append sweep
        if(i == 1){
            gfp = tgfp
        }
        else{
            gfp = rbind(gfp, tgfp)
        }
        
    }
    
    # Return the GFP time series. 
    return(gfp)
}

##########
"
DESCRIPTION:

gfp.subjects converts time series in subject data structure to a time series of GFP
values.

INPUT:

    data:   subject data structure returned from import_eeg.rdata

    number_of_channels: You know by now

OUTPUT:

    data:   updated data with sweeps converted to GFP. Let's hope this is a good 
            data reduction step. 

Christopher W Bishop
2/15
"

gfp.subjects <- function(data, number_of_channels = 56){
    
    # Loop through each subject's data, replace sweeps with GFP
    for(i in 1:length(data)){
        
        # Update user so we know what's going on
        message(paste('Converting subject', as.character(i), 'of', as.character(length(data)), ' to GFP.'))
        
        # Convert sweeps to GFP
        data[[i]]$sweeps = gfp.sweeps(data = data[[i]]$sweeps, number_of_channels = number_of_channels)
        
        # Update subject structure
        data[i] = update.data.subject(data = data[i], number_of_channels = number_of_channels)
    }
    
    # Return the updated data structure
    return(data)
    
}