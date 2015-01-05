function import_subjects(subject_id, varargin)
%% DESCRIPTION:
%
%   This function imports data from CSV files provided by Kaggle into
%   EEGLAB compatible data structures. 
%
%   A data structure will be written for each training and test session
%   individually. Training/test sessions will also be merged to form larger
%   data sets for ERP and other analyses.
%
% INPUT:
%
%   subject_id: cell array of subject IDs
%
% Parameters:
%
%   XXX
%
% OUTPUT:
%
%   Lots of .set files
%
% Christopher W Bishop
%   University of Washington
%   12/14

% Get user parameters
opts = varargin2struct(varargin{:}); 

training_subjects = {'S02' 'S06' 'S07' 'S11' 'S12' 'S13' 'S14' 'S16' 'S17' 'S18' 'S20' 'S21' 'S22' 'S23' 'S24' 'S26'};
test_subjects = {'S01' 'S03' 'S04' 'S05' 'S08' 'S09' 'S10' 'S15' 'S19' 'S25'};

% Study directory
study_directory = 'D:\GitHub\Kaggle\P300_Speller'; 
channel_locations = fullfile(study_directory, 'ChannelsLocation.loc');

% Session numbers
%   There are 5 sessions in this paradigm 
session_labels = {'Sess01', 'Sess02', 'Sess03', 'Sess04', 'Sess05'}; 

%% MAKE SUBJECT DIRECTORIES
%   We'll mimic CWB's typical project data structure to facilitate data
%   processing and code development.
for s=1:numel(subject_id)
    
    % We have to pass an empty event_table for test set subjects since we
    % don't have that information (it's a test, remember?) 
    if ismember(subject_id{s}, training_subjects)
        
        % Event table will be empty for test subjects
        event_table = fullfile(study_directory, 'TrainLabels.csv');
        
    elseif ismember(subject_id{s}, test_subjects)
        event_table = '';
    else
        error([subject_id{s} ' did not match the training or test subjects listed here. Something weird happening'])        
    end % if ismember
    
    % Make subject directory
    subject_directory = fullfile(study_directory, subject_id{s}); 
    mkdir(subject_directory); 
    
    % Make subdirectories
    %   This will help me keep the data organized and in clearly
    %   identifiable locations. Also will make processing easier later. 
    mkdir(fullfile(subject_directory, 'eeg')); 
    mkdir(fullfile(subject_directory, 'analysis')); 
    mkdir(fullfile(subject_directory, 'jobs')); 
    
    % Import training data
    for i=1:numel(session_labels)

        % File name to process next
        filename = fullfile(study_directory, 'data', ['Data_' subject_id{s} '_' session_labels{i} '.csv']);

        % Import the file
        EEG(i) = import_dataset('data_name', filename, 'channel_locations', channel_locations, 'event_table', event_table); 

        % Create output file name
        output_file = fullfile(subject_directory, 'eeg', [EEG(i).setname '.set']); 

        % Break into file parts
        [pathstr, name, ext] = fileparts(output_file); 

        % Now save as a .set file
        pop_saveset(EEG(i), 'filepath', pathstr, 'filename', [name ext], 'check', 'off', 'savemode', 'onefile');

    end % for i=1:numel(session_labels)

    % Now merge all training sets into a single set
    EEG = pop_mergeset(EEG, 1:length(EEG));

    % Change set name
    EEG.setname = fullfile(pathstr, [subject_id{s} ext]);

    % Save merged data set
    [pathstr, name, ext] = fileparts(EEG.setname);
    pop_saveset(EEG, 'filepath', pathstr, 'filename', [name ext], 'check', 'off', 'savemode', 'onefile'); 

    % Clear variables
    clear EEG; 
    
end % for 