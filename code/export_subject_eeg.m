function export_subject_eeg(subjects, varargin)
%% DESCRIPTION:
%
%   Function to export EEG sweeps to a CSV file that can be imported into
%   R.
%
% INPUT:
%
%   subjects:   cell array, subject IDs
%
% Parameters:
%
%   erp_label:  string, erp_label for loading and file writing purposes.
%
% OUTPUT:
%
%   csv file containing EEG sweep data.
%
% Christopher W Bishop
%   12/14
%   University of Washington

% Hard-coded constants. 
study_directory = 'D:\GitHub\Kaggle\P300_Speller';

% Get key/value pairs
opts = varargin2struct(varargin{:});

for i=1:numel(subjects)
    
    % So I know where I am in the process
    display(subjects{i}); 
    
    % Subject analysis directory
    subject_directory = fullfile(study_directory, subjects{i}, 'analysis');
    
    % EEG data set file name
    eeg_setname = fullfile(subject_directory, [subjects{i} '-' opts.erp_label '.set']);     
    [pathstr, name, ext] = fileparts(eeg_setname); 
    
    % Load EEG data
    EEG = pop_loadset('filename', [name ext], 'filepath', pathstr, 'loadmode', 'all');
    
    % Output file name 
    output_file = fullfile(pathstr, [name '.csv']); 
    
    % Write EEG data to file
    export_eeg_sweeps(EEG, 'output_file', output_file); 
    
end % for i=1:numel(subjects