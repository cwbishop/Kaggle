function export_eeg_sweeps(EEG, varargin)
%% DESCRIPTION:
%
%   This function exports EEG epochs in a text file format. Each line in
%   the text file corresponds to a single sweep/epoch/trial (whatever term
%   you prefer to use). 
%
%   CWB found it necessary to export these data for further analysis in R.
%   R has more powerful (and maintained) classification packages that are
%   *free*. MATLAB, however, has *free* packages for EEG analysis. So, we
%   have to combine the best aspects of both. 
%
% INPUT:
%
%   EEG:    EEG data set
%
% Parameters:
%
%   'output_file':  path to output file. Can be relative or absolute
%                   (choose your poison).
%
% OUTPUT:
%
%   A text file with single channel sweeps
%
% Development:
%
%   None (yet)
%
% Christopher W Bishop
%   University of Washington 
%   12/14

% Gather input arguments
opts = varargin2struct(varargin{:}); 

% Time_stamps
time_stamps = repmat(EEG.times, 1, size(EEG.data,1)); 

% First entry will be time_stamps
trial_sweeps = []; 

% Include a NaN for the first row. First row will be the time stamps for
% the data. Not necessary for training (usually), but helpful for on the
% fly visualization. 
trial_labels = []; 

for i=1:size(EEG.data,3)
    trial_sweeps(i,:) = reshape(EEG.data(:,:,i)', 1, size(EEG.data,1)*size(EEG.data,2));
    trial_labels(i,1) = EEG.epoch(i).eventbini; 
end % 

% Write data to csv
dlmwrite(opts.output_file, [[NaN; trial_labels] [time_stamps; trial_sweeps] ], 'precision', '%f'); 
% csvwrite(opts.output_file, [trial_labels trial_sweeps]); 
