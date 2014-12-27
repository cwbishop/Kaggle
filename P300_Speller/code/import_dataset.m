function EEG = import_dataset(varargin)
%% DESCRIPTION:
%
%   This function reads in an excel file and converts it to an EEGLAB data
%   set. CWB decided matlab + eeglab were the best choice since EEGlab (and
%   ERPlab) have a lot of visualization functions that will make his life
%   much easier and make the data more digestible. 
%
%   Also, data cleaning algorithms like ICA and the like can be used
%   seamlessly. 
%
% INPUT:
%
% Key/Value Pairs:
%
%   'data_name':    name of file containing data to import. 
%
%   'channel_locations':   name of .locfile containing channel locations
%                           Note: this is generated by import_chanlocs.m
%
% OUTPUT:
%
%   EEG:    an EEG data structure.
%
%   

% Get key/value pairs, place in structure
opts = varargin2struct(varargin{:});

% Get data file information
[pathstr, name, ext] = fileparts(opts.data_name); 

% Get subject ID and session ID
%   These will be used below to generate event data. 
file_specs = strsplit(name, '_'); 
subject_id = file_specs{2}; 
session_id = file_specs{3}; 

% Read in the data
data_table = readtable(opts.data_name); 

% Here we remove channels we do not have location information for.
%   CWB looked at this briefly, and there appear to be two channels we
%   don't have information for
% Message data table into a data matrix. 
%   Rows are channels, columns are data points
data = table2array(data_table)';

% Get time stamps
%   Use this to determine sampling rate
time_stamps = data(1,:); 

% These are the feedback events
feedback_events = data(end,:); 

% Note that this is 200 Hz for s02, but Kaggle lists at 600 Hz. Odd?
FS = 1/time_stamps(2); % sampling rate in Hz. 

% Remove time stamps, Feedback events, and the EOG. Recall that we do NOT
% have location information for EOG data. I don't think this will be a
% useful channel anyway considering where the p300 loads ... famous last
% words, right? 
data = data(2:end-2,:);

% Save data to a mat file for use with pop_importdata
ofile = fullfile(pathstr, [name '.mat']); 
save(ofile, 'data');

% Import data trace to EEG structure
EEG = pop_importdata('dataformat','matlab','nbchan',0,'data',ofile,'srate',FS,'subject',subject_id,'pnts',0,'xmin',0,'chanlocs','C:\\Users\\cwbishop\\Documents\\GitHub\\Kaggle\\P300 Speller\\ChannelsLocation.loc');
EEG.setname = [subject_id '_' session_id];

%% ADD EVENTS
%
%   Here we define events as either feedback on correct trials (1) or a
%   incorrect feedback (2). Recall that (2) vs. (1) should elicit the P300
%   as described by 
%       Perrin, M., Maby, E., Daligault, S., Bertrand, O., & Mattout, J. Objective and subjective evaluation of online error correction during P300-based spelling. Advances in Human-Computer Interaction, 2012, 4.

% Lookup events from labeled event table
event_table = readtable('C:\Users\cwbishop\Documents\GitHub\Kaggle\P300 Speller\TrainLabels.csv');

% Find all subject and session specific events.
feedback_id = event_table.IdFeedBack;

prediction = []; 
feedback_number = [];
for i=1:numel(feedback_id)
    
    % If it's this subject and this session, include it
    file_specs = strsplit(feedback_id{i}, '_');
    
    if isequal(file_specs{1}, subject_id) && isequal(file_specs{2}, session_id)
        
        % Get the prediction and the feedback_number
        prediction(end+1,1) = event_table.Prediction(i);
        feedback_number(end+1,1) = str2double(file_specs{3}(3:end));
    end % 
    
end % for i=1:numel(feedback_id)

% Sanity checks to make sure the event information is sensible. 
%   Do we have the correct number of events?
if numel(find(feedback_events)) ~= numel(prediction)
    error('Mismatch between number of feedback events and predictions');
end % numel(find(feedback ...

% With sanity checks complete, create the event data channel 
%   This will essentially be an additional channel that will be written to
%   file and imported as event information.
feedback_mask = find(feedback_events); 
for i=1:numel(feedback_mask)
    
    % If the prediction is correct (i.e., no P300), label it as 1
    if prediction(i) == 1
        feedback_events(feedback_mask(i)) = 1;
    elseif prediction(i) == 0
        feedback_events(feedback_mask(i)) = 2;
    end % if prediction ...
    
end % for i=1:numel(feedback_mask)

% Write feedback_events to a text file 
ofile = fullfile(pathstr, [name '_events.txt']);
dlmwrite(ofile, [feedback_mask' feedback_events(feedback_mask)'], 'delimiter', '\t'); 

% Insert events into EEG structure
EEG = pop_importevent( EEG, 'event', ofile, 'fields', {'latency' 'type'}, 'timeunit', NaN, 'optimalign', 'off');