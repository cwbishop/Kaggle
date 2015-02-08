function jobs=Kaggle_P300_speller_gab_setup(SID)
%% DESCRIPTION:
%
%   We absolutely needed a branch point for Exp02C.  It was getting
%   too complicated to try and work Exp02C to play nice with the setup files
%   for all previous experiments.
%
% INPUT
%
% OUTPUT

% These are hard-coded based on competition randomization. CWB needed to
% organize data differently for ease of processing. 
training_subjects = {'S02' 'S06' 'S07' 'S11' 'S12' 'S13' 'S14' 'S16' 'S17' 'S18' 'S20' 'S21' 'S22' 'S23' 'S24' 'S26'};
test_subjects = {'S01' 'S03' 'S04' 'S05' 'S08' 'S09' 'S10' 'S15' 'S19' 'S25'};

EXPID = 'P300_Speller';

studyDir=['D:\GitHub\Kaggle\' EXPID filesep];

jobs={};

dataset_path = {};
dataset_filename = {};

%% CREATE JOBS FOR EACH SUBJECT
for s=1:numel(SID)
    
    % Get subject ID and subject directory     
    sid=SID{s};
    subDir=fullfile(studyDir,sid);  
    
    %% INITIAL SETUP JOB
    %   Here we'll convert the data into a usable format for further
    %   analysis/exploration.
    setup = gab_emptyjob; 
    setup.jobName = 'setup'; 
    setup.jobDir = fullfile(subDir, 'jobs'); 
    setup.parent = '';
    
    % Import subject dataset(s)
    setup.task{end+1} = struct(...
        'func', @gab_task_kaggle_p300_speller_import_subjects, ...
        'args', struct( ...
            'subject_id', {{sid}})); 
        
    %% ERP JOB
    % Create the job
    erp_unfiltered=gab_emptyjob;
    erp_unfiltered.jobName='erp_unfiltered';
    erp_unfiltered.jobDir=fullfile(subDir, 'jobs');
    erp_unfiltered.parent='';
    
    % Load environmental variables
    erp_unfiltered.task{end+1}=struct(...
        'func',@gab_task_envvars,...
        'args','');
    
    % Load dataset
    erp_unfiltered.task{end+1}=struct(...
        'func',@gab_task_eeglab_loadset,...
        'args',struct(...
            'filepath', fullfile(studyDir, sid, 'eeg' ), ...
            'filename', [sid '.set']));
       
%     % Resample to 200 Hz
%     ERP.task{end+1}=struct(...
%         'func', @gab_task_eeg_resample, ...
%         'args', struct(...
%             'freq', 200)); 
        
%     % Bandpass filter the data (1 - 9 Hz)
%     ERP.task{end+1}=struct(...
%         'func', @gab_task_erplab_pop_basicfilter, ...
%         'args', struct( ...
%             'chanArray', 1:20, ... % channel 21 is the status channel 
%             'params', ...
%                 {{'Filter', 'lowpass', ...
%                 'Design', 'butter', ...
%                 'Cutoff', [0], ...
%                 'Order', 6, ...
%                 'RemoveDC', 'on', ...
%                 'Boundary', 'boundary'}}));
            
    % Create Event List
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_creabasiceventlist, ...
        'args', struct( ...
            'params', ...
               {{'Eventlist', '', ...
               'BoundaryString', {'boundary'}, ...
               'BoundaryNumeric', {-99}, ...
               'Warning', 'off', ...
               'AlphanumericCleaning', 'on'}}));  

           
    % Binlister
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_binlister, ...
        'args', struct( ...
            'params', {{'BDF', fullfile(studyDir, '..', 'code', ['BINS_' EXPID '.txt']), ...
               'Resetflag', 'off', ... % don't reset artifact rejection flags
               'Forbidden', [], ... % might need to add in a [6] since there's a random even at the beginning of all files
               'Ignore', [], ... % actually, this might be where the 6 should go
               'Warning', 'off', ...
               'SendEL2', 'EEG', ... 
               'Report', 'on', ...
               'Saveas', 'off'}}));
           
    % Overwrite Event Type in EEG Structure
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_overwritevent, ...
        'args', struct(...
            'mainfield', 'binlabel')); % label 'type' with human readable BIN information
        
    % Epoch    
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_epochbin,...
        'args', struct(...
            'trange', [-50 1000], ... % look at +1 sec after feedback event. Will likely need to narrow this later. 
            'blc', 'pre')); % baseline based on pre-stimulus onset.   

    % Run ICA
    erp_unfiltered.task{end+1} = struct(...
        'func', @gab_task_eeglab_runica, ...
        'args', struct()); 
    
    % Save set    
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_eeglab_saveset, ...
        'args', struct(...
            'params', {{'filename', [sid '-erp_unfiltered.set'], ...
               'filepath', fullfile(subDir, 'analysis'), ... 
               'check', 'off', ... 
               'savemode', 'onefile'}}));
    
    % Create ERPs
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_averager, ...
        'args', struct(...
            'params', {{'DSindex', 1, ...
                'Criterion', 'good', ...
                'SEM', 'on', ...
                'ExcludeBoundary', 'on', ...
                'Warning', 'off'}}));
            
    % Apply bin operator
    erp_unfiltered.task{end+1} = struct(...
        'func', @gab_task_erplab_pop_binoperator, ...
        'args', struct(...
            'formulas', fullfile(studyDir, '..', 'code', 'p300_speller_binops.txt')));
        
    % Save ERP        
    erp_unfiltered.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_savemyerp, ...
        'args', struct(...
            'params', {{'erpname', [sid '-erp_unfiltered'], ...
                'filename', [sid '-erp_unfiltered.erp'], ...
                'filepath', fullfile(subDir, 'analysis'), ...
                'gui', 'none', ...
                'Warning', 'off'}}));
    
    % clear options
    clear opts; 
    
    % ERP [1 - 20 Hz] bandpass. 
    %   CWB chose these parameters to better match the filtering parameters
    %   used in the paper from which these data were gathered.
    erp_1to20 = modify_erp_job(erp_unfiltered, [1 20]); 
    
    % ERP [0.05 - 20 Hz] bandpass
    %   CWB was running this job overnight so wanted to have an additional
    %   highpass cutoff in case the 1 Hz removed the effects.
    erp_0p05to20 = modify_erp_job(erp_unfiltered, [0.05, 20]); 
    
    % ERP [0.5 - 20 Hz] bandpass
    %   Want to see if this wipes out the massive effect as well.
    erp_0p5to20 = modify_erp_job(erp_unfiltered, [0.5 20]);     
    
    erp_0p05to20_extended = modify_erp_job(erp_unfiltered, [0.05 20], [-50 1800]);
            
    % Also try an extended epoch duration, but with only a low pass filter.
    % CWB's notes suggest that high pass filtering might not buy us much
    % ... and we shouldn't filter unless we have to.
    erp_0to20_extended = modify_erp_job(erp_unfiltered, [0 20], [-50 1800]); 
    
    % Also try a 1 - 20 Hz filter to remove the low-frequency information.
    % CWB discovered that the low-frequency information is not *consistent*
    % across individuals. It's absolutely informative for many if not most
    % subjects, but the sign of the difference wave changes.
    erp_1to20_extended = modify_erp_job(erp_unfiltered, [1 20], [-50 1800]); 
    
    % Also try a 1 - 8 Hz bandpass filter. This removes some of the high
    % frequency noise while (I think) maintaining much of the information
    % in the difference wave.
    erp_1to8_extended = modify_erp_job(erp_unfiltered, [1 8], [-50 1800]); 
    
    % Create the job
    erp_1to20_extended_neb=gab_emptyjob;
    erp_1to20_extended_neb.jobName=[erp_1to20_extended.jobName '_NEB'];
    erp_1to20_extended_neb.jobDir=fullfile(subDir, 'jobs');
    erp_1to20_extended_neb.parent='';
    
    % Load environmental variables
    erp_1to20_extended_neb.task{end+1}=struct(...
        'func',@gab_task_envvars,...
        'args','');
    
    % Load dataset
    erp_1to20_extended_neb.task{end+1}=struct(...
        'func',@gab_task_eeglab_loadset,...
        'args',struct(...
            'filepath', fullfile(studyDir, sid, 'analysis' ), ...
            'filename', [sid '-erp_filtered_1to20_epoched_-50to1800_NEB.set']));
    
    % Create ERPs
    erp_1to20_extended_neb.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_averager, ...
        'args', struct(...
            'params', {{'DSindex', 1, ...
                'Criterion', 'good', ...
                'SEM', 'on', ...
                'ExcludeBoundary', 'on', ...
                'Warning', 'off'}}));
            
    % Apply bin operator
    erp_1to20_extended_neb.task{end+1} = struct(...
        'func', @gab_task_erplab_pop_binoperator, ...
        'args', struct(...
            'formulas', fullfile(studyDir, '..', 'code', 'p300_speller_binops.txt')));
        
    % Save ERP        
    erp_1to20_extended_neb.task{end+1}=struct(...
        'func', @gab_task_erplab_pop_savemyerp, ...
        'args', struct(...
            'params', {{'erpname', [sid '-erp_filtered_1to20_epoched_-50to1800_NEB'], ...
                'filename', [sid '-erp_filtered_1to20_epoched_-50to1800_NEB.erp'], ...
                'filepath', fullfile(subDir, 'analysis'), ...
                'gui', 'none', ...
                'Warning', 'off'}}));
            
    % CWB wants to do some data cleaning to estimate the time course of eye
    % blinks and other noise sources based on a group average ICA
    % decomposition. To do this, let's concatenate all the training subject
    % data sets into one big data set, then do ICA. Let's see if we can pick
    % out "typical" eye blink and other noise components that we can remove
    % from the data.
    %
    % For now, just append the raw data sets, refilter it, then run ICA. If we
    % use the saved data sets from ERPLAB, merging fails because urevents has
    % been removed. SO annoying!
    %
    % Let's look at the 1 - 20 Hz filtered data since we know we need to remove
    % the line noise and low-frequency drift (for now). 
    if ismember(sid, training_subjects)
        
        dataset_path{end+1} = fullfile(subDir, 'eeg');
        dataset_filename{end+1} = fullfile([sid '.set']); 
        
    end % 
    
    %% JOBS
%     jobs{end+1} = setup; 
%     jobs{end+1} = erp_0to20_extended;
%     jobs{end+1}=erp_unfiltered;  
%     jobs{end+1} = erp_1to20; 
%     jobs{end+1} = erp_0p05to20;
%     jobs{end+1} = erp_0p05to20_extended;
%     jobs{end+1} = erp_1to20_extended;
    jobs{end+1} = erp_1to20_extended_neb;
%     jobs{end+1} = erp_1to8_extended;
%     jobs{end+1} = erp_0p5to20; 
    
end % s

%% GROUP COMPARISONS
% jobs{end+1} = make_grand_average('erp_unfiltered'); 
% jobs{end+1} = make_grand_average('erp_filtered_0.05to20_epoched_-50to1800');
% jobs{end+1} = make_grand_average('erp_filtered_1to20_epoched_-50to1800');
jobs{end+1} = make_grand_average('erp_filtered_1to20_epoched_-50to1800_NEB');
% jobs{end+1} = make_grand_average('erp_filtered_1to8_epoched_-50to1800');
% jobs{end+1} = make_grand_average('erp_filtered_1to20_epoched_-50to1000'); 
% jobs{end+1} = make_grand_average('erp_filtered_0.5to20_epoched_-50to1000'); 
% jobs{end+1} = make_grand_average('erp_filtered_0.05to20_epoched_-50to1000');
% jobs{end+1} = make_grand_average('erp_filtered_0to20_epoched_-50to1800');

% Add to jobs structure
% jobs{end+1} = group_erp_unfiltered; 


% erp_label = 'erp_filtered_1to20_epoched_-50to1800';
% dataset_path = {};
% dataset_filename = {};
% for j = 1:numel(jobs)
% 
%     % We only want to include the training subjects in this concatenated
%     % set.
%        
%     % Get the subject ID from the job directory
%     sid_by_jobDir = jobs{j}.jobDir;
%     sid_by_jobDir = strsplit(strrep(sid_by_jobDir, studyDir, ''), filesep);
%     sid_by_jobDir = sid_by_jobDir{1};
%     % Remove study directory
%     
%     if isequal(jobs{j}.jobName, erp_label) && ismember(sid_by_jobDir, training_subjects)
%         
%         % Find the dataset name
%         saveset_index = gab_find_task(jobs{j}, 'gab_task_eeglab_saveset', 1);
%         opts = varargin2struct(jobs{j}.task{saveset_index}.args.params{:}); 
%         dataset_path{end+1} = opts.filepath;
%         dataset_filename{end+1} = opts.filename; 
% %         dataset_files{end+1} = fullfile(opts.filepath, opts.filename); 
%     elseif ismember(sid_by_jobDir, test_subjects)
%         error('Should not be including test subjects'); 
%         
%     end %
% 
% end % for i=1:numel(jobs)
   
% Setup a group job to load all data sets, merge them, write the data set.

% Then write a job to load that data set and run ICA on it to estimate the
% demixing (mixing?) matrix.
sid = 'group';
subDir = fullfile(studyDir, sid);
group_concat_1to20_extended = gab_emptyjob; 
group_concat_1to20_extended.jobName = ['group_concat_1to20_extended']; 
group_concat_1to20_extended.jobDir = fullfile(subDir, 'jobs'); 
group_concat_1to20_extended.parent = '';

% Start EEGLAB
group_concat_1to20_extended.task{end+1}=struct(...
    'func',@gab_task_envvars,...
    'args','');

% Load datasets
group_concat_1to20_extended.task{end+1}=struct(...
    'func',@gab_task_eeglab_loadset,...
    'args',struct(...
        'filepath', {dataset_path}, ...
        'filename', {dataset_filename}));
        
% Merge data sets into one (very long) data set.
group_concat_1to20_extended.task{end+1}=struct(...
    'func',@gab_task_eeg_mergeset,...
    'args',struct());

% Filter data 1-20 Hz.
group_concat_1to20_extended.task{end+1} = struct(...
    'func', @gab_task_erplab_pop_basicfilter, ...
    'args', struct( ...
        'chanArray', 1:56, ... % 56 channel array
        'params', ...
            {{'Filter', 'bandpass', ...
            'Design', 'butter', ...
            'Cutoff', [1 20], ...
            'Order', 4, ... % use 4th order filter 
            'RemoveDC', 'on', ...
            'Boundary', 'boundary'}}));

% Run ICA
group_concat_1to20_extended.task{end+1}=struct(...
    'func',@gab_task_eeglab_runica,...
    'args',struct( ...
        'dataset',  1, ...
        'chanind',  1:56, ...
        'concatenate',  'on', ...
        'icatype',  'runica'));
    
% Save new data set
group_concat_1to20_extended.task{end+1}=struct(...
    'func', @gab_task_eeglab_saveset, ...
    'args', struct(...
        'params', {{'filename', [sid '_concat_1to20_extended.set'], ...
           'filepath', fullfile(subDir, 'analysis'), ... 
           'check', 'off', ... 
           'savemode', 'onefile'}}));
       
% Add to job struct
% jobs{end+1} = group_concat_1to20_extended; 

    function job = make_grand_average(erp_label)
    %% DESCRIPTION:
    %
    %   Function to make Group ERPs from an erp_label

    %% GROUP ANALYSIS
    %   Create group average ERPs
    sid = 'group';
    subDir = fullfile(studyDir, sid);
    job = gab_emptyjob; 
    job.jobName = ['group_' erp_label]; 
    job.jobDir = fullfile(subDir, 'jobs'); 
    job.parent = '';

    % Get ERP file names from ERP job
    ERPF = {}; 
    for i = 1:numel(jobs)

        if isequal(jobs{i}.jobName, erp_label)
            opts = varargin2struct(jobs{i}.task{end}.args.params{:}); 
            ERPF{end+1} = fullfile(opts.filepath, opts.filename); 
        end %

    end % for i=1:numel(jobs)

    % Error check to make sure we have the correct number of files.
    if numel(ERPF) ~= numel(SID)
        error('Incorrect number of ERP files found');
    end % if numel(ERPF ...

    % Start EEGLAB
    % Load environmental variables
    job.task{end+1}=struct(...
        'func',@gab_task_envvars,...
        'args','');

    % LOAD ALL FILES
    job.task{end+1} = struct(...
        'func', @gab_task_erplab_pop_loaderp, ...
            'args', struct(...
                'ERPF', {ERPF})); 

    % Compute grand average
    job.task{end+1} = struct(...
        'func', @gab_task_erplab_pop_gaverager, ...
         'args', struct(...
            'params', {{
                'Erpsets', 1:numel(ERPF), ...
                'Weighted', 'off', ...
                'ExcludeNullBin', 'on', ...
                'SEM',  'on', ...
                'Criterion', 1}})); 

    % Save the ERP
    job.task{end+1}=struct(...
            'func', @gab_task_erplab_pop_savemyerp, ...
            'args', struct(...
                'params', {{'erpname', [sid '-' erp_label '(N=' num2str(numel(SID)) ')'], ...
                    'filename', [sid '-' erp_label '(N=' num2str(numel(SID)) ').erp'], ...
                    'filepath', fullfile(subDir, 'analysis'), ...
                    'gui', 'none', ...
                    'Warning', 'off'}}));
    end % make_grand_average
end % 

function job = modify_erp_job(job, corner_frequencies, epoch_window, filter_order)
%% DESCRIPTION:
%
%   Reworks the original ERP job to use a different bandpass filter.
%   Changes all necessary fields (ERP names, job names, etc.)

if ~exist('filter_order', 'var') || isempty(filter_order), filter_order = 4; end

% Use the current epoch_window by default
if ~exist('epoch_window', 'var')
    task_index = gab_find_task(job, 'gab_task_erplab_pop_epochbin', 1);
    epoch_window = job.task{task_index}.args.trange;
end % if exist('epoch_window', 'var'); 
    
% ERP Label    
erp_label = ['filtered_' num2str(corner_frequencies(1)) 'to' num2str(corner_frequencies(2)) '_epoched_' num2str(epoch_window(1)) 'to' num2str(epoch_window(2))];
    
% Change job name
job.jobName = strrep(job.jobName, 'unfiltered', erp_label);

% Add in filtering ste
filter_task =struct(...
    'func', @gab_task_erplab_pop_basicfilter, ...
    'args', struct( ...
        'chanArray', 1:56, ... % 56 channel array
        'params', ...
            {{'Filter', 'bandpass', ...
            'Design', 'butter', ...
            'Cutoff', corner_frequencies, ...
            'Order', filter_order, ... % use 4th order filter 
            'RemoveDC', 'on', ...
            'Boundary', 'boundary'}}));

% Change epoch settings 
if exist('epoch_window', 'var')
    task_index = gab_find_task(job, 'gab_task_erplab_pop_epochbin', 1);
    job.task{task_index}.args.trange = epoch_window; 
end % if exist(...
    
% Insert filter task
job = gab_insert_task(job, filter_task, 3); 

% Rename the data set written to file
task_index = gab_find_task(job, 'gab_task_eeglab_saveset', 1); 

% Get parameters from that job
opts = varargin2struct(job.task{task_index}.args.params{:});

% Change filename
opts.filename = strrep(opts.filename, 'unfiltered', erp_label);

% Convert struct to key/value list, reassign to job structure
job.task{task_index}.args.params = struct2keyval(opts); 

% Rename the ERP 
task_index = gab_find_task(job, 'gab_task_erplab_pop_savemyerp', 1); 
opts = varargin2struct(job.task{task_index}.args.params{:});
opts.erpname = strrep(opts.erpname, 'unfiltered', erp_label);
opts.filename = strrep(opts.filename, 'unfiltered', erp_label); 
job.task{task_index}.args.params = struct2keyval(opts);
end % function