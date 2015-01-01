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
            'formulas', fullfile(studyDir, '..', 'code', 'P300_speller_binops.txt')));
        
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
    
    %% JOBS
    jobs{end+1} = setup; 
    jobs{end+1} = erp_0to20_extended;
    jobs{end+1}=erp_unfiltered;  
    jobs{end+1} = erp_1to20; 
    jobs{end+1} = erp_0p05to20;
    jobs{end+1} = erp_0p05to20_extended;
    jobs{end+1} = erp_0p5to20; 
    
end % s

%% GROUP COMPARISONS
jobs{end+1} = make_grand_average('erp_unfiltered'); 
jobs{end+1} = make_grand_average('erp_filtered_0.05to20_epoched_-50to1800');
jobs{end+1} = make_grand_average('erp_filtered_1to20_epoched_-50to1000'); 
jobs{end+1} = make_grand_average('erp_filtered_0.5to20_epoched_-50to1000'); 
jobs{end+1} = make_grand_average('erp_filtered_0.05to20_epoched_-50to1000');
jobs{end+1} = make_grand_average('erp_filtered_0to20_epoched_-50to1800');

% Add to jobs structure
% jobs{end+1} = group_erp_unfiltered; 

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

function job = modify_erp_job(job, corner_frequencies, epoch_window)
%% DESCRIPTION:
%
%   Reworks the original ERP job to use a different bandpass filter.
%   Changes all necessary fields (ERP names, job names, etc.)

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
            'Order', 4, ... % use 4th order filter 
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