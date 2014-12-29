function results = gab_task_kaggle_p300_speller_import_subjects(args)
%% DESCRIPTION:
%
%   GAB wrapper for import_subjects function
%
% INPUT:
%
%   args:
%
%   'subject_id':   cell array of subject IDs
%
% OUTPUT:
%
%   results:
%
% Christopher W Bishop
%   University of Washington 
%   12/14

import_subjects(args.subject_id);

% Useless return
results = 'done';