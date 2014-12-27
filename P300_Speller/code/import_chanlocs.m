function import_chanlocs(varargin)
%% DESCRIPTION:
%
%   Converts channel locations in the CSV file provided by Kaggle to a .loc
%   file supported by readlocs.m.
%
% INPUT:
%
% Key/value
%
%   'kaggle_file':  path to kaggle file containing channel locations
%
% OUTPUT:
%
%   A .loc file written to the same directory as the Kaggle file.
%
% Christopher W Bishop
%   University of Washington
%   12/14

% get input parameters
opts = varargin2struct(varargin{:}); 

klocs = readtable(opts.kaggle_file); 
 
% Get ID field
ID = klocs.Id;

% Get angle field
ang = klocs.Phi; 

% Radius
r = klocs.Radius;

% Labels
labels = klocs.Labels; 

% Make a table 
t = table(ID, ang, r, labels); 

% Get file name information
[pathstr, name, ext] = fileparts(opts.kaggle_file); 

% Get output file
ofile = fullfile(pathstr, [name '.txt']);
oofile = fullfile(pathstr, [name '.loc']); 

% Write table
writetable(t, ofile, 'Delimiter', ' ', 'WriteVariableNames', false); 

% rename the txt file to .loc
system(['move "' ofile '" "' oofile '"'], '-echo');