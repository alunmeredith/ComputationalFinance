%% Import data from text file.
% Script for importing data from the following text file:
%
%    C:\Users\inbre\Documents\Southampton\Term2\ComputationalFinance\CW1\filenames.csv
%
% To extend the code to different selected data or a different text file,
% generate a function instead of a script.

% Auto-generated by MATLAB on 2016/02/22 17:26:46

%% Initialize variables.
filename = 'C:\Users\inbre\Documents\Southampton\Term2\ComputationalFinance\CW1\filenames.csv';
delimiter = ',';
startRow = 2;

%% Format string for each line of text:
%   column2: text (%q)
% For more information, see the TEXTSCAN documentation.
formatSpec = '%*q%q%[^\n\r]';

%% Open the text file.
fileID = fopen(filename,'r');

%% Read columns of data according to format string.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);

%% Close the text file.
fclose(fileID);

%% Post processing for unimportable data.
% No unimportable data rules were applied during the import, so no post
% processing code is included. To generate code which works for
% unimportable data, select unimportable cells in a file and regenerate the
% script.

%% Allocate imported array to column variable names
x = dataArray{:, 1};


%% Clear temporary variables
clearvars filename delimiter startRow formatSpec fileID dataArray ans;