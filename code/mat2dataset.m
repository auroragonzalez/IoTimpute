function d = mat2dataset(x,varargin)
%MAT2DATASET Convert matrix to dataset array.
%   D = MAT2DATASET(X) converts the M-by-N matrix X to an M-by-N dataset
%   array D.  Each column of X becomes a variable in D.
%
%   D = MAT2DATASET(X, 'PARAM1', VAL1, 'PARAM2', VAL2, ...) specifies optional
%   parameter name/value pairs that determine how the data in X are converted.
%
%      'VarNames'  A cell array of strings containing variable names for D.
%                  The names must be valid MATLAB identifiers, and must be
%                  unique.
%      'ObsNames'  A cell array of strings containing observation names for D.
%                  The names need not be valid MATLAB identifiers, but must be
%                  unique.
%      'NumCols'   A vector of non-negative integers that determines the
%                  number of columns for each variable in D, by combining
%                  multiple columns in X into a single variable in D.
%                  'NumCols' must sum to SIZE(X,2).
%
%   See also CELL2DATASET, STRUCT2DATASET, DATASET.

%   Copyright 2012 The MathWorks, Inc.


if ~ismatrix(x)
    error(message('stats:mat2dataset:NDArray'));
end
[nrows,ncols] = size(x);

pnames = {'VarNames' 'ObsNames' 'NumCols'};
dflts =  {       []         []        [] };
[varnames,obsnames,numCols,supplied] ...
    = parseArgs(pnames, dflts, varargin{:});

if supplied.NumCols
    if isnumeric(numCols) && isvector(numCols) && ...
            all(round(numCols)==numCols) && all(numCols>=0)
        if sum(numCols)~=ncols
            error(message('stats:mat2dataset:NumColsWrongSum'));
        end
    else
        error(message('stats:mat2dataset:InvalidNumCols'));
    end
else
    % Each column of X becomes a variable in D
    numCols = ones(1,ncols);
end

args = cell(1,0);
if supplied.VarNames
    args(end+1:end+2) = {'VarNames',varnames};
else
    baseName = inputname(1);
    nvars = length(numCols);
    if ~isempty(baseName) && (nvars > 0)
        varnames = strcat(baseName,cellstr(num2str((1:nvars)','%-d'))');
        args(end+1:end+2) = {'VarNames',varnames};
    end
end
if supplied.ObsNames
    args(end+1:end+2) = {'ObsNames',obsnames};
end

vars = mat2cell(x,nrows,numCols);
if isempty(vars) % creating a dataset with no variables
    % Give the output dataset the same number of rows as the input struct ...
    if supplied.ObsNames % ... using either the supplied observation names
        d = dataset(args{:});
    else                 % ... or by tricking the constructor
        dummyNames = cellstr(num2str((1:nrows)'));
        d = dataset('ObsNames',dummyNames(1:nrows),args{:});
        d.Properties.ObsNames = {};
    end
else
    d = dataset(vars{:},args{:});
end
