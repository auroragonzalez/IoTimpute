function [best,Vbest,zest,index]=regression(c,z,order,K);

% regression                - parameters estimation in a linear regression model (Jan 1,2001)
%
% Standard implementation of the least squares estimation
% procedure in a linear regression model, where the
% deterministic part of the linear model is a polynomial
% of arbitrary order. Though it is presented here in a
% spatial context, it can be used for a wide variety of
% other non spatial cases. 
%
% SYNTAX :
%
% [best,Vbest,zest,index]=regression(c,z,order,K); 
%
% INPUT :
%
% c        n by d       matrix of coordinates for the locations. A line
%                       corresponds to the vector of coordinates at a
%                       location, so the number of columns in c corresponds
%                       to the dimension of the space. There is no restriction
%                       on the dimension of the space.
% z        n by 1       vector of values at the c coordinates.
% order    scalar       order of the polynomial mean along the spatial axes
%                       specified in c, where order>=0.
% K        n by n       optional square symmetric matrix of covariance for the
%                       values specified in z. When K is specified, the generalized
%                       least squares parameter estimates are computed, whereas the
%                       ordinary least squares parameter estimates are computed
%                       otherwise.
%
% OUTPUT :
%
% best     k by 1       vector of parameter estimates.
% Vbest    k by k       square symmetric matrix  of covariance for the best parameter
%                       estimates.
% zest     n by 1       vector of estimated regression values at the c coordinates.
% index    k by 1 or 2  matrix associated with best. The first column specifies the
%                       degree of the estimated polynomial term for the corresponding
%                       best element, and the second column specifies the axis number
%                       to which this polynomial term belongs. The axis are numbered
%                       according to the columns of c. E.g., the axis 2 corresponds to
%                       the second column of c. Note that the value 0 in the second
%                       column of index is associated with the polynomial term of degree
%                       equal to 0 (i.e., the constant term) that is defined jointly for
%                       all the axes. In the singular case where c is a column vector
%                       (i.e., the dimension of the space is equal to 1), there is only
%                       one column for the index variable.
%
% NOTE :
%
% 1- It is also possible to process several variables at the same time
% (multivariate case). It is needed to specify additionally tags in the
% c matrix. These tags are provided as a vector of values that refers to
% the variable, the values ranging from 1 to nv, where nv is the number
% of variables. E.g., if there are 3 variables, the input index column vector
% must be defined, and the elements in index are equal to 1, 2 or 3. The
% c and index variables are grouped using the MATLAB cell array notation,
% so that c={c,index}, is now the correct input variable. Using the same
% logic, order is now a column vector specifying the order of the polynomial
% mean for each variable. For the output variable index, there is an additional
% first column that refers to the variable number associated with the
% corresponding best elements. If regression.m is used for processing several
% variables at the same time and if K is not specified, the results are the
% same than those obtained when using the function separately for each variable.
% This is because there are no interactions taken into account between the
% variables in the ordinary least squares case.
%
% 2- For space/time data, the convention is that the last column of the c
% matrix of coordinates corresponds to the time axis. Is is then possible to
% specify a different order for the polynomial along the spatial axes and the
% temporal axis. For the univariate case, order is a 1 by 2 vector, where
% order(1) is the order of the spatial polynomial and order(2) is the order of
% the temporal polynomial. For the multivariate case where nv different variables
% are considered, order is a nv by 2 matrix, where the first and second columns
% of order contain the order of the spatial and the temporal polynomial for
% each of the nv variables, respectively. If in that case order is entered as
% a 1 by 2 matrix, the same spatial order corresponding to order(1) and the same
% temporal order corresponding to order(2) will be used for all the variables.
 
best=[];
Vbest=[];
zest=[];
index=[];

[X,index]=designmatrix(c,order);  
index=index';  
[n,p]=size(X);    
Xt=X';  

if ~isempty(X);
  if nargin<4,
    invXtX=inv(Xt*X);
    best=invXtX*Xt*z;
    zest=X*best;
    resi=z-zest;
    s2=(resi'*resi)/(n-p);
    Vbest=invXtX*s2;
  else
    XtinvK=X'*inv(K);
    invXtinvKX=inv(XtinvK*X);
    best=invXtinvKX*XtinvK*z;
    zest=X*best;
    Vbest=invXtinvKX;
  end;
end;

