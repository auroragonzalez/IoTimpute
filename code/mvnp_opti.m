function   [P,Err,Info]=mvnp_opti(lowerL,upperL,covMatrix,maxpts,aEps,rEps)

% lowerL = [-1.57 -0.9295 -0.0708]
% upperL = [0.63 1.2705 2.1292]
% covMatrix = [2.7746 -0.0757 0.0358; -0.0757 3.5240 1.3156; 0.0358 1.3156 3.7585]
% maxpts = 50000
% aEps = 0
% rEps = 1.0000e-04
% mvnp_opti(a,b,c,maxpts,aEps,rE)

% mvnAG1                    - Multi Variate Normal probability, using the AG1 algorithm (Jan 1, 2001)
%
% Calculates the probability P of a multivariate normal random variable with
% zero mean and covariance matrix C to have a value in a hyper rectangle
% define by the lower bound a and upper bound b.
% The numerical algorithm used is AG1 (Adaptative Genz 1) which uses a
% uses a subregion adaptative method to do the multiple integration of the 
% transformed mvn integral.
% Reference: Alan Genz, "Numerical Computation of Multivariate Normal 
% Probabilities", J. of Computational and Graphical Stat., 1(1992), pp. 141-149
%
% SYNTAX :
%
% [P,Err,Info]=mvnAG1(a,b,C,maxpts,aEps,rEps);
%
% INPUT :
%
% a       N by 1     vector of lower integration limits of length N (N<20)
% b       N by 1     vector of upper integration limits of length N.
% C       N by N     covariance matrix of size N by N
% maxpts  scalar     maximum number of function values allowed. Start with
%                    maxpts = 1000*N, and increase maxpts if Err is too large.
% aEps    scalar     absolute error tolerance.
% rEps    scalar     relative error tolerance.
%
% OUTPUT :
%
% P       scalar     estimated value for the integral
% Err     scalar     estimated absolute error, with 99% confidence level.
% Info    scalar     if INFO = 0, normal completion with ERR < EPS;
%                    if INFO = 1, completion with ERR > EPS and MAXPTS 
%                    function values used; increase MAXPTS to decrease ERROR;


%%%%%%%%%%%%%%%%%     Ejemplo       %%%%%%%%%%%%%%%%%%%%%%

% a = [-1.57 -0.9295 -0.0708]
% b = [0.63 1.2705 2.1292]
% c = [2.7746 -0.0757 0.0358; -0.0757 3.5240 1.3156; 0.0358 1.3156 3.7585]
% maxpts = 50000
% aEps = 0
% rEps = 1.0000e-04

% [P,Err,Info]=mvnAG1(a,b,C,maxpts,aEps,rEps);

% P % DEBE VALER 0.08349516
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  %tic

  %%% CONTROL DE ERRORES
  %if(any(lowerL > upperL))
  %  stop("lower>upper integration limits");
  %elseif(any(lowerL == upperL))
  %  P = 0;
  %  return;
  %endif
  
  %%%%% 1
  m = length(lowerL);
  y = zeros(1, m);
  d = zeros(1, m);
  e = zeros(1, m);
  f = zeros(1, m);

  %%%%% 2
  C = chol(covMatrix)';

  %%%%% 3
  Intsum = 0;
  N = 0;
  Varsum = 0;
  d(1) = phi_opti(lowerL(1)/C(1,1));
  e(1) = phi_opti(upperL(1)/C(1,1));
  f(1) = e(1)-d(1);

  %%%%% 4
  do
  % a)
    w = rand(1,m);
    
  % b)
  
    for(i = 2:m)
      y(i-1) = 1/(phi_opti(d(i-1)+w(i-1)*(e(i-1)-d(i-1))));  %es esto correcto? la y siempre tendra su ultima coordenada cero
      %y = 1/(phi(d+w.*(e-d)));  %es esto correcto? la y siempre tendra su ultima coordenada cero
      aux = 0;
      for(j = 1:(i-1))
        aux = aux + C(i,j)*y(j);
      end
      d(i) = phi_opti((lowerL(i)-aux)/C(i,i));
      e(i) = phi_opti((upperL(i)-aux)/C(i,i));
      f(i) = (e(i)-d(i))*f(i-1);
    end
  

  quad("f",-Inf,y)
% AGV esto estaba dentro del bucle siguiente
  y = 1./(phi_opti(d+w.*(e-d)));
    for(i = 2:m)
      % AGV lo pongo fuera
      %y(i-1) = 1/(phi(d(i-1)+w(i-1)*(e(i-1)-d(i-1))));  %es esto correcto? la y siempre tendra su ultima coordenada cero
      aux = 0;
      for(j = 1:(i-1))
        aux = aux + C(i,j)*y(j); 
%        aux = aux + C.*y; ESTA NO va bien
      end
      d(i) = phi_opti((lowerL(i)-aux)/C(i,i));
      e(i) = phi_opti((upperL(i)-aux)/C(i,i));
      f(i) = (e(i)-d(i))*f(i-1);
    end
    
  % c)
   N = N+1;
   delta = (f(m)-Intsum)/N;
   Intsum = Intsum + delta;
   Varsum = (N-2)*Varsum/N + delta^2;
   Error = 2.5*sqrt(Varsum);
   
  until(Error < rEps || N == maxpts)

  %%%%% 5
  P = Intsum;
  Err = Error;
  Info = 0;

  %if(N >= maxpts)
  %  Info = 1;
  %endif
  
  %toc
  
endfunction