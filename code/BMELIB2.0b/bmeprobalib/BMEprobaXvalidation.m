function [zXval,info]=BMEprobaXvalidation(skOption,sh,ss,zh,softpdftype,nl,limi,probdens,...
        covmodel,covparam,nhmax,nsmax,dmax,order,options);

% BMEprobaXvalidation       - Cross validation depending upon different sets of estimation grids
%
% SYNTAX :
%
% INPUT :
%
% skOption   scalar       1 for sk=sh, remove hard data at estimation point and do cross validation
%                         2 for sk=[sh;ss]; remove either hard or soft data at est pt and do cross validation
% 
% See BMEprobaMoments.m for explanations for other inupt variables
%
% OUTPUT :
%
% zXval      nk by 1      Vector of the first order moments estimate at each nk estimation points.
%                         The moments are the BME mean
%
% info       nk by 1      vector of information about the computation in the local 
%                         neighbourhood around the estimation point :
%                         info=NaN : no computation, no hard or soft data
%                         info=0   : computation using BME with soft data 
%                         info=1   : dubious results, integration error above tolerance
%                                    when integrating over the soft pdf. Try to increase 
%                                    the value of options(3)
%                         info=3   : computation using only hard data
%                         info=4   : computation provides hard data value at estimation point
%                         info=10  : dubious results from integration routine. Try reducing
%                                    the number of soft data points ns, or option(3) or option(4)            

options(8)=1;     % to calculate the mean (only first moments) of the BME posterior pdf

switch skOption
  case 1, sk=sh;
  case 2, sk=[sh;ss];
  otherwise, error('Bad value for skOptions');
end
        
for ik=1:size(sk,1)
  clear skk zhh nll limii;
  if size(sh,1)==size(sk,1)              % sk=sh : when skOption is 1
    skk=sk; skk(ik,:)=[];
    shK=skk(1:size(sh,1)-1,:);
    zhh=zh; zhh(ik)=[]; zhK=zhh;
    ssK=ss; nlK=nl; limiK=limi; probdensK=probdens;
    
  elseif size(sh,1)~=size(sk,1)          % sk=[sh;ss] : when skOption is 2
    skk=sk; skk(ik,:)=[];
    if ik <= size(sh,1)
      shK=skk(1:size(sh,1)-1,:);
      zhh=zh; zhh(ik)=[]; zhK=zhh;
      ssK=skk(size(sh,1):end,:); %or  ssK=ss;
      nlK=nl; limiK=limi; probdensK=probdens;
    elseif ik >size(sh,1)
      shK=skk(1:size(sh,1),:);
      zhh=zh; zhK=zhh;
      ssK=skk(size(sh,1)+1:end,:);
      nll=nl; nll(ik-size(sh,1))=[]; nlK=nll;
      limii=limi; limii(ik-size(sh,1),:)=[]; limiK=limii;
      probdenss=probdens; probdenss(ik-size(sh,1),:)=[]; probdensK=probdenss;
    end
  end
  [moments,info]=BMEprobaMoments(sk(ik,:),shK,ssK,zhK,softpdftype,nlK,limiK,probdensK,...
     covmodel,covparam,nhmax,nsmax,dmax,order,options);
  zXval(ik,1)=moments(1);
end
