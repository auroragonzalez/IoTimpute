function [y,pdf,info]=BMEintervalTPdf(y,ck,ch,cs,yh,a,b,covmodel,covparam,nhmax,nsmax,dmax,yfile,cdfyfile,options);% BMEintervalTPdf           - BME mode prediction with transform and interval data (Jan 1,2001) %% Compute at a single estimation location the complete posterior% probability distribution function using both hard and interval% soft data and a transformation of the variables. The function% is closely related to the BMEintervalTMode.m function, but the% complete distribution is provided at the estimation location,% instead of merely the mode of this distribution. %% SYNTAX :%% [y,pdf,info]=BMEintervalTPdf(y,ck,ch,cs,yh,a,b,covmodel,covparam,nhmax,nsmax,dmax,yfile,cdfyfile,options);%% INPUT :%% y          nz by 1      vector of values on the original (non-Gaussian)%                         scale sorted in ascending order for which the%                         distribution must be computed. It is recommended%                         to have a finely discretized set of values, so that%                         the distribution can be computed on a fine scale.% ck         1  by d      vector of coordinates for the estimation location,%                         where d is the dimension of the space. For doing%                         computation at different estimation locations, the%                         function can be invoked in a loop so that a new ck%                         vector and probabilistic soft data can be used.% ch         nh by d      matrix of coordinates for the hard data locations,%                         with the same convention as for ck.% cs         ns by d      matrix of coordinates for the soft data locations,%                         with the same convention as for ck.% yh         nh by 1      vector of values on the original (non-Gaussian) scale%                         for the hard data at the coordinates specified in ch.% a          ns by 1      vector of values on the original (non-Gaussian) scale%                         for the lower bound of the intervals at the coordinates%                         specified in cs.% b          ns by 1      vector of values on the original (non-Gaussian) scale%                         for the upper bound of the intervals at the coordinates%                         specified in cs.% covmodel   string       that contains the name of the covariance model that is%                         used for estimation on the Gaussian scale. If this model%                         has to be estimated from the data, it is needed to%                         transform the data on the Gaussian scale prior to its%                         estimation (see other2gauss.m).% covparam   1 by k       vector of values for the parameters of covmodel, according%                         to the convention for the corresponding covariance model.% nhmax      scalar       maximum number of hard data values that are considered%                         for the estimation at the locations specified in ck.% nsmax      scalar       maximum number of soft data values that are considered for%                         the estimation at the locations specified in ck. As the%                         computation time is exponentially increasing with nsmax,%                         it is not advised to use more than few soft data locations.%                         In any case, nsmax should be lower than 20 in order to%                         avoid numerical computation problems.% dmax       scalar       maximum distance between an estimation location and%                         existing hard/soft data locations. All hard/soft data%                         locations separated by a distance smaller than dmax from an%                         estimation location will be included in the estimation process%                         for that location, whereas other data locations are neglected.% yfile      m by 1       vector of values sorted in ascending order for which the%                         cumulative distribution distribution is provided on the original%                         scale. It is recommended to have a finely discretized set of%                         values, so computations can be realized on a fine scale.% cdfyfile   m by 1       vector of values for the cumulative distribution function at the%                         yfile values.% options    1 by 4       vector of optional parameters that can be used if default values%                         are not satisfactory (otherwise this vector can simply be omitted%                         from the input list of variables), where :%                         options(3) specifies the maximum number of evaluation that can be%                         done by the FORTRAN77 subroutines for the integrals (default value%                         is 50 000 ; this value should be increased if a warning message%                         appears on the screen during the computation). options(4) specifies%                         the maximum admissible relative error on the estimation of these%                         integrals (default value is 1e-4).%                         The values for options(1) and options(2) are not used.%% OUTPUT :%% y          nzout by 1   possibly modified vector of values for which the updated%                         distribution has been computed. The output y vector differs%                         from the input y vector when there is an interval data at the%                         estimation location. In that case, the bounds of the intervals%                         are added to the original y vector.% pdf        nzout by 1   vector of values for the probability distribution function%                         computed at the y values. The values of pdf associated with%                         values of y that are outside the definition of yfile, cdfyfile%                         are coded as NaN's.% info       nzout by 1   vector for information about the computation of estimated values.%                         The different possible values are the same as for info in%                         BMEintervalTMode.m, except for info=2. For info=5, the output pdf%                         variable is a uniform distribution with bounds equal to those of%                         the interval specified on the original scale at the estimation%                         location.%% NOTE :%% 1- As the function computes the values of the posterior probability% distribution function using the derivative of the transformation% function, it is very highly recommended that the cumulative% distribution function specified in cdfyfile is at least twice% differentiable (i.e., the corresponding probability distribution% function is differentiable). This property makes sure that the% transformed posterior probability distribution function in the% original domain is smooth, so that the mode of the distribution% can be easily identified. It is thus not advised to use an empirical% cumulative distribution function as provided by the cdfest.m function,% which is expected to be not smooth at all. Instead, an estimate of% the cumulative distribution function as provided by kernelsmoothing.m% or a discrete definition of a differentiable parametric distribution% is to be preferred.%% 2- Using BMEintervalTPdf.m requires that the variable has a% constant mean. If this hypothesis does not hold, it is necessary% to remove first the values of the mean from the yh vector. This% can be done using, e.g., the regression.m or the kernelregression.m% functions).%% 3- If there are several variables to be processed at the same time% (see the multivariate case described in kriging.m), the yfile and% cdfyfile variables must be defined as 1 by nv cell arrays, where% each cell corresponds to the definition of the distribution for the% corresponding variable. %% 4- Note that in the case there are no available hard data at all,% ch and zh can be entered as the empty [ ] matrices.%% 5-  All the specific conventions for specifying nested models,% multivariate or space-time cases are the same as for BMEprobaMoments.m.%%%%%% Error messagesif sum(nsmax)>20,  error('sum(nsmax) must not exceed 20');end;%%%%%% Initialize the parametersglobal INFOINTEG      % information message from the Fortran integration subroutineif nargin<15,         % initialize options with default values if not provided as input  options(3)=50000;  options(4)=1e-4;end;%%%%%% Transform the y values to Gaussian-distributed valuesnoindex=~iscell(ck);if noindex==1,  z=other2gauss(y,yfile,cdfyfile);  zh=other2gauss(yh,yfile,cdfyfile);  a=other2gauss(a,yfile,cdfyfile);  b=other2gauss(b,yfile,cdfyfile);  yfile={yfile};  cdfyfile={cdfyfile};  indexk=1;else  indexk=ck{2};  indexh=ch{2};  indexs=cs{2};  z=other2gauss({y,indexk},yfile,cdfyfile);  zh=other2gauss({yh,indexh},yfile,cdfyfile);  a=other2gauss({a,indexs},yfile,cdfyfile);  b=other2gauss({b,indexs},yfile,cdfyfile);end;%%%%%% Select the local neighbourhood[chlocal,zhlocal,dh,sumnhlocal]=neighbours(ck,ch,zh,nhmax,dmax);[cslocal,ablocal,ds,sumnslocal]=neighbours(ck,cs,[a b],nsmax,dmax);if ~isempty(ablocal),  alocal=ablocal(:,1);blocal=ablocal(:,2);else  alocal=[];blocal=[];end;%%%%%% Test if there is a hard data at estimation pointiscomputed=0;[index]=findpairs(ck,chlocal);                      % test if there is a hard data at estimation pointif ~isempty(index),  zk=zhlocal(index(2));                             % return the hard data value as the estimated value  yk=gauss2other(zk,yfile{indexk},cdfyfile{indexk});% back-transform the zk value to the original scale  pdf=zeros(size(y));                               % initialize the content of the pdf variable  pdf(y==yk)=Inf;                                   % set the corresponding pdf value to +Inf  info=4;                                           % set the value of info to 4   iscomputed=1;                                     % specify that the pdf has been computedend;%%%%%% Test if there is a soft data at estimation pointisduplicate=0;[index]=findpairs(ck,cslocal);             if ~isempty(index),                       [csest,cslocal,abest,ablocal]=split(cslocal,[alocal blocal],index(2));  alocal=ablocal(:,1);  blocal=ablocal(:,2);  aest=abest(:,1);  best=abest(:,2);  indexin=find(z>aest && z<best);               % find the z values inside the interval [aest,best]  ayest=gauss2other(aest,yfile{indexk},cdfyfile{indexk});  byest=gauss2other(best,yfile{indexk},cdfyfile{indexk});  y=[ayest;y(indexin);byest];                  % select a subset of y and add the bounds of the interval  z=[aest;z(indexin);best];                    % select a subset of z and add the bounds of the interval  sumnslocal=sumnslocal-1;  isduplicate=1;                               % specify that there is a soft data at estimation pointend;%%%%%% Returns NaN or the uniform pdf when (sumnhlocal=0)&&(sumnslocal==0)&&(iscomputed==0)if (sumnhlocal==0)&&(sumnslocal==0)&&(iscomputed==0),  if isduplicate==1,    pdf=uniformpdf(z,[ayest,byest]);    info=ones(size(z))*5;  else    nz=length(z);    pdf=ones(nz,1)*NaN;    info=ones(nz,1)*NaN;;  end;end;%%%%%% Returns the simple kriging pdf when nslocal==0if (sumnhlocal>0)&&(sumnslocal==0)&&(iscomputed==0), % test if there are no soft data  K=coord2K(chlocal,chlocal,covmodel,covparam);    % built the left-hand side  k=coord2K(chlocal,ck,covmodel,covparam);         % built the right-hand side  lam=K\k;                                         % compute the kriging weights lam  lamt=lam';  zk=lamt*zhlocal;                                 % compute the kriging estimates zk(i)  vk=K(1,1)-2*lamt*k+lamt*K*lam;                   % and the kriging variance vk(i)  pdf=gausspdf(z,[zk vk]);                         % compute the value of the Gaussian pdf  info=ones(size(z))*3;                               % set the value of info to 3end;%%%%%% Returns the BME pdf when nslocal>0if (sumnslocal>0)&&(iscomputed==0),  pdf=zeros(size(z))*NaN;                         % initialize the pdf values  info=zeros(size(z))*NaN;                        % initialize the info values  Kkk=coord2K(ck,ck,covmodel,covparam);           % compute variance for estimation point  Khh=coord2K(chlocal,chlocal,covmodel,covparam); % compute covariance matrix for hard data  Kss=coord2K(cslocal,cslocal,covmodel,covparam); % compute covariance matrix for soft data  Kkh=coord2K(ck,chlocal,covmodel,covparam);      % compute cross-covariance vector estimation/hard data  Kks=coord2K(ck,cslocal,covmodel,covparam);      % compute cross-covariance vector estimation/soft data  Ksh=coord2K(cslocal,chlocal,covmodel,covparam); % compute cross-covariance matrix soft/hard data  Kkhkh=[[Kkk,Kkh];[Kkh',Khh]];                   % build estimation+hard data covariance matrix  Kskh=[Kks',Ksh];                                % build cross-covariance matrix soft/estimation+hard data  if sumnhlocal==0,                               % if no hard data use the soft covariance    [fden]=intBMEinterval([],alocal,blocal,[],Kss,options);  else                                            % else use the conditional soft covariance    invKhh=inv(Khh);    KshinvKhh=Ksh*invKhh;    cstden=1/(((2*pi)^(sumnhlocal/2))*sqrt(det(Khh)));    [Pden]=intBMEinterval(zhlocal,alocal,blocal,KshinvKhh,Kss-KshinvKhh*Ksh',options);    fden=cstden*exp(-0.5*zhlocal'*invKhh*zhlocal)*Pden;  end;  if fden==0,                                     % if denominator=0, set pdf values to 0    pdf=zeros(size(z));  else                                            % else compute the numerator values    cstnum=1/(((2*pi)^((sumnhlocal+1)/2))*sqrt(det(Kkhkh)));    invKkhkh=inv(Kkhkh);    KskhinvKkhkh=Kskh*invKkhkh;    for i=1:length(z),      [Pnum]=intBMEinterval([z(i);zhlocal],alocal,blocal,KskhinvKkhkh,Kss-KskhinvKkhkh*Kskh',options);      fnum=cstnum*exp(-0.5*[z(i);zhlocal]'*invKkhkh*[z(i);zhlocal])*Pnum;      pdf(i)=fnum/fden;      if INFOINTEG==0,info(i)=0;end;      if INFOINTEG==1,info(i)=1;end;    end;  end;end;%%%%%% Reset de pdf values in the interval [aest,best] if neededif isduplicate==1,                          % if there is a soft data at estimation point  cst=trapezint(z,pdf,aest,best);           % integrate the function over [aest,best]  pdf=pdf/cst;                              % normalize the pdf values by the integral  z=[aest;z;best];                          % add the bounds of the interval to the z vector  y=[ayest;y;byest];                        % add the bounds of the interval to the y vector  pdf=[0;pdf;0];                            % add 0 as value of the pdf at the bounds  ninfo=length(info);                       % compute the length of the info vector   info=[info(1);info;info(ninfo)];          % add first and last info values at the boundsend;%%%%%% Compute the posterior pdf on the original scaleif iscomputed==0,  [dgfile]=transformderiv(yfile{indexk},cdfyfile{indexk});  [zfile]=other2gauss(yfile{indexk},yfile{indexk},cdfyfile{indexk});  isinterp=(z>=min(zfile) && z<=max(zfile));  ytemp=y(isinterp);  ztemp=z(isinterp);  pdftemp=pdf(isinterp);  dgtemp=interp1(zfile,dgfile,ztemp,'linear');  pdftemp=pdftemp./dgtemp;  pdf(isinterp)=pdftemp;  pdf(~isinterp)=NaN;end;