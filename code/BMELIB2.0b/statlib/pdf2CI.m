function [zlCI,zuCI,pdfCI,lCI]=pdf2CI(z,pdf,p);% pdf2CI                    - Compute confidence set from a pdf (Jan 1, 2001)%% Compute the confidence set corresponding to a specified confidence % probability, so that the probability of being in this set is equal% to the specified confidence probability.% In the simplest case, the confidence set is a single interval with% lower and upper bounds corresponding to the same pdf value. However,% when the pdf has several local maximas, the confidence set may be% the union of disconnected intervals. In that case, the function% returns this set of intervals.% % SYNTAX:%% [zlCI,zuCI,pdfCI,lCI]=pdf2CI(z,pdf,p);%% INPUT:% z         n by 1   vector of values.% pdf       n by 1   vector of the probability distribution function%                    values at the z values.% p         scalar   probability level of the confidence interval.%% OUTPUT:%% zlCI      k by 1   vector of lower bounds values for the disjoint %                    confidence intervals that define the confidence set.%                    It there is only one interval, zlCI is a scalar.% zuCI      k by 1   vector of upper bounds values for the disjoint%                    confidence intervals, with same conventions as%                    for zlCI.% pdfCI     scalar   pdf value at the lower and upper bounds of the intervals.% lCI       scalar   total length of the confidence intervals.%% NOTE :%% It is possible to specify simultaneously several probability levels.% The p variable is then a 1 by m vector, and the output zlCI and zuCI% variables are 1 by m cell arrays, where the ith cells contains the% values corresponding to the ith probability level p(i). The pdfCI and% lCI variables are then 1 by m vectors.%%%%%% Check the input dataif length(z)~=length(pdf),   error('z and pdf must have the same length');end; z=z(:);pdf=pdf(:);if min(p)<0.01 | max(p)>0.99  error('the confidence probabilities p must all be between 0.01 and 0.99');end;if min(pdf)<0,   error('pdf values cannot be negative');end;%%%%%% Sort the values in ascending order[z,index]=sort(z);pdf=pdf(index);%%%%%% If the pdf has some NaN, try to remove themisnanpdf=isnan(pdf);if sum(~isnanpdf)<=1 & sum(isinf(pdf))==0  for i=1:length(p);    zlCI{1,i}=NaN;    zuCI{1,i}=NaN;    pdfCI(1,i)=NaN;    lCI(1,i)=NaN;   end;  return;elseif sum(isnanpdf)>0  warning('Removing NaN values from the pdf when calculating the CI');  z=z(~isnanpdf);  pdf=pdf(~isnanpdf);end;%%%%% If the pdf has an Inf value, then the RV has a deterministic value = z(isinf(pdf))idxInf=find(isinf(pdf));if length(idxInf)>1,   error('pdf must have at most one infinite value'); end;if length(idxInf)==1,  zDeterministic=z(idxInf);  for i=1:length(p);    zlCI{1,i}=zDeterministic;    zuCI{1,i}=zDeterministic;  end;  pdfCI=Inf*ones(1,length(p));  lCI=zeros(1,length(p));  return;end;%%%%% add 0 for the pdf at the beginning and end of definition domain if neededif pdf(1)>0,  z=[z(1);z];  pdf=[0;pdf];end;if pdf(end)>0  z=[z;z(end)];  pdf=[pdf;0];end;%%%%%% Normalize the pdfdz=diff(z);pdfave=0.5*(pdf(1:end-1)+pdf(2:end));A=sum(dz.*pdfave);if A<0.95 | A>1.05  warning(sprintf('The area A under the pdf =%f. The pdf was normalized so A=1',A));end;pdf=1/A*pdf;%%%%%% Find the confidence intervals[pdfmode imode]=max(pdf);mode=z(imode);ng=100;if length(pdf)>=ng-1,  pdfg=sort(pdf);else  pdfg=sort([pdf;(0:pdfmode/(ng-length(pdf)):pdfmode)']);end;  pdfg=[pdfg(1);pdfg(find(diff(pdfg)~=0)+1)];for i=1:length(pdfg),  Pg(i)=pdf2P(pdfg(i),z,pdf);end;for i=1:length(p),  pdfCI(i)=interp1monotone(Pg,pdfg,p(i));  [pcalc(i),zlCI{i},zuCI{i},lCI(i)]=pdf2P(pdfCI(i),z,pdf);end;%%%% Transform zlCI and zuCI into vectors instead of cell arrays%%%% when length(p)=1if length(p)==1,  zlCI=zlCI{1};  zuCI=zuCI{1};end;%%%% function [P,zl,zu,lI]=pdf2P(fP,z,f)%% For a pdf f(z), this function finds the domain A={z:f(z)>fP},% and the associated probability P(A).% The domain A is defined as the union of disconnected intervals% with lower bounds given in zl and upper bound given in zu, such that% A=Ui{zl(i)<z<zu(i)}%function [P,zl,zu,lI]=pdf2P(fP,z,f)if fP<=0,  zl=z(1);  zu=z(end);  P=1;  lI=zu-zl;elseif fP>=max(f)  zl=NaN;  zu=NaN;  P=0;  lI=0;else  abovefP=(f>=fP);  diffabovefP=diff(abovefP);  dz=diff(z);  df=diff(f);  fave=0.5*(f(1:end-1)+f(2:end));  P=0;  idx= find( abovefP(1:end-1)==1 & diffabovefP==0 );  if length(idx)>0    P=P+sum(dz(idx).*fave(idx));  end;  idx= find( diffabovefP==1 );  if length(idx)>0,    z1=z(idx);    z2=z(idx+1);    f1=f(idx);    if sum(f1>=fP)>0 error('Something went wrong!'); end;    f2=f(idx+1);    if sum(f2<fP)>0 error('Something went wrong!'); end;    zl=z1+(z2-z1).*(fP-f1)./(f2-f1);    P=P+0.5*sum((z2-zl).*(f2+fP));    end;  idx= find(diffabovefP==-1);  if length(idx)>0,    z1=z(idx);    z2=z(idx+1);    f1=f(idx);    if sum(f1<fP)>0 error('Something went wrong!'); end;    f2=f(idx+1);    if sum(f2>=fP)>0 error('Something went wrong!'); end;    zu=z1+(z2-z1).*(f1-fP)./(f1-f2);    P=P+0.5*sum((zu-z1).*(f1+fP));    end;  lI=sum(zu-zl);end;%%%% function [P,zl,zu,lI]=pdf2P(fP,z,f)%% A modified interp1 function that works if a and y are monotonic %function yi=interp1monotone(x,y,xi)idx=find(xi==x);if ~isempty(idx);  yi=min(y(idx));else  i1=min(find(x<xi));  i2=max(find(x>xi));  x1=x(i1);  x2=x(i2);  y1=y(i1);  y2=y(i2);  yi=y1+(y2-y1)*(xi-x1)/(x2-x1);end;    