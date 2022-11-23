function [pdf]=cdf2pdf(z,cdf);% cdf2pdf                   - compute the pdf from the cdf (Jan 1,2001)%% Compute the values of the probability distribution function% based on a discrete definition of the cumulative distribution% function.%% SYNTAX :%% [pdf]=cdf2pdf(z,cdf);%% INPUT :%% z      n by 1   vector of values.% cdf    n by 1   vector of the cumulative distribution function values%                 at the z values.%% OUPUT :%          % pdf    n by 1   vector of the probability distribution function values%                 at the z values.%% NOTE :%% As the differentiation of the cumulative distribution function is% obtained using a finite difference scheme, it is recommended to% have a finely discretized definition of this distribution. [z,index1]=sort(z);           %%% Keep track of the rank for the sorted valuescdf=cdf(index1);              %%% Sort the pdf values accordinglyforward=diff(cdf)./diff(z);   %%% Compute the pdf using finite differencesn=length(z);pdf=zeros(n,1);pdf(1)=forward(1);pdf(n)=forward(n-1);pdf(2:n-1)=0.5*forward(2:n-1)+0.5*forward(1:n-2);[index1,index2]=sort(index1); %%% Resort the values in the original orderpdf=pdf(index2);