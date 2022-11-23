
clear;
clc;

page_screen_output(0);
page_output_immediately(1);


cd(fileparts(mfilename('fullpath')));

addpath BMELIB2.0b/bmecatlib;
addpath BMELIB2.0b/bmeintlib;
addpath BMELIB2.0b/bmehrlib;
addpath BMELIB2.0b/bmeprobalib;
addpath BMELIB2.0b/exlib;
addpath BMELIB2.0b/extensions;
addpath BMELIB2.0b/genlib;
addpath BMELIB2.0b/graphlib;
addpath BMELIB2.0b/iolib;
addpath BMELIB2.0b/modelslib;
addpath BMELIB2.0b/mvnlib;
addpath BMELIB2.0b/simulib;

pkg load statistics;

tic
ruta = './tmp';

predFinal = [];

Rgt = dlmread([ruta '/Rgt.csv']);
NofSensors = size(Rgt,1);

locations = dlmread([ruta '/locations.txt']);
locations = locations(:,2:3);

RMSE_normalised = [];
MSE_normalised = [];
MAE_normalised = [];
MAPE_normalised = [];
CVRMSE_normalised = [];

maxepoch = importdata([ruta '/epochs.txt']);
CLU = dlmread([ruta '/groupsk.txt']);
%numclus = size(CLU,2)-1;
numclus = max(CLU(:,2));   # JOSE: MODIFICADO 10//20202

cluster = repelem(1:numclus,1:numclus);
RMSE_inner = zeros(maxepoch, length(cluster));
MSE_inner = zeros(maxepoch, length(cluster));
MAE_inner = zeros(maxepoch, length(cluster));
MAPE_inner = zeros(maxepoch, length(cluster));
CVRMSE_inner = zeros(maxepoch, length(cluster));

NAS = importdata([ruta '/NAS.txt']);
%NAS = 45
[Nt,M] = size(Rgt);
M = importdata([ruta '/N.txt']);
Rgt = Rgt(:,1:M);
msize = Nt*M;

final = zeros(1,numclus);
finalT = zeros(1,numclus);

Rgt2 = Rgt;
imputaciones = zeros(length(NAS),4);
DataWhithNA = Rgt;

kk = NAS
  
  RMSEhard = [];
  MSEhard = [];
  MAEhard = [];
  MAPEhard = [];
  CVRMSEhard = [];
  timehard = [];
  nNA = floor(Nt*M*kk/100);
  rand(kk);
  matrixForNA = zeros(Nt,M);
  elements = randperm(msize, nNA);
  matrixForNA(elements) = 1;

  ssaux = 1;
  CLU = dlmread([ruta '/groupsk.txt']);
  k = numclus;
  CLU = CLU(:,[1,(k+1)]);
%for k = 1:numclus
    %CLU = dlmread([ruta '/groupsk.txt']);

    for index = 1:max(CLU(:,2))
      fprintf('############\n');
      index
      fprintf('############\n');
      submotes = CLU(find(CLU(:,2)==index),1).';
      submatrixForNA = matrixForNA(submotes,:);
      elements2 = find(submatrixForNA');

      mote = repelem(1:length(submotes),M);
      obs = repmat(1:M,1,length(submotes));

      temp = [];
      tempN = [];

      for i= 1:length(submotes)
        a = Rgt(submotes(i),:);
        temp = [temp; a'];
        b = (a-min(a))/(max(a)-min(a));
        tempN = [tempN; b'];
      end

      all_data = zeros(length(mote),3);

      all_data(:,1) = mote;
      all_data(:,2) = obs;
      all_data(:,3) = tempN;

      all_index = 1:length(mote);
      test_index = elements2;
      ismem = ismember(all_index,test_index);
      train_index = all_index(~ismem);

      train = all_data(train_index,:);
      test = all_data(test_index,:);

      % 
      % PMF original code extracted from Ruslan Salakhutdinov webpage: https://www.cs.toronto.edu/~rsalakhu/BPMF.html
      %

      restart = 0;
      epsilon = 50;
      lambda = 0.01;
      momentum = 0.8;

      epoch = 1;

      mean_rating = mean(train(:,3));

      pairs_tr = length(train);
      pairs_pr = length(test);

      numbatches = 1;
      num_m = size(Rgt,2);
      num_p = size(Rgt,1);
      num_feat = 10;

      w1_M1 = 0.1*randn(num_m, num_feat);
      w1_P1 = 0.1*randn(num_p, num_feat);
      w1_M1_inc = zeros(num_m, num_feat);
      w1_P1_inc = zeros(num_p, num_feat);
      foo = 1;
      tic
                
      for epoch = epoch:maxepoch
                    
        epoch

        rr = randperm(pairs_tr);
        train = train(rr,:);
        clear rr

         tic        
        for batch = 1:numbatches

          N=length(train)/numbatches;

          aa_p   = double(train((batch-1)*N+1:batch*N,1));  % ESTO NO PUEDE IR FUERA DEL BUCLE PORQUE TRAIN SE REHACE CADA VEZ
          aa_m   = double(train((batch-1)*N+1:batch*N,2));
          rating = double(train((batch-1)*N+1:batch*N,3));
          rating = rating-mean_rating;

          %%%%%%%%%%%%%% Compute Predictions %%%%%%%%%%%%%%%%%
          pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2);

          %%%%%%%%%%%%%% Compute Gradients %%%%%%%%%%%%%%%%%%%
          IO = repmat(2*(pred_out - rating),1,num_feat);
          Ix_m = IO.*w1_P1(aa_p,:) + lambda*w1_M1(aa_m,:);
          Ix_p = IO.*w1_M1(aa_m,:) + lambda*w1_P1(aa_p,:);

          dw1_M1 = zeros(num_m,num_feat);
          dw1_P1 = zeros(num_p,num_feat);

          
          %%%%%%%%%% Bucle a vectorizar si es posible  %%%%%%%%%%%%%%%%%%
          %for ii=1:N
          %  dw1_M1(aa_m(ii),:) =  dw1_M1(aa_m(ii),:) +  Ix_m(ii,:);
          %% dw1_P1(aa_p(ii),:) =  dw1_P1(aa_p(ii),:) +  Ix_p(ii,:);
          end
             
          nam = numel(aa_m);
          nap = numel(aa_p);
          nca = size(dw1_M1)(1);
          npa = size(dw1_P1)(1);
          dw1_M1 = sparse(aa_m, 1:nam, 1, nca, nam) * Ix_m;
          dw1_P1 = sparse(aa_p, 1:nap, 1, npa, nap) * Ix_p;
          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          
          %%%% Update movie and user features %%%%%%%%%%%
          w1_M1_inc = momentum*w1_M1_inc + epsilon*dw1_M1/N;
          w1_M1 =  w1_M1 - w1_M1_inc;

          w1_P1_inc = momentum*w1_P1_inc + epsilon*dw1_P1/N;
          w1_P1 =  w1_P1 - w1_P1_inc;
        end

       

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Compute predictions on the validation set %%%%%%%%%%%%%%%%%%%%%%
        NN = pairs_pr;

        aa_p = double(test(:,1));
        aa_m = double(test(:,2));

        pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2) + mean_rating;

        realTest = temp(test_index);
        maxTrain = max(temp(train_index));
        minTrain = min(temp(train_index));

        predTest = pred_out*(maxTrain-minTrain) + minTrain;
        predFinal = predTest;
        
        RMSE = sqrt(mean((realTest - predTest).^2));
        MSE = mean((realTest - predTest).^2);
        MAE = mean(abs(realTest - predTest));
        MAPE = mean(abs((realTest - predTest)./realTest));
        CVRMSE = sqrt(mean((realTest - predTest).^2))/sum(realTest)*length(realTest);
        
        RMSE
       % RMSE_inner(epoch, ssaux) = RMSE;
        timehard = [timehard, toc];
        RMSEhard = [RMSEhard,RMSE];
        MSEhard = [MSEhard,MSE];
        MAEhard = [MAEhard,MAE];
        MAPEhard = [MAPEhard,MAPE];
        CVRMSEhard = [CVRMSEhard,CVRMSE];
      end
%end          
 
auxInd = 1;
  for i = 1:rows(DataWhithNA)
    for j = 1:columns(DataWhithNA)
        if(matrixForNA(i,j) == 1)
            Rgt2(i,j) = predFinal(auxInd);
            
            imputaciones(auxInd,1) = locations(i,1);
            imputaciones(auxInd,2) = locations(i,2);
            imputaciones(auxInd,3) = j;
            imputaciones(auxInd,4) = predFinal(auxInd);
            
            DataWhithNA(i,j) = NaN;
            
            auxInd = auxInd + 1;
        endif
    endfor
  endfor
  
  matrixForNA;
  imputaciones;
  
  % save the results
  csvwrite([ruta '/informacionImputacionesPMF.csv'], imputaciones);
  csvwrite([ruta '/datosImputadosPMF.csv'], Rgt2);
  csvwrite([ruta '/dataWithNA.csv'], DataWhithNA);
  
 RMSEfinal = mean(RMSEhard)
 MSEfinal = mean(MSEhard)
 MAEfinal = mean(MAEhard)
 MAPEfinal = mean(MAPEhard)
 CVRMSEfinal = mean(CVRMSEhard)
 timefinal = sum(timehard)

csvwrite([ruta '/predFinalPMF.csv'], predFinal);
csvwrite([ruta '/RMSEhardPMF.csv'], RMSEfinal);
csvwrite([ruta '/MSEhardPMF.csv'], MSEfinal);
csvwrite([ruta '/MAEhardPMF.csv'], MAEfinal);
csvwrite([ruta '/MAPEhardPMF.csv'], MAPEfinal);
csvwrite([ruta '/CVRMSEhardPMF.csv'], CVRMSEfinal);
csvwrite([ruta '/timeshardPMF.csv'], timefinal);


rmpath BMELIB2.0b/bmecatlib;
rmpath BMELIB2.0b/bmeintlib;
rmpath BMELIB2.0b/bmehrlib;
rmpath BMELIB2.0b/bmeprobalib;
rmpath BMELIB2.0b/exlib;
rmpath BMELIB2.0b/extensions;
rmpath BMELIB2.0b/genlib;
rmpath BMELIB2.0b/graphlib;
rmpath BMELIB2.0b/iolib;
rmpath BMELIB2.0b/modelslib;
rmpath BMELIB2.0b/mvnlib;
rmpath BMELIB2.0b/simulib;
