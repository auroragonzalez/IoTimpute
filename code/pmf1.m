function prediction = pmf1(data, A, B, maxepoch)

  cd(fileparts(mfilename('fullpath')));

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

  data = data(:,1:B);
  REAL = data(A,B);
  prediction = 0;
  data(A,B) = NaN;
  
  [Nt,M] = size(data);
  mote = repelem(1:Nt,M);
  obs = repmat(1:M,1,Nt);
  temp = [];
  tempN = [];
        
  for i= 1:Nt
    a = data(i,:);
    temp = [temp; a'];
    b = (a-min(a))/(max(a)-min(a));
    tempN = [tempN; b'];
  end
  
  all_data = zeros(length(mote),3);

  all_data(:,1) = mote;
  all_data(:,2) = obs;
  all_data(:,3) = tempN;

  all_index = 1:length(mote);
  test_index = find(isnan(all_data(:,3)));
  ismem = ismember(all_index,test_index);
  train_index = all_index(~ismem);

  train = all_data(train_index,:);
  test = all_data(test_index,:); 

  restart = 0;
  epsilon = 50;
  lambda = 0.01;
  momentum = 0.8;
  epoch = 1;

  mean_rating = mean(train(:,3));
  pairs_tr = length(train);
  pairs_pr = length(test);

  numbatches = 1;
  num_m = size(data,2);
  num_p = size(data,1);
  num_feat = 10;  % AUTOMATIZAR ESTO, ES LA K DEL PAPER
  
  w1_M1 = 0.1*randn(num_m, num_feat);
  w1_P1 = 0.1*randn(num_p, num_feat);
  w1_M1_inc = zeros(num_m, num_feat);
  w1_P1_inc = zeros(num_p, num_feat);
  foo = 1;   
  N = length(train);
  batch = 1;
  
  %dw1_M1 = zeros(num_m,num_feat);
  %dw1_P1 = zeros(num_p,num_feat);
  
  %nam = pairs_tr;
  %nap = pairs_tr;
  %nca = size(dw1_M1)(1);
  %npa = size(dw1_P1)(1);
  
  %tic;
                
  for epoch = epoch:maxepoch
    %epoch

    rr = randperm(pairs_tr);
    train = train(rr,:);
    clear rr
        
    aa_p   = double(train((batch-1)*N+1:batch*N,1)); 
    aa_m   = double(train((batch-1)*N+1:batch*N,2));
    rating = double(train((batch-1)*N+1:batch*N,3));
    rating = rating-mean_rating;
  
    %%%%%%%%%%%%%% Compute Predictions %%%%%%%%%%%%%%%%%
    pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2);  % del train

    %%%%%%%%%%%%%% Compute Gradients %%%%%%%%%%%%%%%%%%%
    IO = repmat(2*(pred_out - rating),1,num_feat);
    Ix_m = IO.*w1_P1(aa_p,:) + lambda*w1_M1(aa_m,:);
    Ix_p = IO.*w1_M1(aa_m,:) + lambda*w1_P1(aa_p,:);
    
    %%AQUIIIII
    
    dw1_M1 = zeros(num_m,num_feat);
    dw1_P1 = zeros(num_p,num_feat);
    
    nam = numel(aa_m);
    nap = numel(aa_p);
    nca = size(dw1_M1)(1);
    npa = size(dw1_P1)(1);
    
    dw1_M1 = sparse(aa_m, 1:nam, 1, nca, nam) * Ix_m;
    dw1_P1 = sparse(aa_p, 1:nap, 1, npa, nap) * Ix_p;
    
    %%AQUIIIII
    
    %dw1_M1 = sparse(aa_m, 1:nam, 1, nca, nam) * Ix_m;
    %dw1_P1 = sparse(aa_p, 1:nap, 1, npa, nap) * Ix_p;
          
    %%%% Update movie and user features %%%%%%%%%%%
    w1_M1_inc = momentum*w1_M1_inc + epsilon*dw1_M1/N;
    w1_M1 =  w1_M1 - w1_M1_inc;

    w1_P1_inc = momentum*w1_P1_inc + epsilon*dw1_P1/N;
    w1_P1 =  w1_P1 - w1_P1_inc;
    
    %%% Compute predictions on the validation set %%%%%%%%%%%%%%%%%%%%%%
    NN = pairs_pr;

    aa_p = double(test(:,1));
    aa_m = double(test(:,2));

    pred_out = sum(w1_M1(aa_m,:).*w1_P1(aa_p,:),2) + mean_rating;

    maxTrain = max(temp(train_index));
    minTrain = min(temp(train_index));

    predTest = pred_out*(maxTrain-minTrain) + minTrain;
    
    if(abs(predTest-prediction) < 2.5 || prediction == 0)
      prediction = predTest;
    endif;
        
    dw1_M1 = zeros(num_m,num_feat);
    dw1_P1 = zeros(num_p,num_feat);
    
  end
  
  %toc;
  
  REAL
  prediction
  
  prediction = REAL-prediction;
  
  
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
  
endfunction