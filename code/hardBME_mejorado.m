
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

predFinal = [];

ruta = './tmp';

Rgt = importdata([ruta '/Rgt.csv']);
CLU = dlmread([ruta '/groupsk.txt']);

NofSensors = size(Rgt,1)
ncl = size(CLU,2)-1

locations = dlmread([ruta '/locations.txt']);
locations = locations(:,2:3);

params = dlmread([ruta '/parametrosVariograma.csv']);
modeloVar = textread([ruta '/modeloVariograma.txt'], '%s');

nei = importdata([ruta '/nei.txt']);

%% Variables nuevas
neiMin = nei;
neiMax = nei;

N = importdata([ruta '/N.txt']);
NAS = importdata([ruta '/NAS.txt']);
Rgt = Rgt(:,1:N);
MAPE = zeros(length(NAS),1);
MAE = zeros(length(NAS),1);
RMSE = zeros(length(NAS),1);
MSE = zeros(length(NAS),1);
CVRMSE = zeros(length(NAS),1);
MSEhard = zeros(length(NAS),ncl);
RMSEhard = zeros(length(NAS),ncl);
MAEhard = zeros(length(NAS),ncl);
MAPEhard = zeros(length(NAS),ncl);
CVRMSEhard = zeros(length(NAS),ncl);
timehard= zeros(length(NAS),ncl);

Rgt2 = Rgt(:,1:N);
imputaciones = zeros(length(NAS),4);
DataWhithNA = Rgt;

for NCL = 1:ncl 
  
  NCL
  errorAux = zeros(NCL,1);
  mseAux = zeros(NCL,1);
  maeAux = zeros(NCL,1);
  mapeAux = zeros(NCL,1);
  cvrmseAux = zeros(NCL,1);
  timeAux = zeros(NCL,1);

  indice = 1;
  for neighb = neiMin:neiMax
    
    neighb
    
    if(NAS == 0)
        matrixForNA = isna(Rgt);
        nNA = sum(sum(matrixForNA,1));
    else
        n = NAS;
        matrixForNA = zeros(NofSensors,N);
        [nrow, ncol] = size(matrixForNA);
        nNA = floor(nrow*ncol*n/100);
        msize = numel(matrixForNA);
        rand(n);
        elements = randperm(msize, nNA);
        matrixForNA(elements) = 1 ;
    endif

    [rId, cId] = find(matrixForNA);
    sortrows([rId, cId],1);
        
    tope = max(max(CLU(:,NCL+1)),(NCL+1));
    for index = 2:tope
      index;
      subset = find(CLU(:,NCL+1)==(index-1)).';
      submatrixForNA = matrixForNA(subset,:);

      NAPerSensor = sum(submatrixForNA,2)';
      [row,col] = find(submatrixForNA);
      sr = sortrows([row,col],1);
      NAPositionsWithinSensors = sr(:,2)';

      ck = repelem(locations(subset,:),NAPerSensor,1);
      thesensor = repelem(subset, NAPerSensor);

      cs = [];

      mysize = NofSensors-1;
      mloc = zeros(NofSensors-1,2,length(thesensor));

      v = zeros(1, length(thesensor));
      temp2 = zeros(length(subset), length(thesensor));

      tempReal = zeros(sum(NAPerSensor),1);
      
      ssaux = 1;
      for sens = 1:length(subset)
        for k = 1:NAPerSensor(sens)
          motesWithNAsamePosition = subset(sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1));
          motes = subset(~ismember(subset,motesWithNAsamePosition));
          loc = locations(motes,:);
          v(ssaux) = length(loc(:,1));
          mloc(1:size(loc,1),:,ssaux) = loc;
          temp2(1:length(motes),ssaux) = Rgt(motes(1:length(motes)),NAPositionsWithinSensors(ssaux));
          
          %%%%%%%%%%%%%% Compute real temperature %%%%%%%%%%%%%%%%%%%
          tempReal(ssaux) = Rgt(subset(sens),NAPositionsWithinSensors(ssaux));
          
          ssaux = ssaux+1;
        end
      end
      
      a_temperature = [];
      b_temperature = [];

      %%%%%%%%%%%%%% Compute BME %%%%%%%%%%%%%%%%%%%
      modelCsand = {modeloVar};
      paramsand = {[mean(params(:,1)),mean(params(:,2))]};
      nhmax = neighb;
      nsmax = 0;
      total = nhmax+nsmax;
      dmax = 1000;
      order = 0;

      real_temperature = tempReal;

      tic;
      
      ck;
      mloc;
      v;
      cs;
      temp2;
      a_temperature;
      b_temperature;
      modelCsand;
      paramsand;
      nhmax;
      nsmax;
      dmax;
      order;
      
      predict_temprature_BME6 = BMEintervalMode6(ck,mloc,v,cs,temp2,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
      predFinal = predict_temprature_BME6;
      timeAux(index-1) = toc;

      errorAux(index-1) = sqrt(nanmean((real_temperature - predict_temprature_BME6 ).^2));
      mseAux(index-1) = nanmean((real_temperature - predict_temprature_BME6 ).^2);
      maeAux(index-1) = nanmean(abs(real_temperature - predict_temprature_BME6));
      mapeAux(index-1) = nanmean(abs((real_temperature - predict_temprature_BME6)./real_temperature));
      cvrmseAux(index-1) = sqrt(nanmean((real_temperature - predict_temprature_BME6 ).^2))/sum(real_temperature)*length(real_temperature);

    end

    RMSEhard(indice,NCL) = mean(errorAux);
    MSEhard(indice,NCL) = mean(mseAux);
    MAEhard(indice,NCL) = mean(maeAux);
    MAPEhard(indice,NCL) = mean(mapeAux);
    CVRMSEhard(indice,NCL) = mean(cvrmseAux);
    timehard(indice,NCL) = sum(timeAux);
    indice = indice+1;
  end
  
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
  csvwrite([ruta '/informacionImputacionesHard.csv'], imputaciones);
  csvwrite([ruta '/datosImputadosHard.csv'], Rgt2);
  csvwrite([ruta '/dataWithNA.csv'], DataWhithNA);
  csvwrite([ruta '/predFinalHard.csv'], predFinal);
  if(NAS != 0)
      csvwrite([ruta '/RMSEhardNeighbours.csv'], RMSEhard);
      csvwrite([ruta '/MSEhardNeighbours.csv'], MSEhard);
      csvwrite([ruta '/MAEhardNeighbours.csv'], MAEhard);
      csvwrite([ruta '/MAPEhardNeighbours.csv'], MAPEhard);
      csvwrite([ruta '/CVRMSEhardNeighbours.csv'], CVRMSEhard);
      csvwrite([ruta '/timehardNeighbours.csv'], timehard);
      RMSEhard
      MSEhard
      MAEhard
      MAPEhard
      CVRMSEhard
  else
    predFinal
  endif
  
end

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
