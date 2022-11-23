
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

ruta = './tmp';

predFinal = [];

R = importdata([ruta '/R.csv']);
Rgt = dlmread([ruta '/Rgt.csv']);
CLU = dlmread([ruta '/groupsk.txt']);
    
NofSensors = size(R,1);
ncl = size(CLU,2)-1;

locations = dlmread([ruta '/locations.txt']);
locations = locations(:,2:3);
params = dlmread([ruta '/parametrosVariograma.csv']);
modeloVar = textread([ruta '/modeloVariograma.txt'], '%s');

nei = importdata([ruta '/nei.txt']);

%% Variables nuevas
neiMin = nei;
neiMax = nei;

hard = importdata([ruta '/hard.csv']);
soft = importdata([ruta '/soft.csv']);
delta = importdata([ruta '/delta.csv']);

N = importdata([ruta '/N.txt'])
NAS = importdata([ruta '/NAS.txt'])
R = R(:,1:N);
Rgt = Rgt(:,1:N);
MAPE = zeros(length(NAS),1);
MAE = zeros(length(NAS),1);
RMSE = zeros(length(NAS),1);
MSE = zeros(length(NAS),1);
CVRMSE = zeros(length(NAS),1);
MSEsoft = zeros(length(NAS),ncl);
RMSEsoft = zeros(length(NAS),ncl);
MAEsoft = zeros(length(NAS),ncl);
MAPEsoft = zeros(length(NAS),ncl);
CVRMSEsoft = zeros(length(NAS),ncl);
timesoft= zeros(length(NAS),ncl);

Rgt2 = R(:,1:N);
imputaciones = zeros(length(NAS),4);
DataWhithNA = Rgt;
    
for NCL = 1:ncl
  
  NCL;
  errorAux = zeros(NCL,1);
  mseAux = zeros(NCL,1);
  maeAux = zeros(NCL,1);
  mapeAux = zeros(NCL,1);
  cvrmseAux = zeros(NCL,1);
  timeAux = zeros(NCL,1);
  errorAuxReal = zeros(NCL,1);

  indice = 1;
  for neighb = neiMin:neiMax
    
    neighb;
    
    if(NAS == 0)
        matrixForNA = isna(R);
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

      subset(ismember(subset, soft));

      ck = repelem(locations(subset,:),NAPerSensor,1);
      thesensor = repelem(subset, NAPerSensor);

      cs = [];
      subset(ismember(subset, soft));

      mysize = NofSensors-1;
      mloc = zeros(NofSensors-1,2,length(thesensor));

      v = zeros(1, length(thesensor));
      temp2 = zeros(length(subset), length(thesensor));

      mlocH = zeros(NofSensors-1,2,length(thesensor));
      mlocS = zeros(NofSensors-1,2,length(thesensor));
      vH = zeros(1, length(thesensor));
      vS = zeros(1, length(thesensor));
      tempS = zeros(mysize, length(thesensor));
      tempH = zeros(mysize, length(thesensor));
      
      tempReal = zeros(sum(NAPerSensor),1);
      tempRealReal = zeros(sum(NAPerSensor),1);
      
      ssaux = 1;
      for sens = 1:length(subset)
        for k = 1:NAPerSensor(sens)
          motesWithNAsamePosition = subset(sr(find(sr(:,2)==NAPositionsWithinSensors(ssaux)),1));
          motes = subset(~ismember(subset,motesWithNAsamePosition));
          motesSoft = motes(ismember(motes, soft));
          motesHard = motes(ismember(motes, hard));
          % AURO 12/05  vectorizacion (he borrado dos bucles for aqui y he puesto los indices dentro ademas de eliminar el eval)
          tempS(1:length(motesSoft),ssaux) = R(motes(1:length(motesSoft)),(NAPositionsWithinSensors(ssaux)));
          tempH(1:length(motesHard),ssaux) = R(motes(1:length(motesHard)),(NAPositionsWithinSensors(ssaux)));
          % params hard
          locH = locations(motesHard,:);  
          vH(ssaux) = size(locH,1);
          mlocH(1:size(locH,1),:,ssaux) = locH;

          % params soft
          locS = locations(motesSoft,:); 
          vS(ssaux) = size(locS,1);
          mlocS(1:size(locS,1),:,ssaux) = locS;
          
          %%%%%%%%%%%%%% Compute real temperature %%%%%%%%%%%%%%%%%%%
          tempReal(ssaux) = R(subset(sens),NAPositionsWithinSensors(ssaux));
          tempRealReal(ssaux) = Rgt(subset(sens), NAPositionsWithinSensors(ssaux));
          
          ssaux = ssaux+1;
        end
      end

      a_temperature = tempS-delta;
      b_temperature = tempS+delta;

      %%%%%%%%%%%%%% Compute BME %%%%%%%%%%%%%%%%%%%
      modelCsand = {modeloVar};
      paramsand = {[mean(params(:,1)),mean(params(:,2))]};
      nhmax = 3;
      nsmax = neighb;
      total = nhmax+nsmax;
      dmax = 1000;
      order = 0;

      real_temperature = tempReal;
      realreal_temperature = tempRealReal;

      tic;
      
      ck;
      mlocH;
      vH;
      cs;
      mlocS;
      vS;
      tempH;
      a_temperature;
      b_temperature;
            
      
      predict_temprature_BME6 = BMEintervalMode7(ck,mlocH,vH,cs,mlocS,vS,tempH,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
      predFinal = predict_temprature_BME6;
      timeAux(index-1) = toc;

      errorAux(index-1) = sqrt(nanmean((real_temperature - predict_temprature_BME6 ).^2));
      mseAux(index-1) = nanmean((real_temperature - predict_temprature_BME6 ).^2);
      maeAux(index-1) = nanmean(abs(real_temperature - predict_temprature_BME6));
      mapeAux(index-1) = nanmean(abs((real_temperature - predict_temprature_BME6)./real_temperature));
      cvrmseAux(index-1) = sqrt(nanmean((real_temperature - predict_temprature_BME6 ).^2))/sum(real_temperature)*length(real_temperature);
      
      errorAuxReal(index-1) = sqrt(nanmean((realreal_temperature - predict_temprature_BME6 ).^2));

    end

    RMSEsoft(indice,NCL) = mean(errorAux);
    MSEsoft(indice,NCL) = mean(mseAux);
    MAEsoft(indice,NCL) = mean(maeAux);
    MAPEsoft(indice,NCL) = mean(mapeAux);
    CVRMSEsoft(indice,NCL) = mean(cvrmseAux);
    timesoft(indice,NCL) = sum(timeAux);
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
  csvwrite([ruta '/informacionImputacionesSoft.csv'], imputaciones);
  csvwrite([ruta '/datosImputadosSoft.csv'], Rgt2);
  csvwrite([ruta '/dataWithNA.csv'], DataWhithNA);  
  csvwrite([ruta '/predFinalSoft.csv'], predFinal);
  if(NAS != 0)
      csvwrite([ruta '/RMSEsoftNeighbours.csv'], RMSEsoft);
      csvwrite([ruta '/MSEsoftNeighbours.csv'], MSEsoft);
      csvwrite([ruta '/MAEsoftNeighbours.csv'], MAEsoft);
      csvwrite([ruta '/MAPEsoftNeighbours.csv'], MAPEsoft);
      csvwrite([ruta '/CVRMSEsoftNeighbours.csv'], CVRMSEsoft);
      csvwrite([ruta '/timesoftNeighbours.csv'], timesoft);
      RMSEsoft
      MSEsoft
      MAEsoft
      MAPEsoft
      CVRMSEsoft
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
