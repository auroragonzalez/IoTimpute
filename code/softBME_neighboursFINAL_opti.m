
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

R = dlmread([ruta '/R.csv']);
Rgt = dlmread([ruta '/Rgt.csv']);
CLU = dlmread([ruta '/groupsk.txt']);
%CLU = dlmread([ruta '/groupsk.txt'],' ',0,1)
    
NofSensors = size(R,1);
ncl = size(CLU,2)-1;

locations = dlmread([ruta '/locations.txt']);
locations = locations(:,2:3);
params = dlmread([ruta '/parametrosVariograma.csv']);
modeloVar = textread([ruta '/modeloVariograma.txt'], '%s');

nei = importdata([ruta '/nei.txt'])

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
RMSE = zeros(length(NAS),1);
CVRMSE = zeros(length(NAS),1);
RMSEsoft = zeros(length(NAS),ncl);
timesoft= zeros(length(NAS),ncl);

nodesR = 1:NofSensors;

 % AURO 12/05  vectorizacion (esto no hace falta, en vez de node1, node2... utilizaremos directamente R)   
%for m = 1:NofSensors
%  eval(['node' num2str(m) '= R(m,:);']);
%end
    
for NCL = 1:ncl
  
  NCL
  errorAux = zeros(NCL,1);
  timeAux = zeros(NCL,1);
  errorAuxReal = zeros(NCL,1);

  indice = 1;
  for neighb = neiMin:neiMax
    
    neighb
    n = NAS;
    matrixForNA = zeros(NofSensors,N);
    [nrow, ncol] = size(matrixForNA);
    nNA = floor(nrow*ncol*n/100);
    msize = numel(matrixForNA);
    rand(n);
    elements = randperm(msize, nNA);
    matrixForNA(elements) = 1;
    [rId, cId] = find(matrixForNA);
    sortrows([rId, cId],1);

    for index = 2:(NCL+1)
      index;
      subset = CLU(find(CLU(:,NCL+1)==(index-1)),1).';
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

          ssaux = ssaux+1;
        end
      end

      %%%%%%%%%%%%%% Compute real temperature %%%%%%%%%%%%%%%%%%%
      tempReal = zeros(sum(NAPerSensor),1);
      ssaux = 1;
      for sens = 1:length(subset)
        for k = 1:NAPerSensor(sens)
          tempReal(ssaux) = R(subset(sens),NAPositionsWithinSensors(ssaux)); %eval(['node' num2str(subset(sens)) '(NAPositionsWithinSensors(ssaux));']);
          ssaux = ssaux+1;
        end
      end

      % AURO 12/05  vectorizacion (esto no hace falta, en vez de nodeReal1, nodeReal2... 
      %utilizaremos directamente Rgt)   

     % for m = 1:NofSensors
     %   eval(['nodeReal' num2str(m) '= Rgt(m,:);']);
     % end

      tempRealReal = zeros(sum(NAPerSensor),1);
                
      ssaux = 1;
      for sens = 1:length(subset)
        for k = 1:NAPerSensor(sens)
          tempRealReal(ssaux) = Rgt(subset(sens), NAPositionsWithinSensors(ssaux)); %eval(['nodeReal' num2str(subset(sens)) '(NAPositionsWithinSensors(ssaux));']);
          ssaux = ssaux+1;
        end
      end

      a_temperature = tempS-delta;
      b_temperature = tempS+delta;

      %%%%%%%%%%%%%% Compute BME %%%%%%%%%%%%%%%%%%%
      modelCsand = {modeloVar};                                                  ############
      paramsand = {[mean(params(:,1)),mean(params(:,2))]};
      nhmax = 3;
      nsmax = neighb;
      total = nhmax+nsmax;
      dmax = 1000;
      order = 0;

      real_temperature = tempReal;
      realreal_temperature = tempRealReal;

      tic;
      predict_temprature_BME6 = BMEintervalMode7(ck,mlocH,vH,cs,mlocS,vS,tempH,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
      timeAux(index-1) = toc;

      errorAux(index-1) = sqrt(nanmean((real_temperature - predict_temprature_BME6 ).^2));
      errorAuxReal(index-1) = sqrt(nanmean((realreal_temperature - predict_temprature_BME6 ).^2));

    end

    RMSEsoft(indice,NCL) = mean(errorAux);
    timesoft(indice,NCL) = sum(timeAux);
    indice = indice+1;

  end

  RMSEsoft
  
  csvwrite([ruta '/RMSEsoftNeighbours.csv'], RMSEsoft);
  csvwrite([ruta '/timesoftNeighbours.csv'], timesoft);

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