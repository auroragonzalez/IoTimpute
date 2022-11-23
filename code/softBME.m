function prediction = softBME(id, idsH, idsS, dataH, dataS, locations, delta, varargin)

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

  if nargin == 9
    varmodel = varargin{1};
    varparam = varargin{2};
  elseif nargin == 7
    varmodel = 'gaussianC';
    varparam = [7.4566e-01; 1.1597e-04];  % parametros de variograma obtenidos de Beach
  else
    error("Número de parámetros incorrecto.");
  endif
  
  idsH(idsH == id) = [];
  idsS(idsS == id) = [];
    
  ck = locations(id, 1:2);   % localizacion id sensor faltante
  
  mlocH = zeros(size(locations, 1)-1, 2);
  if length(idsH) > 0
    mlocH(1:length(idsH), 1:2) = locations(idsH, 1:2);  % coordenadas ids sensores hard
  endif
  
  vH = length(idsH);
  cs = [];
  
  mlocS = zeros(size(locations, 1)-1, 2);
  if length(idsS) > 0
    mlocS(1:length(idsS), 1:2) = locations(idsS, 1:2);  % coordenadas ids sensores soft
  endif
  
  vS = length(idsS);
  tempH = zeros(size(locations, 1)-1, 1);
  
  if length(dataH) > 0
    tempH(1:length(dataH)) = dataH;
  endif
  
  aux = zeros(size(locations, 1)-1, 1);
  if length(dataS) > 0
    aux(1:length(dataS)) = dataS;
    a_temperature = aux - delta;
    b_temperature = aux + delta;
  endif
  
  modelCsand = {varmodel};
  paramsand = {[varparam(1,1), varparam(2,1)]};
  nhmax = length(idsH);
  nsmax = length(idsS);
  total = nhmax+nsmax;
  dmax = 1000;
  order = 0;
  
  prediction = BMEintervalMode7(ck,mlocH,vH,cs,mlocS,vS,tempH,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
  
    
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