function prediction = hardBME(id, ids, data, locations, varargin)

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

  if nargin == 6
    varmodel = varargin{1};
    varparam = varargin{2};
  elseif nargin == 4
    varmodel = 'gaussianC';
    varparam = [7.4566e-01; 1.1597e-04];  % parametros de variograma obtenidos de Beach
  else
    error("Número de parámetros incorrecto.");
  endif
  
  ck = locations(id, 1:2);   % localizacion id sensor faltante
  mloc = locations(ids, 1:2);  % coordenadas ids sensores sin valor faltante
  v = size(mloc, 1);     % long mloc
  cs = [];
  temp2 = [data; 0.0];   % temperaturas sensores conocidos y el faltante un cero (al final)
  
  a_temperature = [];
  b_temperature = [];
  
  modelCsand = {varmodel};
  paramsand = {[varparam(1,1), varparam(2,1)]};
  nhmax = length(ids);
  nsmax = 0;
  total = nhmax+nsmax;
  dmax = 1000;
  order = 0;

  prediction = BMEintervalMode6(ck,mloc,v,cs,temp2,a_temperature,b_temperature,modelCsand,paramsand,nhmax,nsmax,dmax,order);
  
  
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