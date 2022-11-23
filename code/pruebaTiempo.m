tic
a = [-1.57 -0.9295 -0.0708];
     b = [0.63 1.2705 2.1292];
     c = [2.7746 -0.0757 0.0358; -0.0757 3.5240 1.3156; 0.0358 1.3156 3.7585];
     maxpts = 50000;
     aEps = 0;
     rEps = 1.0000e-04;

csvwrite('./lowerinput.csv', a);
csvwrite('./upperinput.csv', b);
csvwrite('./matrixinput.csv', c);

system('Rscript myscript3.R');

P = csvread('salida.csv');
toc