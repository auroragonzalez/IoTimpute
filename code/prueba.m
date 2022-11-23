
clear;
clc;

page_screen_output(0);
page_output_immediately(1);

data = dlmread('./tmp/Rgt1.csv');

for k = [100,200,300,400]
  for j = 3:size(data,2)
    for i = 1:size(data,1)
      i
      j
      k
      error = pmf(data,i,j,k)
      if(isnan(error))
        error("Error elevado")
      endif
      disp("---------------");
    end
  end
end