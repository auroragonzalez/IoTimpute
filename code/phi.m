function z = phi(y)

  z = 1/(sqrt(2*pi))*quad("f",-Inf,y);
  
endfunction