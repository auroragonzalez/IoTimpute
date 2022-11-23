function z = phi_opti(y)

  z = 1/(sqrt(2*pi))*arrayfun(@quad,"f",-Inf,y);
  
endfunction