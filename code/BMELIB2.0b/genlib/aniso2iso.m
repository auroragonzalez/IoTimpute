function [ciso]=aniso2iso(c,angle,ratio);% aniso2iso                 - convert anisotropic to isotropic coordinates (Jan 1,2001)%% Transform a set of two dimensional or three dimensional coordinates% using rotations and dilatations of the axes, in order to map an% anisotropic space into an isotropic one. The geometric anisotropy% is characterized by the angle(s) of the principal axis of the ellipse% (in 2D) or ellipsoid (in 3D), and by the ratio(s) of the principal axis% length by the other axes lengths. Using this function, an ellipse% (ellipsoid) is thus mapped into a circle (sphere) having as radius the% length of the principal axis. The transformation consist in a rotation% of the axes followed by a dilatation. % % SYNTAX :%% [ciso]=aniso2iso(c,angle,ratio); %% INPUT :%% c       n by d   matrix of coordinates for the locations in the anisotropic%                  space. A line corresponds to the vector of coordinates at a%                  location, so that the number of columns corresponds to the%                  dimension of the space. Only two dimensional or three dimensional%                  space coordinates can be processed by this function.% angle   1 by d-1 vector of angle values that characterize the anisotropy. %                  In a two dimensional space, angle is the trigonometric angle%                  between the horizontal axis and the principal axis of the%                  ellipse. In a three dimensional space, spherical coordinates%                  are used, such that angle(1) is the horizontal trigonometric%                  angle and angle(2) is the vertical trigonometric angle for the%                  principal axis of the ellipsoid. All the angles are measured%                  counterclockwise in degrees and are between -90� and 90�.% ratio   1 by d-1 vector that characterize the ratio for the length of the axes%                  for the ellipse (in 2D) or ellipsoid (in 3D). In a two dimensional%                  space, ratio is the length of the principal axis of the ellipse%                  divided by the length of the secondary axis, so that ratio>1. In a%                  three dimensional space, ratio(1) is the length of the principal%                  axis of the ellipsoid divided by the length of the second axis, %                  whereas ratio(2) is length of the principal axis of the ellipsoid%                  divided by the length of the third axis, so that ratio(1)>1 and%                  ratio(2)>1.%% OUTPUT :%% ciso    n by d   matrix having the same size as c, that gives the new coordinates%                  in the isotropic space.%% NOTE :%% It is possible to specify an additional index vector, taking integer values from 1% to nv. The values in index specify which one of the nv variable is known at each one% of the corresponding coordinates. The c matrix of coordinates and the index vector% are then grouped together using the MATLAB cell array notation, so that c={c,index}.% This allows to perform the same coordinate transformation at once on a set of possibly% different variables. The output variable ciso is then also a cell array that contains% both the new matrix of coordinates and the index vector.%%%%%% Determine the dimension of the space and set angleisindex=iscell(c);if isindex==1,  d=size(c{1},2);else  d=size(c,2);end;angle=angle*2*pi/360;angle=-angle;%%%%%% When d<2 or d>3, errorif (d<2)||(d>3),  error('aniso2iso.m requires coordinates in a 2D or 3D space');end;%%%%%% Case for d=2if d==2,  R=[ cos(angle)  sin(angle)      -sin(angle)  cos(angle)];  if isindex==1,    ciso=c;    ciso{1}=c{1}*R;    ciso{1}(:,2)=ciso{1}(:,2)*ratio;  else    ciso=c*R;    ciso(:,2)=ciso(:,2)*ratio;  end;end;%%%%%% Case for d=3if d==3;  phi=angle(1);  teta=angle(2);  ratioy=ratio(1);  ratioz=ratio(2);  R1=[ cos(phi)   sin(phi)  0       -sin(phi)   cos(phi)  0       0          0         1];  R2=[ cos(teta)  0  sin(teta)       0          1  0      -sin(teta)  0  cos(teta)];  R=R1*R2;    if isindex==1,    ciso=c;    ciso{1}=c{1}*R;    ciso{1}(:,2)=ciso{1}(:,2)*ratioy;    ciso{1}(:,3)=ciso{1}(:,3)*ratioz;  else    ciso=c*R;    ciso(:,2)=ciso(:,2)*ratioy;    ciso(:,3)=ciso(:,3)*ratioz;  end;end;