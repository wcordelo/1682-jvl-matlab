% carpet plotting

close all
clear all
addpath('CarpetPlots')
jvl_setup_trim
set(0,'DefaultAxesFontSize',18);
set(0,'DefaultTextFontSize',24);

% Contour plot

figure
contourf(flap, cjet, carpetRange,10);
xlabel('Flap deflection')
ylabel('CJ')

% Carpet plot
figure
hold on
h = carpet(flap, cjet, carpetRange, 20);
c3 = contourc(flap,cjet,CLRange);
C3conv = carpetcontourconvert(flap,cjet,carpetRange,20,c3);
k = plotcontours(C3conv);
% clabel(C3conv,k);
ylabel('Elevator deflection')
ylim([-10 30])

figure
h = carpet(flap, cjet, CLRange, 60);
ylabel('Trimmed CL')
% ylim([-10 30])


