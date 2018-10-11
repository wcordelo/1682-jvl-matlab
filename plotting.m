clear all
close all
set(0,'defaulttextinterpreter','latex')

cm_full_5 = [0.0471, 0.2150, 0.4064, 0.6340, 0.9005, 1.2018];
cm_full_10 = [-0.0818, -0.1245, -0.1201, -0.0433, 0.1112, 0.3353];
cm_sub_5 = [0.0745, 0.2952, 0.5392, 0.8190, 1.1373, 1.490];
cm_sub_10 = [-0.0514, -0.0348, 0.0280, 0.1625, 0.3738, 0.6539];

flap = [0,10, 20, 30, 40, 50];

figure
hold on
plot(flap, cm_full_5, 'b*-')
plot(flap, cm_full_10, 'bo-')
plot(flap, cm_sub_5, 'r*-')
plot(flap, cm_sub_10, 'ro-')

xlabel('Flap deflection (deg)')
ylabel('$C_m$')
legend('CJ = 5, full scale','CJ = 10, full scale','CJ = 5, sub scale','CJ = 10, sub scale')


%% CJ Sweep
cm_sub = [0.1363, 0.1420, 0.1329, 0.1165, 0.0966, 0.0745, 0.0510, 0.0264, 0.0010, -0.0250, -0.0514];
cm_full = [0.1337, 0.1197, 0.1085, 0.0908, 0.0698, 0.0471, 0.0228, -0.0024, -0.0283, -0.0548, -0.0818];

CJ = [0,1,2,3,4,5,6,7,8,9,10];

figure
hold on
plot(CJ, cm_sub, 'b*-')
plot(CJ, cm_full, 'r*-')

xlabel('CJ')
ylabel('$C_m$')
legend('Sub-scale','Full-scale')