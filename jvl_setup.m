
%--------------------------------------------------------------------
% Setup JVL parameter sweeps
%
% Make sure config.avl and config.mass files in same folder
%
% Single flap across span assumed in this version
% 
% Paste below into command window if needed to add JVL to system path
%   path1 = getenv('PATH');
%   path1 = [path1 ':/usr/local/bin']; % change as needed for system
%   setenv('PATH', path1);
%   !echo $PATH
% 
%--------------------------------------------------------------------

% clear
clear all;
clc;

close all;
set(0,'DefaultAxesFontSize',18);
set(0,'DefaultTextFontSize',24);
% clc
% set(0,'DefaultAxesFontSize',18);
% set(0,'DefaultTextFontSize',24);
% set(0,'DefaultAxesFontName','Arial');
% set(0,'DefaultTextFontName','Arial');
% set(0,'DefaultLineLineWidth',3);
% set(0,'DefaultAxesFontWeight','Bold');
% set(0, 'DefaultFigurePosition', [2641 -29 960 984])
% set(0, 'DefaultFigurePosition', [2641 -29 1920 984])

% load ('constants.mat')

% Parameters for evaluation
config  = 'subscale';
% alpha   = 2; % angle of attack sweep range [deg]
% flap    = 20; % flap angle sweep range [deg]
% cjet    = 10; % Delta CJ sweep range [-]
% alpha   = [0 2 4 6 8 10]; % angle of attack sweep range [deg]
% flap    = [0 20 40]; % flap angle sweep range [deg]
% cjet    = [0.34 8.86 20.86 36.07]; % Delta CJ sweep range [-]
% alpha   = [0 1 2 3 4 5 6 7 8 9 10]; % angle of attack sweep range [deg]
% flap    = [0 10 20 30 40 50]; % flap angle sweep range [deg]

alpha   = [-10:1:10]; % deg
flap    = [0:-5:-60]; % deg
aile    = 0; % deg
elev    = 0; % deg
rudd    = 0; % deg
cjet    = [0:1:10]; % Delta CJ sweep range [-]

% Parameter and output arrays setup
swp1    = length(alpha);
swp2    = length(flap);
swp3    = length(cjet);
CJtot   = zeros(swp1,swp2,swp3);
CXtot   = zeros(swp1,swp2,swp3);
CYtot   = zeros(swp1,swp2,swp3);
CZtot   = zeros(swp1,swp2,swp3);
CLtot   = zeros(swp1,swp2,swp3);
CDtot   = zeros(swp1,swp2,swp3);
CLcir   = zeros(swp1,swp2,swp3);
CLjet   = zeros(swp1,swp2,swp3);
CDind   = zeros(swp1,swp2,swp3);
CDjet   = zeros(swp1,swp2,swp3);
CDvis   = zeros(swp1,swp2,swp3);

% Sweep parameters
for i = 1:swp1
    for j = 1:swp2
        for k = 1:swp3
            
            [fileout] = jvl_run(config,alpha(i),flap(j),aile,elev,rudd,cjet(k),i,j,k) % run JVL
            
            fileID = fopen(fileout,'r'); % open output file
            
            for line = 1:26 % extract data at top of output file
                tline = fgetl(fileID);
                if line == 2
                    CJtot(i,j,k) = str2double(tline)
                elseif line == 3
                    CXtot(i,j,k) = str2double(tline)
                elseif line == 4
                    CYtot(i,j,k) = str2double(tline)
                elseif line == 5
                    CZtot(i,j,k) = str2double(tline)
                elseif line == 6
                    CLtot(i,j,k) = str2double(tline)
                elseif line == 7
                    CDtot(i,j,k) = str2double(tline)
                elseif line == 8
                    CLcir(i,j,k) = str2double(tline)
                elseif line == 9
                    CLjet(i,j,k) = str2double(tline)
                elseif line == 10
                    CDind(i,j,k) = str2double(tline)
                elseif line == 11
                    CDjet(i,j,k) = str2double(tline)
                elseif line == 12
                    CDvis(i,j,k) = str2double(tline)
                end
            end
            
            fclose(fileID); % close output file
            
        end
    end
end
