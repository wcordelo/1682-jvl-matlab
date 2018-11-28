
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

% Parameters for evaluation
config  = 'subscale';

alpha   = [0]; % deg
flap    = [0]; % deg
cjet    = [0:0.5:12]; % Delta CJ sweep range [-]
elev    = [-100:0.5:100]; % deg
rudd    = 0; % deg
aile    = 0; % deg

delta = 0.5;

% Parameter and output arrays setup

% 3rd Iteration --> Change only elevator
% swp4    = length(elev);
swp2    = length(elev);
% swp2    = length(flap);
swp1    = length(cjet);

CJet_elev = zeros(swp1,swp2);
CJtot_elev   = zeros(swp1,swp2);
CXtot_elev   = zeros(swp1,swp2);
CYtot_elev   = zeros(swp1,swp2);
CZtot_elev   = zeros(swp1,swp2);
CLtot_elev   = zeros(swp1,swp2);
CDtot_elev   = zeros(swp1,swp2);
CLcir_elev   = zeros(swp1,swp2);
CLjet_elev   = zeros(swp1,swp2);
CDind_elev   = zeros(swp1,swp2);
CDjet_elev   = zeros(swp1,swp2);
CDvis_elev   = zeros(swp1,swp2);
Cmtot_elev = zeros(swp1,swp2);

tCL_elev = zeros(swp1,swp2);
tCD_elev = zeros(swp1,swp2);
tCm_elev = zeros(swp1,swp2);

index = 1;
% Sweep parameters
for i = 1:swp1
    for j = 1:swp2
       
            
            [fileout] = jvl_run2D(config,alpha,flap,aile,elev(j),rudd,cjet(i),i,j) % run JVL
            
            fileID = fopen(fileout,'r'); % open output file
            
            % Obtain first line of file
            tline = fgetl(fileID);
            
            % While Loop that parses entire file and obtains values needed
            while ischar(tline)
                
                % Parse File and Place Value to CLtot
                if strfind(tline, 'CLtot')
                    clExp = 'CLtot =   ';
                    [clMatch,clNoMatch] = regexp(tline,clExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(clNoMatch) == 2 
                        CLtot_elev(i, j) = str2double(clNoMatch{1,2}(1:7));
                    else
                        CLtot_elev(i, j) = str2double(clNoMatch{1,1}(11:18));
                    end
                end
                
                % Parse File and Place Value to CJtot
                if strfind(tline, 'CJtot')
                    cjtotExp = 'CJtot =   ';
                    [cjtotMatch,cjtotNoMatch] = regexp(tline,cjtotExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cjtotNoMatch) == 2 
                        CJtot_elev(i, j) = str2double(cjtotNoMatch{1,2}(1:7));
                    else
                        CJtot_elev(i, j) = str2double(cjtotNoMatch{1,1}(11:18));
                    end
                end
                
                % Parse File and Place Value to CJet
                if strfind(tline, 'CJet')
                    cjetExp = 'CJet            =   ';
                    [cjetMatch,cjetNoMatch] = regexp(tline,cjetExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cjetNoMatch) == 2 
                        CJet_elev(i, j) = str2double(cjetNoMatch{1,2}(1:7));
                    else
                        CJet_elev(i, j) = str2double(cjetNoMatch{1,1}(11:18));
                    end
                end
                
                % Parse File and Place Value to CXtot          
                if strfind(tline, 'CXtot')
                    cxExp = 'CXtot =  ';
                    [cxMatch,cxNoMatch] = regexp(tline,cxExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cxNoMatch) == 2 
                        CXtot_elev(i, j) = str2double(cxNoMatch{1,2}(1:7));
                    else
                        CXtot_elev(i, j) = str2double(cxNoMatch{1,1}(11:18));
                    end
%                     CXtot(i, j, k) = str2double(cxNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CYtot')
                    cyExp = 'CYtot =  ';
                    [cyMatch,cyNoMatch] = regexp(tline,cyExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cyNoMatch) == 2 
                        CYtot_elev(i, j) = str2double(cyNoMatch{1,2}(1:7));
                    else
                        CYtot_elev(i, j) = str2double(cyNoMatch{1,1}(11:18));
                    end
                    
%                     CYtot(i, j, k) = str2double(cyNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CZtot')
                    czExp = 'CZtot =  ';
                    [czMatch,czNoMatch] = regexp(tline,czExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(czNoMatch) == 2 
                        CZtot_elev(i, j) = str2double(czNoMatch{1,2}(1:7));
                    else
                        CZtot_elev(i, j) = str2double(czNoMatch{1,1}(11:18));
                    end
                    
%                     CZtot(i, j, k) = str2double(czNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'Cmtot')
                    cmExp = 'Cmtot =  ';
                    [cmMatch,cmNoMatch] = regexp(tline,cmExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cmNoMatch) == 2 
                        Cmtot_elev(i, j) = str2double(cmNoMatch{1,2}(1:7));
                    else
                        Cmtot_elev(i, j) = str2double(cmNoMatch{1,1}(11:18));
                    end
                    
%                     CZtot(i, j, k) = str2double(czNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CDtot')
                    cdExp = 'CDtot =   ';
                    [cdMatch,cdNoMatch] = regexp(tline,cdExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdNoMatch) == 2
                        CDtot_elev(i, j) = str2double(cdNoMatch{1,2}(1:7));
                    else
                        CDtot_elev(i, j) = str2double(cdNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CLcir')
                    clcirExp = 'CLcir =   ';
                    [clcirMatch,clcirNoMatch] = regexp(tline,clcirExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(clcirNoMatch) == 2
                        CLcir_elev(i, j) = str2double(clcirNoMatch{1,2}(1:7));
                    else
                        CLcir_elev(i, j) = str2double(clcirNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CLjet')
                    cljetExp = 'CLjet =   ';
                    [cljetMatch,cljetNoMatch] = regexp(tline,cljetExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing�
                    if length(cljetNoMatch) == 2
                        CLjet_elev(i, j) = str2double(cljetNoMatch{1,2}(1:7));
                    else
                        CLjet_elev(i, j) = str2double(cljetNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CDind')
                    cdindExp = 'CDind =   ';
                    [cdindMatch,cdindNoMatch] = regexp(tline,cdindExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdindNoMatch) == 2
                        CDind_elev(i, j) = str2double(cdindNoMatch{1,2}(1:7));
                    else
                        CDind_elev(i, j) = str2double(cdindNoMatch{1,1}(11:18));
                    end
                    
%                     CDind(i, j, k) = str2double(cdindNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CDjet')
                    cdjetExp = 'CDjet =   ';
                    [cdjetMatch,cdjetNoMatch] = regexp(tline,cdjetExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdjetNoMatch) == 2
                        CDjet_elev(i, j) = str2double(cdjetNoMatch{1,2}(1:7));
                    else
                        CDjet_elev(i, j) = str2double(cdjetNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CDvis')
                    cdvisExp = 'CDvis =   ';
                    [cdvisMatch,cdvisNoMatch] = regexp(tline,cdvisExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdvisNoMatch) == 2
                        CDvis_elev(i, j) = str2double(cdvisNoMatch{1,2}(1:7));
                    else
                        CDvis_elev(i, j) = str2double(cdvisNoMatch{1,1}(11:18));
                    end
                    
%                     CDvis(i, j, k) = str2double(cdvisNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'Horizontal Stabilizer')
                    cHExp = 'Horizontal Stabilizer';
                    [cHMatch,cHNoMatch] = regexp(tline,cHExp,'match','split');        
                    
                    if index == 1
                        tCL_elev(i, j) = str2double(cHNoMatch{1,1}(13:21));
                        tCD_elev(i, j) = str2double(cHNoMatch{1,1}(21:29));
                        tCm_elev(i, j) = str2double(cHNoMatch{1,1}(30:36));
         
                        index = index + 1; 
            
                    elseif index == 4
                        index = 1;
                    else
                        index = index + 1;
                    end
                end
                               
                tline = fgetl(fileID);
            end
            
            fclose(fileID); % close output file
            
    end
end

save elev_data.mat CJet_elev CJtot_elev CLtot_elev CXtot_elev CYtot_elev CZtot_elev CDtot_elev CJtot_elev Cmtot_elev  tCL_elev tCD_elev tCm_elev 
