
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

alpha   = [0, 6.5, 9.7, 14.4, 22.8]; % deg
flap    = [0]; % deg
cjet    = [0:0.5:6]; % Delta CJ sweep range [-]
elev    = [0]; % deg
rudd    = 0; % deg
aile    = 0; % deg

% Parameter and output arrays setup
% swp4    = length(elev);
swp3    = length(alpha);
swp2    = length(flap);
swp1    = length(cjet);
% CJet = zeros(swp1,swp2,swp3,swp4);
% CJtot   = zeros(swp1,swp2,swp3,swp4);
% CXtot   = zeros(swp1,swp2,swp3,swp4);
% CYtot   = zeros(swp1,swp2,swp3,swp4);
% CZtot   = zeros(swp1,swp2,swp3,swp4);
% CLtot   = zeros(swp1,swp2,swp3,swp4);
% CDtot   = zeros(swp1,swp2,swp3,swp4);
% CLcir   = zeros(swp1,swp2,swp3,swp4);
% CLjet   = zeros(swp1,swp2,swp3,swp4);
% CDind   = zeros(swp1,swp2,swp3,swp4);
% CDjet   = zeros(swp1,swp2,swp3,swp4);
% CDvis   = zeros(swp1,swp2,swp3,swp4);
% Cmtot = zeros(swp1,swp2,swp3,swp4);

% tCL = zeros(swp1,swp2,swp3,swp4);
% tCD = zeros(swp1,swp2,swp3,swp4);
% tCm = zeros(swp1,swp2,swp3,swp4);

index = 1;
% Sweep parameters
for i = 1:swp1
    for j = 24:swp2
        for k = 40:swp3
%             for m = 118:swp4
            
            [fileout] = jvl_run(config,alpha(k),flap(j),aile,elev,rudd,cjet(i),i,j,k,m) % run JVL
            
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
                        CLtot(i, j, k) = str2double(clNoMatch{1,2}(1:7));
                    else
                        CLtot(i, j, k) = str2double(clNoMatch{1,1}(11:18));
                    end
                end
                
                % Parse File and Place Value to CJtot
                if strfind(tline, 'CJtot')
                    cjtotExp = 'CJtot =   ';
                    [cjtotMatch,cjtotNoMatch] = regexp(tline,cjtotExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cjtotNoMatch) == 2 
                        CJtot(i, j, k) = str2double(cjtotNoMatch{1,2}(1:7));
                    else
                        CJtot(i, j, k) = str2double(cjtotNoMatch{1,1}(11:18));
                    end
                end
                
                % Parse File and Place Value to CJet
                if strfind(tline, 'CJet')
                    cjetExp = 'CJet            =   ';
                    [cjetMatch,cjetNoMatch] = regexp(tline,cjetExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cjetNoMatch) == 2 
                        CJet(i, j, k) = str2double(cjetNoMatch{1,2}(1:7));
                    else
                        CJet(i, j, k) = str2double(cjetNoMatch{1,1}(11:18));
                    end
                end
                
                % Parse File and Place Value to CXtot          
                if strfind(tline, 'CXtot')
                    cxExp = 'CXtot =  ';
                    [cxMatch,cxNoMatch] = regexp(tline,cxExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cxNoMatch) == 2 
                        CXtot(i, j, k) = str2double(cxNoMatch{1,2}(1:7));
                    else
                        CXtot(i, j, k) = str2double(cxNoMatch{1,1}(11:18));
                    end
%                     CXtot(i, j, k) = str2double(cxNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CYtot')
                    cyExp = 'CYtot =  ';
                    [cyMatch,cyNoMatch] = regexp(tline,cyExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cyNoMatch) == 2 
                        CYtot(i, j, k) = str2double(cyNoMatch{1,2}(1:7));
                    else
                        CYtot(i, j, k) = str2double(cyNoMatch{1,1}(11:18));
                    end
                    
%                     CYtot(i, j, k) = str2double(cyNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CZtot')
                    czExp = 'CZtot =  ';
                    [czMatch,czNoMatch] = regexp(tline,czExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(czNoMatch) == 2 
                        CZtot(i, j, k) = str2double(czNoMatch{1,2}(1:7));
                    else
                        CZtot(i, j, k) = str2double(czNoMatch{1,1}(11:18));
                    end
                    
%                     CZtot(i, j, k) = str2double(czNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'Cmtot')
                    cmExp = 'Cmtot =  ';
                    [cmMatch,cmNoMatch] = regexp(tline,cmExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cmNoMatch) == 2 
                        Cmtot(i, j, k) = str2double(cmNoMatch{1,2}(1:7));
                    else
                        Cmtot(i, j, k) = str2double(cmNoMatch{1,1}(11:18));
                    end
                    
%                     CZtot(i, j, k) = str2double(czNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CDtot')
                    cdExp = 'CDtot =   ';
                    [cdMatch,cdNoMatch] = regexp(tline,cdExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdNoMatch) == 2
                        CDtot(i, j, k) = str2double(cdNoMatch{1,2}(1:7));
                    else
                        CDtot(i, j, k) = str2double(cdNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CLcir')
                    clcirExp = 'CLcir =   ';
                    [clcirMatch,clcirNoMatch] = regexp(tline,clcirExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(clcirNoMatch) == 2
                        CLcir(i, j, k) = str2double(clcirNoMatch{1,2}(1:7));
                    else
                        CLcir(i, j, k) = str2double(clcirNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CLjet')
                    cljetExp = 'CLjet =   ';
                    [cljetMatch,cljetNoMatch] = regexp(tline,cljetExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsingﬂ
                    if length(cljetNoMatch) == 2
                        CLjet(i, j, k) = str2double(cljetNoMatch{1,2}(1:7));
                    else
                        CLjet(i, j, k) = str2double(cljetNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CDind')
                    cdindExp = 'CDind =   ';
                    [cdindMatch,cdindNoMatch] = regexp(tline,cdindExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdindNoMatch) == 2
                        CDind(i, j, k) = str2double(cdindNoMatch{1,2}(1:7));
                    else
                        CDind(i, j, k) = str2double(cdindNoMatch{1,1}(11:18));
                    end
                    
%                     CDind(i, j, k) = str2double(cdindNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'CDjet')
                    cdjetExp = 'CDjet =   ';
                    [cdjetMatch,cdjetNoMatch] = regexp(tline,cdjetExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdjetNoMatch) == 2
                        CDjet(i, j, k) = str2double(cdjetNoMatch{1,2}(1:7));
                    else
                        CDjet(i, j, k) = str2double(cdjetNoMatch{1,1}(11:18));
                    end
                    
                end
                
                if strfind(tline, 'CDvis')
                    cdvisExp = 'CDvis =   ';
                    [cdvisMatch,cdvisNoMatch] = regexp(tline,cdvisExp,'match','split');
                    
                    % Needed to account for discrepancy in file parsing
                    if length(cdvisNoMatch) == 2
                        CDvis(i, j, k) = str2double(cdvisNoMatch{1,2}(1:7));
                    else
                        CDvis(i, j, k) = str2double(cdvisNoMatch{1,1}(11:18));
                    end
                    
%                     CDvis(i, j, k) = str2double(cdvisNoMatch{1,2}(1:7));
                end
                
                if strfind(tline, 'Horizontal Stabilizer')
                    cHExp = 'Horizontal Stabilizer';
                    [cHMatch,cHNoMatch] = regexp(tline,cHExp,'match','split');        
                    
                    if index == 1
                        tCL(i, j, k) = str2double(cHNoMatch{1,1}(13:21));
                        tCD(i, j, k) = str2double(cHNoMatch{1,1}(21:29));
                        tCm(i, j, k) = str2double(cHNoMatch{1,1}(30:36));
         
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
end

save polardata3.mat CLtot CXtot CJtot Cmtot flap alpha tCL tCD tCm
