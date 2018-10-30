clear all;
clc;
fid = fopen('subtxtfiles1/subscale8202.txt');
% fid1 = fopen('13914112213912312113212570140144140');
tline = fgetl(fid);
% tline1 = fgetl(fid1);
alpha = [];


alpha   = [-10:1:10]; % deg
flap    = [0:-5:-60]; % deg
cjet    = [0:1:10]; % Delta CJ sweep range [-]

alphaSize = size(alpha) 
flapSize = size(flap)
cjetSize = size(cjet)
aeroCat = {'CL', 'CX', 'CJ', 'd_flap', 'alpha'}
% CL should be a 3x3 Matrix of [Size(CJ): Size(d_flap) : Size(alpha)]
% CJ should be a 3x3 Matrix of [Size(CJ): Size(d_flap) : Size(alpha)]

swp1    = length(alpha);
swp2    = length(flap);
swp3    = length(cjet);
CJtot   = zeros(swp3,swp2,swp1);
CXtot   = zeros(swp3,swp2,swp1);
CLtot   = zeros(swp3,swp2,swp1);
Cmtot = zeros(swp3,swp2,swp1);


%% Need to create a 3D array --> Start with 2D Array and extend
% Current implementation includes: 
% A = [1 2 3; 4 5 6; 7 8 9] % Starts with 2D Space
% A(:,:,2) = [10 11 12; 13 14 15; 16 17 18] % Extends to 3D Space
% data.CL(:,2) --> Column
% data.CL(2,:) --> Row

iAlpha = 1;
iCJ = 1;
iflap = 1;
% iCL = 1;
% iCX = 1;

while ischar(tline)
%      if strfind(tline, 'Alpha')
%          alphaExp = 'Alpha =   ';
%          [aMatch,aNoMatch] = regexp(tline,alphaExp,'match','split');
%          alpha(iAlpha) = str2double(aNoMatch{1,2}(1:5));
%          iAlpha = iAlpha + 1;
%      end
    
%     if strfind(tline, 'CJet')
%         cjetExp = 'CJet            =  ';
%         [cjetMatch,cjetNoMatch] = regexp(tline,cjetExp,'match','split');
%         alpha(iCL) = str2double(cjetNoMatch{1,2}(1:5));
%         iCL = iCL + 1;
%     end
    
    if strfind(tline, 'CLtot')
        clExp = 'CLtot =   ';
        [clMatch,clNoMatch] = regexp(tline,clExp,'match','split');        
        clMatch
        clNoMatch
        CLtot(iCJ, iflap, iAlpha) = str2double(clNoMatch{1,2}(1:7));
    end
    
    if strfind(tline, 'CXtot')
        cxExp = 'CXtot =  ';
        [cxMatch,cxNoMatch] = regexp(tline,cxExp,'match','split');
        CXtot(iCJ, iflap, iAlpha) = str2double(cxNoMatch{1,2}(1:7));
    end
    
    if strfind(tline, 'Cmtot')
        cmExp = 'Cmtot =  ';
        [cmMatch,cmNoMatch] = regexp(tline,cmExp,'match','split');
        cmMatch
        cmNoMatch
        Cmtot(iCJ, iflap, iAlpha) = str2double(cmNoMatch{1,2}(1:7));
    end
    
    tline = fgetl(fid);
 
end
fclose(fid);

%% Example of parsing xml file
fid = fopen('c172p.xml');
tline = fgetl(fid);
alpha = [];

iAlpha = 1; 
while ischar(tline)
%     disp(tline)
    filestart = false;
    if strfind(tline, 'Change_in_CX_due_to_blowing_and_flaps')
        disp(tline);
        filestart = true;
%         while filestrart !== false
            
%         end
    end
%     if strfind(tline, 'Alpha')
%         alphaExp = 'Alpha =   ';
%         [aMatch,aNoMatch] = regexp(tline,alphaExp,'match','split')
%         alpha(iAlpha) = str2double(aNoMatch{1,2}(1:5))
%         iAlpha = iAlpha + 1;
%     end
    
    tline = fgetl(fid);
 
end
fclose(fid);

% c172xml = xml2struct('c172p.xml')
% testxml = xml2struct('test.xml') --> Doesn't Work.

% 1. read an xml file and store it into z :
% z = xmltools('c172p.xml')

% 2. write z into the file :
% xmltools(z,'test1.xml')


% 3. returns only subset of z child which name is tag-name :
% xmltools(z,'get','tag-name')

% and now:

% 4. returns only subset of z child which attrib is attrib-name :
% xmltools(z,'get-attrib', 'attrib-name')

% 5. returns only attribs/value or children of subset of z child which name is tag-name :
% xmltools(z,'get','tag-name', 'attribs'|'value'|'child');
