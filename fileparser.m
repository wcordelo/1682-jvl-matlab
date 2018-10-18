clear all;
clc;
fid = fopen('1171191001171019911010348118122118');
tline = fgetl(fid);
alpha = [];

iAlpha = 1;
while ischar(tline)
    if strfind(tline, 'Alpha')
        alphaExp = 'Alpha =   ';
        [aMatch,aNoMatch] = regexp(tline,alphaExp,'match','split');
        alpha(iAlpha) = str2double(aNoMatch{1,2}(1:5));
        iAlpha = iAlpha + 1;
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
z = xmltools('c172p.xml')

% 2. write z into the file :
% xmltools(z,'test.xml')

% 3. returns only subset of z child which name is tag-name :
% xmltools(z,'get','tag-name')

% and now:

% 4. returns only subset of z child which attrib is attrib-name :
% xmltools(z,'get-attrib', 'attrib-name')

% 5. returns only attribs/value or children of subset of z child which name is tag-name :
% xmltools(z,'get','tag-name', 'attribs'|'value'|'child');
