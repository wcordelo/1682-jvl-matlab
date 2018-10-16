clear all;
clc;
fid = fopen('1171191001171019911010348118122118');
tline = fgetl(fid);
alpha = [];

iAlpha = 1 
while ischar(tline)
    if strfind(tline, 'Alpha')
        alphaExp = 'Alpha =   ';
        [aMatch,aNoMatch] = regexp(tline,alphaExp,'match','split')
        alpha(iAlpha) = str2double(aNoMatch{1,2}(1:5))
        iAlpha = iAlpha + 1;
    end
    
    tline = fgetl(fid);
 
end
fclose(fid);

%% Example of parsing xml file
fid = fopen('c172p.xml');
tline = fgetl(fid);
alpha = [];

iAlpha = 1 
while ischar(tline)
    disp(tline)
    if strfind(tt)
%     if strfind(tline, 'Alpha')
%         alphaExp = 'Alpha =   ';
%         [aMatch,aNoMatch] = regexp(tline,alphaExp,'match','split')
%         alpha(iAlpha) = str2double(aNoMatch{1,2}(1:5))
%         iAlpha = iAlpha + 1;
%     end
    
    tline = fgetl(fid);
 
end
fclose(fid);