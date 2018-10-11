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

dir

% str = 'Alpha =   9.00000     pb/2V =  -0.00000 ';
% expression = 'Alpha =   ';
% [match,noMatch] = regexp(str,expression,'match','split')
% % 
% fid=fopen('1171191001171019911010348118122118');
% tline = fgetl(fid);
% tlines = cell(0,1);
% while ischar(tline)
%     tlines{end+1,1} = tline;
%     tline = fgetl(fid);
% end
% fclose(fid);
% % Find the tlines with equations on them
% eqnLines = regexp(tlines,'.*->.*','match','once')
% eqnLineMask = ~cellfun(@isempty, eqnLines)
% % Convert the non equation lines to the second numeric value
% for i = find(~eqnLineMask)'
%     bothNumbers = str2num(tlines{i});
%     tlines{i} = bothNumbers(2);
% end
% % Make blocks with the equation in the first cell
% blocks = cell(nnz(eqnLineMask),2);
% blocks(:,1) = tlines(eqnLineMask);
% % And the numbers in an array in the second cell
% eqnLineNos = [find(eqnLineMask); length(tlines)+1];
% for i = 1:length(eqnLineNos)-1
%     inds = eqnLineNos(i)+1 : eqnLineNos(i+1)-1;
%     blocks{i,2} = cell2mat(tlines(inds));
% end