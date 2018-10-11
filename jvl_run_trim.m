
function [fileout] = jvl_run_trim(config,alpha,flap,aile,elev,rudd,cjet,i,j,k)
%--------------------------------------------------------------------
% Run JVL with basic inputs
%
% Make sure config.avl and config.mass files in same folder
%
% Test case jvl_run('stoltest',0,20,0,0,0,10,1,1,1)
%
% Paste below into command window if needed to add JVL to system path
%   path1 = getenv('PATH');
%   path1 = [path1 ':/usr/local/bin']; % change as needed for system
%   setenv('PATH', path1);
%   !echo $PATH
%
%--------------------------------------------------------------------

% Delete old input command file if it exists
if (exist('jvlcom.in','file')~= 0)
    unix('rm jvlcom.*');
end

% Create strings for input/output file names and input commands
avlin   = ['LOAD ' config '.avl'];
massin  = ['MASS ' config '.mass'];
% fileout = [config '_' num2str(i) '_' num2str(j) '_' num2str(k) '.txt'];
fileout = [config '.txt'];
Ain     = ['A A ' num2str(alpha)];
D1in    = ['D1 D1 ' num2str(flap)];
D2in    = ['D2 D2 ' num2str(aile)];
%D3in    = ['D3 D3 ' num2str(elev)];
D3in    = ['D3 PM 0'];
D4in    = ['D4 D4 ' num2str(rudd)];
J1in    = ['J1 JJ ' num2str(cjet)];

% Input commands
command{1,:}    = avlin;        % Read configuration input file
command{2,:}	= massin;       % Read mass distribution file
command{3,:}	= 'OPER';       % Compute operating-point run cases
command{4,:}    = Ain;          % Alpha
command{5,:}    = D1in;         % flap
command{6,:}    = D2in;         % aileron
command{7,:}    = D3in;         % elevator
command{8,:}    = D4in;         % rudder
command{9,:}    = J1in;         % CJet
command{10,:}   = 'O';          % Options
command{11,:}   = 'P';          % Print default output for...
command{12,:}   = 'T T F F';	% total, surf, strip, elem
command{13,:}   = ' ';          % Return
command{14,:}   = 'X';          % eXecute run case
command{15,:}   = 'W';          % Write forces to file
command{16,:}   = fileout;      % Enter forces output file
command{17,:}   = ' ';          % Return
command{18,:}   = 'Q';          % Quit JVL

% Write input command file (ASCII-delimited)
for i=1:length(command)
    dlmwrite('jvlcom.in',cell2mat(command(i)),'-append','delimiter','');
end

% Run JVL
unix('jvl/bin/jvl < jvlcom.in');

% Delete input command file
unix('rm jvlcom.*');

return
