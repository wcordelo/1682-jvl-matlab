%-----------------------------------------%
%     JVL uSTOL Geometry File Builder     %
%-----------------------------------------%

% Created By: Ronnie Ogden
% Modified By: William Lopez-Cordero
% Function: Given wing and tail parameters
% for aircraft with blown wing, builds
% the respective JVL file. Enter parameters
% under the "Inputs" section.

%%% Assumptions %%%
% - 2D Airfoil --> Super Long Wingspan
% - Constant taper
% - 2 sections: flap and aileron
% - constant jet across span
% - Lengths are all in same unit
% - Aileron starts at 2/3 of half-span
% - Flaps are from mid-span to start of aileron
% - Each propulsor has same radius and even spacing
% - Unblown tail
% - Best for nProp >= 8
%% Inputss

filename = 'subscale.avl'; 
casename = 'Sub-scale_demonstrator'; % file title

% CG Location Relative to LE of wing
Xref = 2.70*0.30;
Yref = 0;
Zref = 0;

% Wing parameters
b           = 31.1*0.30; % span. For jet height
bWing = 300; % for Superlong Wingspan
cmac        = 4.933*0.30; % mean aerodynamic chord
lambda      = 1.0; % taper ratio
nProp       = 8; % total number of propulsors
R           = 1.121*0.30; % propeller radius
sweep       = 0; % 0 -> centerline is perp, -1 -> LE is perp
alphaI      = 0.0; % incidence angle
airfoil     = '2416'; % NACA airfoil
hinge       = 0.65; % location of hinge as fraction of chord
nCPan       = 12; % number of chordwise panels
nBPanMin    = 18; % minimum number of spanwise panels

% Tail parameters
Vh          = 1.226; % horizontal tail volume
Vv          = 0.1; % vertical tail volume
lRat        = 22.55*0.30; % moment arm/wingspan
ARh         = 4.00; % H-tail aspect ratio
ARv         = 1.00; % V-tail aspect ratio
lambdaHT    = 0.8; % H-tail taper ratio
lambdaVT    = 0.65; % V-tail taper ratio
alphaHT     = -1.0; % H-tail incidence angle
zTail       = -0.5*0.30; % tail z-position
% Sweeps
% -1: TE is perpendicular to longitudinal axis
% other: angle of LE sweep in rad
HTsweep     = -1; 
VTsweep     = deg2rad(40);
hingeH      = 0.6; % H-tail hinge location as fraction of chord
hingeV      = 0.6; % V-tail hinge location as fraction of chord

%% Calculations

% Wing geometry 
cr = 1.5*cmac*(lambda+1)/(lambda^2+lambda+1); % root chord
ct = lambda*cr; % tip chord
cmgc = (ct+cr)/2; % mean geometric chord
S = bWing*cmgc; % total wing planform area
h = pi*R^2*(nProp-1)/b; % jet height
yVec = [0; bWing/3; bWing/2]; % spanwise section position
cVec = cr - 2*(cr-ct)/bWing.*yVec; % section chord
xVec = zeros(3,1); % longitudinal section position
if sweep == 0
    xVec = (cr-cVec)/2; % find section x coord
end

% Tail geometry
l = lRat*b; % moment arm
l = 22.55*0.30;
Sh = Vh*S*cmac/l; % H-tail planform area
Sv = Vv*S*b/l; % V-tail planform area
bH = sqrt(ARh*Sh); % H-tail span
bV = sqrt(ARv*Sv); % V-tail span
cBarH = Sh/bH; % H-tail mean geometric chord
cBarV = Sv/bV; % V-tail mean geometric chord
crH = 2*cBarH/(1+lambdaHT); % H-tail root chord
crV = 2*cBarV/(1+lambdaVT); % V-tail root chord
ctH = crH*lambdaHT; % H-tail tip chord
ctV = crV*lambdaVT; % V-tail tip chord
cVecH = [crH; ctH];
cVecV = [crV; ctV];
if HTsweep == -1
    dxHTt = crH-ctH;
else
    dxHTt = bH/2*tan(HTsweep);
end
if VTsweep == -1
    dxVTt = crV-ctV;
else
    dxVTt = bV*tan(VTsweep);
end
xVecH = [Xref+l+crV/3; Xref+l+crV/3+dxHTt];
xVecV = [Xref+l; Xref+l+dxVTt];
yVecH = [0; bH/2];
zVecV = [zTail; zTail+bV];

%% Write JVL file

startSurf = '#==========================================================';
startSec  = '#----------------------------------------------------------';

% Header
fid = fopen(filename,'w');
fprintf(fid,'%s\n%s\n%.2f\n', casename, '#Mach', 0.0);
fprintf(fid,'%s\n%d        %d       %.2f\n', '#IYsym   IZsym   Zsym', 0, 0, 0.0);
fprintf(fid, '%s\n%.2f    %.2f    %.2f\n', '#Sref    Cref    Bref', S, cmac, b);
fprintf(fid, '%s\n%.2f     %.2f    %.2f\n', '#Xref    Yref    Zref', Xref, Yref, Zref);
fprintf(fid, '%c\n%c\n\n', '#', '#');


% Wing
for k=1:2
    % Surface header
    fprintf(fid, '%s\n%s\n%s %d\n', startSurf, 'SURFACE', 'Wing Section', k);
    fprintf(fid, '%s\n', '#Nchordwise  Cspace   Nspanwise   Sspace');
    fprintf(fid, '%d           %.1f      %d           %.1f\n', nCPan, 1.0, ceil((3-k)/3*nBPanMin), 1.0);
    fprintf(fid, '%c\n%s\n%d\n', '#', 'COMPONENT', 1);
    fprintf(fid, '%c\n%s\n%.1f\n', '#', 'YDUPLICATE', 0.0);
    fprintf(fid, '%c\n%s\n%.1f\n', '#', 'ANGLE', alphaI);
    fprintf(fid, '%c\n%s\n%s\n', '#', 'JET', '#Jname  Jgain  SgnDup   hjet   Nchordwise  Cspace');
    fprintf(fid, '%s    %.1f    +%.1f     ', 'CJet', 1.0, 1.0);
    fprintf(fid, '%.2f   %d          %.1f\n\n', h, nCPan, 1.0);
    % Section builder
    for j = 0:1
        fprintf(fid, '%s\n%s\n', startSec, 'SECTION');
        fprintf(fid, '%s\n', '#Xle    Yle     Zle     Chord   Ainc  Nspanwise  Sspace');
        fprintf(fid, '%.2f    %.2f    %.2f    ', xVec(k+j), yVec(k+j), 0.0);
        fprintf(fid, '%.2f    %.1f   %d          %d\n\n', cVec(k+j), 0.0, 0, 0);
        fprintf(fid, '%s\n%s\n\n', 'NACA', airfoil);
        fprintf(fid, '%s\n%s\n', 'CONTROL', '#Cname   Cgain  Xhinge  HingeVec     SgnDup');
        if k == 1
            fprintf(fid, '%s     %.1f    %.2f    ', 'flap', 1.0, hinge);
            fprintf(fid, '%.1f %.1f %.1f  +%.1f\n\n', 0.0, 0.0, 0.0, 1.0);
        else
            fprintf(fid, '%s  %.1f    %.2f    ', 'aileron', 1.0, hinge);
            fprintf(fid, '%.1f %.1f %.1f  -%.1f\n\n', 0.0, 0.0, 0.0, 1.0);            
        end
    end
end
% 
% % Horizontal tail
% % Surface header
% fprintf(fid, '%s\n%s\n%s\n', startSurf, 'SURFACE', 'Horizontal Stabilizer');
% fprintf(fid, '%s\n', '#Nchordwise  Cspace   Nspanwise   Sspace');
% fprintf(fid, '%d            %.1f      %d           %.1f\n', 8, 1.0, 8, 1.0);
% fprintf(fid, '%c\n%s\n%d\n', '#', 'COMPONENT', 2);
% fprintf(fid, '%c\n%s\n%.1f\n', '#', 'YDUPLICATE', 0.0);
% fprintf(fid, '%c\n%s\n%.1f\n\n', '#', 'ANGLE', alphaHT);
% % Section builder
% for k = 1:2
%     fprintf(fid, '%s\n%s\n', startSec, 'SECTION');
%     fprintf(fid, '%s\n', '#Xle    Yle     Zle     Chord   Ainc  Nspanwise  Sspace');
%     fprintf(fid, '%.2f   %.2f   %.2f    ', xVecH(k), yVecH(k), zTail);
%     fprintf(fid, '%.2f    %.1f   %d          %d\n\n', cVecH(k), 0.0, 0, 0);
%     fprintf(fid, '%s\n%s\n', 'CONTROL', '#Cname   Cgain  Xhinge  HingeVec     SgnDup');
%     fprintf(fid, '%s %.1f    %.2f    ', 'elevator', 1.0, hingeH);
%     fprintf(fid, '%.1f %.1f %.1f  +%.1f\n\n', 0.0, 0.0, 0.0, 1.0);
%     fprintf(fid, '%s\n%s\n\n', 'NACA', '0012');
% end
% 
% % Vertical tail
% % Surface header
% fprintf(fid, '%s\n%s\n%s\n', startSurf, 'SURFACE', 'Vertical Stabilizer');
% fprintf(fid, '%s\n', '#Nchordwise  Cspace   Nspanwise   Sspace');
% fprintf(fid, '%d            %.1f      %d           %.1f\n', 8, 1.0, 8, 1.0);
% fprintf(fid, '%c\n%s\n%d\n', '#', 'COMPONENT', 3);
% fprintf(fid, '%c\n%s\n%.1f\n\n', '#', 'ANGLE', 0.0);
% % Section builder
% for k = 1:2
%     fprintf(fid, '%s\n%s\n', startSec, 'SECTION');
%     fprintf(fid, '%s\n', '#Xle    Yle     Zle     Chord   Ainc  Nspanwise  Sspace');
%     fprintf(fid, '%.2f   %.2f   %.2f    ', xVecV(k), 0.0, zVecV(k));
%     fprintf(fid, '%.2f    %.1f   %d          %d\n\n', cVecV(k), 0.0, 0, 0);
%     fprintf(fid, '%s\n%s\n', 'CONTROL', '#Cname   Cgain  Xhinge  HingeVec     SgnDup');
%     fprintf(fid, '%s   %.1f    %.2f    ', 'rudder', 1.0, hingeV);
%     fprintf(fid, '%.1f %.1f %.1f  +%.1f\n\n', 0.0, 0.0, 0.0, 1.0);
%     fprintf(fid, '%s\n%s\n\n', 'NACA', '0012');
% end

fclose(fid);


