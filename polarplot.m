%% Polar Plot Graphing Mechanism

% ToDo: Go through files with specific run configurations
% Obtain the useful information needed to obtain the CX and CL values
% Iterate through all the files for each use case. 
% For specific alpha, obtain the CX and CL values, CJ value should be fixed
% for all cases.ß
% Plot this on the drag polar plot using MATLAB.


% For now we do this by hand...
% Subscale Parameter (30% Scale): 
%  Sref =  13.810       Cref =  1.4800       Bref =  9.3300    
%  Xref = 0.81000       Yref =  0.0000       Zref =  0.0000   
%   flap            =   0.00000
%   aileron         =   0.00000
%   elevator        =   0.00000
%   rudder          =   0.00000

CLtot = [];
CXtot = [];
CJ = [];
alpha = [];

CLtot(1) =   0.15066;
CXtot(1) =   -0.00176; 
CJ(1) = -0.34000;
alpha(1)  = 0;

CLtot(2) = 0.23857;
CXtot(2) = 0.0067;
CJ(2) =  -0.34000;
alpha(2) = 1;

CLtot(3) = 0.32627;
CXtot(3) = 0.00534;
CJ(3) = -0.34000;
alpha(3) = 2;

CLtot(4) = 0.41376;
CXtot(4) = 0.01224;
CJ(4) = -0.34000;
alpha(4) = 3;

CLtot(5) = 0.50094;
CXtot(5) = 0.02136;
CJ(5) = -0.34000;
alpha(5) = 4;

CLtot(6) = 0.58776;
CXtot(6) = 0.03269;
CJ(6) = -0.34000;
alpha(6) = 5;

CLtot(7) = 0.67413;
CXtot(7) = 0.04622;
CJ(7) = -0.34000;
alpha(7) = 6;

CLtot(8) = .76000;
CXtot(8) = 0.06193;
CJ(8) = -0.34000;
alpha(8) = 7;

CLtot(9) = 0.84530;
CXtot(9) = 0.07980;
CJ(9) = -0.34000;
alpha(9) = 8;

CLtot(9) = 0.84530;
CXtot(9) = 0.07980;
CJ(9) = -0.34000;
alpha(9) = 8;

CLtot(10) = 0.84530;
CXtot(10) = 0.07980;
CJ(10) = -0.34000;
alpha(10) = 9;

% Display results from JVL 
CLtot
CDtot
alpha

