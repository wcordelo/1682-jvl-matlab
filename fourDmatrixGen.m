
%% 4D Matrix Generator
% Using 3 3D Matrices from JVL Runs

load('jet_data.mat')
load('elev_data.mat')
load('flap_data.mat')
load('alpha_data.mat')

alpha   = [-20:0.5:20]; % deg
cjet    = [0:0.5:12]; % Delta CJ sweep range [-]
elev    = [-100:0.5:100]; % deg
flap    = [0:0.5:110]; % deg

swp4    = length(elev);
swp3    = length(alpha);
swp2    = length(flap);
swp1    = length(cjet);

CJet = zeros(swp1,swp2,swp3,swp4);
CJtot   = zeros(swp1,swp2,swp3,swp4);
CXtot   = zeros(swp1,swp2,swp3,swp4);
CYtot   = zeros(swp1,swp2,swp3,swp4);
CZtot   = zeros(swp1,swp2,swp3,swp4);
CLtot   = zeros(swp1,swp2,swp3,swp4);
CDtot   = zeros(swp1,swp2,swp3,swp4);
CLcir   = zeros(swp1,swp2,swp3,swp4);
CLjet   = zeros(swp1,swp2,swp3,swp4);
CDind   = zeros(swp1,swp2,swp3,swp4);
CDjet   = zeros(swp1,swp2,swp3,swp4);
CDvis   = zeros(swp1,swp2,swp3,swp4);
Cmtot = zeros(swp1,swp2,swp3,swp4);

tCL = zeros(swp1,swp2,swp3,swp4);
tCD = zeros(swp1,swp2,swp3,swp4);
tCm = zeros(swp1,swp2,swp3,swp4);

delta = 0.5;

for i = 1:swp1
    for j = 1:swp2
        for k = 1:swp3
            for m = 1:swp4
                
                CXtot(i,j,k,m) = CXtot_jet(i) + delta.*(CXtot_flap(i,j) + CXtot_alpha(i,k) + CXtot_elev(i,m));
                CYtot(i,j,k,m) = CYtot_jet(i) + delta.*(CYtot_flap(i,j) + CYtot_alpha(i,k) + CYtot_elev(i,m));
                CZtot(i,j,k,m) = CZtot_jet(i) + delta.*(CZtot_flap(i,j) + CZtot_alpha(i,k) + CZtot_elev(i,m));                
                
                CJet(i,j,k,m) = CJet_jet(i) + delta.*(CJet_flap(i,j) + CJet_alpha(i,k) + CJet_elev(i,m));
                CJtot(i,j,k,m) = CJtot_jet(i) + delta.*(CJtot_flap(i,j) + CJtot_alpha(i,k) + CJtot_elev(i,m));
                
                CLtot(i,j,k,m) = CLtot_jet(i) + delta.*(CLtot_flap(i,j) + CLtot_alpha(i,k) + CLtot_elev(i,m));
                CDtot(i,j,k,m) = CDtot_jet(i) + delta.*(CDtot_flap(i,j) + CDtot_alpha(i,k) + CDtot_elev(i,m));
                
                tCL(i,j,k,m) = tCL_jet(i) + delta.*(tCL_flap(i,j) + tCL_alpha(i,k) + tCL_elev(i,m));
                tCD(i,j,k,m) = tCD_jet(i) + delta.*(tCD_flap(i,j) + tCD_alpha(i,k) + tCD_elev(i,m));
                tCm(i,j,k,m) = tCm_jet(i) + delta.*(tCm_flap(i,j) + tCm_alpha(i,k) + tCm_elev(i,m));
                          
            end
        end
    end
end

save splinedata1128.mat CJet CJtot CLtot CXtot CYtot CZtot CDtot Cmtot tCL tCD tCm alpha cjet elev flap
