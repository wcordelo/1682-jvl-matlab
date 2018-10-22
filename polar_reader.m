%%
% user input

load("data.mat");

CL = 7;
D_flap = 30; % degrees

%%
% reads polars

Cj = findCj(CL, D_flap)
Cx = findCx(CL, D_flap)

function Cj = findCj(Cl, d_flap)
    S = load("data.mat");
    table = S.data.CL(:,d_flap/10);
    
    interp_factor = 0.01;
    r = 0:0.5:20;
    interp_r = 0:interp_factor:20;
    interp_table = interp1(r,table,interp_r);
    plot(r,table,'o',interp_r,interp_table,':.')
    
    if Cl > max(table)
        Cj = NaN;
    elseif Cl < min(table)
        Cj = NaN;
    else
        [diff Cjindex] = min(abs(interp_table-Cl));
        Cj = interp_r(Cjindex);
    end
end

function Cx = findCx(Cl, d_flap)
    S = load("data.mat");
    Cj = findCj(Cl, d_flap);
    table = S.data.CX(:,d_flap/10);
    
    interp_factor = 0.01;
    r = 0:0.5:20;
    interp_r = 0:interp_factor:20;
    interp_table = interp1(r,table,interp_r);
	plot(r,table,'o',interp_r,interp_table,':.')
    
    if isnan(Cj)
        Cx = NaN;
    else
        Cxindex = find(interp_r == Cj);
        Cx = interp_table(Cxindex);
    end
end