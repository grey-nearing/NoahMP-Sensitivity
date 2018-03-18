function [FOSI,TSI] = Sobol(X,Y)
%% *** Preliminary Statistics *********************************************
[Nsamples,Nparms] = size(Y);
Nparms = (Nparms-2)/2;
Y_single = [Y(:,1);Y(:,2)];
fo = mean(Y_single);
Do = var(Y_single);
% Do = [mean(Y_single.^2)-fo^2]';
%% *** MC Variance Integrals **********************************************
D = zeros(Nparms,4);
for p = 1:Nparms
    for n = 1:Nsamples
        D(p,1) = D(p,1) + Y(n,1)*Y(n,p+2);
        D(p,2) = D(p,2) + Y(n,1)*Y(n,Nparms+p+2);
        D(p,3) = D(p,3) + Y(n,2)*Y(n,Nparms+p+2);
        D(p,4) = D(p,4) + Y(n,2)*Y(n,p+2);
    end
end
D = D./Nsamples - fo^2;
% --- Correction for ??? --------------------------------------------------
Dcorrection = mean(Y(:,1).*Y(:,2)) - fo^2;
%% *** Main Effects *******************************************************
FOSI(:,1) = D(:,1)./Do; %+ Dcorrection
FOSI(:,2) = D(:,3)./Do; %+ Dcorrection
%% *** Total Sensitivity Indexes ******************************************
TSI(:,1) = 1-D(:,2)./Do; %+ Dcorrection
TSI(:,2) = 1-D(:,4)./Do; %+ Dcorrection
%% *** END FUNCTION *******************************************************
