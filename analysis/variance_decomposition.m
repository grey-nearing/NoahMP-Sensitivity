clear all; close all; clc
restoredefaultpath; addpath(genpath(pwd));
%% --- Load Data ----------------------------------------------------------

% load sensitivity indexes
load('./data/FFSI.mat');
load('./data/TTSI.mat');

% remove unused parameters in static veg
TTSI(5:30,:,:,:,:,4) = -9999;

% remove any sites with missing data
Is = [];
for s = 1:size(TTSI,4)
    data = squeeze(TTSI(:,:,:,s,:,:));
    data = data(:);
    if length(find(data>-9000)) > 0
        Is = [Is;s];
    end
end
TSI = TTSI(:,:,:,Is,:,:);
FSI = TTSI(:,:,:,Is,:,:);

% dimensions
Nsites = size(TSI,4);
Nyears = size(TSI,5);
Nconfigs = size(TSI,6);
Nparms = size(TSI,1);
Nouts = size(TSI,3);

%% --- Law of Total Variance ----------------------------------------------

for c = 1:Nconfigs          % calculate separately for each model configuration
    for p = 1:Nparms        % calcualte separaetley for each parameter
        for o = 1:Nouts     % calcualte separaetley for each output
                         
            % clear the intermediate conditional expected value
            Vs = []; 
            Es = [];
            Y = [];
            
            for s = 1:Nsites
                Iy = find(squeeze(TSI(p,1,o,s,:,c)) > -9990);
                if Iy >= 2
                    Vs(s) = var(squeeze(TSI(p,1,o,s,Iy,c)));
                    Es(s) = mean(squeeze(TSI(p,1,o,s,Iy,c)));
                    Y = [Y;squeeze(TSI(p,1,o,s,Iy,c))];
                end
            end
            
            % normalize w.r.t. total variance
            UV(p,o,c) = mean(Vs)/var(Y(:));
            EV(p,o,c) = var(Es)/var(Y(:));
            
        end
    end
end

ev = EV./(EV+UV);
uv = UV./(EV+UV);

% %% --- Calculate Variance-Based First-Order Metrics -----------------------
%
%
% for c = 1:Nconfigs          % calculate separately for each model configuration
%     for p = 1:Nparms        % calcualte separaetley for each parameter
%         for o = 1:Nouts     % calcualte separaetley for each output
%
%             % clear variance of the conditional
%             Vi = [];
%
%             % find missing data
%             XX = []; YY = [];
%             i = 0;
%             for s = 1:Nsites
%                 for y = 1:Nyears
%                     if TSI(p,1,o,s,y,c) > -9990
%                         i = i+1;
%                         X(i,:) = [s,y];
%                         Y(i) = TSI(p,1,o,s,y,c);
%                     end
%                 end
%             end
%
%             % calculate separately for each input variable
%             for i = 1:size(X,2)
%
%                 % clear the intermediate conditional expected value
%                 Exi = [];
%
%                 % find the number of unique values (categories) in current input
%                 Ux = unique(X(:,i));
%
%                 % find conditional expected value for category in current input
%                 for j = 1:length(Ux)
%                     Iu = find(X(:,i)==Ux(j));
%                     Exi(j) = mean(Y(Iu));
%                 end
%
%                 % take variacne of conditional expected values
%                 Vi(i) = var(Exi);
%
%             end
%
%             % normalize w.r.t. total variance
%             Si(:,p,o,c) = Vi./var(Y);
%
%         end
%     end
% end

%% --- Plot Results -------------------------------------------------------

figure(1); close(1); figure(1);
set(gcf,'color','w');
set(gcf,'position',[1640         293        1530        1205])

bar(squeeze(nanmean(ev,1)))

asdf
bar(Si(:,:,1,1)); grid on;
set(gca,'fontsize',18);
set(gca,'xticklabels',XvarNames);
title('Variance Total Effect','fontsize',22);
ylabel('S_T_i [~]','fontsize',20);
set(gca,'fontsize',20)
set(gca,'ylim',[0,1]);


%% --- END SCRIPT ---------------------------------------------------------


