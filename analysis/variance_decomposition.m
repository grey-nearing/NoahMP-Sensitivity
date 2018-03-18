clear all; close all; clc
restoredefaultpath; addpath(genpath(pwd));
%% --- Load Data ----------------------------------------------------------

% load sensitivity indexes
load('./data/FFSI.mat');
load('./data/TTSI.mat');

% remove unused parameters in static veg
TTSI(13:30,:,:,:,:,4) = 0/0;

% remove any sites with missing data
Is = [];
for s = 1:size(TTSI,4)
    data = squeeze(TTSI(:,:,:,s,:,:));
    data = data(:);
    if length(find(~isnan(data))) > 0
        Is = [Is;s];
    end
end
TSI = squeeze(TTSI(:,1,:,Is,:,:));
FSI = squeeze(TTSI(:,1,:,Is,:,:));

% dimensions
[Nparms,Nouts,Nsites,Nyears,Nconfigs] = size(TSI);

%% --- Set Up Plotting Variables ------------------------------------------

% make some plotting tools
markers = ['o','s','x','d','*','p','+','h','<','>','^','v'];

colors = [...
    1,0,0
    0,1,0
    0,0,1
    1,0,1
    0,1,1
    1,1,0
    1,0.5,0
    0.5,0.5,0
    0.5,0,0.5
    0,0.5,0.5];

% names of the different model outputs
obsNames = [{'Latent Heat'}% [W/m^2]'}
    {'Sensible Heat'}% [W/m^2]'}
    {'Surface-Level Soil Moisture'}% [m^3/m^3]'}
    {'Lower-Level Soil Moisture 2 '}%[m^3/m^3]'}
    {'Net Ecosystem Exchange'}];% [umol/m^2s]'}];

% parameter names
fid = fopen('parmNames.txt');
line = fgetl(fid); p = 0;
while line ~= -1
    p = p+1;
    parmNames{p} = line;
    line = fgetl(fid);
end

% model configuration names
configs = [{'Noah-type'},{'CLM-type'},{'SSiB-type'},{'Prescribed LAI'}];

%% --- Law of Total Variance ----------------------------------------------

for c = 1:Nconfigs          % calculate separately for each model configuration
    for p = 1:Nparms        % calcualte separaetley for each parameter
        for o = 1:Nouts     % calcualte separaetley for each output
            
            % clear the intermediate conditional expected value
            Vs = [];
            Es = [];
            Y = [];
            
            for s = 1:Nsites
                Iy = find(squeeze(TSI(p,o,s,:,c)) > -9990);
                if Iy >= 2
                    Vs(s) = var(squeeze(TSI(p,o,s,Iy,c)));
                    Es(s) = mean(squeeze(TSI(p,o,s,Iy,c)));
                    Y = [Y;squeeze(TSI(p,o,s,Iy,c))];
                end
            end
            
            % normalize w.r.t. total variance
            UV(p,o,c) = mean(Vs)/var(Y(:));
            EV(p,o,c) = var(Es)/var(Y(:));
            
        end
    end
end

EV = EV./(EV+UV);
% uv = UV./(EV+UV);


%% --- Plot Results -------------------------------------------------------

% site names
siteNames = [{'Amplero'};{'Blodgett'};{'Bugac'};{'ElSaler2'};{'ElSaler'};{'Espirra'}; ...
    {'FortPeck'};{'Harvard'};{'Hesse'};{'Howlandm'};{'Howard'};{'Hyytiala'}; ...
    {'Kruger'};{'Loobos'};{'Merbleue'};{'Mopane'};{'Palang'};{'Sylvania'}; ...
    {'Tumba'};{'UniMich'}];

fignum = 0;

% different plot for each output variable
for d = 1:Nouts
    
    % initialize a new figure
    fignum = fignum + 1;
    figure(fignum); close(fignum); figure(fignum);
    set(gcf,'color','w','position',[1372 179 1913 1022])
    
    % loop through model configurations (as subplots)
    for c = 1:Nconfigs
        if c == 4 && d == 5; continue; end
        
        % subplots for different model configurations
        subplot(1,Nconfigs,c);
        
        % only plot those parameters that have some sensitivity
        sensitive = zeros(Nparms,1);
        for s = 1:Nsites
            for y = 1:Nyears
                for p = 1:Nparms
                    if TSI(p,d,s,y,c) >= 0.10 && sensitive(p) == 0
                        sensitive(p) = 1;
                    end
                end
            end
            for p = 1:Nparms
                if nanmean(TSI(p,d,s,:,c)) >= 0.1
                    sensitive(p) = 2;
                end    
            end
        end
        
        P1 = zeros(Nparms,1)/0;
        P1(sensitive>=1) = EV(sensitive>=1,d,c);
        b1 = barh(P1,'facecolor',0.8*ones(3,1)); hold on;
        
%         P2 = zeros(Nparms,1)/0;
%         P2(sensitive==2) = EV(sensitive==2,d,c);
%         b2 = barh(P2,'facecolor',0.6*ones(3,1)); hold on;
        
        % loop through sites
        for s = 1:Nsites
            
            % grab data for this site and output
%             Iy = find(all(squeeze(isnan(TSI(1:Nparms,d,s,:,c)))));
%             Yy = 1:Nyears; Iy = Yy(~ismember(Yy,Iy));
            means1 = nanmean(squeeze(TSI(1:Nparms,d,s,:,c)),2);
%             stdvs1 = std(squeeze(TSI(1:Nparms,d,s,Iy,c)),[],2);
            
            if max(means1(:)>-9000)
                h(s) = plot(means1(1:Nparms),1:Nparms,...
                    'linestyle','none','marker',markers(s),'color',colors(s,:),...
                    'linewidth',3,'markersize',10);
                hold on;
            end % is data present at this site-year
            
        end % sites
        
        % aesthetics
        grid on;
        set(gca,'ytick',1:Nparms,'yticklabel',parmNames,'ylim',[0,Nparms+1],'xlim',[0,1]);
        set(gca,'xtick',0:0.2:1);
        title(configs{c},'fontsize',16);
        xlabel('Total Effect Indices','fontsize',16);
        set(gca,'fontsize',18)
%         if c == Nconfigs; legend([{'Site EV'};{'Site EV (avg > 0.1)'};siteNames(Is)],'location',[0.82,0.235,0.065,0.11]); end
        if c == Nconfigs; legend([{'Site EV'};siteNames(Is)],'location',[0.83,0.40,0.065,0.11]); end
%         if d == Nouts && c == Nconfigs - 1;  legend([{'Site EV'};{'Site EV (avg > 0.1)'};siteNames(Is)],'location','ne'); end
        if d == Nouts && c == Nconfigs - 1;  legend([{'Site EV'};siteNames(Is)],'location','ne'); end
        
    end % configurations
    
    % suptitle
    suplabel(obsNames{d},'t',24);
    
    % save figure
    img = getframe(gcf);
    imwrite(img.cdata, strcat('./figures/variance_decomp_',num2str(d),'.jpg'));
    
end % outputs


%% --- END SCRIPT ---------------------------------------------------------


