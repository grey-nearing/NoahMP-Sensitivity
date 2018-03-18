clear all; close all; clc
restoredefaultpath; addpath(genpath(pwd));
%% --- Dimensions of Experiment -------------------------------------------

Nsites = 20;            % # sites
Years  = 1999:2006;     % year range (not all sites have all years)
Nsamps = 3000;          % # samples per site
Nparms = 49;            % # parameters for each model config
Noutps = 5;             % # model output variables
Nconfg = 4;             % # model configurations

% load sensitivity indexes
load('./data/FFSI.mat');
load('./data/TTSI.mat');

% remove unused parameters in static veg
TTSI(5:30,:,:,:,:,4) = -9999;

%% --- Set Up Plotting Variables ------------------------------------------

% make some plotting tools
markers = ['o','s','x','d','*','p','+','h','<','>','^','v'];

colors = [0,0,0
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
    {'Soil Moisture 1'}% [m^3/m^3]'}
    {'Soil Moisture 2 '}%[m^3/m^3]'}
    {'Net Ecosystem Exchange'}];% [umol/m^2s]'}];

% site names
siteNames = [{'Amplero'};{'Blodgett'};{'Bugac'};{'ElSaler2'};{'ElSaler'};{'Espirra'}; ...
    {'FortPeck'};{'Harvard'};{'Hesse'};{'Howlandm'};{'Howard'};{'Hyytiala'}; ...
    {'Kruger'};{'Loobos'};{'Merbleue'};{'Mopane'};{'Palang'};{'Sylvania'}; ...
    {'Tumba'};{'UniMich'}];

% remove any sites with missing data
Is = [];
for s = 1:Nsites
    data = squeeze(TTSI(:,:,:,s,:,:));
    data = data(:);
    if length(find(data>-9000)) > 0
        Is = [Is;s];
    end
end
TSI = TTSI(:,:,:,Is,:,:);
FSI = TTSI(:,:,:,Is,:,:);

% number of sites with good data
Nsites = length(Is);

% parameter names
fid = fopen('parmNames.txt');
line = fgetl(fid); p = 0;
while line ~= -1
    p = p+1;
    parmNames{p} = line;
    line = fgetl(fid);
end

% model configuration names
configs = [{'NOAH-type'},{'CLM-type'},{'SSiB-type'},{'Prescribed LAI'}];

%% --- Make Plots ---------------------------------------------------------

fignum = 0;

% different plot for each output variable
for d = 1:Noutps
    
    % initialize a new figure
    fignum = fignum + 1;
    figure(fignum); close(fignum); figure(fignum);
    set(gcf,'color','w','position',[1372         179        1913        1022])
    
    % loop through model configurations (as subplots)
    for c = 1:Nconfg
        if c == 4 && d == 5; continue; end
        
        % loop through sites
        for s = 1:Nsites
            
            % grab data for this site and output
            Iy = find(all(squeeze(TSI(1:Nparms,1,d,s,:,c))<-9990));
            Yy = 1:length(Years); Iy = Yy(~ismember(Yy,Iy));
            means1 = mean(squeeze(TSI(1:Nparms,1,d,s,Iy,c)),2);
            means2 = mean(squeeze(TSI(1:Nparms,2,d,s,Iy,c)),2);
            stdvs1 = std(squeeze(TSI(1:Nparms,1,d,s,Iy,c)),[],2);
            stdvs2 = std(squeeze(TSI(1:Nparms,2,d,s,Iy,c)),[],2);
            
            % subplots for different model configurations
            subplot(1,Nconfg,c);
            
            if max(means1(:)>-9000)
                
                h(s) = plot(means1(1:Nparms),1:Nparms,...
                    'linestyle','none','marker',markers(s),'color',colors(s,:),...
                    'linewidth',3,'markersize',10);
                hold on;
                
            end % is data present at this site-year
            
            % aesthetics
            grid on;
            set(gca,'ytick',1:Nparms,'yticklabel',parmNames,'ylim',[1,Nparms+0.01],'xlim',[-0.2,1]);
            title(configs{c},'fontsize',16);
            xlabel('Total Effect Index','fontsize',16);
            set(gca,'fontsize',18)
            if c == Nconfg && s == Nsites; legend(h,siteNames(Is),'location','se'); end
            
        end % configurations
    end % sites
    
    % suptitle
    suplabel(obsNames{d},'t',24);
    
    % save figure
    img = getframe(gcf);
    imwrite(img.cdata, strcat('./figures/tsi_plots_',num2str(d),'.jpg'));
    
end % outputs

%% --- END SCRIPT ---------------------------------------------------------


