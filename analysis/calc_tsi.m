clear all; close all; clc
restoredefaultpath; addpath(genpath(pwd));
%% --- Dimensions of Experiment -------------------------------------------

Nsites = 20;            % # sites
Years  = 1999:2006;     % year range (not all sites have all years)
Nsamps = 2000;%3000;          % # samples per site
Nparms = 49;            % # parameters for each model config
Noutps = 5;             % # model output variables
Nconfg = 1;             % # model configurations

% initialize storage of the sensitivity indexes
TTSI = zeros(Nparms,2,Noutps,Nsites,length(Years),Nconfg)-9999;
FFSI = zeros(Nparms,2,Noutps,Nsites,length(Years),Nconfg)-9999;

%% --- Gather Data from Each Noah-MP Configuration ------------------------

for s = 1:Nsites                  % loop through sites
    for y = 1:length(Years)         % loop through years
        for c = 1:Nconfg               % loop through model configurations
            
            % match model config index with directory name
            switch c
%                case(1); mvar = 'noah_type';
                case(1); mvar = 'clm_type';
%                case(3); mvar = 'ssib_type';
%                case(4); mvar = 'dveg3';
            end
            
            % try to load resutls for site/year
            try
                tempParms = Nparms;
                fname = strcat('../sensitivity_',mvar,'/run_',num2str(s),'_',num2str(Years(y)),'/progress.mat');
                load(fname);
                Npc = Nparms;
                Nparms = tempParms;
            catch
                continue
            end % site/year exists
            
            % calculate Sobol indices for each output dimension
            for d = 1:Noutps
                
                % concatenate samples of model output
                YY = [Y1(1:Nsamps,:,d),Y2(1:Nsamps,:,d),Y12(1:Nsamps,:,d),Y21(1:Nsamps,:,d)];
                
                % concatenate parameters used to generate samples
                XX = [Q1,Q2];
                
                % calculate sobol indices
                [FFSI(1:Npc,:,d,s,y,c),TTSI(1:Npc,:,d,s,y,c)] = Sobol(YY);
                
                % screen report
                fprintf('successfully completed site=%d, year=%d, config=%d \n',s,y,c);
                
            end % % output variables
        end % configs
    end % years
end % sites

%% --- Save Results -------------------------------------------------------

save('./data/FFSI.mat','FFSI','-v7.3');
save('./data/TTSI.mat','TTSI','-v7.3');

%% --- END SCRIPT ---------------------------------------------------------
