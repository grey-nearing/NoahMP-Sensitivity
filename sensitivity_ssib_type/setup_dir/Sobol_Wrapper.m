%% ************************************************************************
%% ------------------------------------------------------------------------
%%     SOBOL SENSITIVITY ANALYSIS 
%%          - Function Sampling and Variance-based
%%              Sensitivity Integration
%%          
%%      Grey Nearing
%%      University of Arizona
%%      Jan, 2010
%%     
%% ************************************************************************

clc
clear all
close all

%% *** Initialize Runtime Environment *************************************
fname = strcat('obs.txt');
z = load(fname);
z = z(:,4:8);
for d = 1:5
 I{d} = find(z(:,d)>-9000);
end

% get test parms
Parms       = test_model_parms();  % gets parameters with bounds
Parm_dexes  = find(Parms(:,4)~=0); % find parameters to be considered
Nparms      = length(Parm_dexes);  % number of parameters to be considered
[TotParms,~]= size(Parms);         % total number of parms

% hold parms that are not varying
fname = './parms_original.txt';
pp = load(fname);
hold = find(Parms(1:66,4)==0);
Parms(hold,1) = pp(hold);

% user controls
Nsample     = 3000;                % size of training sample
NLoops      = 1;                   % number of monte carlo trials
%nstart      = 1;
load('nstart.txt');


%% *** Monte Carlo Loop ***************************************************
for loopdex = 1:NLoops
% --- Sample Parameter Space ----------------------------------------------
    D1 = lhsdesign(Nsample,TotParms,'criterion','maximin','iterations',10); % latin hypercube sample of [0-1] parameter space
    Q1 = DtoQ(D1,Parms);      % expands [0-1] parameter space; each row represents a set of parameters;
    D2 = lhsdesign(Nsample,TotParms,'criterion','maximin','iterations',10);
    Q2 = DtoQ(D2,Parms); % a second set of parameter samples necessary for SOBOL'
% --- Loop Through Parameter Sample ---------------------------------------
    Y1 = zeros(Nsample,1,5);
    Y2 = zeros(Nsample,1,5);
    Y12 = zeros(Nsample,Nparms,5);
    Y21 = zeros(Nsample,Nparms,5);

    if nstart > 1
     ostart = nstart;
     mstart = Nsample;
     load('progress.mat');
     nstart = ostart;
     Nsample = mstart;
     clear ostart mstart
    end

% --- Loop Through Function Samples ---------------------------------------
    for n = nstart:Nsample
        tic 
        fprintf('MC Loop: %d of %d - Sample: %d of %d - ',loopdex,NLoops,n,Nsample);
% --- First Homogeneous Term ----------------------------------------------
        run_parms   = Q1(n,:);
        Y1(n,:)       = test_model(run_parms,z,I);
% --- Second Homogeneous Term ---------------------------------------------
        run_parms   = Q2(n,:);
        Y2(n,:)       = test_model(run_parms,z,I);
% --- Heterogeneous Terms -------------------------------------------------
        for p_dex = 1:length(Parm_dexes)
            p = Parm_dexes(p_dex);

            run_parms    = Q2(n,:);
            run_parms(p) = Q1(n,p);
            Y12(n,p_dex,:) = test_model(run_parms,z,I);
            
            run_parms    = Q1(n,:);
            run_parms(p) = Q2(n,p);
            Y21(n,p_dex,:) = test_model(run_parms,z,I);
        end% Heterogeneous Parameter Loop
      toc
      if rem(n,30) == 0
       save('progress.mat'); 
       save('nstart.txt','n','-ascii');
      end;
    end% Function Sample Loop
% --- Integrate Sobol' Sensitivity ----------------------------------------
    save('progress.mat'); 
    for d = 1:5
     Y = [Y1(:,:,d),Y2(:,:,d),Y12(:,:,d),Y21(:,:,d)];
     X = [Q1,Q2];
     [FOSI(:,:,loopdex,d),TSI(:,:,loopdex,d)] = Sobol(X,Y);
    end
% --- Save Results to File(s) ---------------------------------------------
    Ffname = strcat('FOSI_',num2str(loopdex),'.dat');
    Tfname = strcat('TSI_',num2str(loopdex),'.dat');
    save(Ffname,'FOSI');
    save(Tfname,'TSI');
end%loopdex




