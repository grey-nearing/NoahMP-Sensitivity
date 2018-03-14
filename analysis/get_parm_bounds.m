clear all
close all
clc

Ns = 20;

for s = 1:Ns
 fname = strcat('../parms/extract_parms/site_data/parms_',num2str(s),'.txt');
 parms(:,s) = load(fname);
end

parms = parms';

Pmin = min(parms);%-0.1*min(parms);
Pmax = max(parms);%+0.1*max(parms);
Pmean = mean(parms);

[Pmean',Pmin',Pmax',ones(size(Pmean'))]

N = length(Pmin');


fname = 'test_model_parms.m';
fid = fopen(fname,'w');
fprintf(fid,'function Q = test_model_parms() \n \n');
for p = 1:N
 fprintf(fid,'Q(%d,:) = [%f %f %f 1]; \n',p,Pmean(p),Pmin(p),Pmax(p));
end
fclose(fid);

