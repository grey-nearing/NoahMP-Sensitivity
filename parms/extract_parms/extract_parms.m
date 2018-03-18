clear all
close all
clc

Nsites = 20;


for s = 1:Nsites
 s
 cmd = strcat('cp ../gather_parms/site_data/parms_',num2str(s),'.txt ./parms.txt');
 system(cmd);
 [~,~] = system('./driver.exe namelist'); 
 cmd = strcat('cp parms.out ./site_data/parms_',num2str(s),'.txt');
 system(cmd);
 cmd = strcat('cp time_parms.txt ./site_data/time_parms_',num2str(s),'.txt');
 system(cmd);
end
