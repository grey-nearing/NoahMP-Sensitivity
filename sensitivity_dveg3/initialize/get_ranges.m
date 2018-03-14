clear all
close all
clc

Ns = 20;

for s = 1:Ns
 data(:,s) = load(strcat('./site_data/plant_init_',num2str(s),'.txt'));
end

min(data')
max(data')

