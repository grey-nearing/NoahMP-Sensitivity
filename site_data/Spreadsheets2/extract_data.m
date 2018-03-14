clear all
close all
clc

Ns = 20;

sites = [{'Amplero'},
         {'Blodgett'}
         {'Bugac'}
         {'ElSaler2'}
         {'ElSaler'}
         {'Espirra'}
         {'FortPeck'}
         {'Harvard'}
         {'Hesse'}
         {'Howard'}
         {'Howlandm'}
         {'Hyytiala'}
         {'Kruger'}
         {'Loobos'}
         {'Merbleue'}
         {'Mopane'}
         {'Palang'}
         {'Sylvania'}
         {'Tumba'}
         {'UniMich'}];

datestr = '2003-01-01 00:00:00';
datezero = datenum(datestr);

D = 77;

for s = 1:Ns
 fprintf('Site %d of %d \n',s,Ns);

 fname = strcat('../',sites{s},'Fluxnet.1.4_met.nc');
 lat(s) = ncread(fname,'latitude');
 lon(s) = ncread(fname,'longitude');

 for y = 1994:2006
  fname = strcat(site_abbrev{s},'.',num2str(y),'synth.hourly.coreplusquality.csv');
  fid = fopen(fname);
  header = textscan(fid,'%s',D,'delimiter',',');
  data   = textscan(fid,'%f','delimiter',',');
  fclose(fid);
  N = size(data{1},1)/D;
  data = reshape(data{1},D,N)';

  forcing = zeros(N,8)-9999;
  obs     = zeros(N,7)-9999;

  forcing(:,1) = data(:,18);  % uu [m/s] 
  forcing(:,2) = 175;         % u dir [deg] 
  forcing(:,3) = data(:,11)+273.15;  % Ta [K] 
  forcing(:,4) = data(:,);  % Qa [kg/kg] 
  forcing(:,5) = data(:,);  % Pa [hPa] 
  forcing(:,6) = data(:,);  % Rs [W/m2] 
  forcing(:,7) = data(:,);  % Rl [W/m2] 
  forcing(:,8) = data(:,15)/(60*30);  % PP [mm/s] 
  
  obs(:,1) = data(:,5); % NEE
  obs(:,2) = data(:,8); % Qle
  obs(:,3) = data(:,9); % Qh
  obs(:,4) = data(:,16); % SM1
  obs(:,5) = data(:,17); % SM2
  obs(:,6) = data(:,12); % ST1
  obs(:,7) = data(:,13); % ST1

 end % year

end % site 


