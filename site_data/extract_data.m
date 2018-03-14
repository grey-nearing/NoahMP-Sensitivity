clear all
close all
clc

Ns = 20;

sites = [{'Amplero'}
         {'Blodgett'}
         {'Bugac'}
         {'ElSaler2'}
         {'ElSaler'}
         {'Espirra'}
         {'FortPeck'}
         {'Harvard'}
         {'Hesse'}
         {'Howlandm'}
         {'Howard'}
         {'Hyytiala'}
         {'Kruger'}
         {'Loobos'}
         {'Merbleue'}
         {'Mopane'}
         {'Palang'}
         {'Sylvania'}
         {'Tumba'}
         {'UniMich'}];

site_ab = [{'IT-Amp'}
           {'US-Blo'}
           {'HU-Bug'}
           {'ES-ES2'}
           {'ES-ES1'}
           {'PT-Esp'}
           {'US-FPe'}
           {'US-Ha1'}
           {'FR-Hes'}
           {'US-Ho1'}
           {'AU-How'}
           {'FI-Hyy'}
           {'ZA-Kru'}
           {'NL-Loo'}
           {'CA-Mer'}
           {'BW-Ma1'}
           {'ID-Pag'}
           {'US-Syv'}
           {'AU-Tum'}
           {'US-UMB'}];

datestr = '2003-01-01 00:00:00';
datezero = datenum(datestr);

for s = 1:Ns
 fprintf('Site %d of %d \n',s,Ns);

% read netcdf file for forcing and some of obs
 fname = strcat('../site_data/',sites{s},'Fluxnet.1.4_met.nc');
 lat(s) = ncread(fname,'latitude');
 lon(s) = ncread(fname,'longitude');

 Ta = ncread(fname,'Tair'); % K
 PP = ncread(fname,'Rainf'); % mm/s
 Qa = ncread(fname,'Qair'); % kg/kg
 uu = ncread(fname,'Wind'); % m/s
 Pa = ncread(fname,'PSurf'); % Pa
 Rl = ncread(fname,'LWdown'); % W/m2
 Rs = ncread(fname,'SWdown'); % W/m2

 Nt = length(Ta);
 obs     = zeros(Nt,5)-9999;
 date    = zeros(Nt,3)-9999;
 forcing = zeros(Nt,8)-9999;

 forcing(:,1) = uu;
 forcing(:,2) = 175;
 forcing(:,3) = Ta;
 forcing(:,4) = Qa;
 forcing(:,5) = Pa/100;
 forcing(:,6) = Rs;
 forcing(:,7) = Rl;
 forcing(:,8) = PP;

 fname = strcat('../site_data/',sites{s},'Fluxnet.1.4_flux.nc');
 lat(s) = ncread(fname,'latitude');
 lon(s) = ncread(fname,'longitude');
 Qe  = ncread(fname,'Qle'); % W/m2
 Qh  = ncread(fname,'Qh'); % W/m2
 Nee = ncread(fname,'NEE'); % umol/m2/s


 obs(:,1) = Qe;
 obs(:,2) = Qh;

 obs(:,5) = Nee.*44/10^6; 
 I = find(Nee<=-9000);
 obs(I,5) = -9999;

% read spreadsheet for the rest of obs
 edex = 0;
 D = 77;
 for y = 1994:2006
  try
   fname = strcat('./Spreadsheets2/',site_ab{s},'.',num2str(y),'.synth.hourly.coreplusquality.csv');
   fid = fopen(fname);
   header = textscan(fid,'%s',D,'delimiter',',');
   data   = textscan(fid,'%f','delimiter',',');
   fclose(fid);
   N = size(data{1},1)/D;
   data = reshape(data{1},D,N)';

   sdex = edex+1;
   edex = sdex+N-1;

   for t = 1:N
    if data(t,16) > -9000
     obs(sdex+t-1,3) = data(t,16)/100; % SM1
    else
     obs(sdex+t-1,3) = -9999;
    end

    if data(t,17) > -9000
     obs(sdex+t-1,4) = data(t,17)/100; % SM1
    else
     obs(sdex+t-1,4) = -9999;
    end
   end

   date(sdex:edex,:) = data(:,1:3);

  end % try
 end % year
 assert(edex==Nt)

 % write forcing file
 fname = strcat('site_data/forcing_',num2str(s),'.txt');
 fid1 = fopen(fname,'w');
 fname = strcat('site_data/obs_',num2str(s),'.txt');
 fid2 = fopen(fname,'w');
 fprintf('Writing output at site %d of %d \n',s,Ns);
 for t = 1:Nt
 % fprintf('Writing output at site %d of %d, time %d of %d \n',s,Ns,t,Nt);
  fprintf(fid1,'%d %d %5.3f %17.10f %17.10f %17.10f %17.10f %17.10f %17.10f %17.10f %17.10f \n',date(t,:),forcing(t,:));
  fprintf(fid2,'%d %d %5.3f %17.10f %17.10f %17.10f %17.10f %17.10f \n',date(t,:),obs(t,:));
 end
 fclose(fid1);
 fclose(fid2);
end % sites

fid = fopen('Sites.txt','w');
for s = 1:Ns
 fprintf(fid,'%f %f \n',lat(s),lon(s));
end
fclose(fid);





