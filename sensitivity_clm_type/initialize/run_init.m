clear all
close all
clc

dir = '/discover/nobackup/gnearing/projects/PALS_SysID/';
sites = load(strcat(dir,'site_data/Sites.txt'));
Ns = length(sites);

for s = 1:Ns

 system('/bin/rm forcing.txt');
 system('/bin/rm parms.txt');
 system('/bin/rm num_times.txt');
 system('/bin/rm lat_lon.txt');
 system('/bin/rm startdate.txt');
 system('cp soil_init.orig soil_init.txt');
 system('cp plant_init.orig plant_init.txt');

 % forcing
 cmd = strcat('cp /',dir,'site_data/site_data/forcing_',num2str(s),'.txt ./forcing.txt');
 system(cmd);

 % parms
 cmd = strcat('cp /',dir,'parms/extract_parms/site_data/parms_',num2str(s),'.txt ./parms.txt');
 system(cmd);
 cmd = strcat('cp /',dir,'parms/extract_parms/site_data/time_parms_',num2str(s),'.txt ./time_parms.txt');
 system(cmd);

% startdate
 offset = sites(s,3);
 if offset < 0
   startdate = 200012311200 + (12+offset)*100;  % offset here is negative to the west
 else
   startdate = 200101011200 - (12-offset)*100;
 end 
 startdate = num2str(startdate);
 fname = 'startdate.txt';
 fid = fopen(fname,'w');
 fprintf(fid,'%s',startdate);
 fclose(fid);

 % lat/lon
 lat = sites(s,1);
 lon = sites(s,2);
 latlon = [lat;lon];
 save('lat_lon.txt','latlon','-ascii');

 % num timesteps
 u = load('forcing.txt');
 Nt = size(u,1);
 save('num_times.txt','Nt','-ascii');

 for rep = 1:11
  % screen report
  fprintf('Running Site: %d - Rep: %d - Num Timesteps: %d \n',s,rep,Nt);

  % run model
  [~,dummy] = system('/bin/rm output.out');
  system('./main.exe');
 
  % collect initial state
  x = load('output.out');
  sm = x(end,11:14);
  pl = x(end,15:18);
  sm = sm(:); pl = pl(:);
  save('soil_init.txt','sm','-ascii');
  save('plant_init.txt','pl','-ascii');
 end

 % collect initial state
 x = load('output.out');
 sm = x(end,11:14);
 pl = x(end,15:18);
 sm = sm(:); pl = pl(:);
 save(strcat('site_data/soil_init_',num2str(s),'.txt'),'sm','-ascii');
 save(strcat('site_data/plant_init_',num2str(s),'.txt'),'pl','-ascii');

 for y = 1996:2008
  o = x(find(x(:,1)==y),:);
  if ~isempty(o)
   o = o(end,:);
   sm = x(end,11:14);
   pl = x(end,15:18);
   sm = sm(:); pl = pl(:);
   save(strcat('site_data/soil_init_',num2str(s),'_',num2str(y),'.txt'),'sm','-ascii');
   save(strcat('site_data/plant_init_',num2str(s),'_',num2str(y),'.txt'),'pl','-ascii');
  end
 end

end % site
