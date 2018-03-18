clear all
close all
clc

vegtypes = [10,1,10,1,12,2,10,4,4,8,1,1,9,1,11,8,2,5,2,4];
HVT = [20.0,20.0,18.0,16.0,16.0,1.10,1.10,13.0,10.0,1.00,5.00,2.00,15.0,1.50,0.00,0.00,0.00,4.00,2.00,0.50];

sites = load(strcat('../site_data/Site_Years.txt'));
Ns = length(sites);

for s = 1:Ns

 cmd = strcat('mkdir runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)));
 system(cmd);

 fname = strcat('runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/nstart.txt');
 fid = fopen(fname,'w');
 fprintf(fid,'%d',1);
 fclose(fid);

 fname = strcat('runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/hvt.txt');
 fid = fopen(fname,'w');
 fprintf(fid,'%d',HVT(vegtypes(sites(s,1))));
 fclose(fid);

 cmd = strcat('cp ./setup_dir/main.exe runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)));
 system(cmd);

 cmd = strcat('cp ./setup_dir/*.m runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)));
 system(cmd);

 cmd = strcat('cp ./setup_dir/run_matlab.sh runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)));
 system(cmd);

 cmd = strcat('cp ./setup_dir/da_flag.txt runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)));
 system(cmd);

 cmd = strcat('cp ./setup_dir/init.txt runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)));
 system(cmd);

 cmd = strcat('cp ../site_data/site_data/obs_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'.txt ./','runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/obs.txt');
 system(cmd);
 
 cmd = strcat('cp ../parms/extract_parms/site_data/parms_',num2str(sites(s,1)),'.txt ./','runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/parms.txt');
 system(cmd);
 
 cmd = strcat('cp ../parms/extract_parms/site_data/parms_',num2str(sites(s,1)),'.txt ./','runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/parms_original.txt');
 system(cmd);
 
 cmd = strcat('cp ../parms/extract_parms/site_data/time_parms_',num2str(sites(s,1)),'.txt ./','runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/time_parms_original.txt');
 system(cmd);

 cmd = strcat('cp ../site_data/site_data/forcing_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'.txt ./','runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/forcing_1.txt');
 system(cmd);

 a = load(strcat('runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/forcing_1.txt'));
 Nt = length(a);
 save(strcat('runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/num_times.txt'),'Nt','-ascii');

 ll = load(strcat('../site_data/Sites.txt'));
 ll = ll(sites(s,1),1:2); ll = ll(:);
 save(strcat('runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/lat_lon.txt'),'ll','-ascii');

% startdate
 offset = load(strcat('../site_data/Sites.txt'));
 offset = offset(sites(s,1),3);
 if offset < 0
   startdate = 200012311200 + (12+offset)*100;  % offset here is negative to the west
 else
   startdate = 200101011200 - (12-offset)*100;
 end
 startdate = num2str(startdate);
 fname = strcat('runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/startdate.txt');
 fid = fopen(fname,'w');
 fprintf(fid,'%s',startdate);
 fclose(fid);

 cmd = strcat('cp ./initialize/site_data/soil_init_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'.txt runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/soil_init.txt');
 system(cmd);

 cmd = strcat('cp ./initialize/site_data/plant_init_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'.txt runtime_dirs/run_',num2str(sites(s,1)),'_',num2str(sites(s,2)),'/plant_init.txt');
 system(cmd);

end


