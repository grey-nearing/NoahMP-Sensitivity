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

Missing = ones(Ns,15,5)./0;

fid = fopen('Site_Years.txt','w');

for s = 1:Ns
  for y = 1994:2006
    try
      fname = strcat('./site_data/obs_',num2str(s),'_',num2str(y),'.txt');
      data = load(fname);
    catch
      continue
    end

    data = data(:,4:8);
    for d = 1:5
      N(s,y-1993,d) = sum(data(:,d)<=-9000);
    end
    
    fprintf('Site: %d of %d, Year: %d  -- %d %d %d %d %d \n',s,Ns,y,N(s,y-1993,:));
    fprintf(fid,'%d %d \n',s,y);

  end % year
    disp('------------------------------------------------------------');

end % site
fclose(fid)

