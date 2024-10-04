%This is the main program, to generate a fit2obs scorecard
%from the /noscrub/archive/exp/fits/fit* files
%Kristen Bathmann
%2018


clear all
close all
%fdir must be the same as wrkdir in run_extract.sh
fdir='/scratch1/NCEPDEV/stmp4/Kristen.Bathmann/fitdo2';
pngname='do2'; %appears only in the name of the png's.
ex=0; %set =1 to make extra plots. Search for "if (ex>0)" to look at this section

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file_acar=[fdir,'/Acar_fits'];
file_acft=[fdir,'/Acft_fits'];
file_raob=[fdir,'/Raob_fits'];
file_surf=[fdir,'/Surf_fits'];
nreg_acar=1;
nsub_acar=1;
nlev_acar=3;
nvar_acar=4;
nreg_acft=7;
nsub_acft=1;
nlev_acft=3;
nvar_acft=4;
nreg_raob=7;
nsub_raob=1;
nlev_raob=21;
nvar_raob=5;
nreg_surf=7;
nsub_surf=2;
nlev_surf=1;
nvar_surf=5;
hr=[0,6,24,48,72,96,120];
hr=hr/24;
nhr=length(hr);
levs_acar=[1000,700,300];

levs_acft=levs_acar;
levs_surf=[1,2];
levs_raob=[1000,925,850,700,500,400,300,250,200,150,100,70,50,30,20,10,7,5,3,2,1];
labb='Bias';
labr='RMS';
Facar=open_fits(file_acar,nvar_acar,nlev_acar,nreg_acar,nhr);
Facft=open_fits(file_acft,nvar_acft,nlev_acft,nreg_acft,nhr);
Fraob=open_fits(file_raob,nvar_raob,nlev_raob,nreg_raob,nhr);
%Fsurf=open_fits(file_surf,nvar_surf,nsub_surf,nreg_surf,nhr);

[Acar_bias_err,Acar_rms_err]=Bias_RMS_err(nhr,nvar_acar,nlev_acar,nreg_acar,Facar);
[Acft_bias_err,Acft_rms_err]=Bias_RMS_err(nhr,nvar_acft,nlev_acft,nreg_acft,Facft);
[Raob_bias_err,Raob_rms_err]=Bias_RMS_err(nhr,nvar_raob,nlev_raob,nreg_raob,Fraob);
%[Surf_bias_err,Surf_rms_err]=Bias_RMS_err(nhr,nvar_surf,nsub_surf,nreg_surf,Fsurf);

Acar_bias_map=comp_map(nvar_acar,nlev_acar,nreg_acar,nhr,1,Acar_bias_err,Facar);
Acft_bias_map=comp_map(nvar_acft,nlev_acft,nreg_acft,nhr,1,Acft_bias_err,Facft);
Raob_bias_map=comp_map(nvar_raob,nlev_raob,nreg_raob,nhr,1,Raob_bias_err,Fraob);
%Surf_bias_map=comp_map(nvar_surf,nsub_surf,nreg_surf,nhr,1,Surf_bias_err,Fsurf);

Acar_rms_map=comp_map(nvar_acar,nlev_acar,nreg_acar,nhr,2,Acar_rms_err,Facar);
Acft_rms_map=comp_map(nvar_acft,nlev_acft,nreg_acft,nhr,2,Acft_rms_err,Facft);
Raob_rms_map=comp_map(nvar_raob,nlev_raob,nreg_raob,nhr,2,Raob_rms_err,Fraob);
%Surf_rms_map=comp_map(nvar_surf,nsub_surf,nreg_surf,nhr,2,Surf_rms_err,Fsurf)


if (ex>0)
%Fraob(nstats,nvar,nlev,nregions,nhours)
%nstats-
%1-ave bias exp-ctl
%2-ave rms exp-ctl
%3 stdev of 1
%4 stdev of 2
%5-sample size 
%6-ave bias of exp
%7-ave rms of exp
%8-ave bias of clt
%9-ave rms of clt
%10-nobs exp
%11-nobs ctl

%nvar=5 for raob
%1-T
%2-Z (probably not correct!)
%3-W
%4-Q
%5-SLP

%nlev, see levs_raob for this

%nregions=7 for raob
%1-Glob
%2-NHem
%3-SHem
%4-Trop
%5-North Amr
%6-Europe
%7-Asia

%nhours-[0,6,24,...,120]/24
figure(9)
%vertical plot of temperature RMSE in NH at 0hr
plot(reshape(Fraob(7,1,:,2,1),[1,length(levs_raob)]),levs_raob,'k')
hold on
plot(reshape(Fraob(9,1,:,2,1),[1,length(levs_raob)]),levs_raob,'r--')
hold off
set(gca,'ydir','reverse')
ylim([20,1000])
grid on
legend('Exp','CTL')
title('T NHEM RMS, 0 hours')
xlabel('Pressure')
print(9,'-dpng',[pngname,'Trmsnh0vert'])
  
figure(11)
%three paneled plot of Q bias at 300 hpa vs forecast hour
subplot(3,1,1)
plot(hr,reshape(Fraob(6,4,7,2,:),[1,7]),'k--*','linewidth',1.2)
hold on
plot(hr,reshape(Fraob(8,4,7,2,:),[1,7]),'k-','linewidth',1.2)
hold off
grid on
%ylim([-0.001,0.01])
set(gca,'fontsize',13)
legend('EXP','CTL','location','SouthWest')
ylabel('Bias NH')
subplot(3,1,2)
plot(hr,reshape(Fraob(6,4,7,3,:),[1,7]),'k--*','linewidth',1.2)
hold on
plot(hr,reshape(Fraob(8,4,7,3,:),[1,7]),'k-','linewidth',1.2)
hold off
grid on
%ylim([-0.03,0.01])
set(gca,'fontsize',13) 
ylabel('Bias SH')
subplot(3,1,3)
plot(hr,reshape(Fraob(6,4,7,4,:),[1,7]),'k--*','linewidth',1.2)
hold on
plot(hr,reshape(Fraob(8,4,7,4,:),[1,7]),'k-','linewidth',1.2)
hold off
grid on
set(gca,'fontsize',13) 
ylabel('Bias Tropics')
xlabel('Forecast Length, Days')
print(11,'-dpng',[pngname,'fhQbias300'])

figure(16)
%Q RMSE at 300 hpa in Tropics vs forecast hour
plot(hr,reshape(Fraob(7,4,7,4,:),[1,7]),'k')
hold on
plot(hr,reshape(Fraob(9,4,7,4,:),[1,7]),'r--')
hold off
grid on
legend('EXP','CTL')
title('Q Tropics RMS, 300 hpa')
xlabel('Forecast Length, Days')
print(16,'-dpng',[pngname,'TrQrms300'])

end

%acar and acft plot
%acft_plots(labb,nvar_acar,nlev_acar,nreg_acar,nhr,'ACARACFT',pngname,hr,levs_acar,1,Acar_bias_map,Acft_bias_map);
%acft_plots(labr,nvar_acar,nlev_acar,nreg_acar,nhr,'ACARACFT',pngname,hr,levs_acar,2,Acar_rms_map,Acft_rms_map);

%acft plot
nreg_acft=5;
%hmap_plots(labb,nvar_acft,nlev_acft,nreg_acft,nhr,'ACFT',pngname,hr,levs_acft,3,Acft_bias_map);
%hmap_plots(labr,nvar_acft,nlev_acft,nreg_acft,nhr,'ACFT',pngname,hr,levs_acft,4,Acft_rms_map);

%raob plot
nreg_raob=5;
nlev_raob=15;
hmap_plots(labb,nvar_raob,nlev_raob,nreg_raob,nhr,'RAOB',pngname,hr,levs_raob,5,Raob_bias_map);
hmap_plots(labr,nvar_raob,nlev_raob,nreg_raob,nhr,'RAOB',pngname,hr,levs_raob,6,Raob_rms_map);
return
%surf plots don't work
%surf plot
%hmap_plots(labb,nvar_surf,nsub_surf,nreg_surf,nhr,'SURF',pngname,hr,levs_surf,7,Surf_bias_map);
%hmap_plots(labr,nvar_surf,nsub_surf,nreg_surf,nhr,'SURF',pngname,hr,levs_surf,8,Surf_rms_map);
