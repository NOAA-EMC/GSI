clear all
close all
plotn=2; %1 for cris, 2 for iasi
ploterrs=1; %1 for yes, 2 for no
if (plotn==1) 
  load cristics.mat
  xt=xtc;
  xl=xlc;
  yt=ytc;
  yl=ylc;
  err=errc;
  ni=100;
  filel='Rcorr_cris-fsr_npp_sea';
  filee='err_cris-fsr_npp_sea';
  savecorr='crisnppcorr.png';
  saveerr='criserr.png';
  plotcorrtitle='CrIS-NPP Correlation Matrix';
  ploterrtitle='CrIS Error';
  num=fopen('err_cris-fsr_npp_sea','r','l');
  El=fread(num,'float');
  fclose(num);
else
  load iasitics.mat
  ni=174;
  filel='Rcorr_iasi_metop-b_land';
  filee='err_iasi_metop-b_sea';
  savecorr='iasiblandcorr.png';
  saveerr='iasiberr.png';
  plotcorrtitle='IASI-B Land Correlation Matrix';
  ploterrtitle='IASI-B Error';
  num=fopen('err_iasi_metop-b_land','r','l');
  El=fread(num,'float');
  fclose(num);
end
num=fopen(filee,'r','l');
Es=fread(num,'float');
fclose(num);

num=fopen(filel,'r','l');
Rl=fread(num,'float');
Rl=reshape(Rl,[ni,ni]);
fclose(num);



Cmin=-.2;
Cmax=1;
figure(1)
imagesc(Rl)
set(gca,'XTick',xt)
set(gca,'XTickLabel',num2str(xl))
set(gca,'YTick',yt)
set(gca,'YTickLabel',num2str(yl))
hcb=colorbar;
cmap2=hsv(48);
cmap2=cmap2(1:4:48,:);
cmap2(1,2)=1.0;
cmap2(1,3)=1.0;
colormap(cmap2); %KAB
hcb=colorbar;
set(gca,'Clim',[Cmin,Cmax])
%set(hcb,'YTick',[Cmin,-0.25,0,0.25,0.5,0.75,Cmax])
set(gca,'fontsize',11)
title(plotcorrtitle)
xlabel('Wavenumber (cm^{-1})')
ylabel('Wavenumber (cm^{-1})')
print(1,'-dpng', savecorr)

if (ploterrs==1) 
  figure(2)
  plot(Es,'r-','linewidth',1.5)
  hold on
  plot(El,'b-','linewidth',1.5)
  plot(err,'k-','linewidth',1.5)
  hold off
  set(gca,'fontsize',11)
  xlim([1,ni])
  set(gca,'XTick',xt)
  set(gca,'XTickLabel',num2str(xl))
  xlabel('Channel Wavenumber, cm^{-1}')
  ylabel('Error')
  title(ploterrtitle)
  if (plotn==1) 
    legend('Cris-FSR N20','CrIS-FSR NPP','Satinfo','location','NorthWest')
  else
    legend('IASI-B Sea','IASI-B Land','Satinfo','location','NorthWest')
  end
  print(2,'-dpng', saveerr)
end
