function acft_plots(lab,nvar,nlev,nreg,nhr,instr,sat,fhrs,levs,num,Hmap1,Hmap2)
coun=1;
cmap=[0,31,103;0,66,135;32,114,214;255,255,255;255,127,0;230,38,44;118,18,19];
cmap=cmap/255;
Clab={'99.9% Imp', '99% Imp  ', '95% Imp  ', 'Neutral  ', '95% Deg  ', '99% Deg  ', '99.9% Deg'};
Hmm=zeros(nlev*(nvar-1)+1,nhr*2);
for ivar=1:nvar
   Hm=reshape(Hmap1(ivar,1:nlev,1,1:nhr),[nlev,nhr]);
   Hm1=zeros(nlev,nhr);
   for i=1:nlev
      Hm1(i,:)=Hm(nlev-i+1,:);
   end
   Hmm((1+nlev*(ivar-1)):(nlev*ivar),1:nhr)=Hm1(:,:);
   Hm=reshape(Hmap2(ivar,1:nlev,5,1:nhr),[nlev,nhr]);
   Hm1=zeros(nlev,nhr);
   for i=1:nlev
      Hm1(i,:)=Hm(nlev-i+1,:);
   end
   Hmm((1+nlev*(ivar-1)):(nlev*ivar),nhr+1:2*nhr)=Hm1(:,:);
end
yt=1:nlev*nvar;
yl=zeros(length(yt),1);
for i=1:nvar
   for j=1:nlev
     yl((i-1)*nlev+j,1)=levs(nlev-j+1);
   end
end
xt=1:nhr*2;
xl=zeros(length(xt),1);
for j=1:nhr
   xl(j,1)=fhrs(j);
   xl(j+nhr,1)=fhrs(j);
end

figure(num)
imagesc(Hmm)
hold on
h1=rectangle('position',[0.5 0.5 2*nhr nlev]);
set(h1,'EdgeColor',[.4,.3,1],'linewidth',2);
h2=rectangle('position',[0.5 0.5+nlev 2*nhr nlev]);
set(h2,'EdgeColor',[.4,.3,1],'linewidth',2);
h3=rectangle('position',[0.5 0.5+(nlev*2) 2*nhr nlev]);
set(h3,'EdgeColor',[.4,.3,1],'linewidth',2);
h4=rectangle('position',[0.5 0.5+(nlev*3) 2*nhr nlev]);
set(h4,'EdgeColor',[.4,.3,1],'linewidth',2);

v1=rectangle('position',[0.5 0.5 nhr nlev*nvar+1]);
set(v1,'EdgeColor',[.4,.3,1],'linewidth',2);
v2=rectangle('position',[0.5+nhr 0.5 nhr nlev*nvar+1]);
set(v2,'EdgeColor',[.4,.3,1],'linewidth',2);


hold off
grid on
set(gca,'fontsize',9)
set(gca,'XTick',xt)
set(gca,'XTickLabel',num2str(xl))
set(gca,'YTick',yt)
set(gca,'YTickLabel',num2str(yl))
xlabel('Forecast Length (Days)','fontsize',13)
set(gca,'Clim',[-3,3])
colormap(cmap)
lcolorbar(Clab,'fontsize',8)
title([lab],'fontsize',15)
annotation('textbox',[0.001,0.755,0.1,0.1],'String','T','EdgeColor','none','HorizontalAlignment','center','fontsize',13)
annotation('textbox',[0.001,0.545,0.1,0.1],'String','Z','EdgeColor','none','HorizontalAlignment','center','fontsize',13)
annotation('textbox',[0.001,0.355,0.1,0.1],'String','W','EdgeColor','none','HorizontalAlignment','center','fontsize',13)
annotation('textbox',[0.001,0.145,0.1,0.1],'String','Q','EdgeColor','none','HorizontalAlignment','center','fontsize',13)


annotation('textbox',[0.25,0.867,0.1,0.1],'String','ACAR','EdgeColor','none','HorizontalAlignment','center','fontsize',13)
annotation('textbox',[0.6,0.867,0.1,0.1],'String','ACFT','EdgeColor','none','HorizontalAlignment','center','fontsize',13)

print(num,'-dpng',[sat,instr,'_',lab,'_score'])
A=1;
end
