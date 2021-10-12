function hmap_plots(lab,nvar,nlev,nreg,nhr,instr,sat,fhrs,levs,num,Hmap)
coun=1;
cmap=[0,31,103;0,66,135;32,114,214;255,255,255;255,127,0;230,38,44;118,18,19];
cmap=cmap/255;
Clab={'99.9% Imp', '99% Imp  ', '95% Imp  ', 'Neutral  ', '95% Deg  ', '99% Deg  ', '99.9% Deg'};
regstart=2;
if nreg < 2
   regstart=1;
end
margins=[0,0];
if nvar == 5 
   if nlev > 6
      Hmm=zeros(nlev*(nvar-2)+8,nhr*(nreg-regstart+1));
      yt=1:nlev*(nvar-2)+8;
   else
      Hmm=zeros(nlev*(nvar-1)+1,nhr*(nreg-regstart+1));
      yt=1:nlev*(nvar-1);
   end
else
   Hmm=zeros(nlev*nvar,nhr*(nreg-regstart+1));
   yt=1:nlev*nvar;

end
yl=zeros(length(yt),1);
for i=1:4 %nvar -1
   if i < 4
      for j=1:nlev
        yl((i-1)*nlev+j,1)=levs(nlev-j+1);
      end
   elseif nlev > 6
      for j=1:7
        yl((i-1)*nlev+j,1)=levs(7-j+1);
      end
   else
      for j=1:nlev
        yl((i-1)*nlev+j,1)=levs(nlev-j+1);
      end
   end

end

for ivar=1:nvar
   for ireg=regstart:nreg
      if ivar < 4
         Hm=reshape(Hmap(ivar,1:nlev,ireg,1:nhr),[nlev,nhr]);
         Hm1=zeros(nlev,nhr);
         for i=1:nlev
            Hm1(i,:)=Hm(nlev-i+1,:);
         end
         Hmm((1+nlev*(ivar-1)):(nlev*ivar),(1+nhr*(ireg-regstart)):(nhr*(ireg-regstart+1)))=Hm1(:,:);
      elseif ivar==4
         if nlev > 6
            Hm=reshape(Hmap(ivar,1:nlev,ireg,1:nhr),[nlev,nhr]);
            Hm1=zeros(7,nhr);
            for i=1:7
               Hm1(i,:)=Hm(7-i+1,:);
            end
            Hmm((1+nlev*(ivar-1)):(nlev*(ivar-1)+7),(1+nhr*(ireg-regstart)):(nhr*(ireg-regstart+1)))=Hm1(:,:);
          else
            Hm=reshape(Hmap(ivar,1:nlev,ireg,1:nhr),[nlev,nhr]);
            Hm1=zeros(nlev,nhr);
            for i=1:nlev
               Hm1(i,:)=Hm(nlev-i+1,:);
            end
            Hmm((1+nlev*(ivar-1)):(nlev*ivar),(1+nhr*(ireg-regstart)):(nhr*(ireg-regstart+1)))=Hm1(:,:);
         end
      else
         Hm=reshape(Hmap(ivar,1,ireg,1:nhr),[1,nhr]);
         Hmm(end,(1+nhr*(ireg-regstart)):(nhr*(ireg-regstart+1)))=Hm(:,:); 
      end 
      coun=coun+1;
   end
end

xt=1:nhr*(nreg-regstart+1);
xl=zeros(length(xt),1);
for i=1:nreg-regstart+1
   for j=1:nhr
      xl((i-1)*nhr+j,1)=fhrs(j);
   end
end
figure(num)
imagesc(Hmm)
hold on
h1=rectangle('position',[0.5 0.5 (nreg-regstart+1)*nhr nlev]);
set(h1,'EdgeColor',[.4,.3,1],'linewidth',2);
h2=rectangle('position',[0.5 0.5+nlev (nreg-regstart+1)*nhr nlev]);
set(h2,'EdgeColor',[.4,.3,1],'linewidth',2);
h3=rectangle('position',[0.5 0.5+(nlev*2) (nreg-regstart+1)*nhr nlev]);
set(h3,'EdgeColor',[.4,.3,1],'linewidth',2);
if nlev > 6
   h4=rectangle('position',[0.5 0.5+(nlev*3) (nreg-regstart+1)*nhr 7]);
   set(h4,'EdgeColor',[.4,.3,1],'linewidth',2);
else
   h4=rectangle('position',[0.5 0.5+(nlev*3) (nreg-regstart+1)*nhr nlev]);
   set(h4,'EdgeColor',[.4,.3,1],'linewidth',2);
end
if nvar > 4
   if nlev > 6
      h5=rectangle('position',[0.5 0.5+(nlev*3)+7 (nreg-regstart+1)*nhr 1]);
      set(h5,'EdgeColor',[.4,.3,1],'linewidth',2);
   else
      h5=rectangle('position',[0.5 0.5+(nlev*3)+8 (nreg-regstart+1)*nhr 1]);
      set(h5,'EdgeColor',[.4,.3,1],'linewidth',2);
   end
end
if nvar > 4
   if nlev > 6
      v1=rectangle('position',[0.5 0.5 nhr nlev*(nvar-2)+8]);
      set(v1,'EdgeColor',[.4,.3,1],'linewidth',2);
      v2=rectangle('position',[0.5+nhr 0.5 nhr nlev*(nvar-2)+8]);
      set(v2,'EdgeColor',[.4,.3,1],'linewidth',2);
      v3=rectangle('position',[0.5+(nhr*2) 0.5 nhr nlev*(nvar-2)+8]);
      set(v3,'EdgeColor',[.4,.3,1],'linewidth',2);
      v4=rectangle('position',[0.5+(nhr*3) 0.5 nhr nlev*(nvar-2)+8]);
      set(v4,'EdgeColor',[.4,.3,1],'linewidth',2);
   else
      v1=rectangle('position',[0.5 0.5 nhr nlev*(nvar-1)+1]);
      set(v1,'EdgeColor',[.4,.3,1],'linewidth',2);
      v2=rectangle('position',[0.5+nhr 0.5 nhr nlev*(nvar-1)+1]);
      set(v2,'EdgeColor',[.4,.3,1],'linewidth',2);
      v3=rectangle('position',[0.5+(nhr*2) 0.5 nhr nlev*(nvar-1)+1]);
      set(v3,'EdgeColor',[.4,.3,1],'linewidth',2);
      v4=rectangle('position',[0.5+(nhr*3) 0.5 nhr nlev*(nvar-1)+1]);
      set(v4,'EdgeColor',[.4,.3,1],'linewidth',2);
   end
else
   if nlev > 6
      v1=rectangle('position',[0.5 0.5 nhr nlev*(nvar-1)+7]);
      set(v1,'EdgeColor',[.4,.3,1],'linewidth',2);
      v2=rectangle('position',[0.5+nhr 0.5 nhr nlev*(nvar-1)+7]);
      set(v2,'EdgeColor',[.4,.3,1],'linewidth',2);
      v3=rectangle('position',[0.5+(nhr*2) 0.5 nhr nlev*(nvar-1)+7]);
      set(v3,'EdgeColor',[.4,.3,1],'linewidth',2);
      v4=rectangle('position',[0.5+(nhr*3) 0.5 nhr nlev*(nvar-1)+7]);
      set(v4,'EdgeColor',[.4,.3,1],'linewidth',2);
   else
      v1=rectangle('position',[0.5 0.5 nhr nlev*nvar]);
      set(v1,'EdgeColor',[.4,.3,1],'linewidth',2);
      v2=rectangle('position',[0.5+nhr 0.5 nhr nlev*nvar]);
      set(v2,'EdgeColor',[.4,.3,1],'linewidth',2);
      v3=rectangle('position',[0.5+(nhr*2) 0.5 nhr nlev*nvar]);
      set(v3,'EdgeColor',[.4,.3,1],'linewidth',2);
      v4=rectangle('position',[0.5+(nhr*3) 0.5 nhr nlev*nvar]);
      set(v4,'EdgeColor',[.4,.3,1],'linewidth',2);
   end
end
hold off
grid on
set(gca,'fontsize',5)
set(gca,'XTick',xt)
set(gca,'XTickLabel',num2str(xl))
set(gca,'YTick',yt)
set(gca,'YTickLabel',num2str(yl))
xlabel('Forecast Length (Days)','fontsize',11)
set(gca,'Clim',[-3,3])
colormap(cmap)
lcolorbar(Clab,'fontsize',6)
if nlev > 6 
   annotation('textbox',[0.03,0.749,0.1,0.1],'String','T','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   annotation('textbox',[0.03,0.505,0.1,0.1],'String','Z','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   annotation('textbox',[0.03,0.29,0.1,0.1],'String','W','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   annotation('textbox',[0.03,0.112,0.1,0.1],'String','Q','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   if nvar > 4
      annotation('textbox',[0.03,0.044,0.1,0.1],'String','SLP','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   end
else
   annotation('textbox',[0.03,0.755,0.1,0.1],'String','T','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   annotation('textbox',[0.03,0.545,0.1,0.1],'String','Z','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   annotation('textbox',[0.03,0.355,0.1,0.1],'String','W','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   annotation('textbox',[0.03,0.145,0.1,0.1],'String','Q','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   if nvar > 4
      annotation('textbox',[0.03,0.044,0.1,0.1],'String','SLP','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
   end
end
annotation('textbox',[0.165,0.875,0.1,0.1],'String','N. Hem','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
annotation('textbox',[0.34,0.875,0.1,0.1],'String','S. Hem','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
annotation('textbox',[0.51,0.875,0.1,0.1],'String','Tropics','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
annotation('textbox',[0.69,0.875,0.1,0.1],'String','N. Amr','EdgeColor','none','HorizontalAlignment','center','fontsize',11)
print(num,'-dpng',[sat,instr,'_',lab,'_score'])

end
