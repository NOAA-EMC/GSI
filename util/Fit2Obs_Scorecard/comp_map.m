function Hmap=comp_map(nvar,nlev,nreg,nhr,stat,Err,Fit)
Hmap=zeros(nvar,nlev,nreg,nhr);
%negative means exp is better than ctl
%positive means ctl is better than exp

for i=1:3
   for ivar=1:nvar
      for ilev=1:nlev
         for ireg=1:nreg
            for ihr=1:nhr
               max_int=Err(i,ivar,ilev,ireg,ihr)+Fit(stat,ivar,ilev,ireg,ihr);
               min_int=Fit(stat,ivar,ilev,ireg,ihr)-Err(i,ivar,ilev,ireg,ihr);
               if (max_int<0)
                  if abs(Fit(stat+5,ivar,ilev,ireg,ihr))< abs(Fit(stat+7,ivar,ilev,ireg,ihr))
                     Hmap(ivar,ilev,ireg,ihr)=-i;
                  else
                     Hmap(ivar,ilev,ireg,ihr)=i;
                  end
               elseif (min_int>0) 
                  if Fit(stat+5,ivar,ilev,ireg,ihr)> abs(Fit(stat+7,ivar,ilev,ireg,ihr))
                     Hmap(ivar,ilev,ireg,ihr)=i;
                  else
                     Hmap(ivar,ilev,ireg,ihr)=-i;
                  end
               end
            end
         end
      end
   end
end


end
