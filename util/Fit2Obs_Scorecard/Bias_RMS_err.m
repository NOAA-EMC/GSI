function [Bias_err,RMS_err]=Bias_RMS_err(nhr,nvar,nlev,nreg,Fit)
Bias_err=zeros(3,nvar,nlev,nreg,nhr);
Stdev_err=Bias_err;
al95=1.96;
al99=2.576;
al999=3.291;

for ihr=1:nhr
   for ivar=1:nvar
      for ilev=1:nlev
         for ireg=1:nreg
            Bias_err(1,ivar,ilev,ireg,ihr)=al95*Fit(3,ivar,ilev,ireg,ihr)/(sqrt(Fit(5,ivar,ilev,ireg,ihr)));
            Bias_err(2,ivar,ilev,ireg,ihr)=al99*Fit(3,ivar,ilev,ireg,ihr)/(sqrt(Fit(5,ivar,ilev,ireg,ihr)));
            Bias_err(3,ivar,ilev,ireg,ihr)=al999*Fit(3,ivar,ilev,ireg,ihr)/(sqrt(Fit(5,ivar,ilev,ireg,ihr)));

            RMS_err(1,ivar,ilev,ireg,ihr)=al95*Fit(4,ivar,ilev,ireg,ihr)/(sqrt(Fit(5,ivar,ilev,ireg,ihr)));
            RMS_err(2,ivar,ilev,ireg,ihr)=al99*Fit(4,ivar,ilev,ireg,ihr)/(sqrt(Fit(5,ivar,ilev,ireg,ihr)));
            RMS_err(3,ivar,ilev,ireg,ihr)=al999*Fit(4,ivar,ilev,ireg,ihr)/(sqrt(Fit(5,ivar,ilev,ireg,ihr)));
          end
       end
   end
end

end
