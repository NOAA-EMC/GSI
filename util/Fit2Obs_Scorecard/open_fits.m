function [Fit]=open_fits(filen,nvar,nlev,nreg,nhr)
num=fopen(filen,'r','b');
Fit=fread(num,'float');
fclose(num);
Fit=reshape(Fit,[11,nvar,nlev,nreg,nhr]);
end
