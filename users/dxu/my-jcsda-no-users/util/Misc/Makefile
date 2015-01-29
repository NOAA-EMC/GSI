FCP           = mpif90
FCS           = ifort
FFLAGS        = -O3 -xHOST -assume byterecl -warn all -implicitnone -traceback -openmp 
FREEFLAG      = -FR
OMPFLAG       = 
COREINC       = /scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/incmod
CORELIB       = /scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/lib
INCsigio      = $(COREINC)/sigio_4
INCsfcio      = $(COREINC)/sfcio_4
EXECdir	      = .

.SUFFIXES: .o .f .f90 .F90

all: chgdatesig chgdatesfc chgdate

chgdatesig:
	$(FCS) $(FFLAGS) -I $(INCsigio) -o $(EXECdir)/chgdatesig.x chgdatesig.f -L$(CORELIB) -lsp_4 -lsigio_4

chgdatesfc:
	$(FCS) $(FFLAGS) -I $(INCsfcio) -o $(EXECdir)/chgdatesfc.x chgdatesfc.f -L$(CORELIB) -lsp_4 -lsfcio_4

chgdate:
	$(FCS) $(FFLAGS) -I $(INCsigio) -I $(INCsfcio) -o $(EXECdir)/chgdate.x chgdate.f90 -L$(CORELIB) -lsp_4 -lsigio_4 -lsfcio_4

clean:
	rm -f  *.o *.mod *.x 

.f90.o:
	$(FCS) $(FFLAGS) -c $< 

.F90.o:
	$(FCS) $(FFLAGS) -c $< 

.f.o:
	$(FCS) $(FFLAGS) -free -c $< 

%.o : %.mod
