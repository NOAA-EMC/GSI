import numpy as np
import sys

# utility to create vlocal_eig.dat for modulated ensemble model-space
# vertical localization in EnKF

if len(sys.argv) < 4:
    sys.stdout.write('python create_vlocal_eig.py <cutoff> <thresh> <hyblev_file>\n')
    sys.stdout.write('<cutoff> is vertical localization cutoff in scale heights\n')
    sys.stdout.write('<thresh> is percent variance explained threshold\n')
    sys.stdout.write('<hyblev_file> is global_hyblev.l##.txt file\n')
    sys.stdout.write('eigenvectors written to vlocal_eig.dat\n')
    raise SystemExit

# read in localization cutoff distance in (units on lnp)
cutoff = float(sys.argv[1])
# read in threshold for truncating eigenspace of localization matrix (95 = 95% var explained)
thresh = 0.01*float(sys.argv[2])
# read in hybrid levels (hyblevs file from fix/fix_am)
siglev = sys.argv[3]

# get ak,bk from hyblevs file
siglev_data = np.loadtxt(siglev)
nlevs = int(siglev_data[0,1]-1)
ak = siglev_data[1:nlevs+2,0]
bk = siglev_data[1:nlevs+2,1]

# constants
rd = 2.8705e+2
cp = 1.0046e+3
kap = rd/cp
kapr = cp/rd
kap1 = kap + 1.0

# localization functions.
def localization(r):
    r = np.clip(r,1.e-13,1.)
    twor = 2.*r
    # Gaspari-Cohn polynomial.
    taper1 = np.where(r <= 0.5, -(1./4.)*twor**5+(1./2.)*twor**4+(5./8.)*twor**3-(5./3.)*twor**2+1, 0.)
    cond1 = r > 0.5; cond2 = r < 1.0
    taper = np.where(np.logical_and(cond1,cond2),
    (1./12.)*twor**5-(1./2.)*twor**4+(5./8.)*twor**3+(5./3.)*twor**2-5.*twor+4.-(2./3.)*(1./twor), taper1)
    # Gaussian approx to GC
    #taper = np.exp(-(r**2/0.15)) # Gaussian
    return taper

# set mean surface pressure (has to be a global constant)
psgmean = 1.e5

pressimn = np.empty((nlevs+1),'d')  # interface pressure
presslmn = np.empty((nlevs),'d')  # mid-layer pressure
for k in range(nlevs+1):
    pressimn[k] = ak[k] + bk[k]*psgmean
for k in range(nlevs):
    # phillips vertical interpolation from guess_grids.F90 in GSI (used for global model)
    presslmn[k] = ((pressimn[k]**kap1-pressimn[k+1]**kap1)/(kap1*(pressimn[k]-pressimn[k+1])))**kapr
    # simple average of interface pressures (used by fv3_regional in GSI)
    #presslmn[k] = 0.5*(pressimn[k]+pressimn[k+1])
    # linear in logp interpolation from interface pressures
    #presslmn[k] = np.exp(0.5*(np.log(pressimn[k])+np.log(pressimn[k+1])))
    print k,presslmn[k]
logp = -np.log(presslmn) # (ranges from -2 to -11)


covlocal = np.zeros((nlevs,nlevs),'d')
for j in range(nlevs):
    covlocal[j,:] = localization(abs(logp-logp[j])/cutoff)

#import matplotlib.pyplot as plt
#plt.figure(1)
#imgplot=plt.imshow(covlocal)
#plt.colorbar()

evals,eigs=np.linalg.eigh(covlocal)
evalsum = evals.sum(); neig = 0
evals = np.where(evals > 1.e-10, evals, 1.e-10)
frac = 0.0
while frac < thresh:
    frac = evals[nlevs-neig-1:nlevs].sum()/evalsum
    neig += 1
print 'neig = ',neig
zz = (eigs*np.sqrt(evals/frac)).T
#print evals
f = open('vlocal_eig.dat','w')
f.write('%s %s %s\n' % (neig,thresh,cutoff))
print 'rescaled eigenvalues'
eigsum = 0.
for j in range(neig):
    f.write('%s\n' % evals[nlevs-j-1])
    print j+1,evals[nlevs-j-1]/frac
    eigsum += evals[nlevs-j-1]/frac
    for k in range(nlevs):
        f.write('%s\n' % zz[nlevs-j-1,k])
f.close()
print 'sum of scaled truncated eigvals should equal sum of original evals'
print '(difference below should be nearly zero)'
print np.abs(eigsum-evals.sum())

# check data
f = open('vlocal_eig.dat','r')
evals2 = np.zeros(neig,np.float)
evecs2 = np.zeros((neig,nlevs),np.float)
f.readline()
for j in range(neig):
    evals2[j] = float(f.readline())
#   print j,evals2[j]/frac
    for k in range(nlevs):
        evecs2[j,k] = float(f.readline())
# this should be a diagonal matrix with eigvals on diagonal
covlocal2 = np.dot(evecs2,evecs2.T)
print 'diagonal elements of scaled dot(E,E^T), should be scaled evals'
print np.diag(covlocal2)
mask = np.ones(covlocal2.shape, dtype=bool)
np.fill_diagonal(mask, 0)
print 'max/min off diagonal elements (should be zero)',covlocal2[mask].max(),covlocal2[mask].min()
# this should be the (truncated) localization matrix
covlocal2 = np.dot(evecs2.T,evecs2)
#print covlocal2.shape
print 'diagonal of localization matrix (should be ones)'
print np.diag(covlocal2)
#print covlocal2[nlevs/2,:]

#plt.figure(2)
#plt.plot(covlocal2[nlevs/2,:],'r')
#plt.plot(covlocal[nlevs/2,:],'k')
#plt.xlim(0,nlevs-1)

#z = zz[nlevs-neig:nlevs,:]
#print z[-1].min(), z[-1].max()

#plt.figure(3)
#imgplot=plt.imshow(zz)
#plt.colorbar()

# plot the 1st eig vector
#plt.figure(4)
##plt.plot(-z[-1],np.arange(nlevs))
#plt.plot(-z[-1],0.01*presslmn)
##plt.semilogy(-z[-1],0.01*presslmn,basey=2)
##plt.ylim([0,nlevs])
#plt.ylim(1000,0)
##yticks = [1000,850,700,500,300,200,100,70,50,30,10]
##yticklabels = ['%s' % p for p in yticks]
##plt.yticks(yticks,yticklabels)
#plt.xlim(-0.25,1.25)
#plt.ylabel('Pressure (hPa)')
#plt.xlabel('Eigenvector')
#plt.axvline(0,color='k')
#for k in range(nlevs):
#    plt.axhline(0.01*presslmn[k],color='k',linestyle='dotted')
#plt.title('First Eigenvector of Vertical Localization Matrix')
##plt.grid(True)
#plt.savefig('eig1.png')
#
#plt.show()
