#To simulate data for dynamic factor analysis model with auto- and cross-regressions for 2 factors, 2lags (0 and 1 lag) each
#for data generation with initial time point explicity written into model
#source("C:/Documents and Settings/Diane Losardo/Desktop/CSproj/Master/CubicSpline/CubicSpline/dfasim_initialcond.R")

# innov z residuals e
a=matrix(0,ntt,ne)
y=matrix(0,ntt,ny)
x=matrix(0,ntt,nx)
yall=matrix(0,ntt*np,ny)
all = matrix(0,nt*np,ne)
yntonly = matrix(0,nt*np,ny)

for (i in 1:np){
	if (INIT){
		if (model==3){
			aStat = rnorm(1)%*%P0sStat
				
		} else{
			init = t(a0)+rnorm(ne)%*%P0s
			a[1,1:ne]=t(init)
			etmp=t(rnorm(ny)%*%Us)
			ytmp=Z%*%a[1,1:ne]+etmp+d
			y[1,1:ny]=ytmp
			}
		} #end INIT
	if (trueI==4){
		if (sdys){
			#y[1,1:ny]=t(sdYall[i,])
			if (model==3){
				a[1,1]=aStat
				a[1,2:ne]=t(sdXall[i,])
			} else{
				a[1,1:ne]=t(sdXall[i,])
				}
			etmp=t(rnorm(ny)%*%Us)
			ytmp=Z%*%a[1,1:ne]+etmp+d
			y[1,1:ny]=ytmp
			}
		}
	for (t in 2:ntt){
		ztmp=t(rnorm(ne)%*%Vs)
		etmp=t(rnorm(ny)%*%Us)
	    atmp=as.matrix(a[t-1,1:ne])
		atmp=T%*%atmp+ztmp+c
		a[t,1:ne]=t(atmp)
		ytmp=Z%*%atmp+etmp+d
		y[t,1:ny]=ytmp
		}
	yall[(1+(i-1)*ntt):(i*ntt),1:ny] = y
	yntonly[(1+(i-1)*nt):(i*nt),1:ny] = y[(ist:ntt),1:ny]
	all[ (1+(i-1)*nt):(i*nt),1:ne] = a[(ist:ntt),1:ne]
	}
###yntonly structure - each person's data going from t=1 to t=nt stacked into rows, across the ny indicators

#Transforming to wide format for complete data set
Wide1 = NULL
All1=NULL
for (i in 1:np){
	timet = NULL
	newt = NULL
	newt2 = NULL
	newt3 = NULL
	newall= NULL
	for (t in 1:nt){
		personi = yntonly[t+(nt*(i-1)),]
		truei = all[t+(nt*(i-1)),1]
		truei2= all[t+(nt*(i-1)),2]
		if (model==3){
			if (sdys){
			truei3 = all[t+(nt*(i-1)),3]
		}
		}
		timet = c(timet,personi)
		newt = c(newt,truei)
		newt2 = c(newt2,truei2)
		if (model==3){
			if (sdys){
			newt3=c(newt3,truei3)
			newt3=as.matrix(t(newt3))
		}
		}
		timet = as.matrix(t(timet))
		newt = as.matrix(t(newt))
		newt2 = as.matrix(t(newt2))
		newall=c(newt,newt2)
		if (model==3){
			if (sdys){
			newall=c(newall,newt3)
			}
			}
	}
	Wide1 = rbind(Wide1,timet)
	All1 = rbind(All1,newall)
}

