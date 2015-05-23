library(TeachingDemos)
nt = 20
np = 100
#Saving factor scores for Stationary Model
allSx1 = matrix(NA,nrow(all),1)
allSx2 = matrix(NA,nrow(all),1)
allSx1[1:(nrow(all)),] = all[,1]
allSx2[1:(nrow(all)),] = all[,2]

plot(1:nt,allSx1[1:nt,1],type="n",   ylim=c(min(allSx1),max(allSx1)))#ylim=c(-10,10))
for (i in 1:np){
	lines(1:nt,allSx1[(1+(i-1)*nt):(i*nt),1])
}
	lines(1:nt,allSx1[1:nt,1])
	lines(1:nt,allSx1[951:1000,1])


dirx1Stat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1Stat.dat"
#write(allSx1,dirx1Stat,ncolumns=1)
dirx2Stat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2Stat.dat"
#write(allSx2,dirx2Stat,ncolumns=1)

#Saving factor scores for Nonstationary Model
allNSx1 = matrix(NA,nrow(all),1)
allNSx2 = matrix(NA,nrow(all),1)
allNSx1[1:(nrow(all)),] = all[,1]
allNSx2[1:(nrow(all)),] = all[,2]

dirx1NStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1NStat.dat"
#write(allNSx1,dirx1NStat,ncolumns=1)
dirx2NStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2NStat.dat"
#write(allNSx2,dirx2NStat,ncolumns=1)

#Saving factor scores for Partially Stationary Model
allPSx1 = matrix(NA,nrow(all),1)
allPSx2 = matrix(NA,nrow(all),1)
allPSx3 = matrix(NA,nrow(all),1)
allPSx1[1:(nrow(all)),] = all[,1]
allPSx2[1:(nrow(all)),] = all[,2]
allPSx3[1:(nrow(all)),] = all[,3]

dirx1PStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1PStat.dat"
#write(allPSx1,dirx1PStat,ncolumns=1)
dirx2PStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2PStat.dat"
#write(allPSx2,dirx2PStat,ncolumns=1)
dirx3PStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx3PStat.dat"
#write(allPSx3,dirx3PStat,ncolumns=1)

#Saving factor scores for Nonstationary Model (mild)
allNSx1 = matrix(NA,nrow(all),1)
allNSx2 = matrix(NA,nrow(all),1)
allNSx1[1:(nrow(all)),] = all[,1]
allNSx2[1:(nrow(all)),] = all[,2]

dirx1NStatMild = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1NStatMild.dat"
#write(allNSx1,dirx1NStatMild,ncolumns=1)
dirx2NStatMild = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2NStatMild.dat"
#write(allNSx2,dirx2NStatMild,ncolumns=1)


#reading in factor scores
allSx1 = read.table(dirx1Stat)
allSx2 = read.table(dirx2Stat)
allNSx1 = read.table(dirx1NStat)
allNSx2 = read.table(dirx2NStat)
#allPSx1 = read.table(dirx1PStat)
#allPSx2 = read.table(dirx2PStat)
#allPSx3 = read.table(dirx3PStat)
allNSx1Mild = read.table(dirx1NStatMild)
allNSx2Mild = read.table(dirx2NStatMild)

pdf(file = paste("C:/Documents and Settings/Diane Losardo/Desktop/dissertation/latex/TSplot1.pdf",sep="",collapse=""))

#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
#layout(matrix(c(1,2,0,3,4,0,5,6,7), 3, 3, byrow = TRUE))
#layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
#layout(rbind(c(4,1,1,5), c(2,2,3,3)))
#layout(rbind(c(1,1,2,2), c(3,3,4,4)))

split.screen(c(3,1))
split.screen(c(1,2),1)
split.screen(c(1,2),2) 
split.screen(c(1,2),3)
#split.screen(c(1,3),3) 
par(cex.main=.8)

screen(4)
par(mar=c(2,4,2,2))
plot(1:nt,allSx1[1:nt,1],type="n", main="Model 1: Stationary AR(1)", ylab="X1",xlab="Time",ylim=c(min(allSx1[,1]),max(allSx1[,1])))
for (i in 1:np){
	ytemp = (allSx1[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}

screen(5)
par(mar=c(2,4,2,2))
plot(1:nt,allSx2[1:nt,1],type="n", main="Model 1: Stationary AR(1)", ylab="X2",xlab="Time",ylim=c(min(allSx2[,1]),max(allSx2[,1])))
for (i in 1:np){
	ytemp = (allSx2[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}

screen(6)
par(mar=c(2,4,2,2))
plot(1:nt,allNSx1[1:nt,1],type="n",main="Model 2: Moderately Nontationary AR(1)",ylab="X1",xlab="Time",ylim=c(min(allNSx1[,1]),max(allNSx1[,1])))
for (i in 1:np){
	ytemp = (allNSx1[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}

screen(7)
par(mar=c(2,4,2,2))
plot(1:nt,allNSx2[1:nt,1],type="n",main="Model 2: Moderately Nonstationary AR(1)",ylab="X2",xlab="Time", ylim=c(min(allNSx2[,1]),max(allNSx2[,1])))
for (i in 1:np){
	ytemp = (allNSx2[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}

screen(8)
par(mar=c(2,4,2,2))
plot(1:nt,allNSx1Mild[1:nt,1],type="n",main="Model 2: Mildly Nontationary AR(1)",ylab="X1",xlab="Time",ylim=c(min(allNSx1Mild[,1]),max(allNSx1Mild[,1])))
for (i in 1:np){
	ytemp = (allNSx1Mild[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}

screen(9)
par(mar=c(2,4,2,2))
plot(1:nt,allNSx2Mild[1:nt,1],type="n",main="Model 2: Mildly Nonstationary AR(1)",ylab="X2",xlab="Time", ylim=c(min(allNSx2Mild[,1]),max(allNSx2Mild[,1])))
for (i in 1:np){
	ytemp = (allNSx2Mild[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}


par(xpd=NA)
xaxisv1 = -.05
yaxisv1 = 1
xaxisv2 = -.05
yaxisv2 = .67
xaxisv3 = -.05
yaxisv3 = .34

tmp1 <- cnvrt.coords(xaxisv1, yaxisv1, input='tdev' )$usr # get location for marker
tmp2 <- cnvrt.coords(xaxisv2, yaxisv2, input='tdev' )$usr # get location for marker
tmp3 <- cnvrt.coords(xaxisv3, yaxisv3, input='tdev' )$usr # get location for marker

legend(tmp1$x,tmp1$y,cex=1.5,"(A)",bty="n")
legend(tmp2$x,tmp2$y,cex=1.5,"(B)",bty="n")
legend(tmp3$x,tmp3$y,cex=1.5,"(C)",bty="n")


dev.off()


screen(8)
par(mar=c(2,4,2,2))
plot(1:nt,allPSx1[1:nt,1],type="n",main="Model 3: Stationary AR(1)",ylab="X1",xlab="Time",ylim=c(min(allPSx1[,1]),max(allPSx1[,1])))
for (i in 1:np){
	ytemp = (allPSx1[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}

screen(9)
par(mar=c(2,4,2,2))
plot(1:nt,allPSx2[1:nt,1],type="n",main="Model 3: Local Level",ylab="X_INT",xlab="Time",ylim=c(min(allPSx2[,1]),max(allPSx2[,1])))
for (i in 1:np){
	ytemp = (allPSx2[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}
screen(10)
par(mar=c(2,4,2,2))
plot(1:nt,allPSx3[1:nt,1],type="n",main="Model 3: Local Slope",ylab="X_SLP",xlab="Time",ylim=c(min(allPSx3[,1]),max(allPSx3[,1])))
for (i in 1:np){
	ytemp = (allPSx3[(1+(i-1)*nt):(i*nt),1])
	#ytemp = (ytemp - mean(ytemp,na.rm=T))/sd(ytemp,na.rm=T)
	lines(1:nt,ytemp,lwd=1)
}
#wide = NULL
#for (i in 1:np){
#	personi = allSx1[(1+(nt*(i-1))):(i*nt),]
#	wide = rbind(wide,personi)
#}

x1S.ts = ts(allSx1)
pacf(x1S.ts)
x2S.ts = ts(allSx2,start=1,freq=nt)

x1NS.ts = ts(allNSx1,start=1,freq=nt)
x2NS.ts = ts(allNSx2, start=1, freq=nt)

x1PS.ts = ts(allPSx1,start=1,freq=nt)
x2PS.ts = ts(allPSx2, start=1, freq=nt)
x3PS.ts = ts(allPSx3,start=1,freq=nt)


#plot(x1S.ts, x2S.ts) #Here is the monthly scatterplot
#Create aggregated annual time series
#acf(x1S.ts)
#x1S.ts2 = as.vector(aggregate(x1S.ts))
#x2S.ts2 = as.vector(aggregate(x2S.ts))
#plot(x1S.ts2)
#pacf(x2S.ts2)

#Saving factor scores for Stationary Model for 1 Time Series
np=1
allSx1np1 = matrix(NA,nt,1)
allSx2np1 = matrix(NA,nt,1)
allSx1np1[1:nt,] = allSx1[1:nt,1]
allSx2np1[1:nt,] = allSx2[1:nt,1]
pacf(allSx1np1)
pacf(allSx2np1)


dirx1Stat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1Stat_1TS.dat"
#write(allSx1np1,dirx1Stat,ncolumns=1)
dirx2Stat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2Stat_1TS.dat"
#write(allSx2np1,dirx2Stat,ncolumns=1)

#Saving factor scores for Nonstationary Model
allNSx1np1 = matrix(NA,nt,1)
allNSx2np1 = matrix(NA,nt,1)
allNSx1np1[1:nt,] = allNSx1[1:20,1]
allNSx2np1[1:nt,] = allNSx2[1:20,1]
pacf(allNSx1np1)
pacf(allNSx2np1)

dirx1NStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1NStat_1TS.dat"
#write(allNSx1np1,dirx1NStat,ncolumns=1)
dirx2NStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2NStat_1TS.dat"
#write(allNSx2np1,dirx2NStat,ncolumns=1)

#Saving factor scores for Mild Nonstationary Model
allNSx1Mild = matrix(NA,nt,1)
allNSx2Mild = matrix(NA,nt,1)
allNSx1Mild[1:nt,] = all[1:nt,1]
allNSx2Mild[1:nt,] = all[1:nt,2]

dirx1NStatMild = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1NStat_1TSMild.dat"
#write(allNSx1Mild,dirx1NStatMild,ncolumns=1)
dirx2NStatMild = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2NStat_1TSMild.dat"
#write(allNSx2Mild,dirx2NStatMild,ncolumns=1)

#Saving factor scores for Partially Stationary Model
#allPSx1 = matrix(NA,nrow(all),1)
#allPSx2 = matrix(NA,nrow(all),1)
#allPSx3 = matrix(NA,nrow(all),1)
#allPSx1[1:(nrow(all)),] = all[,1]
#allPSx2[1:(nrow(all)),] = all[,2]
#allPSx3[1:(nrow(all)),] = all[,3]

#dirx1PStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx1PStat_1TS.dat"
#write(allPSx1,dirx1PStat,ncolumns=1)
#dirx2PStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx2PStat_1TS.dat"
#write(allPSx2,dirx2PStat,ncolumns=1)
#dirx3PStat = "C:/Documents and Settings/Diane Losardo/Desktop/dissertation/plots/FSx3PStat_1TS.dat"
#write(allPSx3,dirx3PStat,ncolumns=1)

#reading in factor scores
allSx1 = read.table(dirx1Stat)
allSx2 = read.table(dirx2Stat)
allNSx1 = read.table(dirx1NStat)
allNSx2 = read.table(dirx2NStat)
allNSx1Mild = read.table(dirx1NStatMild)
allNSx2Mild = read.table(dirx2NStatMild)
#allPSx1 = read.table(dirx1PStat)
#allPSx2 = read.table(dirx2PStat)
#allPSx3 = read.table(dirx3PStat)
pdf (file = paste("C:/Documents and Settings/Diane Losardo/Desktop/dissertation/latex/ACFplot1.pdf",sep="",collapse=""))

split.screen(c(3,1))
split.screen(c(1,2),1)
split.screen(c(1,2),2) 
split.screen(c(1,2),3) 
par(cex.main=.8)


screen(4)
par(mar=c(3,4,3,3))
acf(allSx1,main="Model 1: Stationary AR(1)",ylab="X1",xlab="Time")
screen(5)
par(mar=c(3,4,3,3))
acf(allSx2,main="Model 1: Stationary AR(1)",ylab="X2",xlab="Time")
screen(6)
par(mar=c(3,4,3,3))
acf(allNSx1,main="Model 2: Moderately Nonstationary AR(1)",ylab="X1",xlab="Time")
screen(7)
par(mar=c(3,4,3,3))
acf(allNSx2,main="Model 2: Moderately Nonstationary AR(1)",ylab="X2",xlab="Time")
screen(8)
par(mar=c(3,4,3,3))
acf(allNSx1Mild,main="Model 2: Mildly Nonstationary AR(1)",ylab="X1",xlab="Time")
screen(9)
par(mar=c(3,4,3,3))
acf(allNSx2Mild,main="Model 2: Mildly Nonstationary AR(1)",ylab="X2",xlab="Time")

par(xpd=NA)
xaxisv1 = -.05
yaxisv1 = 1
xaxisv2 = -.05
yaxisv2 = .67
xaxisv3 = -.05
yaxisv3 = .34

tmp1 <- cnvrt.coords(xaxisv1, yaxisv1, input='tdev' )$usr # get location for marker
tmp2 <- cnvrt.coords(xaxisv2, yaxisv2, input='tdev' )$usr # get location for marker
tmp3 <- cnvrt.coords(xaxisv3, yaxisv3, input='tdev' )$usr # get location for marker

legend(tmp1$x,tmp1$y,cex=1.5,"(A)",bty="n")
legend(tmp2$x,tmp2$y,cex=1.5,"(B)",bty="n")
legend(tmp3$x,tmp3$y,cex=1.5,"(C)",bty="n")

dev.off()

screen(8)
par(mar=c(3,4,3,3))
acf(allPSx1,main="Model 3: Stationary AR(1)",ylab="X1",xlab="Time")
screen(9)
par(mar=c(3,4,3,3))
acf(allPSx2,main="Model 3: Local Level",ylab="X_INT",xlab="Time")
screen(10)
par(mar=c(3,4,3,3))
acf(allPSx3,main="Model 3: Local Slope",ylab="X_SLP",xlab="Time")

pdf (file = paste("C:/Documents and Settings/Diane Losardo/Desktop/dissertation/latex/PACFplot1.pdf",sep="",collapse=""))

split.screen(c(3,1))
split.screen(c(1,2),1)
split.screen(c(1,2),2) 
split.screen(c(1,2),3) 
par(cex.main=.8)

screen(4)
par(mar=c(3,4,3,3))
pacf(allSx1,main="Model 1: Stationary AR(1)",ylab="X1",xlab="Time")
screen(5)
par(mar=c(3,4,3,3))
pacf(allSx2,main="Model 1: Stationary AR(1)",ylab="X2",xlab="Time")
screen(6)
par(mar=c(3,4,3,3))
pacf(allNSx1,main="Model 2: Moderately Nonstationary AR(1)",ylab="X1",xlab="Time")
screen(7)
par(mar=c(3,4,3,3))
pacf(allNSx2,main="Model 2: Moderately Nonstationary AR(1)",ylab="X2",xlab="Time")
screen(8)
par(mar=c(3,4,3,3))
pacf(allNSx1Mild,main="Model 2: Mildly Nonstationary AR(1)",ylab="X1",xlab="Time")
screen(9)
par(mar=c(3,4,3,3))
pacf(allNSx2Mild,main="Model 2: Mildly Nonstationary AR(1)",ylab="X2",xlab="Time")

par(xpd=NA)
xaxisv1 = -.05
yaxisv1 = 1
xaxisv2 = -.05
yaxisv2 = .67
xaxisv3 = -.05
yaxisv3 = .34

tmp1 <- cnvrt.coords(xaxisv1, yaxisv1, input='tdev' )$usr # get location for marker
tmp2 <- cnvrt.coords(xaxisv2, yaxisv2, input='tdev' )$usr # get location for marker
tmp3 <- cnvrt.coords(xaxisv3, yaxisv3, input='tdev' )$usr # get location for marker

legend(tmp1$x,tmp1$y,cex=1.5,"(A)",bty="n")
legend(tmp2$x,tmp2$y,cex=1.5,"(B)",bty="n")
legend(tmp3$x,tmp3$y,cex=1.5,"(C)",bty="n")

dev.off()

screen(8)
par(mar=c(3,4,3,3))
pacf(allPSx1,main="Model 3: Stationary AR(1)",ylab="X1",xlab="Time")
screen(9)
par(mar=c(3,4,3,3))
pacf(allPSx2,main="Model 3: Local Level",ylab="X_INT",xlab="Time")
screen(10)
par(mar=c(3,4,3,3))
pacf(allPSx3,main="Model 3: Local Slope",ylab="X_SLP",xlab="Time")

###Used to be in MAIN

#number of time points, 1st i value, last i value, name where data is located, column where data is located,
#standardized?, y-axis label, x-axis label, title, lower limit for y-axis, upper limit for y-axis
plotsF(nt,1,20,yall,1,FALSE,"Time","Values","AR(1)= .7",min(yall),max(yall))
windows()
plotsF(nt,20,1,yall,1,FALSE,"Time","Values","AR(1)= -.7",min(yall),max(yall))
windows()
plotsF(nt,1,20,yall,1,FALSE,"Time","Values","AR(1)=1 (Random Walk)",min(yall),max(yall))
windows()
plotsF(nt,1,20,yall,1,FALSE,"Time","Values","AR(1)=1 (Random Walk with Drift)",min(yall),max(yall))

library(TeachingDemos)

pdf (file = paste("C:/Documents and Settings/Diane Losardo/Desktop/dissertation/latex/ProposalSlides/ARplots.pdf",sep="",collapse=""))

op <- par(mfrow = c(2,2), ## split region
           oma = c(0,1.5,1.5,0) + 0, ## create outer margin - bottom, left, top, right
          mar = c(4.5,1.75,3,1.75) + 0,## shrink some margins
		   cex.lab=1.5)  

xaxisv = .55
yaxisv = .34
xaxisv1 = .0005
yaxisv1 = 1
xaxisv2 = .48
yaxisv2 = 1
xaxisv3 = .0005
yaxisv3 = .52
xaxisv4 = .48
yaxisv4 = .52

#need to change population values before running each plot!
plotsF(nt,1,20,yall,1,FALSE,"","Time","AR(1): alpha = .7",min(yall),max(yall))
plotsF(nt,20,1,yall,1,FALSE,"","Time","AR(1): alpha = -.7",min(yall),max(yall))
plotsF(nt,1,20,yall,1,FALSE,"","Time","AR(1): alpha =1 (Random Walk)",min(yall),max(yall))
plotsF(nt,1,20,yall,1,FALSE,"","Time","AR(1): alpha =1 (Random Walk with Drift)",min(yall),max(yall))

par(xpd=NA)

tmp1 <- cnvrt.coords(xaxisv1, yaxisv1, input='tdev' )$usr # get location for marker
tmp2 <- cnvrt.coords(xaxisv2, yaxisv2, input='tdev' )$usr # get location for marker
tmp3 <- cnvrt.coords(xaxisv3, yaxisv3, input='tdev' )$usr # get location for marker
tmp4 <- cnvrt.coords(xaxisv4, yaxisv4, input='tdev' )$usr # get location for marker

legend(tmp1$x,tmp1$y,cex=1.5,"(A)",bty="n")
legend(tmp2$x,tmp2$y,cex=1.5,"(B)",bty="n")
legend(tmp3$x,tmp3$y,cex=1.5,"(C)",bty="n")
legend(tmp4$x,tmp4$y,cex=1.5,"(D)",bty="n")

par(op)

dev.off()