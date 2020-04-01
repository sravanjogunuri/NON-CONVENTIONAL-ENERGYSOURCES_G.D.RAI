
        
#Reading the data Spectral irradiance data from file named "ASTMG173.csv"
#ASTM G173-03 Reference Spectra Derived from SMARTS v. 2.9.2
data<-read.csv("ASTMG173.csv")

#To display attribute or column names of data
names(data)

#Extracting first column Wvlgthnm(wavelength_nm)from data 
#and assigning it to lambda_nm
lambda_nm<-data[,1]

#Extracting first column Extra terrestrial radiation from data 
#and assigning it to irrad
irrad<-data[,2]

#calculating the number of rows present in the data and displaying
crow<-nrow(data)
crow

#integrate spectral radiance, Ps to obtain insolation,L: 
#to calculate the area under the curve
#Trapezoidal integration is used
#yint=yint+dL*(x1+x2)/2
yint<-0
insol<-0
for(i in 2:crow){
        yint <- yint +(lambda_nm[i]-lambda_nm[i-1])*(irrad[i]+irrad[i-1])/2                
        insol[i]<-yint
       
        }
#insol
#transposing the rows to columns for plotting graph
insol<-t(insol)
#insol



#plot(data)
#plotting graph between lambda_nm versus irradiance

plot(lambda_nm,irrad,axes=T,ylim=c(0,2.2),xlab="lambda_nm",ylab="irrad_w/m2/nm",
     type="l",col="black",las=1,main="Spectral Wavelength Vs 
     Solar Radiation spectrum & Insolation")

#adding a new graph(insolation) to already existing irradiance graph
par(new=TRUE)

#adjusting the margins so that the axis on all sides be in plotting window
par(mar=c(5,4,4,2)+c(0,0,0,0))

#plotting graph between lambda_nm versus insolation on existing plot
plot(lambda_nm,insol,type="l",xlab="", ylab="", ylim=c(0,1500),axes=F,col="red")

#initializing the secondary axis
mtext("Insolation_w/m2",side=4,col="black" ,line=0) 
axis(4, ylim=c(0,1500), col="black",col.axis="black",las=0)

