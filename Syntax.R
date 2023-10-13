library(orcutt)
library(faraway)
library(lmtest)
library(plm)

###=========================================================================###
data = read.csv(choose.files(),sep=',',header=T)
str(data)
head(data)
colnames(data) = c("Lembaga","Tahun","Y1","X1","X2","X3","X4","X5","X6")
data=data[,-7]

#Uji Chow
fit.pooled = plm(Y1~X1+X2+X3+X4+X5+X6,data=data,model="pooling",index=c("Lembaga","Tahun"))
fit.fixed = plm(Y1~X1+X2+X3+X4+X5+X6,data=data,model="within",index=c("Lembaga","Tahun"))
pFtest(fit.fixed,fit.pooled)  
#p-value = 0.004304; H0 ditolak maka model didapatkan adalah FEM
#p-value = 0.002152; H0 ditolak maka model didapatkan adalah FEM
#p-value = 0.8588
#p-value = 0.8843; H0 diterima maka model didapatkan adalah CEM
#p-value = 0.7756; H0 diterima maka model CEM

#Uji Haussman
fit.fixed = plm(Y1~X1+X2+X3,data=data,model="within",index=c("Lembaga","Tahun"))
fit.random = plm(Y1~X1+X2+X3,data=data,model="random",index=c("Lembaga","Tahun"))
phtest(fit.random,fit.fixed)
# p-value = 5.582e-06; H0 ditolak maka pilih model FEM


pcdtest(fit.fixed,test="lm")
pcdtest(fit.fixed,test="cd")
plmtest(fit.pooled,type="bp")
#p-value = 0.02531  ; REM
#p-value = 0.08796

fit.random = plm(Y1~X2,data=data,model="random",effect="twoways",random.method = "amemiya")
ranef(fit.random)
summary(fit.random)

femdum=lm(Y1~.,data=data)

#============ ASUMSI =================#
femdum = lm(Y1~X1+X2+X3+factor(Lembaga),data=data)
summary(femdum)
summary(fit.fixed)
#Heteroskedastisitas
	bptest(femdum,data=data1,studentize=T)
	#p-value =  0.8752,  H0 diterima 
	#data Homoskedastisitas
	#p-value = 0.8662

#Autokorelasi
	dwtest(femdum)
	#p-value = 0.8823, H0 diterima
	#Tidak Terjadi Autokorelasi 
	#p-value = 0.5965

#Multikolinearitas
	library(car)
	vif(femdum)
	#VIF<10 Tidak Terjadi Multikolinearitas 

#UJI LINEARITAS
	resettest(femdum,type="fitted",data=data)
	#p-value = 0.3568, H0 diterima
	#linier

#Normalitas
	residual=resid(femdum)
	ks.test(residual,"pnorm",mean(residual,sd(residual)))
	#p-value = 0.002742, H0 ditolak

library(sandwich)
coeftest(femdum,vcov=NeweyWest(femdum))

#lain-lain
fixed <- lm(femdum)
library(sandwich)
coeftest(fixed, vcov=sandwich)
anova(femdum)






