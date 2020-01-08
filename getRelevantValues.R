
		   
## Soil Specific
## Basinwide

#Data Preparation
setwd("C:/Users/BBarnhart.DOMAIN1/OneDrive - National Council for Air and Stream Improvement Inc/Projects/Other/Legacy/brisbane/7_choices")
out = read.csv("fulldata_2019-04-08.csv")

#transportcoeff = data.frame(out$N210_Napp,out$N180_Napp,out$N120_Napp,out$N060_Napp)
#names(Napp) <- c("N210","N180","N120","N060")

#Nloss = data.frame(out$N210_Nloss,out$N180_Nloss,out$N150_Nloss,out$N120_Nloss,out$N090_Nloss,out$N060_Nloss,out$N040_Nloss)
#names(Nloss) <- c("N210","N180","N150","N120","N090","N060","N040")

#GM = data.frame(out$N210_GM,out$N180_GM,out$N150_GM,out$N120_GM,out$N090_GM,out$N060_GM,out$N040_GM)
#names(GM) <- c("N210","N180","N150","N120","N090","N060","N040")


scen1 = data.frame(out$Cellno,out$N210_Nloss,out$N210_Nloss*out$Transport_coefficient,out$N210_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,210)
names(scen1) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")
scen2 = data.frame(out$Cellno,out$N180_Nloss,out$N180_Nloss*out$Transport_coefficient,out$N180_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,180)
names(scen2) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")
scen3 = data.frame(out$Cellno,out$N150_Nloss,out$N150_Nloss*out$Transport_coefficient,out$N150_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,150)
names(scen3) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")
scen4 = data.frame(out$Cellno,out$N120_Nloss,out$N120_Nloss*out$Transport_coefficient,out$N120_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,120)
names(scen4) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")
scen5 = data.frame(out$Cellno,out$N090_Nloss,out$N090_Nloss*out$Transport_coefficient,out$N090_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,90)
names(scen5) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")
scen6 = data.frame(out$Cellno,out$N060_Nloss,out$N060_Nloss*out$Transport_coefficient,out$N060_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,60)
names(scen6) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")
scen7 = data.frame(out$Cellno,out$N040_Nloss,out$N040_Nloss*out$Transport_coefficient,out$N040_GM,out$SolCls1m2,out$SolCls2m2,out$SolCls3m2,out$SolCls4m2,40)
names(scen7) <- c("cellno","loss","outlet_loss","gm",'sol1','sol2','sol3','sol4',"scenario")

combined <- rbind(scen1,scen2,scen3,scen4,scen5,scen6,scen7)

## Input Values 
cmatrix_soil = read.csv("nondominated_soil7choices.csv",header=T)

# Current Individual (Wrap in Loop)
for (indiv in 1:length(cmatrix_soil[,1])) {
	
	output_df = data.frame(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

	names(output_df) <- c('cellno','x','y','policy_choice','payment','gm','gm_and_payment','nloss','outlet_nloss','np_choice','np_gm','np_nloss','np_outlet_loss','sol1aream2','sol2aream2','sol3aream2','sol4aream2','pay_per_m2_sol1','pay_per_m2_sol2','pay_per_m2_sol3','pay_per_m2_sol4')
	
	for (i in 1:4020) { 
		incen1 = 0.0
		incen2 = cmatrix_soil[indiv,3]*scen2$sol1[i] + cmatrix_soil[indiv,4]*scen2$sol2[i] + cmatrix_soil[indiv,5]*scen2$sol3[i] + cmatrix_soil[indiv,6]*scen2$sol4[i]
		incen3 = cmatrix_soil[indiv,7]*scen3$sol1[i] + cmatrix_soil[indiv,8]*scen3$sol2[i] + cmatrix_soil[indiv,9]*scen3$sol3[i] + cmatrix_soil[indiv,10]*scen3$sol4[i]
		incen4 = cmatrix_soil[indiv,11]*scen4$sol1[i] + cmatrix_soil[indiv,12]*scen4$sol2[i] + cmatrix_soil[indiv,13]*scen4$sol3[i] + cmatrix_soil[indiv,14]*scen4$sol4[i]
		incen5 = cmatrix_soil[indiv,15]*scen5$sol1[i] + cmatrix_soil[indiv,16]*scen5$sol2[i] + cmatrix_soil[indiv,17]*scen5$sol3[i] + cmatrix_soil[indiv,18]*scen5$sol4[i]
		incen6 = cmatrix_soil[indiv,19]*scen6$sol1[i] + cmatrix_soil[indiv,20]*scen6$sol2[i] + cmatrix_soil[indiv,21]*scen6$sol3[i] + cmatrix_soil[indiv,22]*scen6$sol4[i]
		incen7 = cmatrix_soil[indiv,23]*scen7$sol1[i] + cmatrix_soil[indiv,24]*scen7$sol2[i] + cmatrix_soil[indiv,25]*scen7$sol3[i] + cmatrix_soil[indiv,26]*scen7$sol4[i]
		
		
		temp = max(scen1$gm[i]+incen1, scen2$gm[i]+incen2, scen3$gm[i]+incen3, scen4$gm[i]+incen4, scen5$gm[i]+incen5, scen6$gm[i]+incen6, scen7$gm[i]+incen7)
		temp_np = max(scen1$gm[i], scen2$gm[i],	scen3$gm[i], scen4$gm[i], scen5$gm[i], scen6$gm[i], scen7$gm[i])

		if (temp_np == scen1$gm[i]) { np_choice = 210; np_gm = scen1$gm[i]; np_outlet_nloss = scen1$outlet_loss[i]; np_nloss = scen1$loss[i] }
		if (temp_np == scen2$gm[i]) { np_choice = 180; np_gm = scen2$gm[i]; np_outlet_nloss = scen2$outlet_loss[i]; np_nloss = scen2$loss[i] }
		if (temp_np == scen3$gm[i]) { np_choice = 150; np_gm = scen3$gm[i]; np_outlet_nloss = scen3$outlet_loss[i]; np_nloss = scen3$loss[i] }
		if (temp_np == scen4$gm[i]) { np_choice = 120; np_gm = scen4$gm[i]; np_outlet_nloss = scen4$outlet_loss[i]; np_nloss = scen4$loss[i] }
		if (temp_np == scen5$gm[i]) { np_choice =  90; np_gm = scen5$gm[i]; np_outlet_nloss = scen5$outlet_loss[i]; np_nloss = scen5$loss[i] }
		if (temp_np == scen6$gm[i]) { np_choice =  60; np_gm = scen6$gm[i]; np_outlet_nloss = scen6$outlet_loss[i]; np_nloss = scen6$loss[i] }
		if (temp_np == scen7$gm[i]) { np_choice =  40; np_gm = scen7$gm[i]; np_outlet_nloss = scen7$outlet_loss[i]; np_nloss = scen7$loss[i] }
		

		if (temp == scen1$gm[i]+incen1) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i],210,incen1,scen1$gm[i],scen1$gm[i]+incen1,scen1$loss[i],scen1$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen1$sol1[i],scen1$sol2[i],scen1$sol3[i],scen1$sol4[i],0.0,0.0,0.0,0.0)
		if (temp == scen2$gm[i]+incen2) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i],180,incen2,scen2$gm[i],scen2$gm[i]+incen2,scen2$loss[i],scen2$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen2$sol1[i],scen2$sol2[i],scen2$sol3[i],scen2$sol4[i],cmatrix_soil[indiv,3],cmatrix_soil[indiv,4],cmatrix_soil[indiv,5],cmatrix_soil[indiv,6])
		if (temp == scen3$gm[i]+incen3) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i],150,incen3,scen3$gm[i],scen3$gm[i]+incen3,scen3$loss[i],scen3$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen3$sol1[i],scen3$sol2[i],scen3$sol3[i],scen3$sol4[i],cmatrix_soil[indiv,7],cmatrix_soil[indiv,8],cmatrix_soil[indiv,9],cmatrix_soil[indiv,10])
		if (temp == scen4$gm[i]+incen4) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i],120,incen4,scen4$gm[i],scen4$gm[i]+incen4,scen4$loss[i],scen4$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen4$sol1[i],scen4$sol2[i],scen4$sol3[i],scen4$sol4[i],cmatrix_soil[indiv,11],cmatrix_soil[indiv,12],cmatrix_soil[indiv,13],cmatrix_soil[indiv,14])
		if (temp == scen5$gm[i]+incen5) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i], 90,incen5,scen5$gm[i],scen5$gm[i]+incen5,scen5$loss[i],scen5$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen5$sol1[i],scen5$sol2[i],scen5$sol3[i],scen5$sol4[i],cmatrix_soil[indiv,15],cmatrix_soil[indiv,16],cmatrix_soil[indiv,17],cmatrix_soil[indiv,18])
		if (temp == scen6$gm[i]+incen6) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i], 60,incen6,scen6$gm[i],scen6$gm[i]+incen6,scen6$loss[i],scen6$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen6$sol1[i],scen6$sol2[i],scen6$sol3[i],scen6$sol4[i],cmatrix_soil[indiv,19],cmatrix_soil[indiv,20],cmatrix_soil[indiv,21],cmatrix_soil[indiv,22])
		if (temp == scen7$gm[i]+incen7) output_df[i,] = c(out$Cellno[i],out$x[i],out$y[i], 40,incen7,scen7$gm[i],scen7$gm[i]+incen7,scen7$loss[i],scen7$outlet_loss[i],np_choice,np_gm,np_nloss,np_outlet_nloss,scen7$sol1[i],scen7$sol2[i],scen7$sol3[i],scen7$sol4[i],cmatrix_soil[indiv,23],cmatrix_soil[indiv,24],cmatrix_soil[indiv,25],cmatrix_soil[indiv,26])
		
		rm(incen1, incen2, incen3, incen4, incen5, incen6, incen7, temp, temp_np, np_choice, np_gm, np_outlet_nloss, np_nloss)
		}

	write.csv(output_df,paste("outputs_alldata_soil7choices//soln_",indiv,".csv",sep=""),quote=F,row.names=F)
}

	   
			   
