library(nsga2R)
##Code to get only nondominated solutions
setwd("C:\\Users\\BBarnhart.DOMAIN1\\OneDrive - National Council for Air and Stream Improvement Inc\\Projects\\Other\\Legacy\\brisbane\\7_choices_updated")

soil = read.csv('soil_results\\inspyred-individuals-file-08072019-155742.csv',header=F)

b = fastNonDominatedSorting(cbind(soil$V3,soil$V4))
nondominated_soil = soil[b[[1]],]
soil_out = unique(nondominated_soil[,3:28])
write.csv(soil_out,"soil_results\\nondominated_soil7choices.csv",quote=F,row.names=F)

basin = read.csv('basin_results\\basin_0_5000_inspyred-individuals-file-08182019-113648.csv',header=F)
b = fastNonDominatedSorting(cbind(basin$V3,basin$V4))
nondominated_basin = basin[b[[1]],]
basin_out = unique(nondominated_basin[,3:10])
write.csv(basin_out,"basin_results\\nondominated_basin7choices.csv",quote=F,row.names=F)

sub = read.csv('sub_results/inspyred-individuals-file-08272019-141314.csv',header=F)
b = fastNonDominatedSorting(cbind(sub$V3,sub$V4))
nondominated_sub = sub[b[[1]],]
sub_out = unique(nondominated_sub[,3:118])
write.csv(sub_out,"sub_results\\nondominated_subbasin7choices_seeded3.csv",quote=F,row.names=F)




#Plots
soil_out = read.csv('soil_results\\nondominated_soil7choices.csv',header=T)
basin_out = read.csv('basin_results\\nondominated_basin7choices.csv',header=T)
sub_out = read.csv('sub_results\\nondominated_subbasin7choices.csv',header=T)


#pdf("7policy_results.pdf")
par(mfrow=c(1,2),oma=c(0,0,2,0))

plot(soil_out$V3/1000,soil_out$V4/1e6,col='blue',cex=0.5,pch=19,xlab='Outlet Nloss (tonnes)',ylab='Total Policy Cost ($ millions)',xlim=c(0,600))
points(basin_out$V3/1000,basin_out$V4/1e6,col='black',cex=0.5,pch=19)

points(sub_out$V3/1000,sub_out$V4/1e6,col='red',cex=0.5,pch=19)
grid()
#legend('topright','Soil-Specific Management Incentive Policy',col='blue',pch=19,cex=0.75)

#pdf("soil_7opt_plot_percentages.pdf")
plot((1-soil_out$V3/587432.8)*100,soil_out$V4/1e6,col='blue',cex=0.5,pch=19,xlab='% Outlet Nloss Reductions',ylab='Total Policy Cost ($ millions)',xlim=c(0,100))
points((1-basin_out$V3/587432.8)*100,basin_out$V4/1e6,col='black',cex=0.5,pch=19,xlab='% Outlet Nloss Reductions',ylab='Total Policy Cost ($ millions)',xlim=c(0,100))

points((1-sub_out$V3/587432.8)*100,sub_out$V4/1e6,col='red',cex=0.5,pch=19)
grid()
#legend('topleft','Soil-Specific Management Incentive Policy',col='blue',pch=19,cex=0.75)
#dev.off()


points(out[out$V3 < 2.5e5 & out$V3 >2.3e5 & out$V4 > 2e6 & out$V4<5e6,]$V3,out[out$V3 < 2.5e5 & out$V3 >2.3e5 & out$V4 > 2e6 & out$V4<5e6,]$V4,col='blue',pch=19)


#end
###########################









basin = read.csv('basin_results\\third_run_pareto\\inspyred-individuals-file-01312019-084306.csv',header=F)
a = fastNonDominatedSorting(cbind(basin$V3,basin$V4))
nondominated_basin = basin[a[[1]],]
basin_out = unique(nondominated_basin[,3:7])
write.csv(basin_out,"nondominated_basin.csv",quote=F,row.names=F)

soil = read.csv('soil_results\\inspyred-individuals-file-02082019-085306.csv',header=F)
b = fastNonDominatedSorting(cbind(soil$V3,soil$V4))
nondominated_soil = soil[b[[1]],]
soil_out = unique(nondominated_soil[,3:16])
write.csv(soil_out,"nondominated_soil.csv",quote=F,row.names=F)


setwd("C:\\Users\\BBarnhart.DOMAIN1\\OneDrive - National Council for Air and Stream Improvement Inc\\Projects\\Other\\Legacy\\brisbane")

basin = read.csv('basin_results\\third_run_pareto\\inspyred-individuals-file-01312019-084306.csv',header=F)
cell = read.csv('cell_results\\objs_only_allruns.csv',header=T)

par(mfrow=c(1,2),oma=c(0,0,2,0))
plot(basin$V3[basin$V1==100],basin$V4[basin$V1==100],pch=19,xlab='Nloss',ylab='Total Policy Cost',xlim=c(0,8e5),ylim=c(0,1e7),main="Policy Cost vs. Total Nloss")
plot(100*(1-(basin$V3[basin$V1==100]/max(basin$V3[basin$V1==100]))),basin$V4[basin$V1==100],pch=19,xlab='% Nloss Reductions',ylab='Total Policy Cost',xlim=c(0,100),ylim=c(0,1e7),main="Policy Cost vs. % Reductions")
mtext("Basinwide Policy", outer = T, cex= 1.5)


#plotting fits 
x1 = basin$V3[basin$V1==100]
y1 = basin$V4[basin$V1==100]

x2 = 100*(1-(basin$V3[basin$V1==100]/max(basin$V3[basin$V1==100])))
y2 = basin$V4[basin$V1==100]


#pdf("basin_results_plot.pdf")
par(mfrow=c(1,2),oma=c(0,0,2,0))
plot(basin$V3[basin$V1==100]/1e3,basin$V4[basin$V1==100],pch=19,xlab='Nloss (tonnes)',ylab='Total Policy Cost',xlim=c(0,8e2),ylim=c(0,1e7),main="Policy Cost vs. Total Nloss",cex=0.5)
plot(100*(1-(basin$V3[basin$V1==100]/max(basin$V3[basin$V1==100]))),basin$V4[basin$V1==100],pch=19,xlab='% Nloss Reductions',ylab='Total Policy Cost',xlim=c(0,100),ylim=c(0,1e7),main="Policy Cost vs. % Reductions",cex=0.5)
mtext("Basinwide Policy", outer = T, cex= 1.5)
#dev.off()



#Compare basin vs. cell
out = read.csv("cell_results\\thirdrun_seeded\\objs_only_3.csv")
par(mfrow=c(1,1))
plot(tail(out$out.V3,100),tail(out$out.V4,100),col='blue',pch=19,cex=0.5,xlab='Nloss',ylab='Total Policy Cost',xlim=c(0,8e5),ylim=c(0,1e7),main="Policy Cost vs. Total Nloss")
points(basin$V3[basin$V1==100],basin$V4[basin$V1==100],pch=19,cex=0.5)


#Compare basin vs. soil targeted. 
setwd("C:\\Users\\BBarnhart.DOMAIN1\\OneDrive - National Council for Air and Stream Improvement Inc\\Projects\\Other\\Legacy\\brisbane")
basin = read.csv('basin_results\\third_run_pareto\\inspyred-individuals-file-01312019-084306.csv',header=F)
out = read.csv('soil_results/inspyred-individuals-file-02082019-085306.csv',header=F)
plot(out$V3[out$V1==100]/1e3,out$V4[out$V1==100],pch=19,col='blue',ylim=c(0,1e7),cex=0.5,xlab='Nloss (tonnes)',ylab='Total Policy Cost')
points(basin$V3[basin$V1==100]/1e3,basin$V4[basin$V1==100],pch=19,cex=0.5)
legend('topright',c('General Managment Incentive Policy','Soil-specific Management Incentive Policy'), cex=1,col=c('black','blue'),pch=19)

b = basin$V4[basin$V1==100] #policy costs for 
b = b[order(b)]
a = out$V4[out$V1==100] #policy costs for soil-specific
a = a[order(a)]
hist(((b-a)/1e6),50,xlim=c(0,1.5),main='basinwide vs. soil-specific',xlab='Diff ($Millions)')

#Compare basin vs. soil targeted. 
setwd("C:\\Users\\BBarnhart.DOMAIN1\\OneDrive - National Council for Air and Stream Improvement Inc\\Projects\\Other\\Legacy\\brisbane")
basin = read.csv('basin_results\\third_run_pareto\\inspyred-individuals-file-01312019-084306.csv',header=F)
out = read.csv('soil_results/inspyred-individuals-file-02082019-085306.csv',header=F)
plot(100*(1-(basin$V3[basin$V1==100]/max(basin$V3[basin$V1==100]))),basin$V4[basin$V1==100],pch=19,xlab='% Nloss Reductions',ylab='Total Policy Cost',xlim=c(0,100),ylim=c(0,1e7),main="Policy Cost vs. % Reductions",cex=0.5)
points(100*(1-(out$V3[out$V1==100]/max(basin$V3[basin$V1==100]))),out$V4[out$V1==100],pch=19,cex=0.5,col='blue')
legend('topleft',c('General Managment Incentive Policy','Soil-specific Management Incentive Policy'),col=c('black','blue'),pch=19,cex=0.8)





