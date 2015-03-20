for(i in length(amputee$mm_corrected)){
  if (amputee$mm_corrected[[i]]<=0) {amputee$mm_corrected[[i]] <= 0} 
}


require(lsr)
require(Hmisc)
require(ggplot2) 
require(psych)
#1 read in data
patient=read.table("master_repltrans.csv",header=T,sep=",")
amputee=read.table("master_amputee.csv",header=T,sep=",")
control=read.table("master_control.csv",header=T,sep=",")
controlmean=read.table("controlmean.csv",header=T,sep=",")
ampmean=read.table("amp_means.csv",header=T,sep=",")
ampcontmean=read.table("amp_control_means.csv",header=T,sep=",")
ampwithin=read.table("ampwithin.csv",header=T,sep=",")
calibmean_amp=read.table("calib_mean_amp.csv",header=T,sep=",")
calibmean_cont=read.table("calibmean_control.csv",header=T,sep=",")
calibamp=read.table("calibscores_amp.csv",header=T,sep=",")
patmean=read.table("master_meantransrepl.csv",header=T,sep=",")
tranmean=read.table("master_meantrans.csv",header=T,sep=",")
ampdemo=read.table("AmputeeDemographic.csv",header=T,sep=",")
contdemo=read.table("control_demog.csv",header=T,sep=",")

levels(patient$ah_uh)=c("Affected Hand","Unaffected Hand")
levels(patient$ah_uh_control)=c("Affected Hand","Unaffected Hand")

levels(amputee$ah_uh)=c("Affected Wrist","Unaffected Hand","Unaffected Wrist")
levels(amputee$ah_uh_control)=c("Affected Wrist","Unaffected Hand","Unaffected Wrist")

levels(control$ah_uh)=c("Affected Hand", "Unaffected Hand")
levels(control$ah_uh_control)=c("Unaffected Hand")

levels(controlmean$ah_uh)=c("Affected Hand", "Unaffected Hand")
levels(controlmean$ah_uh_control)=c("Unaffected Hand")

levels(calibamp$effector)=c("Left Hand","Left Forearm","Left Prosthesis","Mouth","Right Hand","Right Forearm","Right Prosthesis")
levels(calibmean_amp$effector)=c("Left Hand","Left Forearm","Left Prosthesis","Mouth","Right Hand","Right Forearm","Right Prosthesis")


#2 mold function that formats date variables and calculates difference times
mold=function(dat){
  x = dat
  x$dot=as.character(x$dot)
  x$dos=as.character(x$dos)
  x$dob=as.character(x$dob)
  x$doi=as.character(x$doi)
  x$dot=as.Date(x$dot,format="%m/%d/%Y")
  x$dos=as.Date(x$dos,format="%m/%d/%Y")
  x$dob=as.Date(x$dob,format="%m/%d/%Y")
  x$doi=as.Date(x$doi,format="%m/%d/%Y") 
  x$t.sincesurgery=as.Date(x$dot,format="%m/%d/%Y")-as.Date(x$dos,format="%m/%d/%Y")
  x$t.sinceinjury=as.Date(x$dot,format="%m/%d/%Y")-as.Date(x$doi,format="%m/%d/%Y") 
  x$t.betweeninjurysurgery=as.Date(x$dos,format="%m/%d/%Y")-as.Date(x$doi,format="%m/%d/%Y")
return(x)
}


#run mold function on data
amputee=mold(amputee)
control=mold(control)
patient=mold(patient)
controlmean=mold(controlmean)

#3 handtest function that creates values for the hand tested on a given trial, need to update to accomodate the amputees
handtest=function(data){
  x=data
  for(i in 1:dim(x)[1]){
    if(x$affhand[i]=="L" & x$ah_uh[i]== "Affected Hand"){
      x$handtested[i]="L"
    }
    else{
      if(x$affhand[i]=="L" & x$ah_uh[i]=="Unaffected Hand"){
        x$handtested[i]="R"
      }
      else{
        if(x$affhand[i]=="R" & x$ah_uh[i]=="Affected Hand"){
          x$handtested[i]="R"
        }
        else{
          x$handtested[i]="L"
        }
      }
    }
  }
  return(x)
}

#run handtest function on each group
amputee=handtest(amputee)
control=handtest(control)
patient=handtest(patient)



#5 create separate data frames for each group
transplant<<-patient[patient$group=="transplant",]
transplant$group=factor(transplant$group)
replant<<-patient[patient$group=="replant",]
replant$group=factor(replant$group)

##SUBSETS OF DATA##
#below elbow amputees
amp_belb=amputee[amputee$init2 != "BF",]
amp_belb=amp_belb[amp_belb$init2 != "HK",]
amp_belb=amp_belb[amp_belb$init2 != "HR",]
amp_belb=amp_belb[amp_belb$init2 != "JN",]
amp_belb=amp_belb[amp_belb$init2 != "KH",]
amp_belb=amp_belb[amp_belb$init2 != "LQ",]
amp_belb=amp_belb[amp_belb$init2 != "NU",]
amp_belb=amp_belb[amp_belb$init2 != "OJ",]
amp_belb=amp_belb[amp_belb$init2 != "PZ",]

#above elbow ]amputees
amp_aelb=amputee[amputee$init2 != "BD",]
amp_aelb=amp_aelb[amp_aelb$init2 != "AF",]
amp_aelb=amp_aelb[amp_aelb$init2 != "BQ",]
amp_aelb=amp_aelb[amp_aelb$init2 != "HQ",]
amp_aelb=amp_aelb[amp_aelb$init2 != "JR",]
amp_aelb=amp_aelb[amp_aelb$init2 != "KK",]
amp_aelb=amp_aelb[amp_aelb$init2 != "ML",]
amp_aelb=amp_aelb[amp_aelb$init2 != "PB",]
amp_aelb=amp_aelb[amp_aelb$init2 != "PF",]
amp_aelb=amp_aelb[amp_aelb$init2 != "PQ",]
amp_aelb=amp_aelb[amp_aelb$init2 != "QC",]


#create master data frame with amputees included
dat_noamp=rbind(control,replant,transplant)
amp_contr=rbind(control,amputee)
amp_belb_cont=rbind(controlmean,amp_belb)
amp_belb_cont$init2=factor(amp_belb_cont$init2)
amp_aelb_cont=rbind(controlmean,amp_aelb)
amp_aelb_cont$init2=factor(amp_aelb_cont$init2)
pat_cont=rbind(patient,control)
pat_cont$init2=factor(pat_cont$init2)
masterdata=rbind(control,amputee,patient)
calib_master=rbind(calibmean_amp,calibmean_cont)



#PLOTS
#barplot of ah_uh by group
#reorder data by groups
dat_noamp$init2=reorder(dat_noamp$init2,dat_noamp$group2)
levels(dat_noamp$ah_uh_control)=rev(levels(dat_noamp$ah_uh_control))
ggplot(dat_noamp, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,105))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=1.5,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=5.5,ymin=-Inf,ymax=Inf), fill="thistle1",alpha=.05)+ geom_rect(data=NULL,aes(xmin=5.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="paleturquoise1",alpha=.02)  +
  stat_boxplot(geom ='errorbar')+geom_boxplot()+ylab("Measurement\n(mm)")+xlab("Participants")+scale_x_discrete(limits=(levels(dat_noamp$init2)))+
  theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+  scale_y_continuous(breaks=seq(0,110,by=10))+
  scale_fill_brewer(palette="Dark2")+annotate("text", x = 1, y = 65, label = "Controls",size=6)+ annotate("text", x = 3.5, y = 65, label = "Replant \nPatients",size=6)+ annotate("text", x = 8, y = 65, label = "Transplant \nPatients",size=6)

#beeswarm plot of ah_uh by group
beeswarm(mm~init1,data=dat,log=F,pch=16,col=rainbow(8))

#for amputee data by participant
ggplot(amp_contr, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,105))  +
  geom_boxplot()+ylab("Measurement\n(mm)")+xlab("Participants")+
  theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+  scale_y_continuous(breaks=seq(0,110,by=10))+
  scale_fill_brewer(palette="Dark2")

##AMPUTEES AND CONTROLS##
#all amputees and controls
amp_contr$init2=reorder(amp_contr$init2,amp_contr$group2)
levels(amp_contr$ah_uh_control)=levels(amp_contr$ah_uh_control)
ggplot(amp_contr, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,70))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  stat_boxplot(geom ='errorbar')+geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+scale_x_discrete(limits=(levels(amp_contr$init2)))+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  scale_y_continuous(breaks=seq(0,70,by=10))+
  scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6)+ annotate("text", x = 11, y = 38, label = "Amputees",size=6, fontface="bold")

ggplot(amputee, aes(x=factor(ah_uh_control),y=mm))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+
  geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+
  scale_fill_brewer(palette="Greys")

#above elbow amputee bargraph with controls
ggplot(amp_aelb_cont, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,70))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.03)+
  geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+scale_x_discrete(limits=(levels(amp_aelb_cont$init2)))+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  scale_y_continuous(breaks=seq(0,70,by=10))+
  scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6,fontface="bold")+ annotate("text", x = 7, y = 38, label = "Above\nElbow\nAmputees",size=6,fontface="bold")

#above elbow amputee bargraph with controls and mm corrected
ggplot(amp_aelb_cont, aes(x=factor(init2),y=mm_corrected,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,70))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  geom_boxplot()+ylab("Corrected Error\n(mm)")+xlab("Participants")+scale_x_discrete(limits=(levels(amp_aelb_cont$init2)))+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  scale_y_continuous(breaks=seq(0,70,by=10))+
  scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6,fontface="bold")+ annotate("text", x = 7, y = 38, label = "Above\nElbow\nAmputees",size=6,fontface="bold")

#below elbow amputee bargraph with controls
ggplot(amp_belb_cont, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,70))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+scale_x_discrete(limits=(levels(amp_belb_cont$init2)))+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  scale_y_continuous(breaks=seq(0,70,by=10))+
  scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6,fontface="bold")+ annotate("text", x = 7, y = 38, label = "Below\nElbow\nAmputees",size=6,fontface="bold")

#below elbow amputee bargraph with controls and mm corrected
ggplot(amp_belb_cont, aes(x=factor(init2),y=mm_corrected,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,70))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  geom_boxplot()+ylab("Corrected Error\n(mm)")+xlab("Participants")+scale_x_discrete(limits=(levels(amp_belb_cont$init2)))+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  scale_y_continuous(breaks=seq(0,70,by=10))+
  scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6,fontface="bold")+ annotate("text", x = 7, y = 38, label = "Below\nElbow\nAmputees",size=6,fontface="bold")

#for amputee data by ah_uh
ggplot(amputee, aes(x=factor(ah_uh),y=mm,fill=group))+theme_bw()+coord_cartesian(ylim=c(0,70))  +
  geom_boxplot()+ylab("Measurement\n(mm)")+xlab("Area Tested")+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+  scale_y_continuous(breaks=seq(0,110,by=10))+
  scale_fill_brewer(palette="Greys")+guides(fill=FALSE)

#plot for area tested among patients
ggplot(patient, aes(x=factor(loc),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,105)) +
  geom_boxplot()+scale_y_continuous(breaks=seq(0,110,by=10))+xlab("Locations")+ylab("Measurement\n(mm)")+theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+ scale_fill_brewer(palette="Dark2")

##plot for area tested among replants
ggplot(replant, aes(x=factor(loc),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,105)) +
  geom_boxplot()+scale_y_continuous(breaks=seq(0,110,by=10))+xlab("Locations")+ylab("Measurement\n(mm)")+theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+ scale_fill_brewer(palette="Dark2")

##plot for area tested among transplants
ggplot(transplant, aes(x=factor(loc),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,105)) +
  geom_boxplot()+scale_y_continuous(breaks=seq(0,110,by=10))+xlab("Locations")+ylab("Measurement\n(mm)")+theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+ scale_fill_brewer(palette="Dark2")

#plot for area tested among controls
ggplot(control, aes(x=factor(loc),y=mm,fill=group))+theme_bw()+coord_cartesian(ylim=c(0,105)) +
  geom_boxplot()+  scale_y_continuous(breaks=seq(0,110,by=10))+xlab("Locations")+ylab("Measurement\n(mm)")+theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+scale_fill_brewer(palette="Dark2")+guides(fill=FALSE)

#group measure
ggplot(masterdata, aes(x=factor(group),y=mm,fill=ah_uh_control))+theme_bw()+coord_cartesian(ylim=c(0,105)) +
  geom_boxplot()+scale_y_continuous(breaks=seq(0,110,by=10))+xlab("Group")+ylab("Measurement\n(mm)")+theme(legend.title=element_blank(),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=12,color="black"),axis.title.x = element_text(size = 12, angle = 00),axis.title.y = element_text(size = 12, angle = 90),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=12))+ scale_fill_brewer(palette="Dark2")



#DESCRIPTIVES
#descriptives for patients, nested in main desc function
desc.ah_uh=function(data){
  x=tapply(data$mm,data$ah_uh,describe,na.rm=T)
  return(x)
 
}

#descriptives for controls, nested in main desc function
desc.hand=function(data){
  x=tapply(data$mm,data$handtested,describe,na.rm=T)
  return(x)
}

#main function for descriptives by group
desc=function(data){
  if(deparse(substitute(data)) == "control"){ 
    name=deparse(substitute(data))
    x=desc.hand(data)
    left=x[2]
    right=x[3]
    left=data.frame(left)
    right=data.frame(right)
    left=left[,2:13]
    right=right[,2:13]
    names(left)=c("N","Mean","SD","Median","Trimmed","mad","min","max","range","skew","kurtosis","SE")
    names(right)=c("N","Mean","SD","Median","Trimmed","mad","min","max","range","skew","kurtosis","SE")
    assign(paste0("desc.left.",name),left,envir = .GlobalEnv)
    assign(paste0("desc.right.",name),right,envir = .GlobalEnv)
  
}
  else{ x=desc.ah_uh(data)
        name=deparse(substitute(data))
        ah=x[1]
        uh=x[2]
        ah=data.frame(ah)
        uh=data.frame(uh)
        ah=ah[,2:13]
        uh=uh[,2:13]
        names(ah)=c("N","Mean","SD","Median","Trimmed","mad","min","max","range","skew","kurtosis","SE")
        names(uh)=c("N","Mean","SD","Median","Trimmed","mad","min","max","range","skew","kurtosis","SE")
        assign(paste0("desc.ah.",name),ah,envir = .GlobalEnv)
        assign(paste0("desc.uh.",name),uh,envir = .GlobalEnv)
}}

#run desc function on groups
desc(patient)
desc(control)
desc(transplant)
desc(replant)


#create group names for a main data frame
masterdesc=rbind(desc.ah.patient,desc.uh.patient,desc.ah.transplant,desc.uh.transplant,desc.ah.replant,desc.uh.replant,desc.left.control,desc.right.control)
Group=c(rep(0,8))
masterdesc=data.frame(Group,masterdesc)
masterdesc[,1]=c("Patients Affected Hand", "Patients Unaffected Hand","Transplant Affected Hand", "Transplant Unaffected Hand", "Replant Affected Hand", "Replant Unaffected Hand", "Controls Left Hand","Controls Right Hand")
rm(Group)

qq=amputee[amputee$ah_uh=="Unaffected Hand",]
qplot(qq$mm , geom = "blank") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "blue") + 
  stat_density(geom = "line", colour = "red")+ggtitle("Intact Hand\n(All Data Points)")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

qq=amputee[amputee$ah_uh=="Unaffected Hand",]
qplot(tapply(qq$mm,qq$init2,mean) , geom = "blank") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "blue") + 
  stat_density(geom = "line", colour = "red")+ggtitle("Intact Hand\n(Means)")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))


tapply(qq$mm,qq$init2,describe,na.rm=T)



qq=amputee[amputee$ah_uh=="Affected Wrist",]
qplot(qq$mm , geom = "blank") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "blue") + 
  stat_density(geom = "line", colour = "red")+ggtitle("Affected Wrist\n(All Data Points)")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))



qq=amputee[amputee$ah_uh=="Affected Wrist",]
qplot(tapply(qq$mm,qq$init2,mean) , geom = "blank") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "blue") + 
  stat_density(geom = "line", colour = "red")+ggtitle("Affected Wrist\n(Means)")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))


tapply(qq$mm,qq$init2,describe,na.rm=T)


qq=amputee[amputee$ah_uh=="Unaffected Wrist",]
qplot(qq$mm , geom = "blank") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "blue") + 
  stat_density(geom = "line", colour = "red")+ggtitle("Unaffected Wrist\n(All Data Points)")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))



qq=amputee[amputee$ah_uh=="Unaffected Wrist",]
qplot(tapply(qq$mm,qq$init2,mean) , geom = "blank") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "blue") + 
  stat_density(geom = "line", colour = "red")+ggtitle("Unaffected Wrist\n(Means)")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))


tapply(qq$mm,qq$init2,describe,na.rm=T)


tapply(amputee$mm,list(amputee$init2,amputee$ah_uh),mean,na.rm=T)

ampcontmean$effector=factor(ampcontmean$effector)
ggplot(ampcontmean, aes(x=factor(effector),y=mean_mm))+geom_rect(data=NULL,aes(xmin=2.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("")+
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+
  scale_fill_brewer(palette="Greys")+xlim("Left","Right","Unaffected Hand", "Unaffected Wrist", "Affected Wrist")

ggplot(amp_contr, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,70))+   geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+  
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+
  scale_y_continuous(breaks=seq(0,70,by=10))+scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6)+ 
  annotate("text", x = 11, y = 38, label = "",size=6, fontface="bold")+scale_x_discrete(limits=(levels(amp_contr$init2)))

ggplot(pat_cont, aes(x=factor(init2),y=mm,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,120))+   
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=5.5,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+  
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  
  scale_y_continuous(breaks=seq(0,120,by=10))+ scale_fill_brewer(palette="Greys")+
  annotate("text", x = 1, y = 38, label = "",size=6)+ annotate("text", x = 11, y = 38, label = "",size=6, fontface="bold")+xlim("Controls","CH","JS","PP","RW","DR1","DR2","DR3","DR4","DR5","EH1","EH2","GF","MS")


ggplot(pat_cont, aes(x=factor(init2),y=mm_miss,fill=ah_uh_control))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,120))+   
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf), fill="gray100",alpha=.02)+geom_rect(data=NULL,aes(xmin=1.5,xmax=5.5,ymin=-Inf,ymax=Inf), fill="gray95",alpha=.05)+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+  
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+  
  scale_y_continuous(breaks=seq(0,120,by=10))+ scale_fill_brewer(palette="Greys")+
  annotate("text", x = 1, y = 38, label = "",size=6)+ annotate("text", x = 11, y = 38, label = "",size=6, fontface="bold")+xlim("Controls","CH","JS","PP","RW","DR1","DR2","DR3","DR4","DR5","EH1","EH2","GF","MS")

plot()

aov.within=aov(mean_mm~subject+Error(effector/subject),data=ampwithin)
summary(aov.within)


aov.within=aov(error~effector,data=calibmean_amp)
summary(aov.within)
TukeyHSD(aov.within)

ggplot(calibamp, aes(x=factor(init),y=mm,fill=effector))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,7))+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("Participants")+  
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+
  scale_y_continuous(breaks=seq(0,7,by=1))+scale_fill_brewer(palette="Greys")+annotate("text", x = 1, y = 38, label = "",size=6)+ 
  annotate("text", x = 11, y = 38, label = "",size=6, fontface="bold")+scale_x_discrete(limits=(levels(calibamp$init)))


#boxplot of the calib error for each effector, all data points, not means per subject.
ggplot(calibamp, aes(x=factor(effector),y=mm))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,7))+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("Effector")+  
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+
  scale_y_continuous(breaks=seq(0,7,by=1))+scale_fill_brewer(palette="Greys")+scale_x_discrete(limits=(levels(calibamp$effector)))




ggplot(calibmean_amp, aes(x=factor(effector),y=error))+theme_bw()+theme(panel.grid.major = element_line(colour = "#808080"))+coord_cartesian(ylim=c(0,7))+
  stat_boxplot(geom ='errorbar',size=1.25)+geom_boxplot()+ylab("Error\n(mm)")+xlab("Effector")+  
  theme(panel.grid.major = element_line(colour = "black"),legend.title=element_blank(),legend.text = element_text(angle = 0, size = 14, face = 'bold'),legend.position="top",axis.text.x = element_text(angle = 0, hjust = .5, size=14,color="black",face='bold'),axis.title.x = element_text(size = 14, angle = 00, face='bold'),axis.title.y = element_text(size = 14, angle = 90,face='bold'),axis.text.y = element_text(size = 14, angle = 0,face='bold'),legend.margin = unit(0, "cm"),legend.key.size=unit(1.2,"cm"),legend.text=element_text(size=14))+
  scale_y_continuous(breaks=seq(0,7,by=1))+scale_fill_brewer(palette="Greys")+scale_x_discrete(limits=(levels(calibmean_amp$effector)))

aov.calib=aov(mm~effector,data=calibamp)
summary(aov.calib)
TukeyHSD(aov.calib)

tapply(calibmean_amp$error,list(calibmean_amp$effector),mean)

tapply(calibmean_cont$error,list(calibmean_cont$effector),mean)

tapply(calibamp$mm,list(calibamp$init,calibamp$effector),mean)

ggplot(patmean, aes(y=mean_mm,x=yss))+geom_point(shape=16,size=3)+geom_smooth(method=lm,se=F,colour="black",weight=4)+ylab("Mean Error (mm)")+xlab("Years Since Surgery")+
  theme(axis.text.x = element_text(angle = 0, hjust = .5, size=16,color="black",face='bold'),axis.text.y = element_text(angle = 0, hjust = .5, size=16,color="black",face='bold'),axis.title.x = element_text(angle = 0, hjust = .5, size=16,color="black",face='bold'),axis.title.y = element_text(angle = 90, hjust = .5, size=16,color="black",face='bold'))

ggplot(tranmean, aes(y=mean_mm,x=ybis))+geom_point(shape=16,size=3)+geom_smooth(method=lm,se=F,colour="black",weight=4)+ylab("Mean Error (mm)")+xlab("Years Between Injury and Surgery")+
  theme(axis.text.x = element_text(angle = 0, hjust = .5, size=16,color="black",face='bold'),axis.text.y = element_text(angle = 0, hjust = .5, size=16,color="black",face='bold'),axis.title.x = element_text(angle = 0, hjust = .5, size=16,color="black",face='bold'),axis.title.y = element_text(angle = 90, hjust = .5, size=16,color="black",face='bold'))


##Amputee anaysis##
#test for amputees aw vs uw
amp_means=tapply(amputee$mm,list(amputee$ah_uh,amputee$init2),mean,NA.RM=T)
wrist_aw=amp_means[1,16:22]
wrist_uw=amp_means[3,16:22]
describe(wrist_aw)
describe(wrist_uw)
t.test(wrist_aw,wrist_uw,paired=T)


age_amp=c(42,62,46,58,43,67,31,64,43,56,56,47,61,86,38,52,29,37,20,65,32,50)
x_aw_age=age_amp[12:22]
y_aw=amp_means[1,12:22]
cor_aw_age=cor.test(x_aw_age,y_aw)
plot(age_amp,amp_means[1,]) #plot of age and aw mean loc
abline(lm(y_aw~x_aw_age), col="red")

x_uh_age=age_amp[c(1:11,14:22)]
y_uh=amp_means[2,c(1:11,14:22)]
cor_uh_age=cor.test(x_uh_age,y_uh)
plot(age_amp,amp_means[2,]) #plot of age and uh mean loc
abline(lm(y_uh~x_uh_age), col="red")


x_uw_age=age_amp[c(16:22)]
y_uw=amp_means[3,c(16:22)]
cor_uw_age=cor.test(x_uw_age,y_uw)
plot(age_amp,amp_means[3,]) #plot of age and uw mean loc
abline(lm(y_aw~x_aw_age), col="red")

gender_amp=c("m","m","f","m","m","f","f","f","f","m","m","m","m","m","f","m","m","f","m","m","m","m")
gender_amp=as.factor(gender_amp)
plot(gender_amp,amp_means[1,]) #plot of gender and aw mean loc
plot(gender_amp,amp_means[2,]) #plot of gender and uh mean loc
plot(gender_amp,amp_means[3,]) #plot of gender and uw mean loc

#test for amputees intact hand vs controls hands
cont_means=tapply(control$mm,list(control$ah_uh_control,control$initials),mean,na.rm=TRUE) #find means of controls locognosia scores per subject
cont_hands=cont_means[1,] #store hand means in a variable
amp_hands=amp_means[2,c(1:11,14:22)] #store amp hand means in var
describe(cont_hands)
describe(amp_hands)
t.test(cont_hands,amp_hands)

contr_age=contdemo




