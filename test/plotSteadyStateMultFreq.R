plotSteadystateMultipleFreq<-function(inputfilename,static,dynamic){
   maxrich<-max(as.numeric(c(static$Gamma,dynamic$Gamma)));
   maxgen<-max(as.numeric(c(static$G,dynamic$G)));
   myf<-unique(sort(dynamic$f));   
   myr0<-unique(sort(dynamic$r0));   

   for(i in 1:length(myr0)){

      mystatic<-static[which(static$r0==myr0[i]),]
      mystatic<-mystatic[which(mystatic$Ncomponents==1),];

      mydynamic<-dynamic[which(dynamic$r0==myr0[i]),]

      outputfilename<-paste(inputfilename,"_r0_",myr0[i],"_steadystate.png",sep="");
      png(outputfilename,width=1980,height=1280,res=300);
      mycolor<-1;
      myx<-mystatic$G+1;
      myy<-mystatic$Gamma;
      plot(log10(myx),myy,xlab="log10(Generations)",ylab="Richness",col=mycolor,type="p",main=paste("Steady State Study\n (d0 = ",signif(myr0[i]),")",sep=""),xlim=c(0,log10(maxgen)),ylim=c(0,400),pch=1)
      mycolor<-mycolor+1;

      for(j in 1:length(myf)){
         mydyn<-mydynamic[which(mydynamic$f==myf[j]),]
         mydyn<-mydyn[which(mydyn$Ncomponents==1),];
         myx<-mydyn$G+1;
         myy<-mydyn$Gamma;
         if(sum(mydyn$Nedges)>0){
            points(log10(myx),myy,col=mycolor,type="p",pch=j+1)
            mycolor<-mycolor+1;
         }
      }
#      if(mycolor <= 6)#if there are not so much values for the legend
#      { 
           par(xpd="TRUE");
           legend("topleft",legend=c("static",signif(myf,2)),col=(1:mycolor),lwd=1,pch=(1:mycolor),title="frequency");
#      }
      dev.off();
   }
#   mydate<-format(Sys.time(), "%H_%M_%S_%Y");system(paste("sh sendmail.sh SteadyStateFig","_",mydate," charles.santana@gmail.com ",outputfilename,sep=""));
}

args <- commandArgs(trailingOnly = TRUE)
staticfilename<-args[1];
dynamicfilename<-args[2];

staticfilename<-"./SitesOutputs_Static_1000.txt"
dynamicfilename<-"./SitesOutputs_Dynamic_1000.txt"
outputfilenamerad<-unlist(strsplit(staticfilename,"_"))[1];
static<-read.csv(staticfilename,sep=" ")
dynamic<-read.csv(dynamicfilename,sep=" ")
plotSteadystateMultipleFreq(outputfilenamerad,static,dynamic);

