plotAreaRichness<-function(inputfilename,dat){
   outputfilename<-paste(inputfilename,".area_richness.png",sep="");
   png(outputfilename,width=1980,height=1280,res=300);
   plot(dat$Nverts,dat$GammaPerComponent,log="xy",xlab="Size of the Component",ylab="Richness of the component",main="Area vs. Richness")
   dev.off()
}

args <- commandArgs(trailingOnly = TRUE)
inputfilename<-args[1];
dat<-read.csv(inputfilename,sep=" ")

plotAreaRichness(inputfilename,dat);

