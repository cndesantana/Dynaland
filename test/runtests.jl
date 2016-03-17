using Dynaland

#;# To change the dc equation according to GM suggestions - dc = d0 * (1 - A(sin(pi*f*t))) 
#;# It means that the changes in the connectivity are a proportion of the mean connectivity radius (d0)
#;# It means that the critical radius ranges between 0 and 2*d0
#;# So we avoid problems with boundary conditions 
#;# So the phase space will consider A varying between 0 and 1 (A=0 means static) 
#;# So the phase space will consider y axis varying between -100 to 100
#;# 
#;# 
#;# To start the system with a different species for each site, in order to make the convergence towards a steady state faster.
#;# To define a way to identify the steady state (based on the ratio between sd(rich)/mean(rich), for example
#;# To check if the number of species in the steady stae matches with the literature (paper in PRE 2003, that says that we would expect 240 species in our system for steady state)
 
function main(mode_,nvals_,gmax_,mr_,jsdev_,jmean_)
	
        println("Testing Dynaland.jl");
        mode = parse(Int,mode_);#mode 1 means static landscape, mode 2 means dynamic landscape
        nvals = parse(Int,nvals_);#number of values of the parameters space
	Gmax = parse(Int,gmax_);#Maximum number of Generations
	Jsdev = parse(Float64,jsdev_);#Standard deviation used to define the distribution of patch sizes (number of indidivuals per site)
	Jmean = parse(Int,jmean_);#Mean Number of inds. per site
	S = 100;#Number of sites
	seed = 1;#seed for random numbers (to control the outputs)
	nreal = 1;#Number of realizations
	landG = 1;#Number of generations before a landscape upgrade
	mr = parse(Float64,mr_);#migration between sites 
	vr = 0.003;#migration from the regional pool (speciation)
        sufix = "Static";
        if(mode == 2)
           sufix = "Dynamic";
        end
        landscapeoutputs = string("LandscapeOutputs_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,"_J",Jmean,"_sd",Jsdev,".txt");
        sitesoutputs = string("SitesOutputs_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,"_J",Jmean,"_sd",Jsdev,".txt");
        phylogenyoutputs = string("PhylogenyOutputs_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,"_J",Jmean,"_sd",Jsdev,".txt");
        landscapeoutputpergen = string("LandscapeOutputsPerGen_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,"_J",Jmean,"_sd",Jsdev,".txt");

        Dynaland.Dynamic(mode,nvals,seed,nreal,Gmax,landG,S,Jmean,Jsdev,mr,vr,landscapeoutputs,sitesoutputs,phylogenyoutputs,landscapeoutputpergen);

#        mydate = Libc.strftime("%Y_%m_%d_%H_%M_%S",time());
#        command = `sh sendmail.sh Finished_at_$mydate charles.santana@gmail.com`
#        run(command);
end

if(length(ARGS) != 6)
   println("\nTo run: julia runtest.jl MODE NVALS GMAX mr sdev\n\n-MODE:\t1 (Static) or 2 (Dynamic)\n-NVALS:\tNumber of values for each parameter\n-GMAX:\tNumber of Generations to run\n-mr: Migration rate\n-jsdev: Standard deviation of the distribution of the number of individuals per site\n-jmean: Mean number of individuals per site\n");
   return;
else
   main(ARGS[1],ARGS[2],ARGS[3],ARGS[4],ARGS[5],ARGS[6]);
end
