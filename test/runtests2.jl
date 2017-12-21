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
 
function main(mode_,nvals_,Gmax_,mr_,sdev_)
	
        mode = parse(Int,mode_);#mode 1 means static landscape, mode 2 means dynamic landscape
        nvals = parse(Int,nvals_);#number of values of the parameters space
	Gmax = parse(Int,Gmax_);#Maximum number of Generations
	sdev = parse(Float64,sdev_);#Standard deviation used to define the distribution of patch sizes (default = 0.5)
	seed = 1;#seed for random numbers (to control the outputs)
	nreal = 100;#Number of realizations
	landG = 1;#Number of generations before a landscape upgrade
	S = 100;#Number of sites
	J = 100;#Number of inds. per site
	mr = parse(Float64,mr_);#migration between sites 
        println("Running Dynaland.jl:");
        println("mr = ",mr);
        println("model = ",mode_);
	vr = 0.001;#migration from the regional pool (speciation)
        sufix = "Static";
        if(mode == 2)
           sufix = "Dynamic";
        end
        landscapeoutputs = string("FSALandscapeOutputs_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,".txt");
        sitesoutputs = string("FSASitesOutputs_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,".txt");
        phylogenyoutputs = string("FSAPhylogenyOutputs_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,".txt");
        landscapeoutputpergen = string("FSALandscapeOutputsPerGen_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,".txt");
        dispersionoutputs = string("FSADispersionMatrix_",sufix,"_nvals",nvals,"_mr",mr,"_G",Gmax,"_J",J,"_sd",sdev,".jld");

        Dynaland.Dynamic(mode,nvals,seed,nreal,Gmax,landG,S,J,sdev,mr,vr,landscapeoutputs,sitesoutputs,phylogenyoutputs,landscapeoutputpergen,dispersionoutputs);
#        Dynaland.Dynamic(mode_,nvals_,seed,nreal,Gmax_,landG,S,J,sdev,mr_,vr,landscapeoutputs,sitesoutputs,phylogenyoutputs,landscapeoutputpergen);

#        mydate = Libc.strftime("%Y_%m_%d_%H_%M_%S",time());
#        command = `sh sendmail.sh Finished_at_$mydate charles.santana@gmail.com`
#        run(command);
end

if(length(ARGS) != 5)
   println("\nTo run: julia runtest.jl MODE NVALS GMAX mr sdev\n\n-MODE:\t1 (Static) or 2 (Dynamic)\n-NVALS:\tNumber of values for each parameter\n-GMAX:\tNumber of Generations to run\n-mr: Migration rate\n-sdev: Standard deviation for the distribution of patch sizes\n");
   return;
else
   main(ARGS[1],ARGS[2],ARGS[3],ARGS[4],ARGS[5]);
end
