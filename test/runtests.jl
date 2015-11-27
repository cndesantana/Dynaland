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
 
function main()
	
        println("Testing Dynaland.jl");
        mode = 1;#mode 1 means static landscape, mode 2 means dynamic landscape
        nvals = 10;#number of values of the parameters space
	seed = 1;#seed for random numbers (to control the outputs)
	nreal = 1;#Number of realizations
	Gmax = 1000;#Maximum number of Generations
	landG = 1;#Number of generations before a landscape upgrade
	S = 100;#Number of sites
	J = 100;#Number of inds. per site
	mr = 0.3;#migration between sites 
	vr = 0.003;#migration from the regional pool (speciation)
        landscapeoutputs = "LandscapeOutputs_Static_$Gmax.txt";
        sitesoutputs = "SitesOutputs_Static_$Gmax.txt";
        phylogenyoutputs = "PhylogenyOutputs_Static_$Gmax.txt";

        Dynaland.Dynamic(mode,nvals,seed,nreal,Gmax,landG,S,J,mr,vr,landscapeoutputs,sitesoutputs,phylogenyoutputs);

        mydate = Libc.strftime("%Y_%m_%d_%H_%M_%S",time());
        command = `sh sendmail.sh Finished_at_$mydate charles.santana@gmail.com`
        run(command);
end

main();
