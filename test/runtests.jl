using Dynaland

function main()
	
        println("Testing Dynaland.jl");
        mode = 1;#mode 1 means static landscape, mode 2 means dynamic landscape
        nvals = 5;#number of values of the parameters space
	seed = 1;#seed for random numbers (to control the outputs)
	nreal = 1;#Number of realizations
	Gmax = 1000;#Maximum number of Generations
	landG = 1;#Number of generations before a landscape upgrade
	S = 100;#Number of sites
	J = 100;#Number of inds. per site
	mr = 0.3;#migration between sites 
	vr = 0.003;#migration from the regional pool (speciation)
        landscapeoutputs = "LandscapeOutputs.txt";
        sitesoutputs = "SitesOutputs.txt";

        Dynaland.Dynamic(mode,nvals,seed,nreal,Gmax,landG,S,J,mr,vr,landscapeoutputs,sitesoutputs);
end

main();
