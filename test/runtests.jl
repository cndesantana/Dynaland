using Dynaland

function main()
	
        println("Testing Dynaland.jl");
	seed = 1;#seed for random numbers (to control the outputs)
	nreal = 1;#Number of realizations
	Gmax = 1000;#Maximum number of Generations
	landG = 1;#Number of generations before a landscape upgrade
	S = 100;#Number of sites
	J = 100;#Number of inds. per site
	mr = 0.3;
	vr = 0.003;
        landscapeoutputs = "LandscapeOutputs.txt";
        sitesoutputs = "SitesOutputs.txt";

        Dynaland.Dynamic(seed,nreal,Gmax,landG,S,J,mr,vr,landscapeoutputs,sitesoutputs);
end

main();
