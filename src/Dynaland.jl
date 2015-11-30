# version 0.1

module Dynaland 

############Random Geometric Graph functions

using Graphs

function vertexTransitivity(D,v1)
	neigh = find(D[v1,:].==1);
	nneigh = length(neigh);
	sum=0;
	transv1 = 0;
	for i in 1:nneigh
		ni = neigh[i];
		for j in 1:nneigh
			nj = neigh[j];
			sum = sum+D[ni,nj];
		end
	end
	if (nneigh > 1)	transv1 = sum/(nneigh*(nneigh-1));
	elseif (nneigh == 1) transv1 = 1;
	end

	return transv1;
end

function graphTransitivity(D)
	nverts = length(D[1,:]);
	transD = Float64[];
	for i in 1:nverts
		ti = vertexTransitivity(D,i);
		push!(transD,ti);
	end
	gTrans = mean(transD);
	
	return gTrans;
end

function createGraph(D,S)	
	g = simple_inclist(S,is_directed=false);
	for i in 1:S
		neigh = find(D[i,i:end].==1);
		for ni in 1:length(neigh)
			add_edge!(g, i, neigh[ni]);
		end
	end

	return g;
end

function transformMatrices(D,Di,S)
	trDi = Di';	
	DI=Di+trDi;
	DI=1./DI;
	changediagonal!(DI,S,0);
	Dc=cumsum(DI.*D,2);#to consider the existence of a path in the calculation of Dc
	
	return Dc,DI;
end

function changediagonal!(M,nrows,value)
	for(i in 1:nrows)
		M[i,i]=value;
	end
end

function getMatrixDistanceRGN(S,w)
        Di = zeros(S,S);

        for i in 1:S-1;
                for j in i+1:S;
                        dx = (w[i,1] - w[j,1])*(w[i,1] - w[j,1]);
                        dy = (w[i,2] - w[j,2])*(w[i,2] - w[j,2]);
                        dist = sqrt(dx + dy);
                        Di[i,j] = dist[1];
                end
        end		
	minDi = minimum(Di[find(Di.!=0)]);	
	maxDi = maximum(Di);

        minDi, maxDi, Di;
end

function RGN(A,f,S,w)
	D = zeros(S,S);
	Di = zeros(S,S);
	
	r = rand();#Random RGN
	dist = 0.0;

	for i in 1:(S-1);
		for j in (i+1):S;
			dx = (w[i,1] - w[j,1])*(w[i,1] - w[j,1]);
			dy = (w[i,2] - w[j,2])*(w[i,2] - w[j,2]);
			dist = sqrt(dx + dy);
			Di[i,j] = dist;
			if (dist <= r)
				D[i,j] = 1;
			end
		end
	end
	r, D, Di;
end

function InitialSeasonalRGN(S,w,r)
	Di = zeros(S,S);
	D = zeros(S,S);
 
	for i in 1:S-1;
		for j in i+1:S;
			dx = (w[i,1] - w[j,1])*(w[i,1] - w[j,1]);
			dy = (w[i,2] - w[j,2])*(w[i,2] - w[j,2]);
			dist = sqrt(dx + dy);
			Di[i,j] = dist[1];
			if (dist <= r)
				D[i,j] = 1;
			end

		end
	end
	Di,D;
end

function SeasonalRGN(r0,A,f,S,count,w)
	D = zeros(S,S);
	Di = zeros(S,S);
 
	r = r0 + A*sin(pi*f*count);#Seasonal RGN

	for i in 1:S-1;
		for j in i+1:S;
			dx = (w[i,1] - w[j,1])*(w[i,1] - w[j,1]);
			dy = (w[i,2] - w[j,2])*(w[i,2] - w[j,2]);
			dist = sqrt(dx + dy);
			if (dist[1] <= r)
				D[i,j] = 1;
			end
			Di[i,j] = dist[1];
		end
	end
	r, D, Di;
end



#########Demographic functions


function GetRichness(R,S)
	richnessspeciesR = [];
	#%gamma richness
	for (i in 1:S)
		AR = sort(R'[:,i])
		richnessspeciesR = [richnessspeciesR; unique(AR)]; 
	end;
	extantR = length(unique(sort(richnessspeciesR)));
	gamma = extantR;

	return gamma;
end


function OutputPerComponent(outputfilepercomp,r,r0,A,f,k,R,S,g,comp,ncomp,gamma)
	for i in 1:ncomp 
		nverts=length(comp[i]);
		richnessspeciesR = [];
		sum=0;
		for u in comp[i]
			sum=sum+length(out_edges(u,g));
			AR = sort(R'[:,u])
			richnessspeciesR = [richnessspeciesR; unique(AR)]; 
		end;
		gammacomp = length(unique(sort(richnessspeciesR)));
		writedlm(outputfilepercomp, [r r0 A f k i nverts sum/2 gammacomp gamma], ' ');
	end
	flush(outputfilepercomp);#To print in the output file each realization
end


function Output(outputfile,lastspecies,ri,S,J,G,maxDi,minDi,r0,A,f,cdynamics,vr,mr,Mr_gamma,Vr_gamma,Vr_r,Mr_r,Vr_t,Mr_t,Vr_e,Mr_e,Vr_c,Mr_c);
	writedlm(outputfile, [ri G maxDi minDi r0 A f cdynamics/G S J vr mr Mr_gamma Vr_gamma lastspecies Vr_r Mr_r Vr_t Mr_t Vr_e Mr_e Vr_c Mr_c], ' ');
	flush(outputfile);#To print in the output file each realization

	return; 
end


function CladogenesisEvent(R,Sti,Individual,lastspecies,ts,phylogenyfile,ri,mr,vr)
	newspeciesClado = lastspecies + 1;
        printPhylogeny(newspeciesClado,R[Sti,Individual],ts,phylogenyfile,ri,mr,vr);
   	R[Sti,Individual] = newspeciesClado;

	return R,newspeciesClado; 
end

function MigrationEvent(R,KillHab,MigrantHab,KillInd,Dc,J,S)
	target = KillHab;
	source=findfirst(Dc[target,:].>=MigrantHab);
        MigrantInd = rand(1:J);
        R[target,KillInd] = R[source,MigrantInd];
	return R;
end;

function BirthEvent(R,BirthLocal,KillInd,KillHab)
	R[KillHab,KillInd] = R[KillHab,BirthLocal];
	return R;
end

function printPhylogeny(new,old,ts,phylogenyfile,ri,mr,vr)
	writedlm(phylogenyfile,[ri mr vr old new ts],' ');
end

#Each site starts with one different species
function initialPopulation(S,J)
   R = zeros(S,J);
   lastspecies = 0;
   for(iJ in 1:J)#iterating over columns
      for(iS in 1:S)
         R[iS,iJ] = iS;
         lastspecies = iS; 
      end
   end   
   return R,lastspecies;
end

function demography(S,J,D,Dc,mr,vr,R,lastspecies,ri,ts,phylogenyfile)
	for (j = 1:(S*J))#For each individual in each site
       		#Demography - Resources
       		KillHab = rand(1:S);#which site to kill
		KillInd = rand(1:J);#which individual to kill
		mvb = rand();
		MigrantHab = rand()*maximum(Dc[KillHab,:]);
		BirthLocal = rand(1:J);#which individual to born

       		if mvb <= mr;#Migration event
			if(sum(Dc) > 0)#to guarantee that the migration will not occur if the landscape is completed disconnected
				R = MigrationEvent(R,KillHab,MigrantHab,KillInd,Dc,J,S);
			end

       		elseif (mvb > mr) && (mvb <= mr+vr);#Cladogenesis Speciation event
			R,lastspecies = CladogenesisEvent(R,KillHab,KillInd,lastspecies,ts,phylogenyfile,ri,mr,vr);

       		else #Birth event
			R = BirthEvent(R,BirthLocal,KillInd,KillHab);
       		end;
	end;#end S*J
	
	return R,lastspecies;
end

#####To calculate the logarithmic space of parameters, we use the function 'logspace': 
###logspace(X,Y,Z), where:
# X - is the power of the lowest value in the series
# Y - is the power of the highest value in the series
# Z - is the number of elements you want to have between 10^X and 10^Y.
# mode can be 1 or 2. If 'mode' has value 1, the model is static. If it has value 2, the model is dynamic
function getParameterValues(mode,highest,lowest,nvals)
    step = (highest-lowest)/nvals;
    R0s = [0.001;collect(lowest:step:highest)];
#    R0s = 0.001
    if(mode == 2)
        As = 1;
#        As = logspace(-3,0,nvals);
#        As = [0.001;collect(lowest:step:highest)];
        Fs = [0.001;collect(lowest:step:highest)];
    elseif (mode == 1)
        As = 0;
        Fs = 0;
    end
    return As,Fs,R0s;
end

function createOutputFiles(landscapeoutputs,sitesoutputs,phylogenyoutputs)
	if(isfile(landscapeoutputs)==false)
		outputfile = open(landscapeoutputs,"w");
		writedlm(outputfile,["ri G maxDi minDi r0 A f cdynamics/G S J vr mr Mr_Gamma Vr_gamma lastspecies Vr_r Mr_r Vr_t Mr_t Vr_e Mr_e Vr_c Mr_c"]);
		close(outputfile);
        end

	if(isfile(sitesoutputs)==false)
        	outputfilepercomp = open(sitesoutputs,"w");
        	writedlm(outputfilepercomp, ["ri r0 A f G Ncomponents Nverts Nedges GammaPerComponent Gamma"]);
        	close(outputfilepercomp);#To close 
        end

	if(isfile(phylogenyoutputs)==false)
		phylogenyfile = open(phylogenyoutputs,"w")	
		writedlm(phylogenyfile,["ri mr vr Ancestral Derived Age"]); 
		close(phylogenyfile);
	end
end

###################### Dynamic of the model

function Dynamic(mode,nvals,seed,nreal,Gmax,landG,S,J,mr,vr,landscapeoutputs,sitesoutputs,phylogenyoutputs)

	createOutputFiles(landscapeoutputs,sitesoutputs,phylogenyoutputs);
	outputfile = open(landscapeoutputs,"a");
	outputfilepercomp = open(sitesoutputs,"a");
	phylogenyfile = open(phylogenyoutputs,"a");

	for (ri in 1:nreal)#realizations
		srand(seed+(7*ri));
	#	G = rand(1:Gmax);	
		G = Gmax;	

		w = rand(S,2);#the location of the points of the landscape.
                minDi, maxDi, Di=getMatrixDistanceRGN(S,w);
		Amax = (maxDi - minDi)/2;

                As,Fs,R0s = getParameterValues(mode,1,0.01,nvals);

###### 	ISOLATED SITES AS INITIAL RGN	
		r0 = R0s[1];
		Di,D = InitialSeasonalRGN(S,w,r0);#To create the first RGN
		Dc, DI = transformMatrices(D,Di,S);
#                println(open("initialDc_$r0.txt","w"),Dc);
		g = createGraph(D,S);
		gtrans = graphTransitivity(D);
		comp = connected_components(g);
		ncomp = length(comp);
		nedges = num_edges(g);

		iR = 1;
		while(iR <= length(R0s))
			r0 = R0s[iR];
			iA = 1;
			while(iA <= length(As))
				A = As[iA];
				iF = 1;
				while(iF <= length(Fs))
					f = Fs[iF];
					cdynamics = 0;
					r_randomdynamics = zeros(Gmax+1);#store one value of r for each generation
					t_randomdynamics = zeros(Gmax+1);#store one value of r for each generation
					e_randomdynamics = zeros(Gmax+1);#store one value of r for each generation
					c_randomdynamics = zeros(Gmax+1);#store one value of r for each generation
					partialgamma = zeros(Gmax+1);#store one value of gamma for each generation
					r = r0;
					#Metacommunity
                                        R,lastspecies = initialPopulation(S,J);#start the population of each of the J individuals of each of the S sites.
#                                        println(open("initialpopulation_$r0.txt","w"),R);
#;#                                        println(open("initialpopulation.txt","w"),R);
					gamma = lastspecies;
					for (k = 1:G)#%metacommunity dynamic (not-tracking multitrophic metacommunity dynamics!)
						OutputPerComponent(outputfilepercomp,r,r0,A,f,k-1,R,S,g,comp,ncomp,gamma);
						r_randomdynamics[k] = r;
						t_randomdynamics[k] = gtrans;
						e_randomdynamics[k] = nedges;
						c_randomdynamics[k] = ncomp;
						partialgamma[k] = gamma;
						if((k % landG) == 0)#The parameter landG defines how often landscape is updated
#                                                        println(open("population_$k$r0.txt","w"),R);
							cdynamics = cdynamics + 1;
#							r, D, Di = SeasonalRGN(r0,A,f,S,cdynamics,w);
							r, D, Di = SeasonalRGN(r0,0,f,S,cdynamics,w);#Static landscape: A == 0
							Dc, DI = transformMatrices(D,Di,S);
#                                                        println(open("Dc_$k$r0.txt","w"),Dc);
							g = createGraph(D,S);
							gtrans = graphTransitivity(D); 
							comp = connected_components(g);
							ncomp = length(comp);
							nedges = num_edges(g);
						end
						R, lastspecies = demography(S,J,D,Dc,mr,vr,R,lastspecies,ri,k,phylogenyfile);
						gamma = GetRichness(R,S);
						push!(partialgamma,gamma);
					end
#                                        println(open("finalDc_$r0.txt","w"),Dc);
#                                        println(open("finalpopulation_$r0.txt","w"),R);
					OutputPerComponent(outputfilepercomp,r,r0,A,f,G,R,S,g,comp,ncomp,gamma);
                                        flush(phylogenyfile); 
					r_randomdynamics[Gmax+1] = r;
					t_randomdynamics[Gmax+1] = gtrans;
					e_randomdynamics[Gmax+1] = nedges;
					c_randomdynamics[Gmax+1] = ncomp;
					partialgamma[Gmax+1] = gamma;
                                        middle_point = round(Int,floor((Gmax + 1)/2)+1)
					#%random dynamic landscape
					Vr_r =  var(r_randomdynamics[middle_point:end]);
					Mr_r =  mean(r_randomdynamics[middle_point:end]);
					Vr_t =  var(t_randomdynamics[middle_point:end]);
					Mr_t =  mean(t_randomdynamics[middle_point:end]);
					Vr_e =  var(e_randomdynamics[middle_point:end]);
					Mr_e =  mean(e_randomdynamics[middle_point:end]);
					Mr_c =  mean(c_randomdynamics[middle_point:end]);
					Vr_c =  var(c_randomdynamics[middle_point:end]);	
					Vr_gamma =  var(partialgamma[middle_point:end]);
					Mr_gamma =  mean(partialgamma[middle_point:end]);
					Output(outputfile,lastspecies,ri,S,J,G,maxDi,minDi,r0,A,f,cdynamics,vr,mr,Mr_gamma,Vr_gamma,Vr_r,Mr_r,Vr_t,Mr_t,Vr_e,Mr_e,Vr_c,Mr_c);
					iF = iF+1;
				end#while nf
				iA = iA+1;
			end#while A
			iR = iR + 1;
		end#while r
	end#for ri
	close(outputfile);
        close(outputfilepercomp);#To close 
	close(phylogenyfile);
end#Dynamic


end #module
