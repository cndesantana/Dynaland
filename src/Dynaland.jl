# version 0.1

module Dynaland 

using StatsBase

function getSampleFromArray(items)
  return sample(items)
end

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
 
	r = r0 + r0*A*sin(pi*f*count);#Seasonal RGN
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
        return r, D, Di;
end



#########Demographic functions


function GetRichness(R,S,alpharich)
	richnessspeciesR = [];
	#%gamma richness
	for (i in 1:S)
		AR = sort(R[i])
		richnessspeciesR = [richnessspeciesR; unique(AR)]; 
		@inbounds alpharich[i] = length(unique(AR));
	end;
	extantR = length(unique(sort(richnessspeciesR)));
	gamma = extantR;

	return gamma,alpharich;
end


function OutputPerComponent(outputfilepercomp,minDi,maxDi,rc,r0,A,f,k,R,S,g,comp,ncomp,gamma)
	for i in 1:ncomp 
		nverts=length(comp[i]);
		richnessspeciesR = [];
		sum=0;
		for u in comp[i]
			sum=sum+length(out_edges(u,g));
			AR = sort(R[u])
			richnessspeciesR = [richnessspeciesR; unique(AR)]; 
		end;
		gammacomp = length(unique(sort(richnessspeciesR)));
		writedlm(outputfilepercomp, [minDi maxDi rc r0 A f k i nverts sum/2 gammacomp gamma], ' ');
	end
	flush(outputfilepercomp);#To print in the output file each realization
end

function OutputPerGeneration(outputfilepergen,lastspecies,ri,S,Ji,G,maxDi,minDi,rc,r0,A,f,vr,mr,gamma,alpharich,gtrans,nedges,ncomp)
	for i in 1:S
	    writedlm(outputfilepergen, [ri maxDi minDi i Ji[i] vr mr rc r0 A f G gtrans nedges ncomp gamma alpharich[i] lastspecies], ' ');
        end
	flush(outputfilepergen);#To print in the output file each realization
	return; 
end



function Output(outputfile,lastspecies,ri,S,Ji,G,maxDi,minDi,rc,r0,A,f,cdynamics,vr,mr,Mr_gamma,Vr_gamma,alpharich,Vr_r,Mr_r,Vr_t,Mr_t,Vr_e,Mr_e,Vr_c,Mr_c)
	for i in 1:S
		writedlm(outputfile, [ri G maxDi minDi rc r0 A f cdynamics/G i Ji[i] vr mr Mr_gamma Vr_gamma alpharich[i] lastspecies Vr_r Mr_r Vr_t Mr_t Vr_e Mr_e Vr_c Mr_c], ' ');
	end
	flush(outputfile);#To print in the output file each realization

	return; 
end


function CladogenesisEvent(R,Sti,Individual,lastspecies,ts,phylogenyfile,ri,mr,vr)
	newspeciesClado = lastspecies + 1;
#        printPhylogeny(newspeciesClado,R[Sti][Individual],ts,phylogenyfile,ri,mr,vr);
   	R[Sti][Individual] = newspeciesClado;

	return R,newspeciesClado; 
end

function MigrationEvent(R,KillHab,KillInd,Dc,Ji,S)
	MigrantHab = rand()*maximum(Dc[KillHab,:]);
	target = KillHab;
	allpos = find(Dc[target,:] .>= MigrantHab);#All the sites at a distance lower than the threshold 'MigrantHab'
	source = minimum(allpos[find(allpos .!= KillHab)]);
#	source = findfirst(Dc[target,:].>=MigrantHab);
	MigrantInd = rand(1:Ji[source]);
	#println("2.5.2.1-typeof(R) = ",typeof(R));
        R[target][KillInd] = R[source][MigrantInd];
	#println("2.5.2.2-typeof(R) = ",typeof(R));
	return R,MigrantHab;
end;

function BirthEvent(R,KillInd,KillHab,BirthLocal,Ji)
        #println("KillInd = ",KillInd);
        #println("BirthLocal = ",BirthLocal);
        #println("length(Ji) = ",length(Ji));
	#println("2.5.3.1-typeof(R) = ",typeof(R));
	R[KillHab][KillInd] = R[KillHab][BirthLocal];
	#println("2.5.3.2-typeof(R) = ",typeof(R));
	return R;
end

function printPhylogeny(new,old,ts,phylogenyfile,ri,mr,vr)
	writedlm(phylogenyfile,[ri mr vr old new ts],' ');
end

###Each site starts with one different species
##function initialPopulation(S,J)
##   R = zeros(S,J);
##   lastspecies = 0;
##   for(iJ in 1:J)#iterating over columns
##      for(iS in 1:S)
##         R[iS,iJ] = iS;
##         lastspecies = iS; 
##      end
##   end   
##   return R,lastspecies;
##end

function repeatval(x,N)
  return map(v -> v=x,1:N);
end

#Each site starts with one different species
function initialPopulation(R,Ji)
  lastspecies = 0;
  for(iJ in Ji)#iterating over columns
     lastspecies = lastspecies+1;
     popi = repeatval(lastspecies,iJ);
     push!(R,popi);
  end
  return R,lastspecies;
end

## return a random sample of N numbers from a normal (Gaussian) distribution
function rand_normal(mean, stdev, N)
    if stdev < 0.0
        error("standard deviation must be positive")
    end
    rn = zeros(Int,N);
    for(i in 1:N)
        u1 = rand()
        u2 = rand()
        r = sqrt( -2.0*log(u1) )
        theta = 2.0*pi*u2
        rn[i] = round(Int,mean + stdev*r*sin(theta));#vector of random integer numbers
    end
    return(rn);
end

function demography(S,Ji,D,Dc,mr,vr,R,lastspecies,ri,ts,phylogenyfile)
	for (ji = 1:(S*sum(Ji)))#For each individual in each site
                #println("ji = ",ji);
                realmr = mr;#Real migration rate, that can be zero if the landscape represents the "infinite island scenario" (all patches are isolated)
       		KillHab = rand(1:S);#which site to kill
        	#println("KillHab = ",KillHab);
        	#println("Ji[KillHab] = ",Ji[KillHab]);
#		KillInd = rand(1:J);#which individual to kill
		KillInd = rand(1:Ji[KillHab]);#which individual to kill
        	#println("KillInd = ",KillInd);
		#println("R = ",size(R));
		#println("R[KillHab] = ",length(R[KillHab]));
                BirthLocal = getSampleFromArray(1:length(R[KillHab]));#which individual to born
#                BirthLocal = rand(1:length(R[KillHab]));#which individual to born
        	#println("BirthLocal = ",BirthLocal);

                if(sum(Dc) == 0)
                   realmr = 0;#if there is no connectivity between patches, the real migration rate is 0 (there is no migration event, and there are more birth events as the sum between birth and speciation rates is still 1).
                end
		mvb = rand();
       		if (mvb <= realmr) && (mvb != 0)                  #;#Migration event
                        #println("Migration!");
			R,MigrantHab = MigrationEvent(R,KillHab,KillInd,Dc,Ji,S);
			#println("2.5.1-typeof(R) = ",typeof(R));
       		elseif (mvb > realmr) && (mvb <= realmr+vr)       #;#Cladogenesis Speciation event
                        #println("Clado!");
			R,lastspecies = CladogenesisEvent(R,KillHab,KillInd,lastspecies,ts,phylogenyfile,ri,realmr,vr);
			#println("2.5.2-typeof(R) = ",typeof(R));
       		else                                              #;#Birth event
                        #println("Birth!");
			R = BirthEvent(R,KillInd,KillHab,BirthLocal,Ji);
			#println("2.5.3-typeof(R) = ",typeof(R));
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
function getLinSpaceParameterValues(mode,highestfreq,lowestfreq,halfmaxDist,minDist,nvals)
    radisolated = minDist/10;# in the infinite island scenario, we use the radius value as one order of magnitude lower than the minimum distance between patches 
    stepmeanrad = (halfmaxDist-minDist)/nvals;
    R0s = [radisolated;collect(minDist:stepmeanrad:halfmaxDist)];
    if(mode == 2)
        stepfreq = (highestfreq-lowestfreq)/nvals;
        As = 1;
        Fs = collect(lowestfreq:stepfreq:highestfreq);
    elseif (mode == 1)
        As = 0;
        Fs = 0;
    end
    return As,Fs,R0s;
end


#####To calculate the logarithmic space of parameters, we use the function 'logspace': 
###logspace(X,Y,Z), where:
# X - is the power of the lowest value in the series
# Y - is the power of the highest value in the series
# Z - is the number of elements you want to have between 10^X and 10^Y.
# mode can be 1 or 2. If 'mode' has value 1, the model is static. If it has value 2, the model is dynamic
function getLogSpaceParameterValues(mode,highestfreq,lowestfreq,maxDist,minDist,nvals)
    R0s = logspace(log10(minDist),log10(maxDist),nvals);
    if(mode == 2)
        stepfreq = (highestfreq-lowestfreq)/nvals;
        As = 1;
        Fs = collect(lowestfreq:stepfreq:highestfreq);
    elseif (mode == 1)
        As = 0;
        Fs = 0;
    end
    return As,Fs,R0s;
end



function createOutputFiles(landscapeoutputs,sitesoutputs,phylogenyoutputs,landscapeoutputpergen)
	if(isfile(landscapeoutputs)==false)
		outputfile = open(landscapeoutputs,"w");
		writedlm(outputfile,["ri G maxDi minDi rc r0 A f cdynamics/G S Ji vr mr Mr_Gamma Vr_Gamma alpharich lastspecies Vr_r Mr_r Vr_t Mr_t Vr_e Mr_e Vr_c Mr_c"]);
		close(outputfile);
        end

	if(isfile(landscapeoutputpergen)==false)
		outputfilepergen = open(landscapeoutputpergen,"w");
	        writedlm(outputfilepergen, ["ri maxDi minDi S Ji vr mr rc r0 A f G gtrans nedges ncomp Gamma alpharich lastspecies"]);
		close(outputfilepergen);
        end


	if(isfile(sitesoutputs)==false)
        	outputfilepercomp = open(sitesoutputs,"w");
        	writedlm(outputfilepercomp, ["minDi maxDi rc r0 A f G Ncomponents Nverts Nedges GammaPerComponent Gamma"]);
        	close(outputfilepercomp);#To close 
        end

	if(isfile(phylogenyoutputs)==false)
		phylogenyfile = open(phylogenyoutputs,"w")	
		writedlm(phylogenyfile,["ri mr vr Ancestral Derived Age"]); 
		close(phylogenyfile);
	end
end

###################### Dynamic of the model

function Dynamic(mode,nvals,seed,nreal,Gmax,landG,S,J,sdev,mr,vr,landscapeoutputs,sitesoutputs,phylogenyoutputs,landscapeoutputpergen)

	createOutputFiles(landscapeoutputs,sitesoutputs,phylogenyoutputs,landscapeoutputpergen);
	outputfile = open(landscapeoutputs,"a");
	outputfilepercomp = open(sitesoutputs,"a");
	phylogenyfile = open(phylogenyoutputs,"a");
	outputfilepergen = open(landscapeoutputpergen,"a");

	for (ri in 1:nreal)#realizations
		srand(seed+(7*ri));
	#	G = rand(1:Gmax);	
		G = Gmax;	

		w = rand(S,2);#the location of the points of the landscape.
                minDi, maxDi, Di=getMatrixDistanceRGN(S,w);
                percDi = maxDi * sqrt(4.52 / (4 * pi * S) );
		Amax = (maxDi - minDi)/2;
                minfreq = 1/G;#the frequency that guarantees that we will have at least one entire cycle along the generations. 
#                As,Fs,R0s = getLinSpaceParameterValues(mode,1,maximum(collect([minfreq,0.01])),maxDi/2,minDi,nvals);#the minimum value of frequency will be 0.01 in normal cases, and will be 1/G if the total number of generations is lower or equal to 100, to guarantee that we will have at least one entire cycle along the generations
#                As,Fs,R0s = getLinSpaceParameterValues(mode,1,maximum(collect([minfreq,0.01])),maxDi,minDi,nvals);#the minimum value of frequency will be 0.01 in normal cases, and will be 1/G if the total number of generations is lower or equal to 100, to guarantee that we will have at least one entire cycle along the generations
#                As,Fs,R0s = getLinSpaceParameterValues(mode,1,maximum(collect([minfreq,0.01])),percDi,minDi,nvals);#the minimum value of frequency will be 0.01 in normal cases, and will be 1/G if the total number of generations is lower or equal to 100, to guarantee that we will have at least one entire cycle along the generations
                As1,Fs1,R0s1 = getLogSpaceParameterValues(mode,1,minimum(collect([minfreq,0.01])),percDi,minDi/10,nvals);#the minimum value of frequency will be 0.01 in normal cases, and will be 1/G if the total number of generations is lower or equal to 100, to guarantee that we will have at least one entire cycle along the generations
                As2,Fs2,R0s2 = getLogSpaceParameterValues(mode,1,minimum(collect([minfreq,0.01])),maxDi,percDi,nvals);#the minimum value of frequency will be 0.01 in normal cases, and will be 1/G if the total number of generations is lower or equal to 100, to guarantee that we will have at least one entire cycle along the generations
		As = [As1 As2];
		Fs = [Fs1 Fs2];
		R0s = [R0s1 R0s2];

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
                                        Ji = rand_normal(J,sdev,S);#S random numbers with mean J, standard deviation sdev
					R = [];
					#println("1-typeof(R) = ",typeof(R));
					#println("R is empty");
                                        R,lastspecies = initialPopulation(R,Ji);#start the population of each of the Ji individuals of each of the S sites.
					#println("2-typeof(R) = ",typeof(R));
                                        #println("size(R) = ",size(R));
#                                        println(open("initialpopulation_$r0.txt","w"),R);
#;#                                        println(open("initialpopulation.txt","w"),R);
					gamma = lastspecies;
					alpharich = ones(S);#each site starts with exactly one different species
					for (k = 1:G)#%metacommunity dynamic (not-tracking multitrophic metacommunity dynamics!)
#						OutputPerComponent(outputfilepercomp,minDi,maxDi,r,r0,A,f,k-1,R,S,g,comp,ncomp,gamma);
						r_randomdynamics[k] = r;
						t_randomdynamics[k] = gtrans;
						e_randomdynamics[k] = nedges;
						c_randomdynamics[k] = ncomp;
						partialgamma[k] = gamma;
					        OutputPerGeneration(outputfilepergen,lastspecies,ri,S,Ji,k-1,maxDi,minDi,r,r0,A,f,vr,mr,gamma,alpharich,gtrans,nedges,ncomp);
						if((k % landG) == 0)#The parameter landG defines how often landscape is updated
							cdynamics = cdynamics + 1;
							r, D, Di = SeasonalRGN(r0,A,f,S,cdynamics,w);#Static landscape: A == 0
							Dc, DI = transformMatrices(D,Di,S);
							g = createGraph(D,S);
							gtrans = graphTransitivity(D); 
							comp = connected_components(g);
							ncomp = length(comp);
							nedges = num_edges(g);
						end
					        #println("2.5-typeof(R) = ",typeof(R));
						R, lastspecies = demography(S,Ji,D,Dc,mr,vr,R,lastspecies,ri,k,phylogenyfile);
					        #println("3-typeof(R) = ",typeof(R));
						gamma,alpharich = GetRichness(R,S,alpharich);
						push!(partialgamma,gamma);
					end
#					OutputPerComponent(outputfilepercomp,minDi,maxDi,r,r0,A,f,G,R,S,g,comp,ncomp,gamma);
					OutputPerGeneration(outputfilepergen,lastspecies,ri,S,Ji,G,maxDi,minDi,r,r0,A,f,vr,mr,gamma,alpharich,gtrans,nedges,ncomp);
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
					Output(outputfile,lastspecies,ri,S,Ji,G,maxDi,minDi,r,r0,A,f,cdynamics,vr,mr,Mr_gamma,Vr_gamma,alpharich,Vr_r,Mr_r,Vr_t,Mr_t,Vr_e,Mr_e,Vr_c,Mr_c);
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
	close(outputfilepergen);
end#Dynamic


end #module
