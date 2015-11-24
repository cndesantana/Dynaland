using DataFrames
using Vega

function dc(d0,A,t,f)
   dc_ = d0 + d0 * A * sin(pi * f * t);
   return(dc_)
end

function builddf(nval,mytime)
   df = DataFrame(d0 = Float64[], A = Float64[], f = Float64[], t = Int64[], dc = Float64[]);
   myAs = logspace(-2,0,nval);
   myFs = logspace(-2,0,nval);
   myD0s = logspace(-2,0,nval);
    
   for(iD0s in 1:nval)
      for(iA in 1:nval)
         for(iFs in 1:nval)
            for(myt in 1:mytime)
               push!(df,[myD0s[iD0s] myAs[iA] myFs[iFs] myt dc(myD0s[iD0s],myAs[iA],myt,myFs[iFs])]);
            end
         end
      end
   end
   return(df)
end

function main()
   mydf = builddf(10,10);
   mydf = mydf[(mydf[:f].==0.01),:]#only the frequency we want
   mydf = mydf[(mydf[:t].==10),:]#only the time step we want
   mydf = mydf[:, filter(x -> x != :f, names(mydf))]#excluding the column of frequency 
   mydf = mydf[:, filter(x -> x != :t, names(mydf))]#excluding the column of time
   hm = heatmap(x = mydf[:d0], y = mydf[:A], color = mydf[:dc]);
   hm.background = "white"
   hm
end
