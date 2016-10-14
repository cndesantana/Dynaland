@everywhere function myparallelfunction1(mr)
#;#	command1 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/Data/Dynaland/revision/runtests.jl $model 20 1000 0.3`;
        command1 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_08Oct/runtests.jl 1 10 1000 $mr 0.5`;
        command2 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_08Oct/runtests.jl 2 10 1000 $mr 0.5`;
#        command2 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_30Sep/runtests.jl 2 10 1000 $mr 1`;

	run(command1);
	run(command2);
#	run(command2);
	return mr;
end

@everywhere function myparallelfunction2(mr)
#;#	command1 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/Data/Dynaland/revision/runtests.jl $model 20 1000 0.3`;
#        command1 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_30Sep/runtests.jl 1 10 1000 $mr 1`;
        command2 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_08Oct/runtests.jl 2 10 1000 $mr 0.5`;

#	run(command1);
	run(command2);
	return mr;
end



function main()
	mr = [0.003, 0.03, 0.3, 0.001, 0.01, 0.1];
	results1 = pmap(myparallelfunction1, mr)
#	results2 = pmap(myparallelfunction2, mr)
end

main();
