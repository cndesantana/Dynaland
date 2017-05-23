@everywhere function myparallelfunction1(mr)
        command1 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_08Oct/runtests.jl 1 10 1000 $mr 0.5 10`;
        command2 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_08Oct/runtests.jl 2 10 1000 $mr 0.5 10`;
	run(command1);
	run(command2);
	return mr;
end

@everywhere function myparallelfunction2(mr)
        command2 = `/home/cdesantana/Downloads/julia/julia /home/cdesantana/results_08Oct/runtests.jl 2 10 1000 $mr 0.5 10`;

	run(command2);
	return mr;
end



function main()
	mr = [0.003, 0.03, 0.3, 0.001, 0.01, 0.1];
	results1 = pmap(myparallelfunction1, mr)
end

main();
