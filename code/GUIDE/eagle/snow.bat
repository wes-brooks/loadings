123321      (do not change this line and the order of the following lines!)
 1          (1=model fitting, 2=importance ranking, 3=data conversion)
"../../../output/eagle/guide.txt"  (name of output file)
 1          (1=one tree, 2=ensemble)
 2          (1=classification, 2=regression)
 1          (1=linear, 2=quantile, 3=Poisson, 4=hazard, 5=longitudinal/multiresponse)
 1          (1=least squares, 2=least median of squares)
 3          (0=stepwise, 1=multiple, 2=polynomial, 3=constant, 4=ANCOVA)
 1          (1=interaction tests, 2=skip them)
 1          (1=prune by CV, 2=no pruning)
"./snow.txt"  (name of data description file)
 1          (1 to use default number of cross-validations, 2 to change it)
 1          (Tree selection method)
    0     (SE number for pruning)
 2          (1=split point from quantiles, 2=use exhaustive search)
 1          (1=use default max split levels, 2=change it)
 1          (1=use default mindat, 2=change it)
 2          (1=skip latex, 2=write latex)
"../../../output/eagle/tree.tex" (latex file name)
 1          (1=vertical tree, 2=sideways tree)
 1          (1=include node numbers, 2=exclude)
 2          (1=number all nodes, 2=only leaf nodes)
 6          (choice of colors: 1-11)
 3          (1=no storage, 2=store fit and split variables, 3=store split variables and values)
"../../../output/eagle/splits.txt" (split variable file name)
 1          (1=do not save node IDs in separate file, 2=save them)
 1          (1=do not save terminal node IDs in a file, 2=save them)
