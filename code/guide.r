#argument_file = '.args.txt'
.guide_data_file = 'guide_data.csv'
description_file = 'guide_description.txt'
batch_file = '.guide_temp.bat'

'%!in%' <- function(x,y)!('%in%'(x,y))


make_batch_file <- function(args, batch_file) {
    
    #What are we asking GUIDE to do, broadly?#
    if( 'mode' %in% names(args) ) { 
        if( typeof(args$mode)=="character" ) { 
            if( args$mode.lower()[1] %in% c('m', 'f') ) { mode=1 } #model fitting 
            else if( args$mode.lower()[1] %in% c('i', 's', 'v') ) { mode=2 } #variable selection 
            else if( args$mode.lower()[1] %in% c('d', 'c') ) { mode=3 } #data conversion 
            else { stop("error: invalid mode") } }
        else if( typeof(args$mode)=="double" ) { mode = args$mode }
        else { stop('error: invalid value for mode') } }
    else { mode=1 } #Default is to do model fitting 


    #Where should GUIDE's output be saved?#
    default_output_file = 'guide_out.txt'
    if( 'output_file' %in% names(args) ) { 
        if( typeof(args$output_file)=="character" ) { output_file=args$output_file }            
        else { stop('error: invalid output file') } }
    else { output_file=default_output_file } #Default value


    #Is this a single tree or a forest?#
    if( 'forest' %in% names(args) ) { 
        if( typeof(args$forest)=="logical" ) { 
            if( args$forest ) { forest=2 }
            else { forest=1 } }
        else if( typeof(args$forest)=="double" ) { forest = args$forest }
        else { stop('error: invalid value for forest') } }
    else { forest=1 } #Default is a single tree
    
    
    #Is the goal classification or regression?#
    if( 'tree_type' %in% names(args) ) { 
        if( typeof(args$tree_type)=="double" ) { tree_type = args$tree_type }
        else if( typeof(args$tree_type)=="character" ) { 
            if( substring( args$tree_type, 1, 1 ) == 'c' ) { tree_type=1 }
            else if( substring( args$tree_type, 1, 1 ) == 'r' ) { tree_type=2 }
            else { stop("error: can't tell if you want classification or regression") } }
        else { stop("error: can't tell if you want classification or regression") } }
    else { tree_type=1 } #Default is to do classification
    
    
    #What regression/classification method?#
    if( 'method' %in% names(args) ) { 
        if( typeof(args$method)=="double" ) { method = args$method }            
        else if( typeof(args$method)=="character" & tree_type == 1 ) { #Classification methods
            if( substring( args$method, 1, 1 ) == 's' ) { method=1 } #simple
            else if( substring( args$method, 1, 1 ) == 'n' ) { method=2 } #nearest-neightbor
            else if( substring( args$method, 1, 1 ) == 'k' ) { method=3 } #kernel
            else { stop("error: can't understand your specified classification method") } }
        else if( typeof(args$method)=="character" & tree_type == 2 ) { #Regression methods
            if( substring( args$method, 1, 1 ) == 'l' ) { method=1 } #linear
            else if( substring( args$method, 1, 1 ) == 'q' ) { method=2 } #quantile
            else if( substring( args$method, 1, 1 ) == 'p' ) { method=3 } #poisson
            else if( substring( args$method, 1, 1 ) == 'h' ) { method=4 } #hazard
            else if( substring( args$method, 1, 1 ) == 'l' ) { method=5 } #longitudinal
            else { stop("error: can't understand your specified regression method") } }
        else { stop("error: can't understand your specified method") } }
    else { method=1 } #Default is to do linear regression or simple classification
    
    
    
    #What regression criterion?#
    if( tree_type == 2 ) { #tree_type==2 means we're doing regression
        if( 'criterion' %in% names(args) ) { 
            if( typeof(args$criterion)=="double" ) { criterion = args$criterion }                
            else if( typeof(args$criterion)=="character" ) { 
                if( substring( args$criterion, 1, 1 ) == 's' ) { criterion=1 } #least sum of squares
                else if( substring( args$criterion, 1, 1 ) == 'm' ) { criterion=2 } #least median of squares
                else { stop("error: invalid regression criterion") } }
            else { stop("error: can't understand your regression criterion") } }
        else { criterion=1 } } #Default to minimizing the sum of squares
    else { criterion=FALSE } #FALSE tells us we're not doing regression and so need no criterion
    
    
    #How should the terminal nodes look?#
    if( tree_type == 2 ) { #tree_type==2 means we're doing regression
        if( 'node_complexity' %in% names(args) ) { 
            if( typeof(args$node_complexity)=="double" ) { node_complexity = args$node_complexity }                
            else if( typeof(args$node_complexity)=="character" ) { 
                if( substring( args$node_complexity, 1, 1 ) == 's' ) { node_complexity=0 } #stepwise
                else if( substring( args$node_complexity, 1, 1 ) == 'm' ) { node_complexity=1 } #multiple
                else if( substring( args$node_complexity, 1, 1 ) == 'p' ) { node_complexity=2 } #polynomial
                else if( substring( args$node_complexity, 1, 1 ) == 'c' ) { node_complexity=3 } #constant
                else if( substring( args$node_complexity, 1, 1 ) == 'a' ) { node_complexity=4 } #ANCOVA
                else { stop("error: invalid node type") } }
            else { stop("error: can't understand node type") } }
        else { node_complexity=3 } } #Default to constant nodes
    else { node_complexity=FALSE } #FALSE tells us we're not doing regression and so need no node type
    
    
    #How should we select the variables for terminal-node regression?#
    if( !identical(node_complexity, FALSE) & node_complexity == 0 ) { #applicable only to stepwise selection
        if( 'step_direction' %in% names(args) ) { 
            if( typeof(args$step_direction)=="double" ) { step_direction = args$step_direction }                
            else if( typeof(args$step_direction)=="character" ) { 
                if( substring( args$step_direction, 1, 1 ) == 'b' ) { step_direction=1 } #forward selection and backward deletion
                else if( substring( args$step_direction, 1, 1 ) == 'f' ) { step_direction=2 } #forward selection
                else if( substring( args$step_direction, 1, 1 ) == 'a' ) { step_direction=3 } #check all subsets
                else { stop("error: invalid choice of stepwise selection") } }
            else { stop("error: can't understand choice of stepwise selection") } }
        else { step_direction=1 } } #Default to forward selection/backward deletion
    else { step_direction=FALSE } #FALSE tells us we don't need to do stepwise selection
    
    
    #How many variables (max) per terminal-node regression equation?#
    if( !identical(node_complexity, FALSE) & node_complexity %in% c(0,1,2,4) ) { #not applicable to constant terminal nodes
        if( 'max_variables' %in% names(args) ) { 
            if( typeof(args$max_variables)=="double" ) { max_variables = args$max_variables }                
            else { stop("error: can't understand max_variables") } }
        else { max_variables=0 } } #Default to all possible variables
    else { max_variables=FALSE } #FALSE tells us we don't need to select variables for the terminal nodes  
    
    
    #What F-statistic for a variable to be selected?#
    if( !identical(node_complexity, FALSE) & node_complexity %in% c(0,1,2,4) ) { #not applicable to constant terminal nodes
        if( 'F_to_enter' %in% names(args) ) { 
            if( typeof(args$F_to_enter)=="double" ) { F_to_enter = args$F_to_enter }                
            else { stop("error: can't understand F_to_enter") } }
        else { F_to_enter=4. } } #Default to 4
    else { F_to_enter=FALSE } #FALSE tells us we don't need F_to_enter 
    
    #What F-statistic for a variable to be removed?#
    if( !identical(node_complexity, FALSE) & node_complexity %in% c(0,1,2,4) ) { #not applicable to constant terminal nodes
        if( 'F_to_delete' %in% names(args) ) { 
            if( typeof(args$F_to_delete)=="double" ) { F_to_delete = args$F_to_delete }                
            else { stop("error: can't understand F_to_delete") } }
        else { F_to_delete=3.99 } } #Default to 4
    else { F_to_delete=FALSE } #FALSE tells us we don't need F_to_delete   
    
    
    #How to truncate predicted values?#
    if( !identical(node_complexity, FALSE) & node_complexity %in% c(0,1,2,4) ) { #not applicable to constant terminal nodes
        if( 'truncate' %in% names(args) ) { 
            if( typeof(args$truncate)=="double" ) { truncate = args$truncate }
            else if( typeof(args$truncate)=="character" ) { 
                if( substring( args$truncate, 1, 4 ) == 'none' ) { truncate=0 } #no truncation
                else if( substring( args$truncate, 1, 4 ) == 'node' & '+' %!in% args$truncate ) { truncate=1 } #truncate to node range
                else if( substring( args$truncate, 1, 4 ) == 'node' & '+' %in% args$truncate ) { truncate=2 } #truncate to node range + 10%
                else if( substring( args$truncate, 1, 1 ) == 'g' ) { truncate=3 } #truncate to global range
                else if( substring( args$truncate, 1, 1 ) == 'w' ) { truncate=4 } #Windsorization (2-sided)
                else { stop("error: invalid truncate parameter") } }
            else { stop("error: can't understand truncate parameter") } }
        else { truncate=0 } } #Default to no truncation
    else { truncate=FALSE } #FALSE tells us we don't need to truncate predicted values
    
    
    #Test for interactions?#
    if( 'interactions' %in% names(args) ) { 
        if( typeof(args$interactions)=="double" ) { interactions = args$interactions }            
        else if( typeof(args$interactions)=="logical" & tree_type == 2 ) { #Options under regression
            if( args$interactions ) { interactions=1 } #test for interactions
            else { interactions=2 } } #skip interaction tests
        else if( typeof(args$interactions)=="logical" & tree_type != 2 ) { #Options under classification and selection
            if( args$interactions ) { interactions=1 } #linear & interaction tests
            else { interactions=3 } } #skip both tests
        else { stop("error: can't understand interaction setting") } }
    else { interactions=1 } #Default is to test for interactions
    
    #Prune the tree?#
    if( mode == 1 ) { #prune is only used for model fitting.
        if( 'prune' %in% names(args) ) { 
            if( typeof(args$prune)=="double" ) { prune = args$prune }                
            else if( typeof(args$prune)=="logical" & tree_type == 2 ) { #for regression, prune is boolean
                if( args$prune ) { prune=1 } #prune by CV
                else { prune=2 } } #no pruning
            else if( typeof(args$prune)=="character" & tree_type == 1 ) { 
                if( substring( args$prune, 1, 1 ) == 'c' ) { prune=1 } #prune by CV
                else if( substring( args$prune, 1, 1 ) == 'v' ) { prune=2 } #prune by validation set
                else if( substring( args$prune, 1, 1 ) == 'n' ) { prune=3 } #no pruning
                else { stop("error: invalid prune parameter") } }
            else if( typeof(args$prune)=="character" & tree_type == 2 ) { 
                if( substring( args$prune, 1, 1 ) == 'c' ) { prune=1 } #prune by CV
                else if( substring( args$prune, 1, 1 ) == 'n' ) { prune=2 } #no pruning
                else { stop("error: invalid prune parameter") } }
            else { stop("error: can't understand prune parameter") } }
        else { prune=1 } } #Default is to prune by CV
    else { prune=FALSE }

    
    #What is the path to the data description file?#
    if( 'data_description' %in% names(args) ) { 
        if( typeof(args$data_description)=="character" ) { data_description = args$data_description }            
        else { stop('error: invalid data description file') } }
    else { stop("error: no data description file is specified") } #Default value

    #How many CV folds?#
    if( prune == 1 ) { #folds only makes sense if we're pruning by CV
        if( 'folds' %in% names(args) ) { 
            if( typeof(args$folds)=="double" ) { 
                folds = args$folds
                if( args$folds==-1 ) { default_folds=1 }
                else { default_folds=2 } }
            else { stop("error: can't understand folds parameter") } }
        else { 
            default_folds=1 #Default is to use the default number of folds
            folds=-1 } }
    else { folds=FALSE }
    
    
    #How to quantify CV error?#
    if( prune == 1 ) { #cv_error only makes sense if we're pruning by CV
        if( 'cv_error' %in% names(args) ) { 
            if( typeof(args$cv_error)=="double" ) { cv_error = args$cv_error }                
            else if( typeof(args$cv_error)=="character" ) { 
                if( substring( args$cv_error, 1, 4 ) == 'mean' ) { cv_error=1 } #mean-based CV tree
                else if( substring( args$cv_error, 1, 6 ) == 'median' ) { cv_error=2 } #median-based CV tree
                else { stop("error: invalid cv_error string") } }
            else { stop("error: can't understand cv_error parameter") } }
        else { cv_error=1 } } #Default is to use the mean-based tree
    else { cv_error=FALSE }
    
    
    #What improvement in CV error is required to justify a split?#
    if( prune == 1 ) { #cv_error only makes sense if we're pruning by CV
        if( 'cv_gain' %in% names(args) ) { 
            if( typeof(args$cv_gain)=="double" ) { cv_gain = args$cv_gain }                
            else { stop("error: can't understand cv_error parameter") } }
        else { cv_gain=0. } } #Default is to include any split that improves the fit 
    else { cv_gain=FALSE }  
    
    
    #How to set the prior probability of each class?#
    if( tree_type == 1 ) { #prior probability is only valid for classification
        if( 'priors' %in% names(args) ) { 
            if( typeof(args$priors)=="double" ) { priors = args$priors }
            else if( typeof(args$priors)=="character" ) { 
                #don't know how to do this check in R:
                #if( os.path.isfile(args$priors) )  #path to file holding the prior probabilities
                    #priors = 3
                    #prior_path = args$priors #
                if( substring( args$priors, 1, 1 ) == 'p' ) { priors=1 } #priors are given by the training set proportions
                else if( substring( args$priors, 1, 1 ) == 'e' ) { priors=2 } #equal priors
                else if( args$priors[1] %in% c('f', 'o') ) { 
                    priors = 3 #file holds the prior probabilities
                    if( prior_path %in% args ) { prior_path=args$prior_path }                        
                        #don't know how to do this check in R  if( os.path.isfile(args$prior_path) ) { prior_path=args$prior_path }
                    else { stop("error: prior_path does not point to any file!") } }
                else {
                    priors=3
                    prior_path=args$priors } }
                #else  return "error: invalid priors string"#             
            else { stop("error: can't understand priors parameter") } }
        else { priors=2 } } #Default is to assign equal prior probability to each class 
    else { priors=FALSE }       
    
    
    #How to set the misclassification cost of each class?#
    if( tree_type == 1 ) { #cost is only valid for classification
        if( 'costs' %in% names(args) ) { 
            if( typeof(args$costs)=="double" ) { costs = args$costs }
            else if( typeof(args$costs)=="character" ) { 
                #Don't know how to do this check in Rif( os.path.isfile(args$costs) )  #path to file holding the misclassification costs
                #costs = 3
                #cost_path = args$costs #
                if( args$costs[1] %in% c('u', 'e') ) { costs=1 } #costs are unity
                else if( args$costs[1] %in% c('o', 'f') ) { 
                    costs = 2 #file holds the misclassification costs
                    if( cost_path %in% args ) { cost_path=args$cost_path }                        
                        #don't know how to do this check in R if( os.path.isfile(args$cost_path) ) { cost_path=args$cost_path }
                    else { stop("error: cost_path does not point to any file!") } }
                else { 
                    costs = 3
                    cost_path = args$costs } }
                #else  return "error: invalid costs string"    #            
            else { stop("error: can't understand costs parameter") } }
        else { costs=1 } } #Default is to assign equal cost to each class 
    else { costs=FALSE }        
    
    #Split on quantiles or do an exhaustive search?#
    if( 'search' %in% names(args) ) { 
        if( typeof(args$search)=="double" ) { search = args$search }
        else if( typeof(args$search)=="logical" ) { 
            if( !args$search ) { search=1 } #split on quantiles
            else { search=2 } } #do an exhaustive search
        else if( typeof(args$search)=="character" ) { 
            if( substring( args$search, 1, 1 ) == 'q' ) { search=1 } #split on quantiles
            else if( substring( args$search, 1, 1 ) == 'e' ) { search=2 } #do an exhaustive search
            else { stop("error: can't tell whether to split on quantiles or do an exhaustive search") } }
        else { stop("error: can't tell whether to split on quantiles or do an exhaustive search") } }
    else { search=2 } #Default is to use an exhaustive search
    
    
    #Set the maximum number of splits#
    if( 'splits' %in% names(args) ) { 
        if( typeof(args$splits)=="double" ) { 
            splits = args$splits
            if( args$splits == -1 ) { default_splits=TRUE }
            else { default_splits=FALSE } }
        else { stop("error: can't understand splits parameter") } }
    else { default_splits=TRUE } #Default is to use the default number of splits
        
    
    #Set the minimum number of observations in each terminal node#
    if( 'min_nodesize' %in% names(args) ) { 
        if( typeof(args$min_nodesize)=="double" ) { 
            min_nodesize = args$min_nodesize
            if( args$min_nodesize == -1 ) { default_min_nodesize=TRUE }
            else { default_min_nodesize=FALSE } }
        else { stop("error: can't understand min_nodesize parameter") } }
    else { default_min_nodesize=TRUE } #Default is to use the default min_nodesize   
    
    
    #Create a LaTeX tree diagram?#
    default_latex_path = 'guide_latex_out.tex'
    if( mode == 1 ) { #Tree diagram is only used in model fitting.
        if( 'latex' %in% names(args) ) { 
            if( typeof(args$latex)=="double" ) { 
                latex = args$latex
                if( args$latex==2 ) { 
                    if( 'latex_path' %in% names(args) ) { 
                        if( typeof(args$latex_path)=="character" ) { latex_path = args$latex_path }
                        else { stop("error: latex_path is not a string") } }
                    else { latex_path=default_latex_path } } } #default latex file
            else if( typeof(args$latex)=="logical" ) { 
                if( isTRUE(args$latex) ) { 
                    latex = 2
                    if( 'latex_path' %in% names(args) & typeof(args$latex_path)=="character" ) { latex_path = args$latex_path }
                    else { latex_path=default_latex_path } }
                else { latex=1 } } #No latex diagram
            else if( typeof(args$latex)=="character" ) { 
                if( 'latex_path' %!in% args ) { 
                    latex=2
                    latex_path=args$latex }
                else { stop("error: conflicting latex and latex_path parameters") } }
            else { stop("error: can't understand latex parameter") } }
        else { 
            latex=2 #Default is to produce a tree diagram
            latex_path=default_latex_path } }
    else { latex=FALSE }
    
    
    #Make a vertical or a sideways tree?#
    if( latex == 2 ) { #vertical/horizontal assumes we're making a tree diagram
        if( 'orientation' %in% names(args) ) { 
            if( typeof(args$orientation)=="double" ) { orientation = args$orientation }
            else if( typeof(args$orientation)=="character" ) { 
                if( substring( args$orientation, 1, 1 ) == 'v' ) { orientation=1 } #vertical tree
                else if( substring( args$orientation, 1, 1 ) == 'h' ) { orientation=2 } } #horizontal tree
            else { stop("error: can't understand orientation parameter") } }
        else { orientation=1 } } #Default is to make a vertical diagram
    else { orientation=FALSE }
    
    
    #Include node numbers?#
    if( latex == 2 ) { #we must have a diagram in which to include node numbers
        if( 'node_numbers' %in% names(args) ) { 
            if( typeof(args$node_numbers)=="double" ) { 
                if( node_numbers==0 ) { node_numbers=2 }
                else { 
                    node_numbers=1
                    which_nodes=args$node_numbers } }
            else if( typeof(args$node_numbers)=="logical" ) { 
                if( args$node_numbers ) { 
                    node_numbers=1
                    which_nodes=2 } #print all node numbers
                else { node_numbers=2 } } #print no node numbers
            else if( typeof(args$node_numbers)=="character" ) { 
                if( substring( args$node_numbers, 1, 1 ) == 'n' ) { node_numbers=2 } #print no node numbers
                else if( substring( args$node_numbers, 1, 1 ) == 't' ) { 
                    node_numbers=1
                    which_nodes=1 } #print terminal node numbers
                else if( substring( args$node_numbers, 1, 1 ) == 'a' ) { 
                    node_numbers=1
                    which_nodes=2 } } #print all node numbers
            else { stop("error: can't understand node_numbers parameter") } }
        else { 
            node_numbers=1 #Default is to print only terminal node numbers
            which_nodes=1 } }
    else { node_numbers=FALSE }
    
    
    #Color the terminal nodes?#
    if( latex == 2 ) { #we must have a diagram to color
        if( tree_type == 1 ) { #then we're doing classification and color is a boolean decision
            if( 'color' %in% names(args) ) { 
                if( typeof(args$color)=="double" ) { color=args$color }
                else if( typeof(args$color)=="logical" ) { 
                    if( isTRUE(args$color) ) { color=1 }
                    else { color=2 } }
                else { stop("error: can't understand color parameter") } }
            else { color=1 } } #default is to color the terminal nodes of a classification tree
        else if( tree_type == 2 ) { #we're doing regression and color is a number from 1-11.
            if( 'color' %in% names(args) ) { 
                if( typeof(args$color)=="double" ) { color = args$color }
                else if( typeof(args$color)=="character" ) { 
                    if( substring( args$color, 1, 1 ) == 'w' ) { color=1 } #white
                    else if( substring( args$color, 1, 1 ) == 'l' ) { color=2 } #lightgray
                    else if( substring( args$color, 1, 4 ) == 'gray' | substring( args$color, 1, 4 )=='grey' ) { color=3 } #gray
                    else if( substring( args$color, 1, 1 ) == 'd' ) { color=4 } #darkgray
                    else if( substring( args$color, 1, 5 ) == 'black' ) { color=5 } #black
                    else if( substring( args$color, 1, 1 ) == 'y' ) { color=6 } #yellow
                    else if( substring( args$color, 1, 1 ) == 'r' ) { color=7 } #red
                    else if( substring( args$color, 1, 4 ) == 'blue' ) { color=8 } #blue
                    else if( substring( args$color, 1, 5 ) == 'green' ) { color=9 } #green
                    else if( substring( args$color, 1, 1 ) == 'm' ) { color=10 } #magenta
                    else if( substring( args$color, 1, 1 ) == 'c' ) { color=11 } #cyan
                    else { stop("error: invalid color parameter") } }
                else { stop("error: can't understand color parameter") } }
            else { color=6 } } } #default is to make terminal nodes of a regression tree yellow
    else { color=FALSE }
    
    
    #save the split/fit variables?#
    if( mode == 1 ) { #Tree diagram is only used in model fitting.
        save_split_fit=1 } #Default is no save
    else { save_split_fit=FALSE }
    
    
    #save the node IDs?#
    if( mode == 1 ) { #Tree diagram is only used in model fitting.
        save_node_IDs=1 } #Default is no save
    else { save_node_IDs=FALSE } 
    
    
    #save the terminal node IDs?#
    if( mode == 1 ) { #Tree diagram is only used in model fitting.
        save_terminal_IDs=1 } #Default is no save
    else { save_terminal_IDs=FALSE } 
    
    
    #open the batch file that we will write to.
    current_wd = paste(getwd(), "/", sep="")
    batch_file = file(paste(current_wd, batch_file, sep=''), 'w')
    
    writeLines( '123321', batch_file)                     #GUIDE expects this at the top of a batch file.
    writeLines( as.character(mode), batch_file)                       #Default mode is 1, for model-fitting.
    writeLines( as.character(paste('"', current_wd, output_file, '"', sep='')), batch_file)    #write the path to the output file.
    writeLines( as.character(forest), batch_file)
    writeLines( as.character(tree_type), batch_file)
    writeLines( as.character(method), batch_file)
    if( criterion ) { writeLines( as.character(criterion), batch_file) }
    if( !identical(node_complexity, FALSE) ) { writeLines( as.character(node_complexity), batch_file) }
    if( !identical(step_direction, FALSE) ) { writeLines( as.character(step_direction), batch_file) }
    if( !identical(max_variables, FALSE) ) { writeLines( as.character(max_variables), batch_file) }
    if( !identical(F_to_enter, FALSE) ) { writeLines( as.character(F_to_enter), batch_file) }
    if( !identical(F_to_delete, FALSE) ) { writeLines( as.character(F_to_delete), batch_file) }
    if( !identical(truncate, FALSE) ) { writeLines( as.character(truncate), batch_file) }
    writeLines( as.character(interactions), batch_file)
    if( !identical(prune, FALSE) ) { writeLines( as.character(prune), batch_file) }
    writeLines( as.character(paste('"', current_wd, data_description, '"', sep='')), batch_file)
    if( !identical(folds, FALSE) ) { 
        writeLines( as.character(default_folds), batch_file)
        if( default_folds==2 ) { writeLines( as.character(folds), batch_file) } }
        
    if( !identical(cv_error, FALSE) ) { writeLines( as.character(cv_error), batch_file) }
    if( !identical(cv_gain, FALSE) ) { writeLines( as.character(cv_gain), batch_file) }
    if( !identical(priors, FALSE) ) { writeLines( as.character(priors), batch_file) }
    if( priors==3 ) { writeLines( as.character(prior_path), batch_file) }
    if( !identical(costs, FALSE) ) { writeLines( as.character(costs), batch_file) }
    if( costs==2 ) { writeLines( as.character(cost_path), batch_file) }
    writeLines( as.character(search), batch_file)
    
    if( default_splits ) { writeLines( as.character(1), batch_file) }
    else { 
        writeLines( as.character(2), batch_file)
        writeLines( as.character(splits), batch_file) }
    
    if( default_min_nodesize ) { writeLines( as.character(1), batch_file) }
    else { 
        writeLines( as.character(2), batch_file)
        writeLines( as.character(min_nodesize), batch_file) }
    
    if( !identical(latex, FALSE) ) { 
        writeLines( as.character(latex), batch_file)
        if( latex==2 ) { writeLines( paste('"', current_wd, as.character(latex_path), '"', sep=''), batch_file) } }
        
    if( !identical(orientation, FALSE) ) { writeLines( as.character(orientation), batch_file) }
    if( !identical(node_numbers, FALSE) ) { 
        writeLines( as.character(node_numbers), batch_file) 
        if( node_numbers==1 ) { writeLines( as.character(which_nodes), batch_file) } }
        
    if( !identical(color, FALSE) ) { writeLines( as.character(color), batch_file) }
    if( !identical(save_split_fit, FALSE) ) { writeLines( as.character(save_split_fit), batch_file) }
    if( !identical(save_node_IDs, FALSE) ) { writeLines( as.character(save_node_IDs), batch_file) }
    if( !identical(save_terminal_IDs, FALSE) ) { writeLines(as.character( save_terminal_IDs), batch_file) }
    
    flush(batch_file)
    close(batch_file)
    
    
    if(!is.na(latex)) {
        if(latex==2) { return(latex_path) }
        else { return(FALSE) } }
    else { return(FALSE) }
}        


dump_data <- function(formula, data) {
    t = terms.formula(formula, data=data)
    target = as.character(attr(t, 'variables')[2])
    
    .description = file(description_file, 'w')
    current_wd = paste(getwd(), "/", sep="")
    
    writeLines(paste('"', current_wd, .guide_data_file, '"', sep=''), .description)
    writeLines('NA', .description)
    writeLines(paste("column", "varname","vartype", sep=' '), .description)
    
    i=1
    
    columns = as.character(attr(t, 'variables'))[-1L]
    model_data = data[,columns]
    
    for(column in columns) {
        
        if( column==target ) { label = 'd' }
        else if( is.factor(data[,column]) ) { label='b' }
        else if( is.numeric(data[,column]) ) { label='n' }
        
        column = gsub('_', '', column, fixed=T)
        writeLines(paste(i, column, label, sep=' '), .description) 
        
        i = i+1 }
    close(.description)
    
    write.table(model_data, file=.guide_data_file, quote=F, sep=',', col.names=F, row.names=F, na="NA")   
}


print_tree_diagram <- function(latex_path) {
    raw <- readLines(latex_path)
    processed = vector()
    in_tree=FALSE
    
    for(line in raw) {
        stripped = gsub("^ *", "", gsub(" *$", "", line))
        if(in_tree==TRUE) {
            if(stripped=="\\end{document}") { in_tree=FALSE }
            else { cat(paste(line, "\n", sep='')) } }
        else if(stripped=="\\begin{document}") { in_tree=TRUE } }
}


guide <- function(formula, data, ...) {
    #parse the arguments
    args=list(...)
    
    #write the data and data description (via dump_data)
    dump_data(formula, data)
    
    #create the batch file (by dumping the arguments to a file and then calling python with guide_wrapper.py)
    args$data_description = description_file
    #dump_args(args)
    
    #determine whether to do regression or classification based on whether the output
    #variable is numeric or a factor, but if a tree_type argument is supplied then use that.
    target = as.character(attr(terms.formula(formula, data=data), 'variables')[2])
    if( 'tree_type' %in% names(args) ) { pass=TRUE }
    else if( is.numeric(data[,target]) ) { args$tree_type='regression' }
    else { args$tree_type='classification' }

    #make the batch file
    latex = make_batch_file(args, batch_file)
    
    #run GUIDE on the batch file
    current_wd = paste(getwd(), "/", sep='')
    if( .Platform$OS.type=="windows" ) { shell(paste("guide < ", current_wd, batch_file, sep='')) }
    else { system(paste("guide < ", current_wd, batch_file, sep='')) }
    
    #get the results
    if('sweave' %in% names(args)) {
        if(typeof(latex)=='character' & isTRUE(args$sweave)) { print_tree_diagram(latex) } }
    
    #delete files that were temporarily created for this process
    unlink( .guide_data_file )
    unlink( description_file )
    unlink( batch_file )
    
}