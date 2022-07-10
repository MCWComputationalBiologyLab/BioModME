split_rate_to_components <- function(){
    #take right hand side of equation and split into variables and parameters
    #store parameters to their proper vector
}

remove_rate_parameters_from_vectors <- function(parameter_to_remove)
{
    #search all parameters lists for parameter and remove it from each. (input, output, eqn, total)
    if (parameter_to_remove %in% params$inputs.vars) {
        params$inputs.vars <-
            params$inputs.vars[!params$inputs.vars %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% params$outputs.vars) {
        params$outputs.vars <-
            params$outputs.vars[!params$outputs.vars %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% params$eqns.vars) {
        params$eqns.vars <-
            params$eqns.vars[!params$eqns.vars %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% params$vars.all) {
        params$vars.all <-
            params$vars.all[!params$vars.all %in% parameter_to_remove]
    }
    #remove all excess variables from created lists if they exist (ie. we generated ui for parameter values and comments.  Will need to remove those)
}
