################################################################################
################# FUNCTION: law_mass_action ####################
# Generated equations for law of mass action for given inputs. Where law of mass action is:
# for equation aA + bB (kr)<-->(kf) cC + dD then the derviation is as follows:
# -(1/a)*A^a = -(1/b)*B^b = (1/c)*C^c = (1/d)*D^d = kf*A^a*B^b - kr*C^c*D^d

# Example equation A + 2B ->(kf1) C used here
# Inputs:
# RHS_coef - Coefficients of variables on RHS of equation in vector form c(1)
# RHS_var - Var Name on right hand side in vector form: c(C)
# LHS_coef - Coefficients of variables on LHS of equation in vector form: c(1, 2)
# LHS_var - Variable names of left hand side equations in vector form: c(A, B)
# arrowtype - Describes if reaction is forward (forward_only) or both (both_directions): "both_directions"
# kf - numerical value of forward rate constant: kf1
# kr - numerical value of reverse rate constant: NULL
# var_on_left - boolean value that tells if the current variable is on the LHS.  For example if this was deriving for A, then TRUE.  IF this was deriving for C then false, (B=TRUE)
# var_coef - coefficient of lookup variable. A-->1, B-->2, C-->1, so if looking up B value would be 2.

# Outputs:
# String of law of mass action result.  For example for A:
################################################################################

law_mass_action <- function(RHS_coef, RHS_var, LHS_coef, LHS_var, arrow_type, kf, kr, var_on_left, var_coef){
    #Case1: A -> B, one var on each side
    if (length(RHS_var) == 1 & length(LHS_var) == 1) {
        #Case 1.1 A <--> B, Reaction flows both ways
        if (arrow_type == "both_directions") {
            ifelse(as.numeric(RHS_coef) == 1, RHS_eqn <- paste0(kr, "*", RHS_var), RHS_eqn <- paste0(kr, "*", RHS_var, "^", RHS_coef))
            ifelse(as.numeric(LHS_coef) == 1, LHS_eqn <- paste0(kf, "*", LHS_var), LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef))
            
            #this if/else multiplies the coefficient of the var to the equation dependent on which var is being processed using var_on_left
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                       )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                       )
            }
        }
        #Case 1.2 A->B, only in forward direction
        else if (arrow_type == "forward_only") {
            ifelse(as.numeric(LHS_coef) == 1, 
                   LHS_eqn <- paste0(kf, "*", LHS_var), 
                   LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef)
                   )
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", eqn_out), 
                       eqn_out <- eqn_out)
            }
        }
    }
    #Case 2
    else if (length(LHS_var) > 1 & length(RHS_var) == 1) {
        #Case A + B <--> C
        if (arrow_type == "both_directions") {
            ifelse(as.numeric(RHS_coef) == 1, 
                   RHS_eqn <- paste0(kr, "*", RHS_var), 
                   RHS_eqn <- paste0(kr, RHS_var, "^", RHS_coef))
            #RHS_eqn <- paste0("(", RHS_eqn, ")")
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i]))
                }
            }
            #eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
            
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                )
            }
        }
        #Case A + B --> C
        else if (arrow_type == "forward_only") {
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }
            }
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", eqn_out), 
                       eqn_out <- eqn_out)
            }
        }
    }
    #Case 3
    else if (length(RHS_var) > 1 & length(LHS_var) == 1) {
        #Case A <--> B + C
        if (arrow_type == "both_directions") {
            ifelse(as.numeric(LHS_coef) == 1, 
                   LHS_eqn <- paste0(kf, "*", LHS_var), 
                   LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef)
                   )

            for (i in seq(length(RHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }
            }
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                )
            }
        }
        #Case: A --> B + c
        else if (arrow_type == "forward_only") {
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", 
                                             LHS_eqn[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_eqn[i], "^", LHS_eqn[i])
                           )
                }
            }
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", eqn_out), 
                       eqn_out <- eqn_out)
            }
        }
    }
    #Case 4
    else{#need to finish.  Here will go if RHS&&LHS>1.  Just copy an paste things from the above sections.  Run and test and hopefully it doens't take too long. 
        #Case A + B <--> C + D
        if (arrow_type == "both_directions") {
            for (i in seq(length(RHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }
            }
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]),
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }
            }
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                )
            }
        }
        #Case: A + B --> C + D
        else if (arrow_type == "forward_only") {
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                } else {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }
            }
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            } else {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", eqn_out), 
                       eqn_out <- eqn_out)
            }
        }
    }
    return(eqn_out)
}
################################################################################
############################# enzyme_reaction ##################################
# creates string equation for a substrate degraded by an enyzme

# Inputs:
# substrate - the species being degraded 
# km - Michealis Menton Constant
# Vmax - Maximum Velocity
# kcat - catylic rate
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for enzyme reaction (Vmax*S/(km+S) )
################################################################################
enzyme_reaction <- function(substrate, km, Vmax, kcat, enzyme, var_on_left) {

    if (!is.na(Vmax) )
    { #if vmax used
        eqn = paste0(Vmax, "*", substrate, "/(", km, "+", substrate, ")") #-Vmax*S/(km+S)
        eqn = ifelse(var_on_left, paste0("-", eqn), eqn) #determines if this is a "-" or "+" reaction
    }
    else
    {
        eqn = paste0(kcat, "*", enzyme, "*", substrate, "/(", km, "+", substrate, ")") #-km*E*S/(km+S)
        eqn = ifelse(var_on_left, paste0("-", eqn), eqn)
    }
    return(eqn)
}
################################################################################
######################## FUNCTION: simple_diffusion ############################

#Uses Ficks law to generate a simple model of diffusion (PS)(C2-C1)
# Inputs:
# RHS_var - Var Name on right hand side in vector form: c(C)
# LHS_var - Variable names of left hand side equations in vector form: c(A, B)
# arrowtype - Describes if reaction is forward (forward_only) or both (both_directions): "both_directions"
# PS - diffusion constant variable string
# var_on_left - boolean value that tells if the current variable is on the LHS.  For example if this was deriving for A, then TRUE.  IF this was deriving for C then false, (B=TRUE)

# Outputs:
# String of law of mass action result.  For example for A:
################################################################################

simple_diffusion <- function(LHS_var, RHS_var, PS, var_on_left){
    if (var_on_left) {
        #PS*(C1-c2) where C1 is left hand side variable
        eqn = paste0(PS, "*(", RHS_var, "-", LHS_var, ")")
    } else {
        eqn = paste0(PS, "*(", LHS_var, "-", RHS_var, ")")
    }
    
    return(eqn)
}


######################## FUNCTION: Input_Outputs ############################

#Generates Rate equations based on Input and Output of data
# Inputs:
# input_or_output - "input" or "output"
# varName - Name of variable controlling rate of in/out
# species - The element that is undergoing I/O
# 
# Outputs:
# String of i/o result
################################################################################

In_Out <- function(input_or_output, varName, species){
    if (input_or_output == "input") {
        eqn = paste0(species, "*", varName)
    }
    else{
        eqn = paste0("-", species, "*", varName)
    }
    return(eqn)
}

################# FUNCTION: extract_data ####################
# This searches a dataframe for every instance of a variable in RHS and LHS var and extracts those rows, returning this subsetted dataframe

# Inputs:
# myModel - saved df that contains model information with RHS variables in the 3rd column and LHS variables in the 5th colun
# var_to_subset_with - variable to search for in 'myModel' dataframe

# Outputs:
# Outputs dataframe subsetting with var_to_subset_with (this is meant to be used with in the differential equation solver)
################################################################################
extract_data <- function(myModel, var_to_subset_with){
    index_of_rows_with_var <- vector()
    for (row in 1:nrow(myModel)) {#search rows of data for the choosen variable and subset them to new df
        #print(myModel[row,])
        #law_of_derivation <- myModel[row,1]
        RHS_var <- str_split(myModel[row,3], " ")[[1]] #grabs RHS vars, splits them so they can be searched for wanted variable
        LHS_var <- str_split(myModel[row,5], " ")[[1]] #Does above for LHS variables
        if (var_to_subset_with %in% RHS_var | var_to_subset_with %in% LHS_var) { #find indices containing var name
            index_of_rows_with_var <- c(index_of_rows_with_var, row) #adds index to vector to subset main df later
        }
    }    
    temp_df <- myModel[index_of_rows_with_var, ] #extract var rows
    return(temp_df)
}


RemovePlusSignFromStart <- function(string) {
    #removes the first letter of a string if it is a plus sign
    #inputs:
    #   @string - string to remove letter from 
    #output:
    #   @out -string without a plus in first letter if it exists
    #ex. string <- "+k_r3",   out <- "k_r3"
    
    split.str <- str_split(string, "")[[1]]
    out <- ""
    for (i in seq(length(split.str))) {
        if (i == 1 & split.str[i] == "+") {
            #pass
        } else {
            out <- paste0(out, split.str[i])
        }
    }
    return(out)
}

################################################################################
##################### Function: regulatorToRate 
################################################################################
regulatorToRate <- function(regulators, rateConstants) {
    #break values from space separated string to vector
    regulators <- str_split(regulators, " ")[[1]]
    rateConstants <- str_split(rateConstants, " ")[[1]]
    
    numRegulators <- length(regulators)
    eqnOut <- c()
    for (i in seq(numRegulators)) { #add each regulator equation to a list (regulator*rateConstant)
        eqnForRegulator <- paste0(rateConstants[i], "*", regulators[i])
        eqnOut <- c(eqnOut, eqnForRegulator)
    }
    out <- paste(eqnOut, collapse = "+")
    if (numRegulators > 1) {
        out <- paste0("(", out, ")")
    }
    return(out)
}

################################################################################
################# enzyme_degradation: extract_data ####################
# creates string equation for a substrate degraded by an enyzme

# Inputs:
# substrate - the species being degraded 
# km - Michealis Menton Constant
# Vmax - Maximum Velocity
# kcat - catylic rate
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for enzyme deg (-Vmax*S/(km+S) )
################################################################################
enzyme_degradation <- function(substrate, km, Vmax, kcat, enzyme, isProd)
{
    if (!is.na(Vmax)) { #if vmax used
        if (isProd) {
            eqn = paste0(Vmax, "*", substrate, "/(", km, "+", substrate, ")") #-Vmax*S/(km+S)
            
        } else {
            eqn = paste0("-", Vmax, "*", substrate, "/(", km, "+", substrate, ")") #-Vmax*S/(km+S)
            
        }
    } else {
        if (isProd) {
            eqn = paste0(kcat, "*", enzyme, "*", substrate, "/(", km, "+", substrate, ")") #-km*E*S/(km+S)
            
        } else {
            eqn = paste0("-", kcat, "*", enzyme, "*", substrate, "/(", km, "+", substrate, ")") #-km*E*S/(km+S)
            
        }
    }
    return(eqn)
}

########################### Output: Mass Action ################################
# creates string equation for a substrate degraded by enzyme/transporter using MA

# Inputs:
# substrate - the species being degraded 
# kout - rate constant of reaction
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for output using mass action
################################################################################
IO_mass_action <- function(substrate, kout, enzyme) {
    eqn = paste0("-", kout, "*", substrate, "*", enzyme)
    return(eqn)
}

########################### EqnStartMinus  #####################################
# determines if eqn starts with a minus or not

# Inputs:
# eqn - string of eqn to be tested

# Outputs:
# TRUE if eqn beings with minus, FALSE if not
################################################################################
EqnStartMinus <- function(eqn) {
    first.letter <- strsplit(eqn, "")[[1]][1]
    if (first.letter == "-") {
        begins.with.minus = TRUE
    } else {
        begins.with.minus = FALSE
    }
    return(begins.with.minus)
}


CalcDiffEqForIO <- function(IO_df, var, InOrOut) {
    # this function is meant to calculate the differential equations for input/output functions
    # Inputs:
    #   @IO_df - df containing all Input/output information
    #   @var - variable to generate differential equation for
    #   @InOrOut - string "input" or "output" depending on direction of IO
    # Output:
    #   @out - c(string version of differential equation relating to I/O of var,
    #            boolean that is TRUE is this var has IO to add,
    #            latex version of equation)
    
    diff.eqn <- ""
    latex.eqn.out <- ""
    input.output.exists <- FALSE
    
    for (row in 1:nrow(IO_df)) {
        # unpack IO_df
        
        input.or.output <- InOrOut          #Input or Output
        type.of.IO <- IO_df[row, 1]         #Rate, Enzyme, Synthesis, etc...
        species <- IO_df[row, 2]            #Species being in or out'd
        rate.constant <- IO_df[row, 3]      #rate associated with IO
        species.dependent <- IO_df[row, 4]   #T or F if rate dependent on species
        Vmax <- IO_df[row, 5]               #Vmax used in enzyme IO
        kcat <- IO_df[row, 6]               #Kcat used in enzyme IO
        enzyme <- IO_df[row, 7]             #enzyme used in enzyme IO
        
        if (species == var) {
            input.output.exists <- TRUE
            if (type.of.IO == "Rate") {
                eqn <- ifelse(species.dependent,
                              paste0(rate.constant, "*", species),
                              rate.constant) 
                
                diff.eqn <- ifelse(input.or.output == "input",
                              paste0(diff.eqn, "+", eqn),
                              paste0(diff.eqn, "-", eqn))
                
                latex.eqn <- IO2Latex(diff.eqn, "out")
                
                latex.eqn.out <- ifelse(input.or.output == "input",
                                        paste0(latex.eqn.out, "+", latex.eqn),
                                        paste0(latex.eqn.out, "-", latex.eqn))
            } 
            else if (type.of.IO == "Synthesis") {
                eqn <- paste0(rate.constant, "*", enzyme) #store factor in enzyme spot
                
                diff.eqn <- ifelse(input.or.output == "input",
                              paste0(diff.eqn, "+", eqn),
                              paste0(diff.eqn, "-", eqn))
                
                latex.eqn <- IO2Latex(eqn, type.of.IO)
                
                latex.eqn.out <- ifelse(input.or.output == "input",
                                        paste0(latex.eqn.out, "+", latex.eqn),
                                        paste0(latex.eqn.out, "-", latex.eqn))
            } 
            else if (type.of.IO == "Enzyme_Degradation") {
                eqn <- enzyme_degradation(species, rate.constant, Vmax, kcat, enzyme)
                diff.eqn <- paste0(diff.eqn, eqn)
                latex.eqn <- enzymeEqn2Latex(eqn)
                latex.eqn.out <- ifelse(startsWith(latex.eqn, "-"),
                                        paste0(latex.eqn.out, latex.eqn),
                                        paste0(latex.eqn.out, "+", latex.eqn))
            } 
            else if (type.of.IO == "mass_action") {
                eqn <- IO_mass_action(species, rate.constant, enzyme)
                diff.eqn <- paste0(diff.eqn, eqn)
            }
        } 
    }
    out <- c(diff.eqn, input.output.exists, latex.eqn.out)
}

##################### FUNCTION: calc_differential_equations ####################
# Model Description: 
# Inputs
# @myModel - df to parse containing all equation parameters (this is typically an output of Rhinsy)
# @vars_to_diffeq - vector of variables that we want to create differential equations for
# @InOutModel - df containing all In/out parameter and values

# Outputs
#list:
# @diff.eqns - vector of differential equations in string form
# @latex.diff.eqns - vector of differential equations in latex form
#############
    
CalcDiffEqnsForChem <- function(chemInfo, searchVar) {
    # jPrint("Calc diff eqns for chem")
    ID         <- chemInfo$ID[1]
    law        <- chemInfo$Law[1]
    LHS.coef   <- str_split(chemInfo$LHS_coef[1], " ")[[1]]
    LHS.var    <- str_split(chemInfo$LHS_var[1],  " ")[[1]]
    RHS.coef   <- str_split(chemInfo$RHS_coef[1], " ")[[1]]
    RHS.var    <- str_split(chemInfo$RHS_var[1],  " ")[[1]] 
    arrow_type <- chemInfo$arrow_type[1]
    kf         <- chemInfo$kf[1]
    kr         <- chemInfo$kr[1]
    FR.bool    <- chemInfo$FM_bool[1] 
    FRs        <- chemInfo$FMs[1] 
    FR.RCs     <- chemInfo$FM_rateC[1] 
    RR.bool    <- chemInfo$RM_bool[1] 
    RRs        <- chemInfo$RMs[1] 
    RR.RCs     <- chemInfo$RM_rateC[1] 
    
    # Rate constant changes if regulators are involved
    if (FR.bool) {kf = regulatorToRate(FRs, FR.RCs)}
    if (RR.bool) {kr = regulatorToRate(RRs, RR.RCs)}
    #match returns index position of var, ex, var = A, list -> c(A,B) match returns 1 for A and 2 for B
    if (searchVar %in% LHS.var) {
        var.on.left = TRUE
        var.coef <- LHS.coef[match(searchVar, LHS.var)] 
    } else if (searchVar %in% RHS.var) {
        var.on.left = FALSE
        var.coef <- RHS.coef[match(searchVar, RHS.var)]
    }
    # jPrint("Finished search var")
    diff.eqn <- law_mass_action(RHS.coef, 
                                RHS.var, 
                                LHS.coef, 
                                LHS.var, 
                                arrow_type, 
                                kf, 
                                kr, 
                                var.on.left, 
                                var.coef)
    
    latex.eqn <- massActionEqn2Latex(diff.eqn)
    
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}

CalcDiffEqnsForEnzyme <- function(enz.info, searchVar) {
    
    # Unpack information
    ID        <- enz.info[1]
    law       <- enz.info[2]
    substrate <- enz.info[3]
    product   <- enz.info[4]
    enzyme    <- enz.info[5]
    kcat      <- enz.info[6]
    Km        <- enz.info[7]
    Vmax      <- enz.info[8]
    
    if (searchVar == substrate) {
        var.on.left = TRUE
    } else if (searchVar == product ) {
        var.on.left = FALSE
    }
    
    # Run solving law
    diff.eqn <- enzyme_reaction(substrate, 
                                Km, 
                                Vmax, 
                                kcat, 
                                enzyme, 
                                var.on.left)
    
    latex.eqn <- enzymeEqn2Latex(diff.eqn)
    
    # Package result
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}

CalcDiffEqnsForSyn <- function(synInfo, searchVar) {
    
    # Unpack Information
    ID     <- synInfo$ID[1]
    Law    <- synInfo$Law[1]
    VarSyn <- synInfo$VarSyn[1]
    RC     <- synInfo$RC[1]
    Factor <- synInfo$Factor[1]
    
    
    if (Law == "rate") {
        diff.eqn  <- RC 
        latex.eqn <- VarToLatexForm(RC)
    } 
    else if (Law == "byFactor") {
        diff.eqn  <- paste0(RC, "*", Factor) 
        latex.eqn <- paste0(VarToLatexForm(RC), "*", VarToLatexForm(Factor))
    }
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}

CalcDiffEqnsForDeg <- function(degInfo, searchVar) {
    # Unpack Information
    ID      <- degInfo$ID[1]
    Law     <- degInfo$Law[1]
    VarDeg  <- degInfo$VarDeg[1]
    ConcDep <- degInfo$ConcDep[1]
    RC      <- degInfo$RC[1]
    Km      <- degInfo$Km[1]
    Enz     <- degInfo$Enz[1]
    Vmax    <- degInfo$Vmax[1]
    Product <- degInfo$Prods[1]
    is.Prod <- FALSE
    # Create Products if they exist
    if (!is.na(Product)) {
        Product <- str_split(Product, " ")[[1]]
        if (searchVar %in% Product) {
            is.Prod <- TRUE
        }
    } 
    
    if (Law == "rate") {
        # if species being degraded
        if (is.Prod) {
            diff.eqn <- ifelse(ConcDep,
                               paste0(RC, "*", VarDeg),
                               paste0(RC))
        } else {
            # if species being generated
            diff.eqn <- ifelse(ConcDep,
                               paste0("-", RC, "*", VarDeg),
                               paste0("-", RC))
        }
        

        latex.eqn <- IO2Latex(diff.eqn, "out")
    }
    else if (Law == "byEnzyme") {
        
        diff.eqn <- enzyme_degradation(VarDeg, 
                                       Km, 
                                       Vmax, 
                                       RC, 
                                       Enz,
                                       is.Prod)
        latex.eqn <- enzymeEqn2Latex(diff.eqn)
    }
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}

CalcDiffForEqns <- function(species,
                            eqn.info.df, 
                            eqn.chem.df,
                            eqn.enz.df,
                            eqn.syn.df,
                            eqn.deg.df) {
    # jPrint(paste("Diff var: ", species))
    diff.eqn <- NA
    latex.eqn <- NA
    first.eqn <- TRUE
    n.eqns <- nrow(eqn.info.df)
    if (n.eqns > 0) {
        for (row in 1:n.eqns) {
            vars <- strsplit(eqn.info.df$Species[row], " ")[[1]]
            for (var in vars) {
                if (var == species){
                    id   <- eqn.info.df$ID[row]
                    type <- eqn.info.df$EqnType[row]
                    #check other dataframes for id
                    # Parse Chem Dataframe
                    if (type == "chem_rxn") {
                        for (i in 1:nrow(eqn.chem.df)) {
                            chem.id <- eqn.chem.df$ID[i]
                            if (id == chem.id){
                                row.info   <- eqn.chem.df[i, ]
                                temp       <- CalcDiffEqnsForChem(row.info, var)
                                temp.eqn   <- temp["Diff"][[1]]
                                temp.latex <- temp["Latex"][[1]]
                            }
                        } 
                    }
                    # Parse Enzyme Dataframe
                    else if (type == "enzyme_rxn") {
                        for (i in 1:nrow(eqn.enz.df)) {
                            enz.id <- eqn.enz.df$ID[i]
                            if (id == enz.id) {
                                row.info   <- eqn.enz.df[i, ]
                                temp       <- CalcDiffEqnsForEnzyme(row.info, var)
                                temp.eqn   <- temp["Diff"][[1]]
                                temp.latex <- temp["Latex"][[1]]
                            }
                        } 
                    }
                    else if (type == "syn") {
                        for (i in 1:nrow(eqn.syn.df)) {
                            syn.id <- eqn.syn.df$ID[i]
                            if (id == syn.id) {
                                row.info   <- eqn.syn.df[i, ]
                                temp       <- CalcDiffEqnsForSyn(row.info, var)
                                temp.eqn   <- temp["Diff"][[1]]
                                temp.latex <- temp["Latex"][[1]]
                            }
                        }
                    }
                    else if (type == "deg") {
                        for (i in 1:nrow(eqn.deg.df)) {
                            deg.id <- eqn.deg.df$ID[i]
                            if (id == deg.id) {
                                row.info   <- eqn.deg.df[i, ]
                                temp       <- CalcDiffEqnsForDeg(row.info, var)
                                temp.eqn   <- temp["Diff"][[1]]
                                temp.latex <- temp["Latex"][[1]]
                            }
                        }
                    }
                    # Add single differential equation to all equations
                    if (first.eqn) {
                        first.eqn <- FALSE
                        diff.eqn  <- temp.eqn
                        latex.eqn <- temp.latex
                    } else {
                        minus <- EqnStartMinus(temp.eqn)
                        if (minus) {
                            diff.eqn <- paste0(diff.eqn, temp.eqn)
                            latex.eqn <- paste0(latex.eqn, temp.latex)
                        } else {
                            diff.eqn <- paste0(diff.eqn, "+", temp.eqn)
                            latex.eqn <- paste0(latex.eqn, "+", temp.latex)
                        } 
                    }
                }
            }
        }
    }
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}

CalcInputsForEqns <- function(species,
                             InputDf,
                             noEquation) {
    # noEquation is a boolean telling if the differential equation has an equation portion
    
    diff.eqn  <- NA
    latex.eqn <- NA
    IO.out <- CalcDiffEqForIO(InputDf, species, "input")
    new.eqn <- IO.out[[1]]
    input.exists <- IO.out[[2]]
    new.latex.eqn <- IO.out[[3]]
    if (input.exists) {
        diff.eqn <- ifelse(noEquation,
                           RemovePlusSignFromStart(new.eqn),
                           new.eqn)
        
        latex.eqn <- ifelse(noEquation,
                            RemovePlusSignFromStart(new.latex.eqn),
                            new.latex.eqn)
    } 
    
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}

CalcOutputsForEqns <- function(species,
                               OutputDf,
                               noEquation) {
    
    diff.eqn  <- ""
    latex.eqn <- ""
    IO.out <- CalcDiffEqForIO(OutputDf, species, "output")
    new.eqn <- IO.out[[1]]
    is.new.eqn <- IO.out[[2]]
    new.latex.eqn <- IO.out[[3]]
    if (is.new.eqn) {
        diff.eqn <- ifelse(noEquation,
                           RemovePlusSignFromStart(new.eqn),
                           paste0(diff.eqn, new.eqn))
        
        latex.eqn <- ifelse(noEquation,
                            RemovePlusSignFromStart(new.latex.eqn),
                            paste0(latex.eqn, new.latex.eqn))
    } 
    
    out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
    return(out)
}



calc_differential_equations <- function(eqn.info.df,
                                        eqn.chem.df,
                                        eqn.enz.df,
                                        eqn.syn.df,
                                        eqn.deg.df,
                                        var_to_diffeq, 
                                        InputDf, 
                                        OutputDf, 
                                        InAdded, 
                                        OutAdded, 
                                        listOfCustomVars,
                                        customVarToIgnore,
                                        customVarDF
                                        )
{
    # Account for custom differential eqns
    custom.vars <- setdiff(listOfCustomVars, customVarToIgnore)
    
    #initialize values
    differential.equations  <- vector()
    differential.eqns.latex <- vector()
     
    #choosing variable to solve the differential equation for
    for (var in var_to_diffeq) {
        diff.eqn  <- ""
        latex.eqn <- ""
        if (var %in% custom.vars) {
            idx <- match(var, customVarDF[,1])
            differential.equations <- c(differential.equations, customVarDF[idx,2])
        } else {
#----Differential Equation Solver if Custom Equation is not used----------------            
            no.input  <- TRUE
            no.output <- TRUE

            out <- CalcDiffForEqns(var, 
                                   eqn.info.df, 
                                   eqn.chem.df, 
                                   eqn.enz.df,
                                   eqn.syn.df,
                                   eqn.deg.df
                                   )
            
            diff.eqn.eqns  <- out["Diff"][[1]]
            latex.eqn.eqns <- out["Latex"][[1]]
            
            if (is.na(diff.eqn.eqns)) {
                no.equation <- TRUE
            } else {
                diff.eqn    <- diff.eqn.eqns
                latex.eqn   <- latex.eqn.eqns
                no.equation <- FALSE
            }
            
            # Adding differential equations for Inputs
            if (InAdded) {
                inputs       <- CalcInputsForEqns(var, InputDf, no.equation)
                diff.eqn.in  <- inputs["Diff"][[1]]
                latex.eqn.in <- inputs["Latex"][[1]]
                
                # Checks if this specific variable has an input
                if (is.na(diff.eqn.in)) {
                    no.input <- TRUE
                } else {
                    diff.eqn  <- paste0(diff.eqn, diff.eqn.in)
                    latex.eqn <- paste0(latex.eqn, latex.eqn.in)
                    no.input  <- FALSE
                }
                
            }
            
            # Adding differential equations for Outputs
            if (OutAdded) {
                outputs       <- CalcOutputsForEqns(var, OutputDf, no.equation)
                diff.eqn.out  <- outputs["Diff"][[1]]
                latex.eqn.out <- outputs["Latex"][[1]]
                
                # Checks if this specific variable has an output
                if (is.na(diff.eqn.out)) {
                    no.output <- TRUE
                } else {
                    diff.eqn  <- paste0(diff.eqn,  diff.eqn.out)
                    latex.eqn <- paste0(latex.eqn, latex.eqn.out)
                    no.output  <- FALSE
                }
            }

            #Sets to zero if no differential solvers were used 
            if (no.equation && no.input && no.output) { 
                diff.eqn = 0
                latex.eqn = 0
            }
            
            differential.equations <- c(differential.equations, diff.eqn)
            differential.eqns.latex <- c(differential.eqns.latex, latex.eqn) 
        }
        
    }
    out.list <- list("diff.eqns" = differential.equations
                     ,"latex.diff.eqns" = differential.eqns.latex)
    return(out.list)
}