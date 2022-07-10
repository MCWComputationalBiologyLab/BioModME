calcChemTextEqn <- function(chemInfo) {
  
  #unpack
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
  FRs        <- str_split(chemInfo$FMs[1], " ")[[1]]
  FR.RCs     <- str_split(chemInfo$FM_rateC[1], " ")[[1]] 
  RR.bool    <- chemInfo$RM_bool[1] 
  RRs        <- str_split(chemInfo$RMs[1], " ")[[1]]  
  RR.RCs     <- str_split(chemInfo$RM_rateC[1], " ")[[1]] 
  
  n.LHS   <- length(LHS.coef)
  n.RHS   <- length(RHS.coef)
  n.f.reg <- length(FRs)
  n.r.reg <- length(RRs)
  
  eqn_LHS <- ""
  for (i in seq(n.LHS)) {
    coef <- LHS.coef[i]
    var  <- LHS.var[i]
    if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
    if (i == as.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
    else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
  }
  
  eqn_RHS <- ""
  for (i in seq(n.RHS)) {
    coef <- RHS.coef[i]
    var  <- RHS.var[i]
    if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
    if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
    else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
  }
  
  if (arrow_type == "both_directions") {
    arrow <- "<-->"
    if (FR.bool && RR.bool) {
      #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
      for (i in seq(n.f.reg)) {
        regulator <- FRs[i]
        rateConstant <- FR.RCs[i]
        if (i > 1) {
          forwardModifiers <- paste0(forwardModifiers, "+", rateConstant, "*", regulator )
        } else {
          forwardModifiers <- paste0(rateConstant, "*", regulator)
        }
      }
      for (i in seq(n.r.reg)) {
        regulator <- RRs[i]
        rateConstant <- RR.RCs[i]
        if (i > 1) {
          reverseModifiers <- paste0(reverseModifiers, "+", rateConstant, "*", regulator )
        } else {
          reverseModifiers <- paste0(rateConstant, "*", regulator)
        }
      }
      arrow <- paste0("(", reverseModifiers, ")", arrow, "(",forwardModifiers ,")")
    }
    else if (FR.bool && !RR.bool) {
      for (i in seq(n.f.reg)) {
        regulator <- FRs[i]
        rateConstant <- FR.RCs[i]
        if (i > 1) {
          forwardModifiers <- paste0(forwardModifiers, "+", rateConstant, "*", regulator )
        } else {
          forwardModifiers <- paste0(rateConstant, "*", regulator)
        }
      }
      arrow <- paste0("(", kr, ")", arrow, "(",forwardModifiers ,")")
    }
    else if (!FR.bool && RR.bool) {
      for (i in seq(n.r.reg)) {
        regulator <- RRs[i]
        rateConstant <- RR.RCs[i]
        if (i > 1) {
          reverseModifiers <- paste0(reverseModifiers, "+", rateConstant, "*", regulator )
        } else {
          reverseModifiers <- paste0(rateConstant, "*", regulator)
        }
      }
      arrow <- paste0("(", reverseModifiers, ")", arrow, "(", kf, ")")
    }
    else
    {
      arrow <- paste0("(", kr, ")", arrow, "(", kf, ")")
    }
  } 
  else if (arrow_type == "forward_only") {
    arrow = "--->"
    if (FR.bool) {
      for (i in seq(n.f.reg)) {
        regulator <- FRs[i]
        rateConstant <- FR.RCs[i]
        if (i > 1) {
          forwardModifiers <- paste0(forwardModifiers, "+", rateConstant, "*", regulator )
        } else {
          forwardModifiers <- paste0(rateConstant, "*", regulator)
        }
      }
      arrow <- paste0(arrow, "(",forwardModifiers ,")")
    }
    else
    {
      arrow <- paste0(arrow, "(", kf, ")")
    }
  }
  textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  return(textOut)
}

calcEnzymeTextEqn <- function(enzInfo) {
  
  ID        <- enzInfo[1]
  law       <- enzInfo[2]
  substrate <- enzInfo[3]
  product   <- enzInfo[4]
  enzyme    <- enzInfo[5]
  kcat      <- enzInfo[6]
  Km        <- enzInfo[7]
  Vmax      <- enzInfo[8]
  arrow     <- "-->"
  
  if (is.na(Vmax)) {
    textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
  }
  else {
    textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)
  }
  return(textOut)
}

calcSynTextEqn <- function(synInfo) {
  
  ID     <- synInfo$ID[1]
  Law    <- synInfo$Law[1]
  VarSyn <- synInfo$VarSyn[1]
  RC     <- synInfo$RC[1]
  Factor <- synInfo$Factor[1]
  
  if (Law == "rate") {
    arrow <- "-->"
    var   <- VarSyn
    rc    <- RC
    type  <- "syn"
    textOut <- paste0(arrow,
                      "(", rc, ")",
                      var
    )
  } 
  else if (Law == "byFactor") {
    arrow  <- "-->"
    var    <- VarSyn
    rc     <- RC
    factor <- Factor
    type   <- "syn"
    textOut <- paste0(factor,
                      arrow,
                      "(", rc, ")",
                      var
    )
  }
  return(textOut)
}

calcDegTextEqn <- function(degInfo) {
  ID      <- degInfo$ID[1]
  Law     <- degInfo$Law[1]
  VarDeg  <- degInfo$VarDeg[1]
  ConcDep <- degInfo$ConcDep[1]
  RC      <- degInfo$RC[1]
  Km      <- degInfo$Km[1]
  Enz     <- degInfo$Enz[1]
  Vmax    <- degInfo$Vmax[1]
  Product <- degInfo$Prods[1]
  
  
}

ReCalcTextEqns <- function(eqn.df, 
                           eqn.chem.df,
                           eqn.enz.df,
                           eqn.syn.df,
                           eqn.deg.df,
                           currentTextEqns) {
  
  #TODO
  #have to program degradation in.  Currently just copies the old ones.
  
  n.eqns <- nrow(eqn.df)
  eqns <- c()
  
  for (row in 1:n.eqns) {
    eqn <- ""
    
    #unpack eqn variables
    id          <- eqn.df$ID[row]
    type        <- eqn.df$EqnType[row]
    law         <- eqn.df$Law[row]
    species     <- eqn.df$Species[row]
    RCs         <- eqn.df$RateConstants[row]
    compartment <- eqn.df$Compartment[row]
    description <- eqn.df$Description[row]
    
    #find equation type and extract its respective type
    if (type == "chem_rxn") {
      for (i in 1:nrow(eqn.chem.df)) {
        chem.id <- eqn.chem.df$ID[i]
        if (id == chem.id){
         row.info <- eqn.chem.df[i, ]
         eqn <- calcChemTextEqn(row.info)
         break
        }
      }
    } else if(type == "enzyme_rxn") {
      for (i in 1:nrow(eqn.enz.df)) {
        enz.id <- eqn.enz.df$ID[i]
        if (id == enz.id){
          row.info <- eqn.enz.df[i, ]
          eqn <- calcEnzymeTextEqn(row.info)
          break
        }
      }
    } else if (type == "syn") {
      for (i in 1:nrow(eqn.syn.df)) {
        syn.id <- eqn.syn.df$ID[i]
        if (id == syn.id){
          row.info <- eqn.syn.df[i, ]
          eqn <- calcSynTextEqn(row.info)
          break
        }
      }
    } else if (type == "deg") {
      eqn <- currentTextEqns[row]
    }
    eqns <- c(eqns, eqn)
  }
  
  return(eqns)
}