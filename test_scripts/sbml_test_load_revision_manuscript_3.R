 # Assume all functions from sbml_fxns.R are loaded.
 

f.sbml <- "C:\\Users\\jwomackbarre\\Downloads\\basic_tutorial.xml"

r.sbml <- LoadSBML(f.sbml)
r.sbml$compartments
r.sbml$species
r.sbml$reactions
r.sbml$parameters
