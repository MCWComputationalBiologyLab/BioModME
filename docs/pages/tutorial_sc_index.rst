============================
Basic Tutorial
============================

This tutorial covers basics features of BioModME by building a simplified, 
single compartment model. In this example, we focus on a set of reactions 
occurring in a cell. Compound “A” is synthesized by “Prot” at a constant rate 
and reacts with a limited supply of compound “B” found in the cell to make an
intermediate “C1”. C1 undergoes an enzymatic conversion to “C2” and this new
intermediate undergoes self-cleavage to the desired end product, “P”, and an 
inhibitor, “I”, of Prot.  This inhibitor feedback binding to Prot stopping the
synthesis of A, and thereby the entire reaction scheme.

.. figure:: tutorial_sc/images/cell_model.png
    :width: 50 %
    :align: center 

|

In the following sections of this tutorial, we will design the above system and
look at how this program can be used to visualize and modify the data.  
The contents include the following:

#. Creating model variables
#. Building a system of equations
#. Entering the inputs and outputs of the system
#. Inputting numerical parameter values
#. Solving and plotting the differential model
#. Exploring the different types of plotting features
#. Exporting the model and its features in meaningful ways


To use the online version of this application visit
https://biomodme.ctsi.mcw.edu/.  Note this app is stored on an Rshiny server 
that close unused applications after 15 minutes of inactivity. Your current
model can be downloaded in the "Export" tab as an .RDS file and can be loaded
in by opening the right sidebar of the application. The application can also 
be directly downloaded at 
https://github.com/MCWComputationalBiologyLab/BioModME and opened using the
R programming language using stand shiny app protocol.

.. toctree::
    :hidden:

    tutorial_sc/1_construct_model
    tutorial_sc/2_visualize_model
    tutorial_sc/3_export


