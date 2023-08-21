============================
Simple Diffusion
============================

In simple diffusion, the substance is driven by concentration difference across
a membrane. In the example below, we have a liver compartment that is 
separated into blood and liver compartments with a passive diffusion
membrane for compound **A**.  

.. container:: bordergrey

    .. figure:: images/simple_diffusion.png
        :width: 400
        :height: 200
        :align: center


For the simple, one-dimensional case shown above,
the mass flux through the membrane follows Ficks Law:

.. math:: 
    J = -D \frac{dC}{dx}

where,

:C: Concentration at membrane location x 
:D: Diffusion coefficient of the molecule 

We assume the concentration gradient across the membrane of thickness, x, falls
linearly with x leading to:

.. math::
    \frac{dC}{dx} = \frac{\Delta C}{\Delta X} = \frac{C_2-C_1}{\Delta x}

.. math:: 
    J = -D \frac{\Delta C}{\Delta x}

The rate of transfer (**f**) of a neutral compound across a biological membrane of
surface area (**S**) is:

.. math::
    f = JS = -SD\frac{\Delta C}{\Delta x} = -PS(C_2-C_1)

where,

:PS: Coefficient of diffusivity

Note: The **negative sign** on the right side of the equation indicates that 
the net transfer due to diffusion is in a direction away from the region with
higher concentration.



