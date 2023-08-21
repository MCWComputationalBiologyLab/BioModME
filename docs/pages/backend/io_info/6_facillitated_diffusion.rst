============================
Facilitated Diffusion
============================

Facilitated  diffusion is a type of diffusion in which the molecules move from 
the regions of higher concentration to the region of lower concentration 
assisted by a carrier molecule. It is a selective process, only allowing 
specific molecules to pass the gradient. In the below figure, we have a 
liver compartment that is separated into blood and liver compartments with  
a facilitated transporter for compound **A**. Note that this is a one-way 
transport.

.. container:: bordergrey

    .. figure:: images/facillitated_diffusion.png
        :width: 400
        :height: 200
        :align: center

For the derivation of the flux equation for facilitated diffusion, we take the
following equation: 

.. math::
    F  = \frac{V_{max}*[S_o]}{K_m}

where,

:|Vmax|: Maximum Velocity
:|So|: Substrate Concentration
:|Km|: Affinity of how much substrate concentration needed to reach :math:`V_{max}`

.. |Vmax| replace:: :math:`V_{max}`
.. |So| replace:: :math:`S_{o}`
.. |Km| replace:: :math:`K_{m}`

The flux derivations for the above example would be: 

.. math:: 
    \begin{align*}
        V_{1} \frac{dA_1}{dt} &= - \frac{V_{max}*A_{1}}{k_m + A_1} \\
        V_{2} \frac{dA_2}{dt} &= \frac{V_{max}*A_{1}}{k_m + A_1} \\
    \end{align*}