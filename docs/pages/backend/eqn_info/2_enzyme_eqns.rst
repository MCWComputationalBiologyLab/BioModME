============================
Enzyme Based Equations
============================

Michaelis Menten
---------------------------------
Michaelis Menten kinetics is a commonly used approximation for describing the 
rate at which enzyme reactions occur. The model most commonly takes the 
following for describing the velocity of a reaction:

.. math::
    \begin{equation*}
        v = \frac{d[P]}{dt} = V_{max}\frac{[S]}{K_M+[S]} = 
        (k_{cat}*[E])\frac{[S]}{K_M+[S]}
    \end{equation*}

where,

:|Vmax|: Maximum Velocity
:E: Enzyme Concentration
:S: Substrate Concentration
:P: Product Concentration
:|KM|: Michaelis Menten Constant
:|kcat|: Catalytic Rate of Enzyme Reaction

.. |Vmax| replace:: :math:`V_{max}`
.. |KM| replace:: :math:`K_M`
.. |kcat| replace:: :math:`k_{cat}`


