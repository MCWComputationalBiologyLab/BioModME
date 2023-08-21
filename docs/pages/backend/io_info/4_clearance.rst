============================
Clearance
============================

Clearance from a compartment is used when a species leaves the compartment 
and is removed from the model. In the example below, we have species 
:math:`A_3` being cleared from compartment 3. 

.. container:: bordergrey

    .. figure:: images/clearance.png
        :width: 350
        :height: 175
        :align: center

It takes the following derivation: 

.. math::
    V_3 \frac{dA_3}{dt} = -k_e * A_3 * V_3

where, 

:|ke|: Rate of clearance
:|A3|: Concentration of Species being cleared
:|V3|: Volume of compartment that species is being removed from

.. |ke| replace:: :math:`k_e`
.. |A3| replace:: :math:`A_3`
.. |V3| replace:: :math:`V_3`