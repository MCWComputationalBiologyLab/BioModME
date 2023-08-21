============================
Flow In
============================

Constant flow into a compartment from an unspecified source. This could be 
something like an input flow into a model of species or a known constant flow
from a source outside the scope of your created model. Flow In and Flow Out 
options can also be pieced together to create a customized Flow Between. In 
the Figure below, we see a constant flow, **F**, carrying species, **A_1**, 
into Compartment 1.

.. container:: bordergrey

    .. figure:: images/flow_in.png
        :width: 350
        :height: 100
        :align: center

The mathematical derivation for this model input would be: 

.. math::
    V_{1} \frac{dA_{1}}{dt} = F * A_{1}