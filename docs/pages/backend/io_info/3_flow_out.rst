============================
Flow Out
============================

Constant flow out of a compartment that does not go to another compartment. 
Flow In and Flow Out options can be pieced together to create a customized Flow 
Between. In the Figure below, we see a constant flow, **F**, carrying species,
**A_1**, out of Compartment 1.

.. container:: bordergrey

    .. figure:: images/flow_out.png
        :width: 350
        :height: 100
        :align: center

The mathematical derivation for this model output would be: 

.. math::
    V_{1} \frac{dA_{1}}{dt} = - F * A_{1}
