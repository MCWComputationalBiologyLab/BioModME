============================
Flow Between
============================

Flow between two or more compartments follow the mechanics described below. In 
the below figure, we have flow between two compartments. We have species 
:math:`A_1` flowing out of compartment 1 into compartment 2 as :math:`A_2`.

.. container:: bordergrey

    .. figure:: images/flow_between.png
        :width: 400
        :height: 300
        :align: center

The flows from a compartment are derived as the flow rate (F) multiplied by the 
concentration of the species leaving the compartment as a minus term. 
Flow to a compartment is the same derivation without the minus term. 
The flows in the above diagram would be derived as:

.. math::
    \begin{align*}
        V_1 * \frac{d[A_1]}{dt} &= -F * A_{1} \\
        V_2 * \frac{d[A_2]}{dt} &= F * A_{1}
    \end{align*}

Split Flow
----------------------------

Often we need to split the flow from one compartment to multiple 
compartments.  This option splits the flow from one compartment to go to 
multiple compartments. In the figure below, we see the flow from compartment 1
is split to go to compartment 2 and compartment 3.

.. container:: bordergrey

    .. figure:: images/flow_between_split.png
        :width: 350
        :height: 320
        :align: center

The summation of the output flows will be equal to the input flow:

.. math::
    F_{out} = \sum_{1}^{n} F_{in}

Given the above figure this equation derives out to:

.. math::
    F = F_1 + F_2

The resulting flow differential equations derive as:

.. math::
    \begin{align*}
        V_{1} \frac{dA_{1}}{dt} = -F * A_{1} \\
        V_{2} \frac{dA_{2}}{dt} = -F_{1} * A_{1} \\
        V_{3} \frac{dA_{3}}{dt} = -F_{2} * A_{1} \\
    \end{align*}