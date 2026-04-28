============================
Bacterial Growth Equations
============================

The bacterial-growth rate laws model how cell populations expand, compete,
and consume substrate. They are useful for batch and chemostat cultures,
co-culture experiments, and host–phage / predator–prey systems.


Exponential Growth
---------------------------------
The simplest population model: each cell divides at a constant per-capita
rate :math:`\mu`, so the population doubles every :math:`\ln(2)/\mu` time
units. There is no resource limit and no death — growth continues
unbounded.

.. math::
    \begin{equation*}
        \frac{dX}{dt} = \mu\,X
    \end{equation*}

where,

:X: population (cells, OD, or biomass)
:|mu|: maximum specific growth rate

.. |mu| replace:: :math:`\mu`

Use exponential growth for the early phase of a batch culture (post-lag,
pre-stationary) where nutrients are in excess and the population has not yet
self-limited.


Logistic Competition
---------------------------------
Two species share a common carrying capacity :math:`K_c`. Each species'
growth slows as the combined biomass approaches :math:`K_c`. The
cross-coefficients :math:`\alpha_{xy}` and :math:`\alpha_{yx}` weight
how much one species' density restricts the other's.

.. math::
    \begin{align*}
        \frac{dX}{dt} &= r_x\,X\!\left(1 - \frac{X + \alpha_{xy}\,Y}{K_c}\right) \\
        \frac{dY}{dt} &= r_y\,Y\!\left(1 - \frac{Y + \alpha_{yx}\,X}{K_c}\right)
    \end{align*}

where,

:X, Y: population sizes of the two species
:|rx|, |ry|: intrinsic growth rates of X and Y
:|axy|, |ayx|: competition coefficients (>1 strong, <1 weak, 0 independent)
:|Kc|: shared carrying capacity

.. |rx| replace:: :math:`r_x`
.. |ry| replace:: :math:`r_y`
.. |axy| replace:: :math:`\alpha_{xy}`
.. |ayx| replace:: :math:`\alpha_{yx}`
.. |Kc| replace:: :math:`K_c`

Outcomes depend on the :math:`\alpha` values: stable coexistence,
competitive exclusion, or bistability.


Monod Growth
---------------------------------
Bacterial growth that depends on a limiting substrate. The functional form
matches Michaelis–Menten: linear in :math:`S` at low concentration,
saturating at :math:`\mu_{max}` when :math:`S \gg K_s`. As cells grow they
consume substrate at a rate set by the yield coefficient :math:`Y`.

.. math::
    \begin{align*}
        \frac{dX}{dt} &= \mu_{max}\,X\,\frac{S}{K_s+S} \\
        \frac{dS}{dt} &= -\frac{1}{Y}\,\mu_{max}\,X\,\frac{S}{K_s+S}
    \end{align*}

where,

:X: population
:S: limiting substrate concentration
:|mumax|: maximum specific growth rate
:|Ks|: half-saturation constant (S where :math:`\mu = \mu_{max}/2`)
:Y: yield coefficient (biomass produced per unit substrate)

.. |mumax| replace:: :math:`\mu_{max}`
.. |Ks| replace:: :math:`K_s`

Monod is the standard substrate-limited growth law for batch and chemostat
cultures. It captures the transition from exponential growth into stationary
phase as the substrate is depleted.


Competitive Monod Growth
---------------------------------
Two strains compete for the same substrate **and** share a carrying
capacity. Each strain has its own kinetic constants, so strain-level
differences drive the competitive outcome.

.. math::
    \begin{align*}
        \frac{dX}{dt} &= \mu_{max,x}\,X\,\frac{S}{K_{s,x}+S}
            \!\left(1 - \frac{X + \alpha_{xy}\,Y}{K_c}\right) \\
        \frac{dY}{dt} &= \mu_{max,y}\,Y\,\frac{S}{K_{s,y}+S}
            \!\left(1 - \frac{Y + \alpha_{yx}\,X}{K_c}\right) \\
        \frac{dS}{dt} &= -Y_x\,\mu_{max,x}\,X\,\frac{S}{K_{s,x}+S}
            \!\left(1 - \tfrac{X + \alpha_{xy}Y}{K_c}\right) \\
        & \quad - Y_y\,\mu_{max,y}\,Y\,\frac{S}{K_{s,y}+S}
            \!\left(1 - \tfrac{Y + \alpha_{yx}X}{K_c}\right)
    \end{align*}

The equation builder offers two toggles:

- **Single species competition** — freezes :math:`Y` so only :math:`X`
  grows competitively. Useful when one strain is treated as a fixed
  background.
- **Remove competitive restriction from substrate consumption** — drops
  the carrying-capacity factor from the :math:`dS/dt` term, so substrate
  is consumed strictly per Monod kinetics.

Competitive Monod is richer than logistic competition (which lacks an
explicit substrate) and richer than plain Monod (which lacks a competitor).
Use it for mixed bioreactor cultures with one shared carbon source or
strain-replacement experiments.


Predator–Prey (Lotka–Volterra)
---------------------------------
Prey :math:`X` grow exponentially in the absence of the predator;
predators :math:`Y` decay exponentially in the absence of prey.
Encounters (rate :math:`\propto X\!\cdot\!Y`) transfer mass from prey to
predator.

.. math::
    \begin{align*}
        \frac{dX}{dt} &= r\,X - a\,X\,Y \\
        \frac{dY}{dt} &= b\,X\,Y - d\,Y
    \end{align*}

where,

:X: prey population
:Y: predator population
:r: prey intrinsic growth rate
:a: predation rate per prey-predator encounter
:b: predator growth rate per encounter
:d: predator natural death rate

The classical system produces neutral oscillations — prey peaks lead
predator peaks. The same form models phage–bacteria, immune cell–
pathogen, and grazer–algae interactions.
