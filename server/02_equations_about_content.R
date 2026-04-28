# Educational content for the equation builder's "About" tab.
#
# Keys are the backend rate-law IDs that flow through input$eqnCreate_reaction_law
# (defined alongside the rate-law registry in 00_reactive_variables.R). The
# CUSTOM_EQUATION key is a shared entry used when the user is in any of the
# custom-equation modes (create_custom, user_custom_law_*, rate_eqn,
# time_dependent).

equation_about_content <- list(

  mass_action = list(
    display_name = "Law of Mass Action",
    math = paste0(
      "\\begin{aligned}",
      "aA + bB &\\xleftrightarrow[k_{-1}]{k_{1}} cC + dD \\\\",
      "-\\frac{1}{a}\\frac{d[A]}{dt} = -\\frac{1}{b}\\frac{d[B]}{dt} ",
      "&= \\frac{1}{c}\\frac{d[C]}{dt} = \\frac{1}{d}\\frac{d[D]}{dt} ",
      "= k_{1}[A]^{a}[B]^{b} - k_{-1}[C]^{c}[D]^{d}",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>The law of mass action states that the rate of a chemical reaction ",
      "is proportional to the product of the concentrations of the reactants, ",
      "each raised to a power equal to its stoichiometric coefficient. It is ",
      "the foundational rate law for elementary chemical reactions in solution.</p>"
    ),
    purpose = paste0(
      "<p>Use mass action when modeling elementary reactions whose rate is ",
      "governed only by the reactant concentrations and a fixed rate constant. ",
      "Forward and reverse rate constants ",
      "(<em>k<sub>1</sub></em>, <em>k<sub>-1</sub></em>) capture the ",
      "thermodynamic equilibrium between reactants and products.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Reversible binding of a ligand to a receptor</li>",
      "<li>Phosphorylation / dephosphorylation cycles with constant kinases or phosphatases</li>",
      "<li>Simple chemical equilibria in well-mixed compartments</li>",
      "<li>Building blocks of larger biochemical pathway models</li>",
      "</ul>"
    )
  ),

  mass_action_w_reg = list(
    display_name = "Mass Action with Regulation",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{d[A]}{dt} &= -a\\!\\left(\\sum_i k_{f,i}\\,reg_i\\right)[A]^{a}[B]^{b} ",
      "+ a\\!\\left(\\sum_i k_{r,i}\\,reg_i\\right)[C]^{c}[D]^{d}",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>A mass-action reaction whose rate constants are themselves modulated ",
      "by other species (the regulators). The regulators do not get consumed ",
      "by the reaction — they only scale how fast it proceeds. ",
      "A classic example is the activation/inactivation of MPF in the cell ",
      "cycle, where CDC25C and WEE1 control the phosphorylation rate ",
      "without being themselves transformed.</p>"
    ),
    purpose = paste0(
      "<p>Use this when the catalytic activity of a reaction depends on a ",
      "regulator concentration (kinase, phosphatase, allosteric activator), ",
      "but the regulator's own dynamics are tracked elsewhere. Multiple ",
      "regulators can act in parallel by summing their (rate constant ",
      "× concentration) contributions.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Kinase-driven phosphorylation where the kinase pool is shared</li>",
      "<li>Cell-cycle transitions modulated by CDK regulators</li>",
      "<li>Signaling cascades with allosteric modifiers</li>",
      "</ul>"
    )
  ),

  synthesis = list(
    display_name = "Synthesis",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{d[Species]}{dt} &= k_{syn} \\quad \\text{(by rate)} \\\\",
      "\\frac{d[Species]}{dt} &= k_{syn}\\,[Factor] \\quad \\text{(by factor)}",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>Models the production of a species at either a constant rate or a ",
      "rate proportional to a driving factor. “By rate” is useful ",
      "when the upstream cause is unknown but the production rate has been ",
      "measured. “By factor” captures cases where a transcription ",
      "factor or signaling molecule drives synthesis without itself being ",
      "consumed (e.g. E2F activating Cyclin E and Cyclin A transcription).</p>"
    ),
    purpose = paste0(
      "<p>Use synthesis to introduce mass into a system without modeling the ",
      "full upstream machinery. The factor variant gives you a switch: when ",
      "the factor is zero, synthesis stops, which is convenient for modeling ",
      "induction.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Constitutive protein expression at a measured rate</li>",
      "<li>Transcription factor-driven gene expression</li>",
      "<li>Background production terms in metabolic models</li>",
      "</ul>"
    )
  ),

  degradation_rate = list(
    display_name = "Degradation (by Rate)",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{d[Species]}{dt} &= -k_{deg}\\,[Species] \\quad \\text{(concentration-dependent)} \\\\",
      "\\frac{d[Species]}{dt} &= -k_{deg} \\quad \\text{(zero-order)}",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>First-order or zero-order loss of a species. ",
      "Concentration-dependent (first-order) degradation matches most ",
      "biological turnover: the more of the species there is, the faster it ",
      "is removed. Zero-order is appropriate when the degradation machinery ",
      "is saturated and operates at a fixed rate.</p>"
    ),
    purpose = paste0(
      "<p>Use this for protein turnover, mRNA decay, or any first-order loss ",
      "process. If degradation produces explicit downstream products, list ",
      "them; otherwise the species is removed from the system.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Protein degradation with measured half-life ",
      "(<em>k<sub>deg</sub> = ln(2) / t<sub>1/2</sub></em>)</li>",
      "<li>mRNA decay</li>",
      "<li>Generic loss / efflux terms</li>",
      "</ul>"
    )
  ),

  degradation_by_enzyme = list(
    display_name = "Degradation by Enzyme",
    math = paste0(
      "\\frac{d[S]}{dt} = -V_{max}\\,\\frac{[S]}{K_{m}+[S]} ",
      "= -(k_{cat}\\,[E])\\,\\frac{[S]}{K_{m}+[S]}"
    ),
    biology = paste0(
      "<p>Enzyme-mediated degradation modeled with Michaelis–Menten ",
      "kinetics. The substrate S is the species being degraded; the enzyme E ",
      "is treated as a catalyst (its concentration scales V<sub>max</sub> but ",
      "is not consumed). Optionally a product P can be tracked, in which case ",
      "P accumulates with the same flux that S loses.</p>"
    ),
    purpose = paste0(
      "<p>Use this when degradation is enzyme-limited rather than ",
      "first-order — i.e. the rate plateaus at V<sub>max</sub> when the ",
      "substrate is in excess. Common for proteasome- or protease-mediated ",
      "turnover.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Proteasomal protein degradation</li>",
      "<li>Specific protease cleavage events</li>",
      "<li>Drug clearance by a metabolizing enzyme</li>",
      "</ul>"
    )
  ),

  michaelis_menten = list(
    display_name = "Michaelis–Menten Kinetics",
    math = paste0(
      "v = \\frac{d[P]}{dt} = V_{max}\\,\\frac{[S]}{K_{M}+[S]} ",
      "= (k_{cat}\\,[E])\\,\\frac{[S]}{K_{M}+[S]}"
    ),
    biology = paste0(
      "<p>The standard approximation for enzyme-catalyzed reactions: rate is ",
      "linear in substrate at low [S] and saturates at V<sub>max</sub> when ",
      "[S] ≫ K<sub>M</sub>. K<sub>M</sub> is the substrate concentration ",
      "at which v = V<sub>max</sub>/2 — a measure of enzyme–substrate ",
      "affinity. The form assumes a quasi-steady-state on the ",
      "enzyme–substrate complex and that [E] ≪ [S].</p>"
    ),
    purpose = paste0(
      "<p>Use Michaelis–Menten when a reaction is catalyzed by an enzyme ",
      "whose total concentration is approximately constant on the timescale ",
      "of interest. It captures saturation behavior that pure mass action ",
      "cannot.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Most metabolic enzyme reactions</li>",
      "<li>Receptor binding / occupancy at quasi-steady state</li>",
      "<li>Transport processes with a saturable carrier</li>",
      "</ul>"
    )
  ),

  exponential_growth = list(
    display_name = "Exponential Growth",
    math = "\\frac{dX}{dt} = \\mu\\,X",
    biology = paste0(
      "<p>The simplest model of bacterial (or any cellular) growth: each cell ",
      "divides at a constant per-capita rate μ, so the population doubles ",
      "every <em>ln(2)/μ</em> time units. There is no resource limit and ",
      "no death — growth continues unbounded.</p>"
    ),
    purpose = paste0(
      "<p>Use exponential growth for the early phase of a batch culture ",
      "(post-lag, pre-stationary) where nutrients are in excess and the ",
      "population has not yet self-limited. μ here is the maximum ",
      "specific growth rate.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Log-phase bacterial culture in a chemostat with excess substrate</li>",
      "<li>Estimating μ<sub>max</sub> from optical density measurements</li>",
      "<li>Sanity-check baseline before adding a more realistic growth law</li>",
      "</ul>"
    )
  ),

  logistic_competition = list(
    display_name = "Logistic Competition (Lotka–Volterra)",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{dX}{dt} &= r_{x}\\,X\\!\\left(1 - \\frac{X + \\alpha_{xy}\\,Y}{K_{c}}\\right) \\\\",
      "\\frac{dY}{dt} &= r_{y}\\,Y\\!\\left(1 - \\frac{Y + \\alpha_{yx}\\,X}{K_{c}}\\right)",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>Two species share a common carrying capacity K<sub>c</sub>. Each ",
      "species' growth slows as the combined biomass approaches K<sub>c</sub>. ",
      "The cross-coefficients α<sub>xy</sub> and α<sub>yx</sub> ",
      "weight how much one species' density restricts the other's: ",
      "α &gt; 1 means strong interference, α &lt; 1 weak ",
      "interference, α = 0 independent growth.</p>"
    ),
    purpose = paste0(
      "<p>Use this for two-species coexistence problems where neither species ",
      "consumes a tracked substrate explicitly — the competition is ",
      "modeled phenomenologically through the carrying capacity. Outcomes ",
      "depend on the α values: stable coexistence, competitive ",
      "exclusion, or bistability.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Co-culture of two bacterial strains in a fixed-volume environment</li>",
      "<li>Plasmid-bearing vs plasmid-free populations</li>",
      "<li>Ecological competition between species sharing a niche</li>",
      "</ul>"
    )
  ),

  monod_growth = list(
    display_name = "Monod Growth",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{dX}{dt} &= \\mu_{max}\\,X\\,\\frac{S}{K_{s}+S} \\\\",
      "\\frac{dS}{dt} &= -\\frac{1}{Y}\\,\\mu_{max}\\,X\\,\\frac{S}{K_{s}+S}",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>Bacterial growth that depends on a limiting substrate concentration ",
      "S. The functional form is identical to Michaelis–Menten: at low S ",
      "growth is roughly first-order in S; at high S growth saturates at ",
      "μ<sub>max</sub>. K<sub>s</sub> is the substrate concentration at ",
      "which μ = μ<sub>max</sub>/2. As cells grow they consume ",
      "substrate at a rate set by the yield coefficient Y.</p>"
    ),
    purpose = paste0(
      "<p>Use Monod when growth is substrate-limited — the standard ",
      "choice for batch or chemostat cultures where a single nutrient ",
      "(carbon source, nitrogen source) governs growth rate. It captures ",
      "the transition from exponential growth into the stationary phase as ",
      "S is depleted.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Glucose-limited <em>E. coli</em> batch culture</li>",
      "<li>Chemostat steady-state analysis</li>",
      "<li>Bioreactor design where substrate feed controls growth</li>",
      "</ul>"
    )
  ),

  competitive_monod = list(
    display_name = "Competitive Monod Growth",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{dX}{dt} &= \\mu_{max,x}\\,X\\,\\frac{S}{K_{s,x}+S}",
      "\\!\\left(1 - \\frac{X + \\alpha_{xy}\\,Y}{K_{c}}\\right) \\\\",
      "\\frac{dY}{dt} &= \\mu_{max,y}\\,Y\\,\\frac{S}{K_{s,y}+S}",
      "\\!\\left(1 - \\frac{Y + \\alpha_{yx}\\,X}{K_{c}}\\right) \\\\",
      "\\frac{dS}{dt} &= -Y_{x}\\,\\mu_{max,x}\\,X\\,\\frac{S}{K_{s,x}+S}",
      "\\!\\left(1 - \\tfrac{X + \\alpha_{xy}Y}{K_{c}}\\right) ",
      "- Y_{y}\\,\\mu_{max,y}\\,Y\\,\\frac{S}{K_{s,y}+S}",
      "\\!\\left(1 - \\tfrac{Y + \\alpha_{yx}X}{K_{c}}\\right)",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>Two strains compete for the <em>same</em> substrate <em>and</em> ",
      "share a carrying capacity. Each strain has its own μ<sub>max</sub>, ",
      "K<sub>s</sub>, and yield Y — strain-level kinetic differences ",
      "drive who wins. Two checkboxes in the form let you (a) freeze Y in ",
      "place so only X grows competitively, and (b) drop the carrying-capacity ",
      "factor from the substrate-consumption term.</p>"
    ),
    purpose = paste0(
      "<p>Use this when two-species coexistence is governed by both nutrient ",
      "competition and density-dependent crowding. Richer than logistic ",
      "competition (which lacks an explicit substrate) and richer than plain ",
      "Monod (which lacks a competitor).</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Mixed bioreactor cultures with one shared carbon source</li>",
      "<li>Strain-replacement experiments (e.g. wild-type vs evolved mutant)</li>",
      "<li>Gut microbiome simplified two-species models</li>",
      "</ul>"
    )
  ),

  predator_prey = list(
    display_name = "Predator–Prey (Lotka–Volterra)",
    math = paste0(
      "\\begin{aligned}",
      "\\frac{dX}{dt} &= r\\,X - a\\,X\\,Y \\\\",
      "\\frac{dY}{dt} &= b\\,X\\,Y - d\\,Y",
      "\\end{aligned}"
    ),
    biology = paste0(
      "<p>The classical Lotka–Volterra predator–prey system. Prey X ",
      "grow exponentially at rate r in the absence of the predator; predators ",
      "Y die exponentially at rate d in the absence of prey. Encounters ",
      "(rate ∝ X·Y) transfer mass from prey to predator: prey are ",
      "lost at rate a·X·Y, predators gain at rate b·X·Y. ",
      "The system produces neutral oscillations — prey peaks lead ",
      "predator peaks.</p>"
    ),
    purpose = paste0(
      "<p>Use predator–prey for any consumer–resource system where ",
      "the consumer's growth requires direct contact with the resource and ",
      "the resource grows on its own. Conceptually the same form models ",
      "phage–bacteria, immune-cell–pathogen, and grazer–algae ",
      "interactions.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Bacteriophage infection of a bacterial host</li>",
      "<li>Immune cell predation of an infectious agent</li>",
      "<li>Demonstrating limit-cycle dynamics in an ecology context</li>",
      "</ul>"
    )
  ),

  CUSTOM_EQUATION = list(
    display_name = "Custom Equation",
    math = "\\frac{d[Species]}{dt} = f\\!\\left(species,\\ parameters,\\ t\\right)",
    biology = paste0(
      "<p>The custom-equation builder lets you write your own rate law or ",
      "time-dependent expression when none of the built-in laws fit. The ",
      "expression is parsed and integrated alongside the rest of the model, ",
      "so it interacts with built-in reactions on the same species.</p>"
    ),
    purpose = paste0(
      "<p>Use a custom equation for non-standard kinetics — Hill ",
      "functions, time-pulsed inputs, switch-like sigmoids, custom inhibition ",
      "terms, or composite forms that mix several mechanisms. Reach for a ",
      "built-in law first when one fits; custom equations are harder to ",
      "validate and to share.</p>"
    ),
    use_cases = paste0(
      "<ul>",
      "<li>Hill-type cooperative binding: <em>v = V<sub>max</sub> S<sup>n</sup> / (K<sup>n</sup> + S<sup>n</sup>)</em></li>",
      "<li>Time-pulsed dosing: piecewise or sinusoidal forcing functions</li>",
      "<li>Inhibition terms (competitive, non-competitive, allosteric)</li>",
      "<li>One-off forms from a paper that don't match a built-in law</li>",
      "</ul>",
      "<p><em>Tip:</em> use underscores in variable names ",
      "(e.g. <code>k_cat</code>, <code>K_M</code>) and the MathJax preview ",
      "will render them as subscripts automatically.</p>"
    )
  )
)
