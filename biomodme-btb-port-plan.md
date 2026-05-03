# Port BobTheBuilder updates into BioModME

## Deployment gotcha to remember (learned 2026-04-27)

When PR #6 (PR 0+1, bacterial-growth-equations) was merged to `main`, the production deploy at `biomodme.ctsi.mcw.edu` ran but **did not actually create the new `functions/rateLaws/` directory** on the production filesystem. Symptoms: app reached "Listening on..." cleanly (no startup errors), but every Shiny session was broken — slow to load, conditional UI panels for reactions blank, equations table mathjax rendered as `dY/dt = (1 - +)` (rate-law template with all variable names empty). Root cause: the production deploy mechanism didn't recurse into the new directory created by `git mv` of all 10 rate-law files into the new subfolder.

**Fix that worked:** bumped version `v1.20 → v1.21` in [ui.R](ui.R) and pushed to `main` (commit `bd1557a`). The new commit triggered a fresh deploy that picked up the directory correctly.

**Lesson for future BTB ports:** any PR that introduces a new top-level directory or moves files into a new subdirectory needs deploy-mechanism verification. Either (a) coordinate with IT before merging so they can verify the deploy handles new dirs, or (b) follow the merge with an immediate version-bump commit to force a clean re-pull. The latter is cheap insurance.

**Pre-existing client-side errors to ignore (confirmed at `61dde91`, predate PR 1):** browser console shows `Unexpected input value mode: '[object Object]'` repeatedly; sidebar auto-collapses. These are a `shinyWidgets`/`colourpicker`/`shinyBS` ↔ modern `shiny` API mismatch — outdated bindings call `Shiny.setInputValue(name, value, {priority: ...})` but newer Shiny expects a primitive string for the third arg. Not introduced by any of our PRs — they just got more visible because PR 1 added more `pickerInput`/`prettyCheckbox` widgets that exercise the broken bindings more frequently. Fix when convenient: `update.packages(c("shinyWidgets","colourpicker","shinyBS"))` on dev and prod.

## Status (resume here on a fresh chat)

**Branch:** `feat/custom-eqn-mathjax-display` (PR 3a-1, NOT yet merged); parent `fix/plot-output-legend-logscale-rexport` (PR 2, NOT yet merged); grandparent `feat/bacterial-growth-equations` (PR 0+1, also NOT yet merged). Three-deep stack, none on `main` yet.

**Done — PR 0 + PR 1 + PR 2 + PR 3a-1 (10 commits, all user-verified):**

PR 0 + PR 1 on `feat/bacterial-growth-equations`:
- `a68904f` refactor: split rate laws into functions/rateLaws/
- `5b5af5f` feat: add exponential growth bacterial reaction law
- `53aa3e1` feat: add logistic_competition bacterial reaction law
- `2b4b421` feat: add monod_growth bacterial reaction law
- `4c00172` feat: add competitive monod growth bacterial reaction law
- `df6927e` feat: add predator-prey bacterial reaction law

PR 2 on `fix/plot-output-legend-logscale-rexport`:
- `7b449d4` feat: add ggplot plotting code to exported R script (BTB `750b6f7`)
- `8e3e1b0` fix: preserve legend order in downloaded plot (BTB `6f6ef25`)
- `f68acb6` feat: add log-scale toggle to lineplot axes (BTB `5c8508f`, log-scale portion only)

PR 3a-1 on `feat/custom-eqn-mathjax-display`:
- `1e78e8e` feat: render underscored variables as MathJax subscripts in custom-equation preview (BTB `5131618`, create-file half only)

PR 2 commits landed in chronological-against-BTB order (R-export, then legend fix, then log-scale) because the legend fix was authored on top of log-scale code in BTB; doing log-scale last meant it applied cleanly to the legend-fixed `13_0_run_lineplot.R`. The plan's original 1-2-3 listing was an arbitrary ordering and not load-bearing.

**Important finding during PR 2 — BTB `5c8508f` is a bundled commit.** It claims to be "added log scale of plot" but actually contains TWO unrelated features:

1. The actual log-scale work (2 files, ~150 lines) — what we ported.
2. An orthogonal **"relative formation" / krel feature for `degradation_by_enzyme` reactions** (4 files, ~460 lines: `server/02_equations.R`, `server/02_equations_renderUI.R`, `server/02_equations_edit.R`, `server/DeriveODEs.R`). This adds a "Relative Formation" checkbox + krel text/numeric inputs to both the add-reaction and edit-reaction modals, krel parameter creation/storage in the addEqnToVector observer, and extends the existing `degradation_rate` krel rate-law modification logic in `DeriveODEs.R` to also cover `degradation_by_enzyme`. The BTB version of `02_equations_edit.R` is also riddled with ~30 leftover `print("DEBUG: ...")` statements that should not be merged anywhere.

This krel feature is genuinely useful and worth porting eventually — see PR 3c below — but is not in scope for PR 2.

The plan's earlier note that "the equation files get small touches because the toggle state lives there" was wrong; the toggle state lives entirely in the lineplot files. Anyone resuming should ignore that note.

**Also deferred (mentioned during PR 1 testing, not yet ported):** `server/13_0_run_lineplot.R` line 19 `gatherData()` raises a benign tidyselect "Elements don't exist" warning when `input$lineplot_yvar` lags behind `rv.RESULTS$results.model.final`. Pre-existing in BioModME, surfaces in the R console but doesn't affect the app. Log-scale didn't change this code path. Defensive `intersect(varsToSelect, names(data))` filter at the top of `gatherData` resolves it cleanly.

## After PR 2 — PR 3: custom-eqn edit/delete + Substrate Synthesis Competition

These were deferred during the original scoping (the user opted for bacterial-equations-first). They're worth picking up after PR 2.

### PR 3a — Custom equation edit/delete + mathjax

**Split during execution into PR 3a-1 (done) and PR 3a-2 (deferred — see below).**

The original assumption ("complements the user's own custom-logic-tab work") was wrong: **BioModME's Sept 2024 custom-logic-tab work fundamentally rewrote the custom-eqn add-flow**, replacing BTB's rhandsontable + `hot_to_r()` round-trips with a much cleaner reactive design (`rv.CE.BUILDER$existing.df` / `new.df` shadow data.frames auto-derived by an `observe()` block). BTB's edit/delete code is bolted onto BTB's BTB-shaped add-flow — same bolts don't fit BioModME.

#### PR 3a-1 — MathJax subscript rendering (DONE)

Ported just the mathjax-display half of BTB `5131618` to BioModME's [server/51_create_custom_eqn.R](server/51_create_custom_eqn.R). Adds `ConvertExpressionToMathJax()` helper (~25-line gsub-based regex that turns `sigma_ABE` into `sigma_{ABE}`) and applies it to LHS and RHS in the existing `custom_law_expression` reactive. Pure improvement, no UI changes, no dependency on edit/delete. Commit: `1e78e8e`.

#### PR 3a-2 — Edit / Delete custom equations (DEFERRED — separate future session)

This is **fresh design work, not transcription**. Briefing for whoever picks it up:

**What BTB has** (post all 3 commits, `f04b7d6` + `5131618` + `29168e3`):
- `server/51_edit_delete_custom_eqn.R` — 714 lines. Two big observers:
  1. `bttn_custom_eqn_update` — essentially "do the create flow but update the existing entry instead of insert". Reads from `hot_to_r(input$RHT_custom_eqn_params_existing_edit)` and `hot_to_r(input$RHT_custom_eqn_params_new_edit)`.
  2. `bttn_custom_eqn_delete_confirm` — solid business logic: find equation IDs, find dependent params/species, check whether each is used elsewhere (other custom eqns, reactions, IO), remove cleanly. **This logic is mostly portable as-is** — the only UI dependency is a preview table that's currently `rHandsontableOutput`, easy to rewrite as DT.
- `ui/modal_custom_eqn_edit.R` — 98-line `shinyBS::bsModal` with picker, LHS/RHS edit fields, two `rHandsontableOutput`s, mathjax preview, Update button.
- `ui/modal_custom_eqn_delete.R` — 55-line `bsModal` with multi-select picker, `rHandsontableOutput` preview, "Close on Delete" checkbox, Delete button.
- `ui/51_create_custom_eqn_ui.R` (`f04b7d6` diff) — sources both modals, wraps the existing-equations table in a "Custom Equations" box, adds two `actionButton`s (`bttn_custom_eqn_edit`, `bttn_custom_eqn_delete`).
- `server.R` — adds one `source(file.path("server", "51_edit_delete_custom_eqn.R"))` line. (Note: `git diff` shows 192/187 on `server.R` but that is **byte-for-byte rewrite from line-ending conversion** — the real semantic change is just the one source line. Don't get spooked by the diff size.)

**What BioModME has that's different / better:**
- `rv.CE.BUILDER` reactive with auto-derived `existing.df` and `new.df` shadow data.frames. The `observe()` block at lines ~265–339 of [server/51_create_custom_eqn.R](server/51_create_custom_eqn.R) is the heart of the design.
- DT (not rhandsontable) for both variable tables and the existing-equations table.
- A `CE_new_type_editor` renderUI + `CE_new_type_picker` observer that lets users change a new variable's Type by clicking a row.

**Design choices for PR 3a-2:**

1. **Two new modals, BioModME-shaped** (DT-based, integrated with `rv.CE.BUILDER`):
   - `ui/modal_custom_eqn_edit.R` — picker + LHS/RHS edit fields + two DTs (read from `rv.CE.BUILDER`, not `hot_to_r`) + mathjax preview (reuse `ConvertExpressionToMathJax` already added in PR 3a-1) + Update button.
   - `ui/modal_custom_eqn_delete.R` — multi-select picker + DT preview + Delete button.
2. **One new server file**, `server/51_edit_delete_custom_eqn.R`, BioModME-shaped. The DELETE business logic ports nearly verbatim from BTB (replace `rHandsontableOutput` preview with `DTOutput`/`renderDT`; otherwise pure R logic). The EDIT observer is the design challenge — see below.
3. **Edit-flow design challenge:** when a user opens the edit modal, the LHS/RHS need to *populate* the edit-form text inputs from the selected equation, and `rv.CE.BUILDER`'s `observe()` should re-derive `existing.df`/`new.df` from those new inputs. **Critical correctness concern BTB does not handle well:** when applying the update, we must NOT create duplicate species/params for variables that were already in the previous version of the equation. Compute the diff: which IDs in the old `Old.Species.id`/`New.Species.id`/`Old.Parameters.id`/`New.Parameters.id` are NOT in the new derived sets → those are now-orphaned and should be considered for removal (same dependency-check logic as delete). Then create truly-new entries for any variables that weren't in the previous version. The cleanest implementation may be a shared helper function used by both `bttn_custom_eqn_enter` (create) and `bttn_custom_eqn_update` (edit) — refactor opportunity.
4. **Avoid input-ID collision:** if the modal reuses input IDs `TI_custom_eqn_LHS_edit` etc., they MUST be distinct from the create form's IDs (they are in BTB's design — `_edit` suffix). The `rv.CE.BUILDER$existing.df`/`new.df` reactive currently keys off `input$TI_custom_eqn_LHS` and `input$TI_custom_eqn_RHS`. Either (a) parameterize the observe() block, (b) introduce a second `rv.CE.BUILDER.EDIT` reactive set, or (c) set the create inputs from the modal (cute but probably confusing — the create form is still visible behind the modal). Option (b) is the simplest and matches BioModME's existing pattern.
5. **Sizing fix `29168e3`** is just CSS / column widths for BTB's delete-modal rhandsontable. Once we rewrite as DT, this fix is irrelevant — size the DT how we want.
6. **`ConvertExpressionToMathJax` is already in BioModME** (PR 3a-1). The edit modal's mathjax preview can just call it.

**Estimated scope:** ~400-600 net new lines across 3 new files plus small touches to `server.R` and `ui/51_create_custom_eqn_ui.R`. More careful than the PRs done so far. Should be its own session and its own PR. Suggested branch: `feat/custom-eqn-edit-delete` off whichever parent is current at the time.

**Suggested verification:**
- Open existing model with custom eqns from `base_models/`, check Edit/Delete buttons appear.
- Edit: change RHS, confirm equation updates, confirm no duplicate species/params, confirm dropped variables get cleaned up if unused elsewhere.
- Delete: select multiple, confirm removal, confirm dependent params/species removed iff unused elsewhere, confirm reactions/IO using those params still work.
- Save/load roundtrip: edit a custom eqn, save `.rds`, reload, verify state restored.

### PR 3b — Substrate Synthesis Competition law (sixth bacterial law)

BobTheBuilder has a sixth bacterial reaction law (`substrate_synthesis_competition`) we didn't port in PR 1. Its rate-law function file is `functions/rateLaws/Substrate_Synthesis_Competition.R` — already exists in BobTheBuilder, sized 67 lines.

Models substrate consumption with optional competitor — produces:
$$\text{rate} = k \cdot S \cdot \left(1 - \frac{X + \alpha Y}{K_c}\right) \quad \text{or simpler form when no competitor}$$

Architecture should follow the same single-row + DeriveODEs pattern as `logistic_competition`/`predator_prey`. Relevant files (search BobTheBuilder for `substrate_synthesis_competition`):

- Copy the rate-law file `Substrate_Synthesis_Competition.R` into `functions/rateLaws/`.
- Add to `00_reactive_variables.R` law registry (Name, BackendName "substrate_synthesis_competition", Type "bacterial").
- Add `reset_all_storage_variables.R` entries.
- Add modal panels (`modal_reaction_add.R` / `modal_reaction_edit.R`).
- Add `02_equations_renderUI.R` builder block.
- Add `02_equations.R` add observer dispatch + storage (companion entry: e.g. `rv.REACTIONS$substrateSynthesisCompetition`).
- Add `02_equations_edit.R` edit dispatch + observer + storage.
- Add `02_equations_text_mathjax.R` branches in 5-6 reactives.
- Add `DeriveODEs.R` special handling: pick the correct per-species rate law from the companion entry, route through `ConvertRateLaw()`.
- Add `load_rds.R` / `load_sbml.R` backfill blocks.

Relevant BobTheBuilder commits: search BTB git history with `git log -S "substrate_synthesis_competition" --oneline` to find the introducing commit and any follow-up fixes.

### PR 3c (optional polish — small standalone fixes)

Each is independent; can be batched into one PR or skipped:

- **krel "relative formation" feature for `degradation_by_enzyme`** — extracted from BTB `5c8508f` during PR 2 (the bundled-commit finding). Touches 4 files: `server/02_equations.R` (krel parameter creation in addEqnToVector observer for the `degradation_by_enzyme` branch), `server/02_equations_renderUI.R` (Relative Formation checkbox + krel text/numeric inputs in the builder layout — note BTB also restructures the column widths from `width=3, offset=1` to `width=9` to make room), `server/02_equations_edit.R` (same UI in the edit modal + krel handling in the edit observer + ~30 `print("DEBUG: ...")` statements that **must be stripped**), `server/DeriveODEs.R` (extends the existing `degradation_rate` krel rate-law logic to also cover `degradation_by_enzyme` — splits into has-division vs no-division branches). Pattern follows the existing `degradation_rate` krel implementation already in BioModME, so the rate-law branch logic should feel familiar.
- Parameter autonumbering (BTB `421be7d`)
- Scientific notation in parameters table (BTB `607ee2c`) — **needs re-implementation against DT** since BioModME migrated off rhandsontable in April 2026.
- Hide-initial-volume-factor option (BTB `c4f5760`)
- MathJax cleanups: parenthesis (`74d9ff9`), volume-removal whitespace (`9a9a3e2`, `2d00653`), double-product visual bug (`6345c7c`)
- Add parameters to regulation reactions (BTB `a0571cf`)
- Edit defaults of differential equations (BTB `380b799`)
- ggplot R output already covered in PR 2.

### Things to NOT port (would conflict with our architecture)

These BobTheBuilder commits fix bugs in BTB's *two-row* multi-species design — bugs that don't exist in our single-row architecture:

- `a08a98b` predator prey update
- `1518220` reaction table only shows one
- `c098608` cleared double equation with predator prey showing
- `382df56` fixed predator prey not editing right
- `437fa14` updated parameter naming for predator prey
- `e6655d2` fixed extra logistic equations showing

Already done in PR 0:

- `73eb5dd` fixed folder issue (server.R `list.files()` `\\.R$` filter)
- `5a219e9` separated rate laws to own folder
- `91ff6ec` added string rate law for catalytic activation irrev (the rate-law file copy)

## Architectural decisions established during PR 1 (carry forward)

These were learned through bug-fix iteration and apply to any future BobTheBuilder ports:

- **Single-row architecture** for multi-species reactions. BobTheBuilder creates 2-4 reaction rows per multi-species reaction (one per species + per substrate-consumption pathway); BioModME consolidates to ONE row with `Species` listing all participants. Per-species rate laws live on a dedicated companion entry (`logisticCompetition`, `competitiveMonod`, `predatorPrey`) keyed by the same ID. This avoids the cross-row contamination bugs that produced malformed `dY/dt` summaries when ODEs were assembled.
- **DeriveODEs special handling** for any reaction whose `Reaction.Law` is in `c("logistic_competition", "competitive_monod", "predator_prey")`. Inside the `for (eqn.id in reactions)` loop in `DeriveEquationBasedODEs`, look up the companion entry and select the rate law that matches the current species's id (`Species.X.id` / `Species.Y.id` / `Substrate.id` / `Prey.id` / `Predator.id`). Always route the selected rate law through `ConvertRateLaw()` so `mj.rate` and `latex.rate` get proper Var2MathJ/Var2Latex formatting — passing the raw plain-string rate law as `mj.rate` triggers KaTeX subscript-rendering failures.
- **`Var2MathJ` and `Var2Latex` were patched** (functions/Var2MathJ.R, server/write_latex_document.R) to escape underscores beyond the first one as `\\_`. Without this fix, names like `mu_max_x_2` produce `mu_{max_x_2}` which KaTeX rejects with "double subscript at position N". Don't undo this when porting.
- **Single set of edit-form input IDs** — do NOT introduce `_2`-suffix variants for checkbox-driven mode toggles (BobTheBuilder uses `PI_..._2` to "preserve values across panel toggles" but on first render they default to `selected = input$..._2 = NULL` → first species "A" — leading to stale-default bugs). Use one set of input IDs and just `conditionalPanel`-hide the irrelevant rows.
- **Defensive `is.null(info)` guards** in edit renderUI dispatches: the lookup `rv.REACTIONS$<companion>[[eqn.ID]]` can return NULL when an old multi-row reaction is the row being edited (orphan from a previous design). Show a friendly warning panel rather than crashing.
- **Backfill missing built-in laws** in `server/load_rds.R` and `server/load_sbml.R` after `rv.REACTIONLAWS$laws <- model$laws` so older saved models still expose newly-added laws in the dropdown. Pattern: `if (!"<backend_name>" %in% rv.REACTIONLAWS$laws$BackendName) rbind(...)`.
- **Reaction-law registry consolidates** in five places (in this exact ordering — keep them in sync): `00_reactive_variables.R` (rv.REACTIONLAWS list + rv.sbml.temp list — both need the Name/BackendName/Type triple), `reset_all_storage_variables.R` (one-line laws data.frame), and the two backfill blocks in `load_rds.R`/`load_sbml.R`.

## Useful BobTheBuilder reference paths (read-only)

- `c:/Users/3536womackj/Documents/GitHub/BobTheBuilder/` — full source tree
- `git log --oneline server/<file>` inside that repo to find the commit that introduced any specific feature
- `git show <sha> -- <file>` to see the diff for any commit referenced above

## Original plan (pre-PR 2)

## Context

BioModME and BobTheBuilder ([root path](../../../Documents/GitHub/BobTheBuilder/)) share a common ancestor and have diverged. BobTheBuilder copied BioModME's state in Feb 2024 (commit `0eb8b17`) and has since added ~43 commits, most importantly a large batch of bacterial growth differential equations in Nov-Dec 2025 plus several plot/output fixes. BioModME has independently added a custom logic tab (Sept 2024), Julia export (early 2024), and just finished migrating its parameter table from `rhandsontable` to `DT` (April 2026).

This plan ports two specific feature groups from BobTheBuilder into BioModME:

1. **Bacterial growth equations** (the largest delta, the user's top priority)
2. **Plot/output fixes** (download legend, log-scale plot, ggplot in R export)

Out of scope (deferred): custom-equation edit/delete, parameter autonumbering, scientific notation, mathjax cleanups, `server.R` `list.files()` pattern fix, the new `13_3_loop_model.R`.

## User-locked decisions

- **Scope:** only the two groups above.
- **Staging:** one feature group per branch / PR.
- **Table backend:** all ported tables must use `DT`. Any `rhandsontable::` / `renderRHandsontable` / `hot_to_r(` code in transcribed hunks must be re-expressed with `DT::renderDT` / `DT::datatable(editable = ...)` and `input$<id>_cell_edit` observers.
- **Rate-law folder:** adopt BobTheBuilder's [functions/rateLaws/](../../Documents/GitHub/BobTheBuilder/functions/rateLaws/) layout.

## Approach

Cherry-picking is not an option — `02_equations_renderUI.R` is 966 lines in BioModME vs 2093 in BobTheBuilder, and BioModME has its own divergent edits sandwiched into the same files. Use **read-patch, hand-apply**: generate `git show <sha> -- <file>` in BobTheBuilder for reference, then locate the BioModME analog of each hunk by searching for stable code anchors (function names, `observeEvent(input$...)` blocks, `backend.call == "..."` switch branches) — never by line number — and transcribe manually, preserving BioModME-specific code between hunks.

For each transcribed hunk: grep for `hot_`, `rhandsontable`, `renderRHandsontable` and rewrite to DT before committing.

## PR sequence

### PR 0 — `refactor/ratelaws-folder-structure`

Prerequisite. Land before any feature work because every ported equation sources from `functions/rateLaws/`.

- Create [functions/rateLaws/](../../Documents/GitHub/BioModME/functions/) in BioModME.
- Identify the existing rate-law files in BioModME's flat [functions/](../../Documents/GitHub/BioModME/functions/) folder (the ten files BobTheBuilder split out: `Degradation_By_*`, `Henri_Michaelis_Menten_*`, `Law_Of_Mass_Action.R`, `Regulated_Law_Of_Mass_Action.R`, `Substrate_Synthesis_Competition.R`, `Synthesis_By_*`).
- Move them into `functions/rateLaws/`.
- Update [server.R](../../Documents/GitHub/BioModME/server.R#L38) to mirror BobTheBuilder's [server.R:38-43](../../Documents/GitHub/BobTheBuilder/server.R#L38-L43): add the `\\.R$` pattern filter to the existing `list.files("functions")` call, then add a second `sapply(...)` block that sources `functions/rateLaws/`.
- Smoke test: app launches, existing rate laws still appear in the reaction-add modal, an existing saved `.rds` model still loads and solves.

### PR 1 — `feat/bacterial-growth-equations`

Port five new growth-model laws plus one new rate-law file.

**New file (straight copy):**
- [functions/rateLaws/r_catalytic_activation_irrev.R](../../Documents/GitHub/BobTheBuilder/functions/rateLaws/r_catalytic_activation_irrev.R) — 73 lines, standalone, no BioModME analog.

**Equations to port** (in this order — start simple, end with the equation that has special ODE-derivation handling):

1. `exponential_growth` — simplest, single species, validates the pipeline.
2. `logistic_competition` — adds carrying capacity.
3. `monod_growth` — substrate saturation.
4. `competitive_monod` — multi-species coupling (also has the "remove competition from substrate" option).
5. `predator_prey` — has a special branch in [DeriveODEs.R](../../Documents/GitHub/BobTheBuilder/server/DeriveODEs.R) at line 194 in BobTheBuilder; port last.

**Per-equation file dependency order** (violating it crashes on app load):

1. [server/00_reactive_variables.R](../../Documents/GitHub/BioModME/server/00_reactive_variables.R) — register the law name. Commit together with the handler.
2. [ui/modal_reaction_add.R](../../Documents/GitHub/BioModME/ui/modal_reaction_add.R) — conditional UI panel.
3. [ui/modal_reaction_edit.R](../../Documents/GitHub/BioModME/ui/modal_reaction_edit.R) — edit modal panel.
4. [server/02_equations_renderUI.R](../../Documents/GitHub/BioModME/server/02_equations_renderUI.R) — `output$equationBuilder_<law>` renderUI.
5. [server/02_equations.R](../../Documents/GitHub/BioModME/server/02_equations.R) — add-reaction `backend.call == "<law>"` branch.
6. [server/02_equations_edit.R](../../Documents/GitHub/BioModME/server/02_equations_edit.R) — edit/delete handler branch.
7. [server/DeriveODEs.R](../../Documents/GitHub/BioModME/server/DeriveODEs.R) — `predator_prey` only.

### PR 2 — `fix/plot-output-legend-logscale-rexport`

Three independent plot/output fixes. Land last because the log-scale diff overlaps the same `02_equations*.R` files PR 1 rewrote.

- **Download plot legend bug** — BobTheBuilder commit `6f6ef25`, file [server/13_0_run_lineplot.R](../../Documents/GitHub/BioModME/server/13_0_run_lineplot.R).
- **Log-scale plot** — BobTheBuilder commit `5c8508f`, files [server/02_equations.R](../../Documents/GitHub/BioModME/server/02_equations.R), [server/02_equations_edit.R](../../Documents/GitHub/BioModME/server/02_equations_edit.R), [server/02_equations_renderUI.R](../../Documents/GitHub/BioModME/server/02_equations_renderUI.R), [server/13_0_run_lineplot.R](../../Documents/GitHub/BioModME/server/13_0_run_lineplot.R), [server/DeriveODEs.R](../../Documents/GitHub/BioModME/server/DeriveODEs.R), [ui/13_run_lineplot_ui.R](../../Documents/GitHub/BioModME/ui/13_run_lineplot_ui.R).
- **ggplot in R export** — BobTheBuilder commit `750b6f7`, file [server/write_R.R](../../Documents/GitHub/BioModME/server/write_R.R).

## Risks / gotchas

- **Don't pull in `13_3_loop_model.R`.** It exists in BobTheBuilder's [server/](../../Documents/GitHub/BobTheBuilder/server/) but is commented out in BobTheBuilder's [server.R:76](../../Documents/GitHub/BobTheBuilder/server.R#L76). BioModME doesn't have it and shouldn't get it. Strip any `loopModel(...)` call that leaks into the log-scale diff.
- **Casing:** BioModME uses [server/repository.R](../../Documents/GitHub/BioModME/server/repository.R), BobTheBuilder uses `repository.r`. Keep BioModME's casing if any source path is transcribed.
- **Line numbers drift.** BobTheBuilder's "predator-prey at line 194" of `DeriveODEs.R` won't be at line 194 in BioModME (379 vs 474 lines). Anchor by code, not number.
- **Catalytic activation visibility.** Confirm whether `r_catalytic_activation_irrev` is wired into BobTheBuilder's reaction-add modal or is code-only. Match that exposure level — don't speculatively add UI.
- **Reactive registration ordering.** `00_reactive_variables.R` law-name registration must precede any `02_equations*.R` handler that dispatches on that name. Commit them together.
- **rhandsontable re-implementation surface.** Grep every transcribed hunk in `02_equations_renderUI.R` and `02_equations_edit.R` for `hot_`, `rhandsontable`, `renderRHandsontable` before committing. Rewrite to DT.

## Verification

Run after each PR.

**PR 0:**
- App starts: `R -e "shiny::runApp('c:/Users/3536womackj/Documents/GitHub/BioModME')"` with no parse / source errors.
- Existing rate laws (Mass Action, Henri-Michaelis-Menten, etc.) still selectable in the reaction-add modal.
- Load an existing `.rds` model from [base_models/](../../Documents/GitHub/BioModME/base_models/); confirm it solves and plots.

**PR 1, per equation:**
1. App starts cleanly.
2. Add a reaction using the new law from the reaction-add modal; conditional UI panel appears and accepts input.
3. Save; the equations DT table renders the MathJax row correctly.
4. Open edit modal, change one parameter, save; DT row updates.
5. Delete the reaction; no orphan rows in the parameters DT.
6. Run model; ODE solver converges, lineplot renders.
7. Regression: load an existing `.rds` that doesn't use the new law; still loads, solves, plots.
8. `predator_prey` only: hand-derive a Lotka-Volterra ODE system and confirm `DeriveODEs.R` produces a matching symbolic form.

**PR 2:**
- Run any model, click download-plot, open the saved PNG; legend present and uncropped.
- Toggle log-scale checkbox in the lineplot UI; axis switches without Inf/NaN warnings for species with zero initial values.
- Export to R code, `source()` the file in a fresh R session; produced ggplot matches the in-app plot.
- Regression: re-run PR 1's bacterial equation smoke tests; no interaction.

## Critical files to modify

- [server.R](../../Documents/GitHub/BioModME/server.R) — PR 0
- [server/00_reactive_variables.R](../../Documents/GitHub/BioModME/server/00_reactive_variables.R) — PR 1
- [server/02_equations.R](../../Documents/GitHub/BioModME/server/02_equations.R) — PR 1, PR 2
- [server/02_equations_renderUI.R](../../Documents/GitHub/BioModME/server/02_equations_renderUI.R) — PR 1, PR 2
- [server/02_equations_edit.R](../../Documents/GitHub/BioModME/server/02_equations_edit.R) — PR 1, PR 2
- [server/DeriveODEs.R](../../Documents/GitHub/BioModME/server/DeriveODEs.R) — PR 1 (predator-prey), PR 2 (log-scale)
- [server/13_0_run_lineplot.R](../../Documents/GitHub/BioModME/server/13_0_run_lineplot.R) — PR 2
- [server/write_R.R](../../Documents/GitHub/BioModME/server/write_R.R) — PR 2
- [ui/modal_reaction_add.R](../../Documents/GitHub/BioModME/ui/modal_reaction_add.R) — PR 1
- [ui/modal_reaction_edit.R](../../Documents/GitHub/BioModME/ui/modal_reaction_edit.R) — PR 1
- [ui/13_run_lineplot_ui.R](../../Documents/GitHub/BioModME/ui/13_run_lineplot_ui.R) — PR 2
- [functions/rateLaws/](../../Documents/GitHub/BioModME/functions/) — created in PR 0; new file `r_catalytic_activation_irrev.R` added in PR 1
