# SBML <unitDefinition> import/export tests.
# Closes the documented limitation that models load as unitless.

parse_unit_defs_xml <- function(xml_str) {
  doc <- xml2::read_xml(xml_str)
  xml2::as_list(doc)$listOfUnitDefinitions
}

unit_defs_block <- function(inner) {
  paste0(
    '<listOfUnitDefinitions xmlns="http://www.sbml.org/sbml/level2">',
    inner,
    '</listOfUnitDefinitions>'
  )
}

test_that("ParseUnitDefinitions handles simple SI units", {
  parsed <- ParseUnitDefinitions(parse_unit_defs_xml(unit_defs_block(
    '<unitDefinition id="substance"><listOfUnits><unit kind="mole"/></listOfUnits></unitDefinition>
     <unitDefinition id="vol"><listOfUnits><unit kind="litre"/></listOfUnits></unitDefinition>
     <unitDefinition id="t"><listOfUnits><unit kind="minute"/></listOfUnits></unitDefinition>'
  )))
  expect_equal(parsed$substance$display, "mol")
  expect_equal(parsed$substance$description, "count")
  expect_equal(parsed$vol$display, "L")
  expect_equal(parsed$vol$description, "volume")
  expect_equal(parsed$t$display, "min")
})

test_that("ParseUnitDefinitions composes mol/L into a concentration", {
  parsed <- ParseUnitDefinitions(parse_unit_defs_xml(unit_defs_block(
    '<unitDefinition id="conc"><listOfUnits>
       <unit kind="mole" exponent="1"/>
       <unit kind="litre" exponent="-1"/>
     </listOfUnits></unitDefinition>'
  )))
  expect_equal(parsed$conc$display, "mol/L")
  expect_equal(parsed$conc$description, "concentration")
})

test_that("ParseUnitDefinitions applies scale prefixes (mmol)", {
  parsed <- ParseUnitDefinitions(parse_unit_defs_xml(unit_defs_block(
    '<unitDefinition id="mm"><listOfUnits>
       <unit kind="mole" exponent="1" scale="-3"/>
       <unit kind="litre" exponent="-1"/>
     </listOfUnits></unitDefinition>'
  )))
  expect_equal(parsed$mm$display, "mmol/L")
  expect_equal(parsed$mm$description, "concentration")
})

test_that("ParseUnitDefinitions warns on unrecognized kinds without crashing", {
  expect_warning(
    parsed <- ParseUnitDefinitions(parse_unit_defs_xml(unit_defs_block(
      '<unitDefinition id="weird"><listOfUnits>
         <unit kind="kelvin"/>
       </listOfUnits></unitDefinition>'
    ))),
    regexp = "kelvin"
  )
  # Falls back to opaque label; doesn't throw.
  expect_true(!is.null(parsed$weird$display))
})

test_that("SbmlUnitsFromString returns NULL for unparseable strings", {
  expect_null(SbmlUnitsFromString(NA))
  expect_null(SbmlUnitsFromString(""))
  expect_null(SbmlUnitsFromString("dimensionless"))
  expect_null(SbmlUnitsFromString("xyzzy"))
  expect_null(SbmlUnitsFromString("mol/L/min"))   # 3-level division not supported
})

test_that("SbmlUnitsFromString decomposes mol, L, min into single <unit>", {
  expect_equal(SbmlUnitsFromString("mol")[[1]]$kind, "mole")
  expect_equal(SbmlUnitsFromString("mol")[[1]]$exponent, 1)
  expect_equal(SbmlUnitsFromString("mol")[[1]]$scale, 0)

  expect_equal(SbmlUnitsFromString("L")[[1]]$kind, "litre")
  expect_equal(SbmlUnitsFromString("min")[[1]]$kind, "minute")
})

test_that("SbmlUnitsFromString decomposes mol/L into kind=mole + kind=litre^-1", {
  d <- SbmlUnitsFromString("mol/L")
  expect_length(d, 2)
  expect_equal(d[[1]]$kind, "mole")
  expect_equal(d[[1]]$exponent, 1)
  expect_equal(d[[2]]$kind, "litre")
  expect_equal(d[[2]]$exponent, -1)
})

test_that("SbmlUnitsFromString handles scale prefixes (mmol -> scale=-3)", {
  d <- SbmlUnitsFromString("mmol/L")
  expect_equal(d[[1]]$kind, "mole")
  expect_equal(d[[1]]$scale, -3)
  expect_equal(d[[2]]$kind, "litre")
})

test_that("SbmlUnitsFromString handles compound denominators (mmol/(L*min))", {
  d <- SbmlUnitsFromString("mmol/(L*min)")
  expect_length(d, 3)
  expect_equal(d[[1]]$kind, "mole")
  expect_equal(d[[2]]$kind, "litre")
  expect_equal(d[[2]]$exponent, -1)
  expect_equal(d[[3]]$kind, "minute")
  expect_equal(d[[3]]$exponent, -1)
})

test_that("Unit definitions round-trip: SBML -> display -> SBML triples", {
  cases <- list(
    list(inner = '<unit kind="mole"/>',
         expect_kind = "mole", expect_scale = 0, expect_exp = 1),
    list(inner = '<unit kind="litre"/>',
         expect_kind = "litre", expect_scale = 0, expect_exp = 1),
    list(inner = '<unit kind="minute"/>',
         expect_kind = "minute", expect_scale = 0, expect_exp = 1),
    list(inner = '<unit kind="mole" scale="-3"/>',
         expect_kind = "mole", expect_scale = -3, expect_exp = 1)
  )
  for (case in cases) {
    parsed <- ParseUnitDefinitions(parse_unit_defs_xml(unit_defs_block(
      paste0('<unitDefinition id="u"><listOfUnits>', case$inner, '</listOfUnits></unitDefinition>')
    )))
    decomp <- SbmlUnitsFromString(parsed$u$display)
    expect_false(is.null(decomp), info = case$inner)
    expect_equal(decomp[[1]]$kind, case$expect_kind, info = case$inner)
    expect_equal(decomp[[1]]$scale, case$expect_scale, info = case$inner)
    expect_equal(decomp[[1]]$exponent, case$expect_exp, info = case$inner)
  }
})
