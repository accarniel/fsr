# fsr 1.0.1

Changes in the development version of the _fsr_ package

## Major changes

- Add a new construction policy in the `spa_creator` function based on the convex hull (an improved implementation from the issue [#29](https://github.com/accarniel/fsr/issues/29))

## Minor changes

- Add the publication that describes the [fsr package](https://dl.acm.org/doi/abs/10.1145/3474717.3484255) into CITATION
- Add an optional parameter in the function `spa_creator` named `digits` (an improved update based on the pull request [#2, @leticiabohnert](https://github.com/accarniel/fsr/pull/28))

## Bug fixes

- Fix `spa_add_component` for checking a specific condition
- Fix the management of CRS when plotting spatial plateau objects (function `fsr_plot`)
- Fix the management of geometry collections resulting from some geometric computations of the spatial plateau geometric set operations (internal function `append_valid_comps`)
  
# fsr 1.0.0

- This is the first release of _fsr_ that consists of the following modules:
  - Fuzzy Spatial Inference Module (e.g., functions to create a fuzzy spatial inference model and functions to evaluate it)
  - Fuzzy Spatial Data Handling Module (e.g., fuzzy numerical operations, fuzzy geometric set operations, and fuzzy topological predicates)
  - Construction Module (e.g., `spa_creator`)
  - Basic Module (e.g., S4 classes and basic functions)