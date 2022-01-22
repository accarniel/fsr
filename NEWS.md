# fsr 1.0.0.9000

Changes in the development version of the _fsr_ package

# Major changes

- Create a new structure for the classes ([#26 - pedbrgs](https://github.com/accarniel/fsr/pull/26))
  - Added two new spatial plateau data types: `pcollection` and `pcomposition`
  - Redefined the class named pgeometry
  - Defined three spatial plateau data types that inherit from pgeometry: ppoint, pline, and pregion
  - Modified the function `spa_pwkt` to deal with the new classes
- Added and updated some functions to deal with the new spatial plateau data types ([#27 - pedbrgs](https://github.com/accarniel/fsr/pull/27))
  - Added a set of auxiliary functions to handle pcollection and pcomposition objects
  - Updated some existing functions to handle these objects (`create_pgeometry`, `fsr_is_empty`, `is_compatible`, `is_pgeometry`, `create_empty_pgeometry`)  
  - Added methods for validating the constraings of spatial plateau objects according to the rules of the Spatial Plateau Algebra (setValidity and `spa_is_valid`)
- Updated the fuzzy numerical operations (i.e., ncomps, area, length, perimeter) to deal with the new classes ([#30 - pedbrgs](https://github.com/accarniel/fsr/pull/30))
- Updated `spa_eval` and `fsr_plot` to manipulate pcollection and pcomposition objects (pedbrgs -  addded as a collaborator now)

# Minor changes

- Added a new external function called `spa_get_type` ([#27 - pedbrgs](https://github.com/accarniel/fsr/pull/27))

# Bug fixes

# fsr 1.0.0

- This is the first release of _fsr_ that consists of the following modules:
  - Fuzzy Spatial Inference Module (e.g., functions to create a fuzzy spatial inference model and functions to evaluate it)
  - Fuzzy Spatial Data Handling Module (e.g., fuzzy numerical operations, fuzzy geometric set operations, and fuzzy topological predicates)
  - Construction Module (e.g., `spa_creator`)
  - Basic Module (e.g., S4 classes and basic functions)
