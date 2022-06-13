# fsr 1.9.9.9000

Changes in the development version of the _fsr_ package

# Major changes

- Created a new structure for the classes ([#26 - pedbrgs](https://github.com/accarniel/fsr/pull/26))
  - Added two new spatial plateau data types: `pcollection` and `pcomposition`
  - Redefined the class named pgeometry
  - Defined three spatial plateau data types that inherit from pgeometry: ppoint, pline, and pregion
  - Modified the function `spa_pwkt` to deal with the new classes
- Added and updated some functions to deal with the new spatial plateau data types ([#27 - pedbrgs](https://github.com/accarniel/fsr/pull/27))
  - Added a set of auxiliary functions to handle pcollection and pcomposition objects
  - Updated some existing functions to handle these objects (`create_pgeometry`, `fsr_is_empty`, `is_compatible`, `is_pgeometry`, `create_empty_pgeometry`)
  - Added methods for validating the constraints of spatial plateau objects according to the rules of the Spatial Plateau Algebra (setValidity and `spa_is_valid`)
- Updated the fuzzy numerical operations (i.e., `spa_avg_degree`, `spa_ncomp`, `spa_area`, `spa_perimeter`, `spa_length`) to deal with the new classes ([#30 - pedbrgs](https://github.com/accarniel/fsr/pull/30))
- Updated `spa_eval` and `fsr_plot` to manipulate pcollection and pcomposition objects (pedbrgs - added as a collaborator now)
- Updated the fuzzy geometric set operations (i.e., `spa_union`, `spa_intersection`, `spa_difference`). Now, these operations can receive ppoint, pline, pregion, pcomposition and pcollection objects as input. That means the operands of these operations can be two objects of different spatial plateau data types. Due to this update, a set of internal functions were also included ([#31 - pedbrgs](https://github.com/accarniel/fsr/pull/31))
- Adapted the fuzzy topological relationships to call the updated functions (they still work on pregion objects only) 
- Deprecated `spa_common_points` and `spa_boundary_pregion`. They will be removed in the next version of the package ([#31 - pedbrgs](https://github.com/accarniel/fsr/pull/31))
- Improved the creation of components. We have refactored the function `create_component` and deprecated `component_from_sfg` (it will be removed in the next version of the package)

# Minor changes

- Added a new external function called `spa_get_type` ([#27 - pedbrgs](https://github.com/accarniel/fsr/pull/27))
- Added two external functions that handle pcollection objects: `pcollection_to_pcomposition` and `spa_flatten` ([#31 - pedbrgs](https://github.com/accarniel/fsr/pull/31))
- Updated the functions that build pregion objects from point datasets

# Bug fixes

- Fix the method that implements the Delaunay triangulation policy. It now generates empty pregion objects when it is not possible to build triangles from the fuzzified point dataset.

# fsr 1.0.0

- This is the first release of _fsr_ that consists of the following modules:
  - Fuzzy Spatial Inference Module (e.g., functions to create a fuzzy spatial inference model and functions to evaluate it)
  - Fuzzy Spatial Data Handling Module (e.g., fuzzy numerical operations, fuzzy geometric set operations, and fuzzy topological predicates)
  - Construction Module (e.g., `spa_creator`)
  - Basic Module (e.g., S4 classes and basic functions)
