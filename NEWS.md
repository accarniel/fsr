# fsr 1.0.0.9000

Changes in the development version of the _fsr_ package

# Major changes

- Makes a new structure for the classes ([#26 - pedbrgs](https://github.com/accarniel/fsr/pull/26))
  - Added two new spatial plateau data types: pcollection and pcomposition
  - Redefined the class named pgeometry
  - Defined three spatial plateau data types that inherit from pgeometry: ppoint, pline, and pregion
  - Modified the function `spa_pwkt` to deal with the new classes

# Minor changes

# Bug fixes

# fsr 1.0.0

- This is the first release of _fsr_ that consists of the following modules:
  - Fuzzy Spatial Inference Module (e.g., functions to create a fuzzy spatial inference model and functions to evaluate it)
  - Fuzzy Spatial Data Handling Module (e.g., fuzzy numerical operations, fuzzy geometric set operations, and fuzzy topological predicates)
  - Construction Module (e.g., `spa_creator`)
  - Basic Module (e.g., S4 classes and basic functions)
