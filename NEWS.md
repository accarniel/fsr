# fsr 1.0.2.9000 (development version)

## Major changes

- Defined a new hierarchical structure for defining the spatial plateau data types:
  - Added two new spatial plateau data types: `pcollection` and `pcomposition`.
  - Redefined the class named `pgeometry`.
  - Defined three spatial plateau data types that are subclasses of `pgeometry`: `ppoint`, `pline`, and `pregion`.
  - Specified validity methods for all classes according to the rules of the Spatial Plateau Algebra
  - These modifications are based on the pull requests performed by Pedro V. A. B. de Venâncio, who will be added as a contributor of the package `fsr` ([#26 - @pedbrgs](https://github.com/accarniel/fsr/pull/26) and [#27 - @pedbrgs](https://github.com/accarniel/fsr/pull/27))

## Minor changes

- Better organization of the source code by creating more R scripts and rearranging the functions and classes in them.

## Change in the list of authors

- Added Pedro Vinícius Almeida Borges de Venâncio (@pedbrgs) as a contributor to the `fsr`.
- We are grateful for the contributions of Felippe Galdino (@ocfgaldino) and Juliana Strieder Philippsen (@JulianaStrieder) in the first version of the package (1.0.0, 1.0.1, and 1.0.2). Their participation in the package was concluded; thus, they were removed from the list of authors in the next version of the package, which will include several changes, improvements, and new methods.

# fsr 1.0.2

## Minor changes

- Added the parameter `d_tolerance` in the function `spa_creator`. This parameter is employed by the package `sf` when computing the Voronoi diagram and Delaunay triangulation in the corresponding construction policies.

## Bug fixes

- Fixed the fuzzy numerical operations when dealing with empty objects.
- Fixed the `spa_core` to return an empty crisp spatial object when the input has not a core.
- Fixed some if statements and calculations in the fuzzy topological relationships.
- Fixed the clipping operation in the `spa_creator` when using the construction policies based on the Voronoi diagram and Delaunay triangulation.

# fsr 1.0.1

## Major changes

- Added a new construction policy in the `spa_creator` function based on the convex hull (an improved implementation from the issue [#29](https://github.com/accarniel/fsr/issues/29)).

## Minor changes

- Added the publication that describes the [fsr package](https://dl.acm.org/doi/abs/10.1145/3474717.3484255) into `CITATION`.
- Added an optional parameter in the function `spa_creator` named `digits` (an improved update based on the pull request [#2, @leticiabohnert](https://github.com/accarniel/fsr/pull/28)).

## Bug fixes

- Fixed `spa_add_component` for checking a specific condition.
- Fixed the management of CRS when plotting spatial plateau objects (function `fsr_plot`).
- Fixed the management of geometry collections resulting from some geometric computations of the spatial plateau geometric set operations (internal function `append_valid_comps`).
  
# fsr 1.0.0

- This is the first release of _fsr_ that consists of the following modules:
  - Fuzzy Spatial Inference Module (e.g., functions to create a fuzzy spatial inference model and functions to evaluate it).
  - Fuzzy Spatial Data Handling Module (e.g., fuzzy numerical operations, fuzzy geometric set operations, and fuzzy topological predicates).
  - Construction Module (e.g., `spa_creator`).
  - Basic Module (e.g., S4 classes and basic functions).