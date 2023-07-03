# fsr 2.0.0

## Major changes

- Defined a new hierarchical structure for defining the spatial plateau data types:
  - Added two new spatial plateau data types: `pcollection` and `pcomposition`.
  - Redefined the class named `pgeometry`.
  - Defined three spatial plateau data types that are subclasses of `pgeometry`: `ppoint`, `pline`, and `pregion`.
  - Specified validity methods for all classes according to the rules of the Spatial Plateau Algebra.
  - These modifications are based on the pull requests performed by Pedro V. A. B. de Venâncio, who is added as a contributor of the package `fsr` in the current version ([#26 - @pedbrgs](https://github.com/accarniel/fsr/pull/26) and [#27 - @pedbrgs](https://github.com/accarniel/fsr/pull/27)).
- Added `spa_get_type()` (based on the pull request [#27 - @pedbrgs](https://github.com/accarniel/fsr/pull/27)).
- Updated and improved functions that create `pgeometry` objects: `create_pgeometry()` and `create_empty_pgeometry()` ([#27 - @pedbrgs](https://github.com/accarniel/fsr/pull/27)).
- Improved the creation of components by redesigning `create_component()`. `component_from_sfg()` is deprecated (it will be removed in the next version of the package).
- Enhanced `fsr_plot()` and added the support for `pcollection` and `pcomposition` objects.
- Added two new external functions that handle `pcollection` objects: `spa_flatten()` and `pcollection_to_pcomposition()` (based on the pull request [#31 - @pedbrgs](https://github.com/accarniel/fsr/pull/31)). They are also used by fuzzy geometric set operations on `pcollection` objects.
- Enhanced fuzzy geometric set operations, which can now handle different data types as input (based on the pull request [#31 - @pedbrgs](https://github.com/accarniel/fsr/pull/31)). 
- Deprecated `spa_common_points()` - use `spa_intersection()` to get the common points of two plateau line objects.
- Deprecated `spa_boundary_pregion()` - use `spa_boundary` to get the boundary of a spatial plateau object.
- Enhanced general operations (e.g., `spa_core()`, `spa_boundary()`) to deal with the new spatial plateau data types (based on the pull request [#31 - @pedbrgs](https://github.com/accarniel/fsr/pull/31)). 
- Added three new general operations: `spa_alpha_cut()`, `spa_strict_alpha_cut()`, and `spa_range()`.
- Removed the dependency of packages `FuzzyR` and `utils`.

## Minor changes

- Better organization of the source code by creating more R scripts and rearranging the functions and classes in them.
- Updated `format.pgeometry()` to include a parameter that determines the maximum number of characters that are shown.
- Updated `spa_pwkt()` to deal with the new classes (based on the pull request [#26 - @pedbrgs](https://github.com/accarniel/fsr/pull/26)).
- Updated `spa_is_empty()` to deal with the new classes (based on the pull request [#26 - @pedbrgs](https://github.com/accarniel/fsr/pull/26)).
- Updated `as_tibble()` to deal with the new classes.
- Extended fuzzy numerical operations to deal with the new classes (based on the pull request [#30 - @pedbrgs](https://github.com/accarniel/fsr/pull/30)).
- Updated fuzzy topological relationships due to the changes in other operators (based on the pull request [#31 - @pedbrgs](https://github.com/accarniel/fsr/pull/31)).
- Minor changes in functions of the construction module to use improved operations when manipulating spatial plateau objects.

## Change in the list of authors

- Added Pedro Vinícius Almeida Borges de Venâncio (@pedbrgs) as a contributor to the `fsr`.
- We are grateful for the contributions of Felippe Galdino (@ocfgaldino) and Juliana Strieder Philippsen (@JulianaStrieder) in the first version of the package (1.0.0, 1.0.1, and 1.0.2). Their participation in the package was concluded; thus, they were removed from the list of authors in this version of the package, which includes several changes, improvements, and new methods.

# fsr 1.0.2

## Minor changes

- Added the parameter `d_tolerance` in the `spa_creator()` function. This parameter is employed by the package `sf` when computing the Voronoi diagram and Delaunay triangulation in the corresponding construction policies.

## Bug fixes

- Fixed the fuzzy numerical operations when dealing with empty objects.
- Fixed `spa_core()` to return an empty crisp spatial object when the input has not a core.
- Fixed some if statements and calculations in the fuzzy topological relationships.
- Fixed the clipping operation in `spa_creator()` when using the construction policies based on the Voronoi diagram and Delaunay triangulation.

# fsr 1.0.1

## Major changes

- Added a new construction policy in the `spa_creator()` function based on the convex hull (an improved implementation from the issue [#29](https://github.com/accarniel/fsr/issues/29)).

## Minor changes

- Added the publication that describes the [fsr package](https://dl.acm.org/doi/abs/10.1145/3474717.3484255) into `CITATION`.
- Added an optional parameter in `spa_creator()` named `digits` (an improved update based on the pull request [#28 - @leticiabohnert](https://github.com/accarniel/fsr/pull/28)).

## Bug fixes

- Fixed `spa_add_component()` for checking a specific condition.
- Fixed the management of CRS when plotting spatial plateau objects (`fsr_plot()`).
- Fixed the management of geometry collections resulting from some geometric computations of the spatial plateau geometric set operations.
  
# fsr 1.0.0

- This is the first release of _fsr_ that consists of the following modules:
  - Fuzzy Spatial Inference Module (e.g., functions to create a fuzzy spatial inference model and functions to evaluate it).
  - Fuzzy Spatial Data Handling Module (e.g., fuzzy numerical operations, fuzzy geometric set operations, and fuzzy topological predicates).
  - Construction Module (e.g., `spa_creator()`).
  - Basic Module (e.g., S4 classes and basic functions).