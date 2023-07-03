# *News*

# FuzzyR 2.3.2

## New features

* Add non-singleton features described in the FuzzIEEE paper 'An Extension of the FuzzyR Toolbox for Non-Singleton Fuzzy Logic Systems'. This is a collaborative work with Yu Zhao. 
* Refactoring code for function showGUI; Add advanced GUI features. Many thanks to the contribution from science@sboldt.com.

# FuzzyR 2.3.1

<!-- ## New features -->


## Bug fixes

* Address the issue of `writefis`, `readfis` and `showfis`.
* Address the issue when one or more rules do not use the first input variable.


# FuzzyR 2.3

## New features

* Add implementation of TSK type fuzzy inference systems based on [An extended ANFIS architecture](https://doi.org/10.1109/FUZZ-IEEE.2016.7737742).
* Add new function `tipper.tsk`.

## Bug fixes

* Address the issue of `evalfis` for interval type-2 fis: it now gives a single (rather than two) crisp defuzzified output.


# FuzzyR 2.2

## New features

* Add `err.trn.fix` flag to function `anfis.optimise`.
* Add height option to membership functions: `gbellmf`, `gbell.fuzzification`, `it2gbellmf`, `singletonmf`, `gaussmf`, `it2gaussmf`, `trapmf`, `it2trapmf`, `trimf`, `it2trimf`, `trimf.fuzzification`.
* New functions: `gauss.fuzzification`, `it2gbell.fuzzification`.
* Change functions `addfis`, `addmf`, `evalfis` to include interval type-2 features (Mamdani).

## Bug fixes

* Change function `evalfis` to address the issue of a fis with multiple outputs.
* Address the issue of initialisation when `a == 0` for funtions `init.params.gbell` and `init.params.it2gbell`.

## Other changes

* Export function: `writefis`.
* Add DOI info for function `km.da`.
* Add new package dependency: `grid`.
* Add reference: [A Comment on “A Direct Approach for Determining the Switch Points in the Karnik–Mendel Algorithm”](https://doi.org/10.1109/TFUZZ.2018.2865134).
* Add reference: [Type-1 and Interval Type-2 ANFIS: A Comparison](https://doi.org/10.1109/FUZZ-IEEE.2017.8015555). 


# FuzzyR 2.1

## New features

* Add Interval Type-2 ANFIS.
* Add accuracy measure UMBRAE: [A new accuracy measure based on bounded relative error for time series forecasting](http://dx.doi.org/10.1371/journal.pone.0174202). 
* Add functions: `fuzzyr.match.fun`, `fuzzyr.accuracy`, `km.da`, `it2gbellmf`, `it2gbell.fuzzification`.

## Bug fixes

* Customise function `match.fun` to address the issue that functions cannot be matched due to environmental settings.
* Change function `gensurf` to address a color issue.
* Change function `defuzz` to address an issue when all membership grades are equal to zero.
* Change function `anfis.dMF.dP.gbellmf` to address the issue of infinite outputs due to large factors.
* Change function `plotmf` to address an issue of customised label.

## Other changes

* Hide functions: `anfis.addnode`, `anfis.optimise.lse`.
* Minor changes to functions: `anfis.optimise`, `anfis.optimise.gradient`, `anfis.optimise.lse`, `anfis.dO3.dO2`, `anfis.plotmf`, `plotvar`, `plot_graph`.
* Change function `anfis.L2.which` to use the [direct approach for determining the switch points](https://doi.org/10.1109/TFUZZ.2017.2699168).
* Add reference: [An extended ANFIS architecture and its learning properties for type-1 and interval type-2 models](https://doi.org/10.1109/FUZZ-IEEE.2016.7737742)


