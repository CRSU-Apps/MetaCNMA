# MetaCNMA
MetaCNMA is an interactive application written in R using the RShiny package.

Currently the application is in alpha development so only core functionality is present.

This app uses the R package  [`netmeta`]() for Frequentist analysis and STAN models derived from [Welton, N.J et al.](https://doi.org/10.1093/aje/kwp014) for the Bayesian analysis from the [`MetaCNMABayes`](https://github.com/CRSU-Apps/MetaCNMABayes) R package. 

**N.B** Both these packages implement component network meta analysis differently, so you should expect different results from the Frequentist and Bayesian analysis, more details can been found in the Wiggle, A. and Béliveau, A. [paper](https://doi.org/10.1002/sim.9520).

To use this app:
First upload data using the Upload Data tab, you can then view a summary of the components under the Component Summary tabs.

Next conduct an analyis using the Frequentist Analyis and Bayesian Analysis tabs. You can modify the outcome and model settings in the Outcome & Model settings tab. For the bayesian analysis you can also modify the STAN sampler options using the settings button on the top right of the `Forest Plot`, or `Model Diagnostics` tabs, changing these settings will require you to rerun the model.

Additionally, select studies you may wish to excude for a sensitivety analysis.

**N.B.** The Frequentist analysis will run automatically when you view the forest plot, however the Bayesian analysis will require you to press the `Run Model` button.

Finally you can download any of the plots using the save button on the top right of the plots, these can be save as `pdf`, `svg` and `png` in any size.