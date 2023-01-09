## LAMRSAControl - Package for modelling the spread and control measures against LA-MRSA in a pig herd

This repository contains the code for a stochastic compartment model
of a farrow-to-finish pig herd and examples of using the model to
simulate the spread of livestock-associated methicillin-resistant
*Staphylococcus aureus* (LA-MRSA) in the herd. The package also
includes scripts to example control measures against disease spread as
well as pre-generated animal movement events.

This model package was used as the basis for modelling various control
measures against LA-MRSA that are presented in the manuscript
"Assessment of control measures against livestock-associated
methicillin-resistant *Staphylococcus aureus* in pig herds using
infectious disease modelling" by Tuominen et al. The model was written
using R language (version 4.2.0).

### Dependencies

Running the model requires installing the SimInf package. The model
simulations were run using the SimInf version 9.0.0. The latest
released version can be installed from
[CRAN](https://cran.r-project.org/web/packages/SimInf/index.html) by
`install.packages("SimInf")`

### Installation

The package can be installed directly from Github by using the
`remotes` package:

```
library(remotes)
install_github("KSTuominen/LAMRSAControl")
```

### Learn more
See the vignette for more examples of running the model. To read the
vignette do the following:

``` R
library('LAMRSAControl')
vignette('LAMRSAControl')
```

### Authors
In alphabetical order:
[![orcid](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-6576-9668)
Thomas Rosendal,
[![orcid](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-2223-9376)
Krista Tuominen (Maintainer) and
[![orcid](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-5745-2284)
Stefan Widgren.


### License
The `LAMRSAControl` package is licensed under the
[GPLv3](https://www.gnu.org/licenses/gpl-3.0.html).
