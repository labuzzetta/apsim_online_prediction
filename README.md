# Online Prediction for APSIM Next Generation
This repository is my Special Project for AGRON 525: Crop and Soil Modeling

My project is an investigation in online prediction methods for APSIM Next Generation data. The Maize Module was the focus for this project, specifically modeling the `Maize.AboveGround.Wt` variable. However, the methods presented in the online_emulate_maize() function of this package are directly applicable to other Maize module variables as well. Simple linear and autoregressive models were used as a baseline for online prediction methods explored in this implmentation before including more complex methods including generalized additive models, random forests, and neural networks. These implementations are an informal exploration of online prediction methods for APSIM data which are being tested in order to provide a first approximation of the feasibility of full APSIMx emulation, a certainly more complex task. This repository serves as a transparent and reproducible source for this project. The methodology for these methods can be found in the writeup for this project. The presentation and writeup are published at the following links:

[Presentation](http://rpubs.com/clabuzze/apsimo)

[Writeup](labuzzetta_525_project.pdf)

This package may be installed via:

```{r}
library(devtools)
install_github("labuzzetta/apsim_online_prediction")
```
