# Data Shapley for the samples misannotation assessment

This repository contains the set of R-scripts which was used to calculate Shapley values (see [A Value for N-Person Games][shapley], [Data shapley: Equitable valuation of data for machine learning][Ghorbani2019] and [Shapley Value as a Quality Control for Mass Spectra of Human Glioblastoma Tissues][Zavorotnyuk2023]) for the ambient ionization mass spectrometry data.

These scripts are intended to be run on a high performance PC with 64 GB of RAM available and at least 10 GB of free disk space.

To start the Shapley value calculation process, the dataset containing the mass spectrometry data in the form of a feature matrix must first be prepared. The Shapley value calculation process usually takes more than five days to complete.

[shapley]: https://doi.org/10.7249/P0295 "A Value for N-Person Games"
[Ghorbani2019]: https://doi.org/10.48550/arXiv.1904.02868 "Data shapley: Equitable valuation of data for machine learning"
[Zavorotnyuk2023]: https://doi.org/10.3390/data8010021 "Shapley Value as a Quality Control for Mass Spectra of Human Glioblastoma Tissues"
