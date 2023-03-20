# covild_metabolic
Effects of lipid turnover and glycemia on pulmonary COVID-19 recovery. The R analysis pipeline for the publication of [Sonnweber T et al.](https://www.nature.com/articles/s41598-023-29654-1) (DOI: 10.1038/s41598-023-29654-1).


## Terms of use

To reference or use the analysis results in your publication, please cite our GitHub repository and our paper. The raw data are available upon request to the authors [Dr. Thomas Sonnweber](mailto:Thomas.Sonnweber@i-med.ac.at) or [Dr. Ivan Tancevski](mailto:Ivan.Tancevski@i-med.ac.at).

## Usage

Source `exec_all.R` to launch the complete pipeline:

```r

source('exec_all.R')

```
Required development packages are available from my GitHub: 

```r
devtools::install_github('PiotrTymoszuk/soucer')
devtools::install_github('PiotrTymoszuk/trafo')
devtools::install_github('PiotrTymoszuk/ExDA')
devtools::install_github('PiotrTymoszuk/lmqc')
devtools::install_github('PiotrTymoszuk/figur')
devtools::install_github('PiotrTymoszuk/caretExtra')


```

## Contact

The repository is curated by [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Dr. Thomas Sonnweber](mailto:Thomas.Sonnweber@i-med.ac.at) or [Dr. Ivan Tancevski](mailto:Ivan.Tancevski@i-med.ac.at).
