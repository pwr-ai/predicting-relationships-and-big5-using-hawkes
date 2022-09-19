Predicting Relationship Labels and Individual Personality Traits from Telecommunication History in Social Networks using Hawkes Processes
================

This repository contains code and data samples accompanying the manuscript
“Hawkes-modeled telecommunication patterns reveal relationship dynamics and personality traits” [\[Nurek et
al, '20\]](https://arxiv.org/abs/2009.02032).

# Repository content:

This repository contains the following dataset samples:

**- NetSense** (the full dataset is available upon request from Prof. Omar Lizardo)
  - `data/netsense/telcodata/telcodata-X.csv` (where X is user ID) – contains the sample of communication history metadata from the NetSense study.
  - `data/netsense/dfRelationshipsTypes.csv`- contains dates of survey completion, relationship types with peers, change point of relationship.

**- NetHealth** (the full dataset is publicly available for research purposes: https://sites.nd.edu/nethealth/)
  - `data/nethealth/telcodata/telcodata-X.csv` (where X is user ID) – contains the sample of communication history metadata from the NetHealth study.
  - `data/nethealth/hawkes-networksurvey.csv`- contains dates of survey completion, relationship types with peers, change point of relationship.

# Code scripts:
To get the results, run the scripts in the following order:
  1. `scripts/fittingHawkes.R` – R script that fits Hawkes process parameters to telcodata. See Section III.D and IV in [\[Nurek et
al, '20\]](https://arxiv.org/abs/2009.02032) for more details.
  2. `scripts/changePointDetection.R` – R scripts to
    detect the change point in relationships. See Section V in [\[Nurek et
al, '20\]](https://arxiv.org/abs/2009.02032) for more details.
  3. `scripts/hawkesEmbeddings.R` – R script that generates user embeddings based on fitted Hawkes parameters. See Section III.D and VI in [\[Nurek et
al, '20\]](https://arxiv.org/abs/2009.02032) for more details.

Remember to set the `upperBound` variable in the scripts according to the memory you have. Fitting processes with a very large number of events will run out of memory.

The following helper scripts are also available: 
  - `scripts/util/evently_feature.R` - R script contains older versions of the functions needed to generate user representations, not available in the latest version of [evently](https://github.com/behavioral-ds/evently) package.
  - `scripts/util/io-util.R` – additional functions for reading and writing data.

# Results:
  - `results/XFitsHoldoutY` (where X is netsense or nethealth, Y is EXP or PL kernel) - folder contains files for each user with the calculated negative holdout log-likelihood per event for the exponential and power-law kernel.
  - `results/XFitsHoldoutY` (where X is netsense or nethealth, Y is EXP or PL kernel) - folder contains files for each user with fitted Hawkes process parameters for the power-law kernel.  The parameters were then used to classify the type of relationship.
  - `results/XChangePointDetection.csv` (where X is netsense or nethealth) - file contains calculated log-likelihood and Hawkes process parameters before and after changing the type of relationship between users.
  - `results/hawkesEmbedding-X.csv` (where X is netsense or nethealth) - generated user embeddings used to predict the Big5 personality traits.

See paper for more details.

# Reference:

- Nurek, M., Michalski, R., Lizardo, O., & Rizoiu, M.-A. Hawkes-modeled telecommunication patterns reveal relationship dynamics and personality traits, 2020.
- Striegel, A., Liu, S., Meng, L., Poellabauer, C., Hachen, D., & Lizardo, O."Lessons learned from the netsense smartphone study." ACM SIGCOMM Computer Communication Review 43.4 (2013): 51-56.
- Liu, S., Hachen, D., Lizardo, O., Poellabauer, C., Striegel, A., & Milenković, T. (2018). Network analysis of the NetHealth data: exploring co-evolution of individuals’ social network positions and physical activities. Applied network science, 3(1), 1-26.
- Kong, Q., Ram, R., & Rizoiu, M. A. (2021, March). Evently: Modeling and analyzing reshare cascades with hawkes processes. In Proceedings of the 14th ACM International Conference on Web Search and Data Mining (pp. 1097-1100).

# License

Both dataset samples and code are distributed under the General Public License
v3 (GPLv3) license, a copy of which is included in this repository, in
the LICENSE file. If you require a different license and for other
questions, please contact us at <mateusz.nurek@pwr.edu.pl>
