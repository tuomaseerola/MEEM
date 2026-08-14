# MEEM

This repository offers data and analyses related to the study titled "Measure of Emotional Episodes with Music (MEEM): development and psychometric evaluation of a modular instrument". 

MEEM is scale developed to measure a emotional episodes related to music, initially outlined in a theory ([Eerola, Kirts & Saarikallio, 2025](https://doi.org/10.1177/03057356241279763)), which has been operationalised and scrutinised through the content validity process ([Kirts et al., in press](https://doi.org/10.1177/20592043251413550)).

These are the all analytical operations carried out in the study in Experiments 1 and 2.

Note. To view these conveniently, go to the rendered site: [https://tuomaseerola.github.io/MEEM/](https://tuomaseerola.github.io/MEEM/)

## Experiment 1

1. [preprocessing](exp1/preprocessing.qmd) gets all the data, reports reliability, displays an initial EFA (Appendix 2 of the manuscript), and visualises the item ratings.

2. [renaming_sub-constructs](exp1/renaming_sub-constructs.qmd) explains how some of the sub-constructs were relabelled to better reflect the items chosen in the process.

3. [CFA_direct](exp1/CFA_direct.qmd) contains full items analysis and 4 and 3 best item analyses.

4. [measurement_invariance](exp1/measurement_invariance.qmd) reports configural, metric, and scalar invariance for the CFA participant-clustered robust SE models.


## Experiment 2

1. [describe_data](exp2/describe_data.qmd) summarises participant-level data (removal of low consistency participants, display demographics, visualise mean item ratings across vignettes). &check;

2. [CFA](exp2/CFA.qmd) reports CFA model fits, measurement invariance, and discriminant fit indices for each construct. Also displays the model summaries (loadings) together with means and standard deviations of the items. &check;

3. [higher_order_structure](exp2/higher_order_structure.qmd) tests higher order structures. Also reports construct reliability measures and discriminant validity tests for the models. &check;

4. [vignette_to_emotions](exp2/vignette_to_emotions.qmd) contains summary and construct validity with GEMIAC and HAAS scales. &check;
