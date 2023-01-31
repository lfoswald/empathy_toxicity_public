# Effects of Preemptive Empathy Interventions on Reply Toxicity among Highly Active Social Media Users

This repository contains supplementary material related to a preregistered survey experiment that aims to reduce reply toxicity in political online discussions.

## Meta Scripts
* `packages_empathy.R` (installs relevant packages, called in various scripts)

## 1. Data collection and preprocessing 

`1-wrangling-descriptives.R`
* data preprocessing
* descriptive statistics on demographics, online behaviour, empathy, political attitudes, emoji etc.
* itemanalysis and EFA of empathy scale
* toxicity classification using Google's Perspective API
* analysis of manipulation checks

## 2. Hypothesis testing
`2-hypotheses-analysis.R`
* boxplots
* covariate selection using lasso
* hypothesis testing models for toxicity and comment length
* exporting latex output
* hypothesis testing attitude distance and online activity
* language split
* legitimacy split

`2-hypotheses-manipulation.R`
* different variation of models for different expressions of data

## 3. Coefficient Plots
`3-coefficient-plots.R`
* coefficient plots for main models using raw data only

`3-coefficient-plots-manipulation.R`
* coefficient plots for main models using different variations of data

## 4. Robustness Checks 
`4-heterogeneity-robustness.R`
* conducts rebustness checks for linear models 
* explores treatment effect heterogeneity

## 5. Language Comparison
`5-language-comparison.R`
* machine translation of English and German comment samples using DeepL API
* toxicity scores for original and translated comments using Google's Perspective API
* statistical test and plot of language comparison
