
# Introduction to Bayesian Statistics

Denis Cohen  
<denis.cohen@uni-mannheim.de>

*Version: GESIS Training Program, 2025-03-19*

## Abstract

Bayesian methods for inference and prediction have become widespread in
the social sciences (and beyond). Over the last decades, applied
Bayesian modeling has evolved from a niche methodology with high
computational and software-specific entry barriers to a readily
available toolbox that virtually everyone can use by running
pre-implemented packages in standard statistical software on generic
PCs. Although Bayesian methods are now more accessible than ever before,
aspiring Bayesian practitioners may be overwhelmed by questions and
choices - including, but not limited to, when and why to use Bayesian
methods in applied research, how to implement and interpret Bayesian
analyses, or which software to use.

This workshop is designed to help participants take these first steps.
It juxtaposes frequentist and Bayesian approaches to estimation and
inference, highlights the distinct characteristics and advantages of
Bayesian methods, and introduces participants to the Bayesian workflow
and applied modeling using the R package brms - an accessible interface
to the probabilistic programming language Stan, which allows users to
perform Bayesian inference with state-of-the-art algorithms by running
little more than a few lines of code in R.

## Prerequisites

Working knowledge of the software environment `R` as well as working
knowledge of (generalized) linear models is required for participation
in this course.

This workshop requires installations of recent versions of
[`R`](https://cran.r-project.org/mirrors.html),
[`RStudio`](https://rstudio.com/products/rstudio/download/#download), as
well as the packages
[`rstan`](https://cran.r-project.org/web/packages/rstan/index.html),
[`brms`](https://cran.r-project.org/web/packages/brms/index.html) (which
depends on `rstan`), and
[`marginaleffects`](https://cran.r-project.org/web/packages/marginaleffects/index.html).

Setting up `rstan` can be somewhat time-consuming as it requires the
installation of a free-of-charge C++ compiler. Before the workshop,
participants should follow [these
instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
on the Stan Development Team’s GitHub to install and configure the
`rstan` package and its prerequisites on their operating system. If you
do not have administrator privileges on your machine, please approach
your system administrator in advance of the workshop. Should you
encounter problems, feel free to send me an email.

## Schedule

The workshop consists of eights sessions à 90 minutes:

1.  Day 1
    1.  09:00-10:30 (lecture)
    2.  10:45-12:15 (lecture)
    3.  13:45-15:15 (lab)
2.  Day 2
    1.  09:00-10:30 (lecture)
    2.  10:45-12:15 (lecture)
    3.  13:45-15:15 (lab)
3.  Day 3
    1.  09:00-10:30 (lab – data projects)
    2.  10:45-12:15 (concluding discussion)

## Course Structure

| Session | Topics |
|:--:|:---|
| 1.1 | **Frequentist Inference: A Refresher** |
|  | ***Session contents:*** |
|  | \- Introduction |
|  | \- Refresher on probability functions and frequentist inference |
|  | \- Limitations of frequentist inference |
| 1.2 | **Bayesian Fundamentals** |
|  | ***Session contents:*** |
|  | \- Fundamental concepts in Bayesian inference |
|  | \- Analytical Bayes |
|  | \- Numerical Bayes via Markov-Chain Monte Carlo Sampling |
| 1.3 | **Lab session: Exercises and solutions** |
| 2.1 | **Applied Bayesian Statistics I: Basics & Workflow** |
|  | \- Going Bayesian in applied research: When, why, and how? |
|  | \- Software solutions: An overview |
|  | \- The Bayesian workflow |
| 2.2 | **Applied Bayesian Statistics II: Using brms** |
|  | ***Session contents:*** |
|  | \- The brms package: Functionality |
|  | \- Doing Bayesian data analysis with brms: A step-by-step walkthrough |
| 2.3 | **Lab session: Exercises and solutions** |
| 3.1 | **Applied Bayesian Statistics III: Implementing a small data project using brms** |
| 3.2 | **Wrap-up** |
|  | ***Session contents:*** |
|  | \- Review of challenges and potentials encountered in working on your data projects |
|  | \- Outlook: Moving forward as Bayesian practitioners |
|  | \- Course evaluations and feedback |

## Using the workshop materials

### Interactive workshop materials

The workshop materials come as
[`learnr`](https://rstudio.github.io/learnr/) tutorials wrapped in an R
package. To download, install, and use the interactive materials, run
the following code:

``` r
# Detach if loaded
if ("bayesintro" %in% (.packages())) {
  detach(package:bayesintro, unload = TRUE)
}

# Uninstall if installed
if ("bayesintro" %in% installed.packages()) {
  remove.packages("bayesintro")
}

# Install if not installed
if (!("devtools" %in% installed.packages())) {
  install.packages("devtools")
}

# Load from GitHub
library(devtools)
devtools::install_github("denis-cohen/bayesintro")

# Load to library
library(bayesintro)

# Run tutorials (one at a time)
learnr::run_tutorial("00-00-int", package = "bayesintro")
learnr::run_tutorial("01-01-lec", package = "bayesintro")
learnr::run_tutorial("01-02-lec", package = "bayesintro")
learnr::run_tutorial("01-03-lab", package = "bayesintro")
learnr::run_tutorial("02-01-lec", package = "bayesintro")
learnr::run_tutorial("02-02-lec", package = "bayesintro")
learnr::run_tutorial("02-03-lab", package = "bayesintro")
```

### Static workshop materials

- You can find static PDF versions of the lecture slides in the folder
  `lecture-materials`.
- If you prefer working on the **lab exercises** outside of the
  interactive `learnr` environment (i.e., in a regular R session), you
  can use the `Rmd` files supplied in the folder `lab-materials`. These
  contains both exercises and solutions.

## About the Instructor

Denis Cohen is a Senior Research Fellow in the Data and Methods Unit at
the [Mannheim Centre for European Social Research
(MZES)](https://www.mzes.uni-mannheim.de/), [University of
Mannheim](https://www.uni-mannheim.de/). He is also lead organizer of
the [MZES Social Science Data
Lab](https://www.mzes.uni-mannheim.de/socialsciencedatalab/page/events/)
and lead editor of the blog [Methods
Bites](https://www.mzes.uni-mannheim.de/socialsciencedatalab/). A
political scientist by training, his substantive work focuses on the
political economy of spatial inequalities, political preferences and
voting behavior, strategic elite behavior, and political competition in
consolidated multiparty democracies. His methodological interests
include advanced statistical modeling, georeferenced data, data
visualization, and causal inference.
