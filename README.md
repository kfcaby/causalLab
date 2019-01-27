
Causal Inference in Introductory Statistics Courses
===================================================

The following activity introduces students to causal inference methods in first and second undergraduate courses in statistics. The data for this activity comes from Kahn 2005. causalLab.pdf is a printable student lab associated with this activity. causalLabGuide.pdf is an instructor guide with additional information and suggestions for instructors using the lab in class.

Causal Diagram
--------------

Figure 1 is a causal diagram describing the variables in this study. An arrow from one variable to another indicates the first variable causes the second. These relationships are specified based on expert knowledge prior to collecting data. Here is a brief justification for the arrows originating from variables in this causal diagram.

-   Age. Older teenagers are more likely to smoke (more peers that smoke, greater freedom from parents, etc.), have higher lung capacity, and be taller.

-   Sex. Men are more likely to smoke than women, be taller, and have higher FEV (even when they are the same height as women).

-   Smoking. Childhood smoking can inhibits growth.

-   Height. Taller people have higher lung capacity.

![Causal diagram depicted relationships between variables in this activity.](./figures/fullDAG.png)

Data Analysis
-------------

``` r
library(tidyverse)
```

Statistical Models
------------------

### Crude Association

### Adjusted Analysis

References:

-   Kahn, Michael. "An exhalent problem for teaching statistics." Journal of Statistics Education 13.2 (2005).
