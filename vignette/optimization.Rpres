Metody optymalizacji w analizie danych
========================================================
author: Artur Gajowniczek, Sebastian Sowik
date: 31.01.2020
autosize: true
font-family: 'Risque'


# Binary classification with L2 regularization nad and L1 cost function using coordinate descent method.

SVM
========================================================
<div align="center">
<img src="./svm_margin.png" width=800 height=800>
</div>


SVM optimization
========================================================

## Primal form
### $$\min_{w} \frac{1}{2} w^{T}w + C \sum_{i=1}^{l} \max(1 - y_i w^T x_i, 0)  $$
## Dual form
### $$\min_{\vec{\alpha}} f(\vec{\alpha}) = \frac{1}{2} \vec{\alpha}^{T}Q\vec{\alpha} - \vec{e}^T \vec{\alpha}  $$
#### $$ \text{subject to } 0 \le \alpha_i \le C, \; \forall i$$
### $$ Q_{ij} = y_iy_j\vec{x_i}^T \vec{x_j}  $$

Coordinate descent
========================================================
<div align="center">
<img src="./coordinate_descent.jpg" width=800 height=800>
</div>


A dual coordinate descent method for Linear SVM
========================================================

- Given $\vec{\alpha}$ and the corresponding $w = \sum_i y_i \alpha_i x_i$

- While $\vec{\alpha}$ is not optimal

  For i - 1,...,l

  a) $G = y_i \vec{w}^T \vec{x}_i - 1 + D_{ii} \alpha_i$

  b)
$$
PG =
\begin{cases}
min(G, 0), & \text{if } \alpha_i = 0\\
max(G, 0), & \text{if } \alpha_i = C\\
G, & \text{if } 0 \lt \alpha_i \lt C\\
\end{cases}
$$

  c)
$$
  \text{If }  |PG| \neq 0, \\
  \text{   } \alpha_{prev} \gets \alpha_{i} \\
  \text{   } \alpha_{i} \gets min(max(\alpha_i - G/Q_{ii},0),C) \\
  \text{   } \vec{w} \gets \vec{w} + (\alpha_i - \alpha_{prev})y_i\vec{x}_i \\
$$


Implementation details
========================================================




### Random Permutation of Sub-problems

Shufling subproblems before next outer iteration can give faster convergence  (Chang et al., 2008)

### Shrinking

Constraint

$0 \lt \alpha_i \lt C$

results in some subproblems being stuck in value of 0 and C for many iterations.

Shrinking technic (Joachims, 1998) reduces the size of the optimization problem without considering some bounded
variables

R package implementation
========================================================

### We implemented R package with solver for linear l1-svm
### using dual coordinate descent method

### https://github.com/sowiks2711/linear-l1svm

Benchmarks and comparison
========================================================
<div align="center">
<img src="./few_features_big_size.png" width=1400 height=1000>
</div>


========================================================
<div align="center">
<img src="./many_features_without_shuffle.png" width=1400 height=1000>
</div>

========================================================
<div align="center">
<img src="./many_features_shuffled.png" width=1400 height=1000>
</div>

========================================================
<div align="center">
<img src="./heatmap_not_shuffled.png" width=1400 height=1000>
</div>

========================================================
<div align="center">
<img src="./heatmap_with_shuffle.png" width=1400 height=1000>
</div>

Thank you!
========================================================

## Sources:

https://www.csie.ntu.edu.tw/~cjlin/liblinear/

https://www.csie.ntu.edu.tw/~cjlin/papers/cddual.pdf

http://cs229.stanford.edu/notes/cs229-notes3.pdf

**An Introduction to Statistical Learning with Applications in R**

Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani
