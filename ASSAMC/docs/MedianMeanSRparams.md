Ricker Model SR Parameters Conversion
========================================================
author: Bai Li
date: 08/22/2020
autosize: true

Ricker Model (Without Bias Adjustment)
========================================================

**BAM and SS**

- $R_{y+1}=\frac{S_y}{\phi_0}exp(h(1-\frac{S_y}{R_0\phi_0}))$

**AMAK**
- $R_{y+1}=\frac{S_y}{\phi_0}exp(h'(1-\frac{S_y}{R_0\phi_0}))$

- $h'=\frac{exp(h)}{4+exp(h)}$

***
**BAM and SS h VS. AMAK h'**

![plot of chunk unnamed-chunk-1](MedianMeanSRparams-figure/unnamed-chunk-1-1.png)

Ricker Model (With Bias Adjustment)
========================================================
**BAM**

E1: $R_{y+1}=\frac{S_y}{\phi_0}exp(h(1-\frac{S_y}{R_0\phi_0}))$

E2: $R_{eq}=\frac{R_0}{\phi_F/\phi_0}(1+\frac{log(BC_{BAM}\times \phi_F/\phi_0)}{h})$

- Virgin recruitment $R_{0}$ and steepness $h$ are median values
- $BC_{BAM}=exp(\sigma_R^2/2)$

***
**SS**

E3: $R_{y+1}=\frac{S_y}{\phi_0}exp(h(1-\frac{S_y}{R_0\phi_0}))\times BC_{SS}$

E4: $R_{eq}=\frac{R_0}{\phi_F/\phi_0}(1+\frac{log(\phi_F/\phi_0)}{h})$

- Virgin recruitment $R_{0}$ and steepness $h$ are mean values
- $BC_{SS}=exp(-\sigma_R^2/2)$

Ricker Model (With Bias Adjustment)
========================================================
Output

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Bullet 1
- Bullet 2
- Bullet 3

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-3](MedianMeanSRparams-figure/unnamed-chunk-3-1.png)
