My Title
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# psvar

<!-- badges: start -->

<!-- badges: end -->

# Methodology

Let ![Y\_t](https://latex.codecogs.com/png.latex?Y_t "Y_t") be an n×1
vector of observables. We assume that the dynamics of the observables
are described by a system of linear simultaneous equations

  
![&#10; A Y \_ { t } = \\sum \_ { j = 1 } ^ { p } \\alpha \_ { j } Y \_
{ t - j } + \\varepsilon \_ { t }
&#10;](https://latex.codecogs.com/png.latex?%0A%20A%20Y%20_%20%7B%20t%20%7D%20%3D%20%5Csum%20_%20%7B%20j%20%3D%201%20%7D%20%5E%20%7B%20p%20%7D%20%5Calpha%20_%20%7B%20j%20%7D%20Y%20_%20%7B%20t%20-%20j%20%7D%20%2B%20%5Cvarepsilon%20_%20%7B%20t%20%7D%20%0A
"
 A Y _ { t } = \\sum _ { j = 1 } ^ { p } \\alpha _ { j } Y _ { t - j } + \\varepsilon _ { t } 
")  

  
![&#10; Y \_ { t } = \\sum \_ { j = 1 } ^ { p } \\delta \_ { j } Y \_ {
t - j } + B \\varepsilon \_ { t }
&#10;](https://latex.codecogs.com/png.latex?%0A%20Y%20_%20%7B%20t%20%7D%20%3D%20%5Csum%20_%20%7B%20j%20%3D%201%20%7D%20%5E%20%7B%20p%20%7D%20%5Cdelta%20_%20%7B%20j%20%7D%20Y%20_%20%7B%20t%20-%20j%20%7D%20%2B%20B%20%5Cvarepsilon%20_%20%7B%20t%20%7D%20%0A
"
 Y _ { t } = \\sum _ { j = 1 } ^ { p } \\delta _ { j } Y _ { t - j } + B \\varepsilon _ { t } 
")  

  
![ u\_t = B \\epsilon\_t
](https://latex.codecogs.com/png.latex?%20u_t%20%3D%20%20B%20%5Cepsilon_t%20
" u_t =  B \\epsilon_t ")  

  
![&#10; E ( u \_ { t } u \_ { t } ^ { \\prime } ) = B B ^ { \\prime } =
\\Sigma \_ { u }
&#10;](https://latex.codecogs.com/png.latex?%0A%20E%20%28%20u%20_%20%7B%20t%20%7D%20u%20_%20%7B%20t%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%29%20%3D%20B%20B%20%5E%20%7B%20%5Cprime%20%7D%20%3D%20%5CSigma%20_%20%7B%20u%20%7D%20%0A
"
 E ( u _ { t } u _ { t } ^ { \\prime } ) = B B ^ { \\prime } = \\Sigma _ { u } 
")  

  
![ &#10;\\left\[ &#10; \\begin{array} &#10; { c } { u \_ { 1 , t } }
\\\\ { ( k \\times 1 ) } \\\\ { u \_ { 2 , t } } \\\\ { ( N - k
\\times 1 ) } &#10; \\end{array} \\right\] = &#10; \\left\[ &#10;
\\begin{array} { c c } { \\beta\_{ 11 } } & { \\beta\_{ 12 } } \\\\ { (
k \\times k ) } & { ( k \\times N - k ) } \\\\ { \\beta \_ { 21 } } & {
\\beta \_ { 22 } } \\\\ { ( N - k \\times k ) } & { ( N - k \\times N -
k ) } &#10; \\end{array} \\right\] \\left\[ &#10; \\begin{array}{c}
&#10; { \\epsilon \_ { 1 , t } } \\\\ { ( k \\times 1 ) } \\\\ {
\\epsilon \_ { 2 , t } } \\\\ { ( N - k \\times 1 ) } &#10; \\end{array}
&#10; \\right\]
&#10;](https://latex.codecogs.com/png.latex?%20%0A%5Cleft%5B%20%0A%20%20%5Cbegin%7Barray%7D%20%0A%20%20%20%20%7B%20c%20%7D%20%7B%20u%20_%20%7B%201%20%2C%20t%20%7D%20%7D%20%5C%5C%20%7B%20%28%20k%20%5Ctimes%201%20%29%20%7D%20%5C%5C%20%7B%20u%20_%20%7B%202%20%2C%20t%20%7D%20%7D%20%5C%5C%20%7B%20%28%20N%20-%20k%20%5Ctimes%201%20%29%20%7D%20%0A%20%20%5Cend%7Barray%7D%20%5Cright%5D%20%3D%20%0A%20%5Cleft%5B%20%0A%20%20%5Cbegin%7Barray%7D%20%7B%20c%20c%20%7D%20%7B%20%5Cbeta_%7B%2011%20%7D%20%7D%20%26%20%7B%20%5Cbeta_%7B%2012%20%7D%20%7D%20%5C%5C%20%7B%20%28%20k%20%5Ctimes%20k%20%29%20%7D%20%26%20%7B%20%28%20k%20%5Ctimes%20N%20-%20k%20%29%20%7D%20%5C%5C%20%7B%20%5Cbeta%20_%20%7B%2021%20%7D%20%7D%20%26%20%7B%20%5Cbeta%20_%20%7B%2022%20%7D%20%7D%20%5C%5C%20%7B%20%28%20N%20-%20k%20%5Ctimes%20k%20%29%20%7D%20%26%20%7B%20%28%20N%20-%20k%20%5Ctimes%20N%20-%20k%20%29%20%7D%20%0A%20%5Cend%7Barray%7D%20%5Cright%5D%20%5Cleft%5B%20%0A%20%20%20%20%5Cbegin%7Barray%7D%7Bc%7D%20%0A%20%20%20%20%7B%20%5Cepsilon%20_%20%7B%201%20%2C%20t%20%7D%20%7D%20%5C%5C%20%7B%20%28%20k%20%5Ctimes%201%20%29%20%7D%20%5C%5C%20%7B%20%5Cepsilon%20_%20%7B%202%20%2C%20t%20%7D%20%7D%20%5C%5C%20%7B%20%28%20N%20-%20k%20%5Ctimes%201%20%29%20%7D%20%0A%20%20%20%20%5Cend%7Barray%7D%20%0A%20%20%5Cright%5D%20%0A
" 
\\left[ 
  \\begin{array} 
    { c } { u _ { 1 , t } } \\\\ { ( k \\times 1 ) } \\\\ { u _ { 2 , t } } \\\\ { ( N - k \\times 1 ) } 
  \\end{array} \\right] = 
 \\left[ 
  \\begin{array} { c c } { \\beta_{ 11 } } & { \\beta_{ 12 } } \\\\ { ( k \\times k ) } & { ( k \\times N - k ) } \\\\ { \\beta _ { 21 } } & { \\beta _ { 22 } } \\\\ { ( N - k \\times k ) } & { ( N - k \\times N - k ) } 
 \\end{array} \\right] \\left[ 
    \\begin{array}{c} 
    { \\epsilon _ { 1 , t } } \\\\ { ( k \\times 1 ) } \\\\ { \\epsilon _ { 2 , t } } \\\\ { ( N - k \\times 1 ) } 
    \\end{array} 
  \\right] 
")  

Consider the following partitioning of B:

  
![&#10; B = \\left\[ \\begin{array} { c c } { \\beta \_ { 1 } } & {
\\beta \_ { 2 } } \\\\ { n \\times k } & { n \\times ( n - k ) }
\\end{array} \\right\], \\quad \\beta \_ { 1 } = \\left\[ \\begin{array}
{ c c } { \\beta \_ { 11 } ^ { \\prime } } & { \\beta \_ { 21 } ^ {
\\prime } } \\\\ { k \\times k } & { k \\times ( n - k ) } \\end{array}
\\right\] ^ { \\prime } , \\quad \\beta \_ { 2 } = \\left\[
\\begin{array} { c c } { \\beta \_ { 12 } ^ { \\prime } } & { \\beta \_
{ 22 } ^ { \\prime } } \\\\ { ( n - k ) \\times k } & { ( n - k )
\\times ( n - k ) } \\end{array} \\right\] ^ { \\prime }
&#10;](https://latex.codecogs.com/png.latex?%0A%20B%20%3D%20%5Cleft%5B%20%5Cbegin%7Barray%7D%20%7B%20c%20c%20%7D%20%7B%20%5Cbeta%20_%20%7B%201%20%7D%20%7D%20%26%20%7B%20%5Cbeta%20_%20%7B%202%20%7D%20%7D%20%5C%5C%20%7B%20n%20%5Ctimes%20k%20%7D%20%26%20%7B%20n%20%5Ctimes%20%28%20n%20-%20k%20%29%20%7D%20%5Cend%7Barray%7D%20%5Cright%5D%2C%20%5Cquad%20%5Cbeta%20_%20%7B%201%20%7D%20%3D%20%5Cleft%5B%20%5Cbegin%7Barray%7D%20%7B%20c%20c%20%7D%20%7B%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%26%20%7B%20%5Cbeta%20_%20%7B%2021%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%5C%5C%20%7B%20k%20%5Ctimes%20k%20%7D%20%26%20%7B%20k%20%5Ctimes%20%28%20n%20-%20k%20%29%20%7D%20%5Cend%7Barray%7D%20%5Cright%5D%20%5E%20%7B%20%5Cprime%20%7D%20%2C%20%5Cquad%20%5Cbeta%20_%20%7B%202%20%7D%20%3D%20%5Cleft%5B%20%5Cbegin%7Barray%7D%20%7B%20c%20c%20%7D%20%7B%20%5Cbeta%20_%20%7B%2012%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%26%20%7B%20%5Cbeta%20_%20%7B%2022%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%5C%5C%20%7B%20%28%20n%20-%20k%20%29%20%5Ctimes%20k%20%7D%20%26%20%7B%20%28%20n%20-%20k%20%29%20%5Ctimes%20%28%20n%20-%20k%20%29%20%7D%20%5Cend%7Barray%7D%20%5Cright%5D%20%5E%20%7B%20%5Cprime%20%7D%20%0A
"
 B = \\left[ \\begin{array} { c c } { \\beta _ { 1 } } & { \\beta _ { 2 } } \\\\ { n \\times k } & { n \\times ( n - k ) } \\end{array} \\right], \\quad \\beta _ { 1 } = \\left[ \\begin{array} { c c } { \\beta _ { 11 } ^ { \\prime } } & { \\beta _ { 21 } ^ { \\prime } } \\\\ { k \\times k } & { k \\times ( n - k ) } \\end{array} \\right] ^ { \\prime } , \\quad \\beta _ { 2 } = \\left[ \\begin{array} { c c } { \\beta _ { 12 } ^ { \\prime } } & { \\beta _ { 22 } ^ { \\prime } } \\\\ { ( n - k ) \\times k } & { ( n - k ) \\times ( n - k ) } \\end{array} \\right] ^ { \\prime } 
")  

  
![&#10; \\beta \_ { 21 } = ( \\Sigma \_ { mu \_ { 1 } } ^ { - 1 }
\\Sigma \_ { mu \_ { 2 } ^ { \\prime } } ) ^ { \\prime } \\beta \_ { 11
}
&#10;](https://latex.codecogs.com/png.latex?%0A%20%5Cbeta%20_%20%7B%2021%20%7D%20%3D%20%28%20%5CSigma%20_%20%7B%20mu%20_%20%7B%201%20%7D%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%20mu%20_%20%7B%202%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%0A
"
 \\beta _ { 21 } = ( \\Sigma _ { mu _ { 1 } } ^ { - 1 } \\Sigma _ { mu _ { 2 } ^ { \\prime } } ) ^ { \\prime } \\beta _ { 11 } 
")  

  
![&#10; \\left. \\begin{array} { l } &#10; { \\beta \_ { 21 } \\beta \_
{ 11 } ^ { - 1 } = ( \\Sigma \_ { mu 1 } ^ { - 1 } \\Sigma \_ { mu \_
{ 2 } ^ { \\prime } } ) ^ { \\prime } } \\\\ { \\beta \_ { 12 } \\beta
\_ { 22 } ^ { - 1 } = ( \\beta \_ { 12 } \\beta \_ { 12 } ^ { \\prime }
( \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } ) ^ { \\prime } + (
\\Sigma \_ { 21 } - \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } \\Sigma
\_ { 11 } ) ^ { \\prime } ) ( \\beta \_ { 22 } \\beta \_ { 22 } ^ {
\\prime - 1 } ) }\\\\ { \\beta \_ { 12 } \\beta \_ { 12 } ^ { \\prime }
= ( \\Sigma \_ { 21 } - \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 }
\\Sigma \_ { 11 } ) ^ { \\prime } Z ^ { - 1 } ( \\Sigma \_ { 21 } -
\\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } \\Sigma \_ { 11 } ) } \\\\ {
\\beta \_ { 22 } \\beta \_ { 22 } ^ { \\prime } = \\Sigma \_ { 22 } +
\\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } ( \\beta \_ { 12 } \\beta \_
{ 12 } ^ { \\prime } - \\Sigma \_ { 11 } ) ( \\beta \_ { 21 } \\beta \_
{ 11 } ^ { - 1 } ) ^ { \\prime } } \\\\ { \\beta \_ { 11 } \\beta \_
{ 11 } ^ { \\prime } = \\Sigma \_ { 11 } - \\beta \_ { 12 } \\beta \_
{ 12 } ^ { \\prime } } \\end{array} \\right.
&#10;](https://latex.codecogs.com/png.latex?%0A%20%5Cleft.%20%5Cbegin%7Barray%7D%20%7B%20l%20%7D%20%0A%20%7B%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%3D%20%28%20%5CSigma%20_%20%7B%20mu%201%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%20mu%20_%20%7B%202%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%5C%5C%20%20%7B%20%5Cbeta%20_%20%7B%2012%20%7D%20%5Cbeta%20_%20%7B%2022%20%7D%20%5E%20%7B%20-%201%20%7D%20%3D%20%28%20%5Cbeta%20_%20%7B%2012%20%7D%20%5Cbeta%20_%20%7B%2012%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%28%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20%2B%20%28%20%5CSigma%20_%20%7B%2021%20%7D%20-%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%2011%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20%29%20%28%20%5Cbeta%20_%20%7B%2022%20%7D%20%5Cbeta%20_%20%7B%2022%20%7D%20%5E%20%7B%20%5Cprime%20-%201%20%7D%20%29%20%7D%5C%5C%20%7B%20%5Cbeta%20_%20%7B%2012%20%7D%20%5Cbeta%20_%20%7B%2012%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%3D%20%28%20%5CSigma%20_%20%7B%2021%20%7D%20-%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%2011%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20Z%20%5E%20%7B%20-%201%20%7D%20%28%20%5CSigma%20_%20%7B%2021%20%7D%20-%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%2011%20%7D%20%29%20%7D%20%5C%5C%20%7B%20%5Cbeta%20_%20%7B%2022%20%7D%20%5Cbeta%20_%20%7B%2022%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%3D%20%5CSigma%20_%20%7B%2022%20%7D%20%2B%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%28%20%5Cbeta%20_%20%7B%2012%20%7D%20%5Cbeta%20_%20%7B%2012%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20-%20%5CSigma%20_%20%7B%2011%20%7D%20%29%20%28%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%5C%5C%20%7B%20%5Cbeta%20_%20%7B%2011%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%3D%20%5CSigma%20_%20%7B%2011%20%7D%20-%20%5Cbeta%20_%20%7B%2012%20%7D%20%5Cbeta%20_%20%7B%2012%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%7D%20%5Cend%7Barray%7D%20%5Cright.%20%0A
"
 \\left. \\begin{array} { l } 
 { \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } = ( \\Sigma _ { mu 1 } ^ { - 1 } \\Sigma _ { mu _ { 2 } ^ { \\prime } } ) ^ { \\prime } } \\\\  { \\beta _ { 12 } \\beta _ { 22 } ^ { - 1 } = ( \\beta _ { 12 } \\beta _ { 12 } ^ { \\prime } ( \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } ) ^ { \\prime } + ( \\Sigma _ { 21 } - \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } \\Sigma _ { 11 } ) ^ { \\prime } ) ( \\beta _ { 22 } \\beta _ { 22 } ^ { \\prime - 1 } ) }\\\\ { \\beta _ { 12 } \\beta _ { 12 } ^ { \\prime } = ( \\Sigma _ { 21 } - \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } \\Sigma _ { 11 } ) ^ { \\prime } Z ^ { - 1 } ( \\Sigma _ { 21 } - \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } \\Sigma _ { 11 } ) } \\\\ { \\beta _ { 22 } \\beta _ { 22 } ^ { \\prime } = \\Sigma _ { 22 } + \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } ( \\beta _ { 12 } \\beta _ { 12 } ^ { \\prime } - \\Sigma _ { 11 } ) ( \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } ) ^ { \\prime } } \\\\ { \\beta _ { 11 } \\beta _ { 11 } ^ { \\prime } = \\Sigma _ { 11 } - \\beta _ { 12 } \\beta _ { 12 } ^ { \\prime } } \\end{array} \\right. 
")  

  
![&#10; Z = \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } \\Sigma \_ { 11
} ( \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } ) ^ { \\prime } - (
\\Sigma \_ { 21 } ( \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } ) ^ {
\\prime } + \\beta \_ { 21 } \\beta \_ { 11 } ^ { - 1 } \\Sigma \_ { 21
} ^ { \\prime } ) + \\Sigma \_ { 22 }
&#10;](https://latex.codecogs.com/png.latex?%0A%20Z%20%3D%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%2011%20%7D%20%28%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20-%20%28%20%5CSigma%20_%20%7B%2021%20%7D%20%28%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%29%20%5E%20%7B%20%5Cprime%20%7D%20%2B%20%5Cbeta%20_%20%7B%2021%20%7D%20%5Cbeta%20_%20%7B%2011%20%7D%20%5E%20%7B%20-%201%20%7D%20%5CSigma%20_%20%7B%2021%20%7D%20%5E%20%7B%20%5Cprime%20%7D%20%29%20%2B%20%5CSigma%20_%20%7B%2022%20%7D%20%0A
"
 Z = \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } \\Sigma _ { 11 } ( \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } ) ^ { \\prime } - ( \\Sigma _ { 21 } ( \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } ) ^ { \\prime } + \\beta _ { 21 } \\beta _ { 11 } ^ { - 1 } \\Sigma _ { 21 } ^ { \\prime } ) + \\Sigma _ { 22 } 
")
