# --------------------------------------------------- #
# Script author: Marius D. Pascariu
# Code author: Carlo G Camarda
# License: MIT
# Last Update: Mon Jan 29 15:11:52 2024
# --------------------------------------------------- #

# Here we bring two MortalitySmooth functions following the removal 
# of the package for the CRAN on which {ungroup} is dependent


#' Truncated p-th Power Function
#' 
#' This is an internal function of package MortalitySmooth which constructs a 
#' truncated p-th power function along an abscissa within the function 
#' MortSmooth_bbase
#' @param x for the abscissa of data;
#' @param tt vector of truncation points;
#' @param p degree of the power
#' @details Internal function used in MortSmooth_bbase. 
#' The vector t contains the knots. The simplest system of truncated power 
#' functions uses p = 0 and it consists of step functions with jumps of size 1 
#' at the truncation points t.
#' @author Carlo G Camarda
#' @keywords internal
MortSmooth_tpower <- function(x, tt, p){
    (x - tt) ^ p * (x > tt)
    ## (x-t)^p gives the curve
    ## (x>t) is an indicator function; it is 1 when x>t
    ## and 0 when x<=t, i.e. before each knot
  }




#' Construct B-spline basis
#' 
#' This is an internal function of package MortalitySmooth which creates 
#' equally-spaced B-splines basis over an abscissa of data.
#' @param x vector for the abscissa of data;
#' @param xl left boundary;
#' @param xr right boundary;
#' @param ndx number of internal knots minus one or number of internal intervals;
#' @param deg degree of the splines.
#' @details The function reproduce an algorithm presented by Eilers and Marx 
#' (2010) using differences of truncated power functions 
#' (see MortSmooth_tpower). The final matrix has a single B-spline for each 
#' of the [ndx + deg] columns. The number of rows is equal to the length of x.
#' 
#' The function differs from bs in the package splines since it automatically 
#' constructed B-splines with identical shape. This would allow a simple 
#' interpretation of coefficients and application of simple differencing.
#' 
#' @return A matrix containing equally-spaced B-splines of degree deg along 
#' x for each column.
#' 
#' @author Carlo G Camarda
#' @keywords internal
MortSmooth_bbase <- function(x, xl, xr, ndx, deg){

    ## distance between knots
    dx <- (xr - xl) / ndx
    ## One needs (ndx+1) internal knots and 
    ## deg knots on both right and left side
    ## in order to joint all the B-splines
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    ## Truncated deg-th power functions
    ## equally-spaced at given knots along x
    P <- outer(x, knots, MortSmooth_tpower, deg)
    ## number of B-splines which equal to the number of knots
    n <- dim(P)[2]
    ## in the numerator we have the matrix
    ## of deg+1 differences for each knots
    ## this matrix is rescaled by 
    ## (deg! * dx^deg) == (gamma(deg + 1) * dx ^ deg)
    D <- diff(diag(n), diff = deg + 1) /
      (gamma(deg + 1) * dx ^ deg)
    ## the matrix of differences is used to compute B-splines
    ## as differences of truncated power functions
    ## in P %*% t(D)
    ## the last factor is (-1) ^ (deg + 1)
    B <- (-1) ^ (deg + 1) * P %*% t(D)
    B
  }