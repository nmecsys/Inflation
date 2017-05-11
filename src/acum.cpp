#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//





// [[Rcpp::export]]
NumericVector acum(NumericMatrix data, NumericMatrix data2, int n, int m) {

    //     int nrow = out.nrow(), ncol = out.ncol();



    for (int j = 0; j < m; j++) {
         for (int i = n; i > 12; i--) {
            data(i-13,j) = data2(i-1,j)*data2(i-2,j)*data2(i-3,j)*
                data2(i-4,j)*data2(i-5,j)*data2(i-6,j)*data2(i-7,j)*
                data2(i-8,j)*data2(i-9,j)*data2(i-10,j)*data2(i-11,j)*data2(i-12,j) ;
        }
    }


    return data;
}

///*** R
//t1 <- acum12(x)
//*/


