#define TMB_LIB_INIT R_init_tsdistributions_TMBExports
#include <TMB.hpp>
#include "distfun.h"
#include "distmodel.hpp"
#ifndef M_PI
#define M_PI 3.141592653589793115998
#endif

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_STRING(model);
  if(model == "distribution") {
    return distmodel(this);
  } else {
    Rf_error("Unknown model.");
  }
  return 0;
}
