//*********************************************************************
/*! @file simpleMatrix.h
  
  Description:
    What does this program or file do?

  Author(s):
    Tommy Chang <email@address>

 @verbatim
 ************************************************************************
 * DISCLAIMER:                                                          *
 * This software was produced by the National Institute of Standards    *
 * and Technology (NIST), an agency of the U.S. government, and by      *
 * statute is not subject to copyright in the United States.            *
 * Recipients of this software assume all responsibility associated     *
 * with its operation, modification, maintenance, and subsequent        *
 * redistribution.                                                      *
 ************************************************************************
 @endverbatim
  @code CVS Status:
     $Revision: 344 $ by $Author: tchang $, 
     $Date: 2006-07-19 11:53:20 -0400 (Wed, 19 Jul 2006) $
  @endcode 
  @par CVS Log and software disclaimer:
  @ref simpleMatrix_h "More..."
  @author  Tommy Chang 
  @date    2004-07-06 */
//**********************************************************************


#ifndef SIMPLEMATRIX_H
#define SIMPLEMATRIX_H
/*----------------------------------*\
 * Preprocessor and Include Headers *
 \*--------------------------------*/


/*-----------------*\
 * Data Structures *
 \*---------------*/
class SimpleMatrixClass {
 public:
  SimpleMatrixClass (int, int, const char*);
  SimpleMatrixClass (const SimpleMatrixClass&); /* copy  constructor */
  ~SimpleMatrixClass ();
  SimpleMatrixClass& operator = (const SimpleMatrixClass&);

  void set_elements (bool, int, int, int, const char*);
  void set_elements (bool, int, int, int, ...);
  void set_elements (const char*);
  void reset_elements ();
  double get_element (int, int);
  void set_element (int, int, double);

  void set_vector (int, const SimpleMatrixClass&);

  void print ();
/*   void mult (const SimpleMatrixClass&, const SimpleMatrixClass&); */
  const SimpleMatrixClass&  mult (const SimpleMatrixClass&, 
                                  const SimpleMatrixClass&);
  void add (const SimpleMatrixClass&, const SimpleMatrixClass&);
  void sub (const SimpleMatrixClass&, const SimpleMatrixClass&);
  void get_dimension (int*, int*);

 private:
  int n_rows;
  int n_cols;
  double *matrix_elements;
  char name[80];                /* hopefully enough */
};

/*--------------------*\
 * Function Prototype *
 \*------------------*/
void createYPRMatrix_leftHand (SimpleMatrixClass &, float, float, float);
void createYPRMatrix_rightHand (SimpleMatrixClass &, float, float, float);

#endif
