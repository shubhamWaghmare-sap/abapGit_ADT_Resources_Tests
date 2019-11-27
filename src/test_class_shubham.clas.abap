CLASS test_class_shubham DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS calculate
    IMPORTING  a TYPE I
               b TYPE I.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS test_class_shubham IMPLEMENTATION.

  METHOD calculate.
    Data c TYPE i.
    c = a + b.
    WRITE c.
  ENDMETHOD.

ENDCLASS.
