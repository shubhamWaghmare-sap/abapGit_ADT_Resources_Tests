CLASS test_class_shubham DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS add
    IMPORTING  a TYPE I
               b TYPE I.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS test_class_shubham IMPLEMENTATION.

  METHOD add.
    Data c TYPE i.
    c = a + b.
    """Written C
    WRITE : 'C:',
             c.
    """"New comment
  ENDMETHOD.

ENDCLASS.
