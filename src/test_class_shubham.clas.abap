CLASS test_class_shubham DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS add
    IMPORTING  a TYPE I
               b TYPE I.

    METHODS sub
    Importing a type i
              b type i.
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
  ENDMETHOD.

  METHOD sub.

  ENDMETHOD.

ENDCLASS.
