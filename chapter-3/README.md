### Chapter 3 - Explore the syntax of Lisp Code

## Data Type

# Symbol

- Symbol is a fundamental data type.
- Symbol is a stand-alone word.
- Symbols are _case sensitive_

# Number
- Lisp support both floating-point and integers
- The presence of "." determines if is floating-point
- Lisp support fractional numbers too
-- Since none of theses number are floating-point

# String
- Between ""
- Strings support "\" special character, the same of C

# List
- Lisp principal data structured
- Basic functions
-- *cons* link to pieces of data
-- *car* reduce just the first element of a list
-- *cdr* remove the list first element, and return it
- Anayone can make combinations of car and cdr with the format c(a|d)+r
-- cadr take the head of the tail, in other words the second element of the list
--- First read the d and next read a
-- caddr take the third element of the list
--- First read the last d, after read the other d and finally read a
- Empty list is the terminator of a list
- Lists can contain other lists