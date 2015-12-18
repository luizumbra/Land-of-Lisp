# Chapter 2 - Creating your First Lisp Program

- The function to create a global variable is (_defparameter_ \***global-variable-name**\* *value*)
- Global variables in Lisp should be between **.
- When you set the value of a global variable, any value previously stored in it will be overwritten.
- The function to create a global function is (_defun_ *function-name* (*arguments*)).
- (_setf_ ***global-variable-name*** *value*) changes the value of the global variable.
- Define local variables (let (_variable-declaretion-1_) (_variable-declaretion-2_)...(_variable-declaration-n_) _body_)
- Define local function in lisp, (*flet* ((_function-name_ (_arguments_) _function-body_)) _body_).