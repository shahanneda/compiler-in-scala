## Alternative to stacks when implementing expressions:
### Temporary variables:

def compile(e1, op e2) : 
will return (code, variable):
 - the code will evaluate the expression and put the result into the variable


### Technique 3:

Use temperary variables, but do arthemetic on registers.

Will return just code and not a variable, code will put result in register 3.
