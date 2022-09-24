Global Variables:
extent: entire execution,
implementation: fixed address (label)


Local Variables:
extent: execuation of the procedure
implemnation: using a stack

# Frame:
- Area of memory for values of  locals of a procedure invocation
frame pointer: holds the address of the begginging of the current frame
e.g.
read b into $3:
LW(3, 4, 29):
- 29 is f.p.
- 4 is offset of b
- 3 is location

Symbol Table:
Maps each variable to a constant offset from the begining of the frame pointer


CS241E Convention: all data is in memory in "chunks":
- compile time chunk as symbol table , which goes from variable -> offsets
- always push/pop stack one chunck one time at a time




















