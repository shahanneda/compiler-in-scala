import cs241e.assignments.A1.{loadAndRun, maximum}
import cs241e.assignments.Assembler
import cs241e.mips.Word

// This is an example of a worksheet. Code that you write here will be immediately
// executed and you will be shown the results. You can use this for quickly testing
// your code. For example:

// The following will print 2:

// The following will give a NotImplementedError until you implement the
// decodeUnsigned method, which is the first thing to do for Assignment 1.
//Assembler.decodeUnsigned(Seq(false))
//Word(Assembler.encodeSigned(-543))
loadAndRun(maximum, Word(Assembler.encodeUnsigned(1)), Word(Assembler.encodeUnsigned(5)))
