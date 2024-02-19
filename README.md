# A compiler for an imperative programming language

Syntax inspired by Scala, compiler is also written in Scala.

- Uses CYK parsing (Earley parsing also implemented)
- Supports types
- Supports nested functions and closures (can assign a function to a variable and pass it around!)
- Does tail recursion optimization

# Sample supported written programs:
```
   def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def f(i: Int) : Int = {
          if(i > 100000){
            10000
          }
          else{
            f(i + 1)
          }
        }

        f(0)
      }
```

```
     def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def increaseByGen(i: Int) : (Int) => Int = {
          def increase(a : Int) : Int = {
            a + i
          }
          increase
        }

        def double(f: (Int) => Int ) : (Int) => Int = {
          def h(a: Int) : Int = {
            f(f(a))
          }
          h
        }

        inc1 = double(increaseByGen(1));
        inc1(30)
```
