You need to have a Scala compiler to be able to compile the source. This is how you would get the compiler running:
1. Compile the source code: 
  $ scalac src/*.scala
2. Run the compiler:
  $ scala CaissonCompiler <filename without the extension .caisson>
For example, if the file you want to compile is lease.caisson, you enter:
  $ scala CaissonCompiler lease
3. If the program type checks, then a file (with name = filename you provided, and .vgen extension) will be created. This is unindented verilog code, and you can use the perl script tests/ind_verilog.pl to indent it. Here is how you go about it:
  $ cd tests
  $ perl ind_verilog.pl <filename>.vgen
This will create a file 2.v in the same directory that is an indented form of the generated code.   

A test caisson file (lease.caisson) has been included for you to check the working of the compiler.


