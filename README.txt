You need to have a Scala compiler to be able to compile the source. This is how you would get the compiler running:
1. Compile the source code: 
  $ scalac src/*.scala
2. Run the compiler:
  $ scala CaissonCompiler <caissonFileName>  [-o <outputFileName>]
For example, if the file you want to compile is lease.caisson, and you want to compile it into lease.v you enter:
  $ scala CaissonCompiler lease.caisson -o lease.v
By default, the outputfile generated is a.v   
3. The generated verilog file is unindented, and might be difficult for maual analysis. We suggest using a verilog code indenter/formatter (one of them is available for free at http://veriindent.sourceforge.net/, it is neither supported or sponsored by us, adn we hold no responsibility for using this software). 
A test caisson file (lease.caisson) has been included for you to check the working of the compiler.


