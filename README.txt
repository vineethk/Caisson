Please refer to our technical publication: "Caisson: A Hardware Description Language for Secure Information Flow", by
Xun Li, Mohit Tiwari, Jason Oberg, Frederic T. Chong, Timothy Sherwood, Ben Hardekopf, published in the 32nd ACM Conference on Programming Language Design and Implementation (PLDI) June 2011, San Jose, California to understand more about this project. 

You need to have a Scala compiler [version 2.8.1 or newer] to be able to compile the source. This is how you would get the compiler running:
1. Compile the source code: 
  $ scalac src/*.scala
2. Run the compiler:
  $ scala CaissonCompiler <caissonFileName>  [-o <outputFileName>]
For example, if the file you want to compile is lease.caisson, and you want to compile it into lease.v you enter:
  $ scala CaissonCompiler lease.caisson -o lease.v
By default, the outputfile generated is a.v   
3. The generated verilog file is unindented, and might be difficult for manual analysis. We suggest using a verilog code indenter/formatter (one of them is available for free at http://veriindent.sourceforge.net/, it is neither supported or sponsored by us, and we hold no responsibility for using this software). 
A test caisson file (lease.caisson) has been included for you to check the working of the compiler.

Please refer to LICENSE.txt for licensing information. 
