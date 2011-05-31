import java.io.FileReader

object CaissonCompiler {
    def main(args: Array[String]) {
       val (inputFileName, outputFileName) = parseArguments(args) //parse arguments and get file names
       try {
          val reader = new FileReader(inputFileName)
          val parser = new CaissonParser()
          val primitiveAST = parser.parseAll(parser.prog, reader) match {
            case parser.Success(p, _) => p //successful parsing, gets the AST
            case f => println(f); throw new CaissonParseException("Could not parse")
          }
          validate(primitiveAST) // check if the implicit assumptions mentioned in the paper hold, documented in the code too
          val ast = primitiveAST.fallTransform // perform a fall transform that labels each of the fall statements with the corresponding labels
          val env = ast.computeEnvironment // compute the environment: type mapping and function (F_cmd, F_def) mapping
          val kappa = Util.computeKappa(env.typeMap) // add all constraints to a system, and check if they are consistent [TODO: rewrite this to be functional]
          ast.caissonType(env, kappa) // type check the ast, will throw an appropriate exception if it doesn't type check
          val writer = new java.io.FileWriter(outputFileName)
          writer.write(ast.codeGen(env)) // generate code and write it to the output file, which is by default "a.v"
          reader.close() //cleanup
          writer.close()
       } catch {
          case e: CaissonTypeException => println(e.message)
          case e: CaissonParseException => println(e.message)
          case e: CaissonCompilerException => println(e.message)
          case e: InvalidProgramException => println(e.message)
          case e: ValidationException => println(e.message)
          case e: NoSuchElementException => println(e.getMessage.split(":")(1)+" is perhaps not declared")
          case e: Exception => e.printStackTrace()
       }
 }

    def validate(ast: Program) {
      /*
       Validate the following assumptions on the Program:
              1. All the variables and type variable have distinct names
              2. A default child state cannot take any parameters
              3. Either both the branches of an if command must execute a goto or fall, or neither of them do.
              4. All paths through a state end in either a goto or a fall
              5. A leaf state cannot contain a fall
              6. Every goto targets a defined label, and can only target a state in the same group and at the same nested depth
       */
      val namesSet = ast.validateNames //get the set of all names used, after making sure they are unique
      val typeVarsSet = ast.validateTypeVars(Set("L", "H")) // get the set of all type variables used, after making sure they are correctly used, and unique (never declared twice in the same scope)
      if (! (namesSet & typeVarsSet).isEmpty) throw new InvalidProgramException("Variable names and Type variable names clash")
      discard(namesSet) //now we have checked 1.
      ast.validateDefaultStateAssumption() //check 2., throw an Exception if it does not hold
      ast.validateFallsAndGotos() //check 3., 4., throw an Exception otherwise
      ast.validateGotos() //check 6.
    }
    
    private def discard(x: AnyRef) {}

    private def printHelpAndExit(): (String, String) = {
      println("Usage: fsc CaissonCompiler <caissonFileName>  [-o <outputFileName>]")
      exit(-1)
    }

    private def parseArguments(args: Array[String]): (String, String) = {
      if (args.length < 1) printHelpAndExit()
      if (args.length == 1) (args(0), "a.v")
      else if (args.length == 2)  printHelpAndExit()
      else if (args.length == 3) {
        args(1) match {
          case "-o" => (args(0), args(2))
          case _ => printHelpAndExit()
        }
      }
      else printHelpAndExit()
    }
}
