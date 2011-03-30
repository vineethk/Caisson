import java.io.FileReader

object CaissonCompiler {
    def main(args: Array[String]) {
        if (args.length != 1) { 
          println("Please provide caisson filename to be compiled")
          exit(-1)
        }
        try {  
          val reader = new FileReader(args(0))
          val parser = new CaissonParser()
          val primitiveAST = parser.parseAll(parser.prog, reader) match {
            case parser.Success(p, _) => p
            case f => println(f); throw new CaissonParseException("Could not parse")
          }
          validate(primitiveAST)  
          val ast = primitiveAST.fallTransform
          val env = ast.computeEnvironment
          val kappa = Util.computeKappa(env.typeMap)
          ast.caissonType(env, kappa)
          println("Succesfully Parsed and Type checked")
        } catch {
          case e: CaissonTypeException => println(e.message)
          case e: CaissonParseException => println(e.message)
          case e: InvalidProgramException => println(e.message)
          case e: Exception => e.printStackTrace()
        }  
 }

    def validate(ast: Program) {
      val namesSet = ast.validateNames
      val typeVarsSet = ast.validateTypeVars(Set("L", "H"))
      if (! (namesSet ** typeVarsSet).isEmpty) throw new InvalidProgramException("Variable names and Type variable names clash")
      discard(namesSet)
    }
    
    private def discard(x: AnyRef) {}
}

