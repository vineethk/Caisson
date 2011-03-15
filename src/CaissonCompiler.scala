import java.io.FileReader

object CaissonCompiler {
    def main(args: Array[String]) {
        val reader = new FileReader(args(0))
        val parser = new CaissonParser()
        println(parser.parseAll(parser.prog, reader))
 }
}
