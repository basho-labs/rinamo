import bootstrap.rinamo.Boot
import scala.tools.nsc.MainGenericRunner

object RinamoConsole {
   def main(args : Array[String]) {
     val b = new Boot()
     b.boot
     MainGenericRunner.main(args)
     exit(0)
   }
}