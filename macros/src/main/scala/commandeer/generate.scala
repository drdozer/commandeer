package commandeer

import scala.collection.immutable.Seq
import scala.meta._

trait CommandeerDSL[Host] {
  trait Operation[T]
}

object CommandeerDSL {
  def apply[Host, DSL <: CommandeerDSL[Host]](host: Host)(implicit dsl: DSL): DSL = dsl
}



class generateDSL extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case trt @ q"trait $name { ..$stmts }" =>
        val opClasses = stmts collect {
          case q"def $op(...$args): $ret" =>
            q"case class ${Type.Name(op.value)}(...$args) extends Operation[$ret]"
        }

        val companion = q"""object ${Term.Name(name.value)} {
              implicit val c_dsl: C_DSL = new C_DSL {}

              trait C_DSL extends CommandeerDSL[$name] {
                ..$opClasses
              }
            }"""
        println(s"Generating companion: $companion")
        Term.Block(Seq(trt, companion))
      case _ =>
        abort(s"@generateDSL must annotate something else than $defn")
    }
  }
}


