//> using scala 3.5.0-RC6
//> using options -language:experimental.namedTuples

val cls = (
  name = "VersionScheme",
  hasApply = false,
  ctor = List(
    "value" -> "String",
    // "classifier" -> "coursier.core.Classifier",
  )
)

val padding = 4

// derived values

val (ctorNames, ctorTypes) = cls.ctor.unzip

def asTuple[T](elems: List[T]): String =
  if elems.isEmpty then "EmptyTuple"
  else if elems.sizeIs == 1 then s"${elems.head} *: EmptyTuple"
  else elems.mkString("(", ", ", ")")

def factory(args: List[String]) =
  val list = args.mkString(",")
  if cls.hasApply then
    s"""${cls.name}.apply($list)"""
  else
    s"""new ${cls.name}($list)"""

println(s"""given Mirror_${cls.name.replace(".", "_")}: Mirror.Product with {
  final type MirroredMonoType = ${cls.name}
  final type MirroredType = ${cls.name}
  final type MirroredElemTypes = ${asTuple(ctorTypes)}
  final type MirroredElemLabels = ${asTuple(ctorNames.map(s => s"$"$s$""))}

  final def fromProduct(p: scala.Product): ${cls.name} = {
    ${ctorTypes.zipWithIndex.map{ (tpe, i) =>
      s"val _${i + 1}: $tpe = p.productElement($i).asInstanceOf[$tpe]"
    }.mkString("", "\n    ","\n")}
    ${factory(ctorTypes.zipWithIndex.map{ (_, i) => s"_${i + 1}"})}
  }
}""".linesIterator.map(s => s"${" " * padding}$s").mkString("\n"))
