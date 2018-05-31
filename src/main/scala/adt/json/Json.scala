package adt.json

// TODO: Implement JsValue

// turn a shape in to json

sealed trait Shape {

  def shapeToJson: JsonAble[Shape] = Shape.shapeToJson

  def toJson: Json = Shape.shapeToJson.toJson(this)
}

object Shape {

  implicit val shapeToJson: JsonAble[Shape] = new JsonAble[Shape] {
    override def toJson(value: Shape): Json =
      value match {
        case c@Circle(_) => Circle.circleToJson.toJson(c)
        case r@Rect(_, _) => Rect.rectToJson.toJson(r)
      }
  }

}

final case class Circle(radius: Double) extends Shape

object Circle {
  val circleToJson: JsonAble[Circle] =
    circle =>
      JsonObject(
        Map(
          "shape" -> JsonString("Circle"),
          "radius" -> JsonNumber(circle.radius)
        ))
}

final case class Rect(height: Double, width: Double) extends Shape

object Rect {
  val rectToJson: JsonAble[Rect] =
    rect =>
      JsonObject(
        Map(
          "shape" -> JsonString("Rectangle"),
          "height" -> JsonNumber(rect.height),
          "width" -> JsonNumber(rect.width)
        ))
}

trait JsonAble[A] {
  def toJson(value: A): Json
}

object JsonAble {
  def jsonify[A](value: A)(implicit jsonAble: JsonAble[A]): Json =
    jsonAble.toJson(value)

  def pure[A](func: A => Json): JsonAble[A] =
    new JsonAble[A] {
      def toJson(value: A): Json =
        func(value)
    }


  implicit def listToJson[A](implicit writer: JsonAble[A]): JsonAble[List[A]] =
    pure(list => JsonArray(list.map(writer.toJson(_))))



}

// take a jsonwriter

sealed trait Json {

  def toJsonString: String = Json.toJsonString(this)
}

object Json {

  def toJsonString(json: Json): String =
    json match {
      case JsonString(v) => s""" "$v" """.trim
      case JsonNumber(v) => s"$v"
      case JsonBool(v) => s"$v"
      case JsonArray(list) => s"[ ${jsonifyList(list)} ]"
      case JsonObject(e) => s"{${jsonifyMap(e)}}"
      case JsonNull(_) => "null"
    }

  private def jsonifyList(l: List[Json]): String =
    l match {
      case Nil => ""
      case head :: Nil => toJsonString(head)
      case head :: tail => s"${toJsonString(head)}, ${jsonifyList(tail)}"
    }

  private def jsonifyMap(m: Map[String, Json]): String = {
    val keyValueList: List[(String, Json)] = m.toList
    val jsonStringList: List[String] = keyValueList.map {
      t => s""" "${t._1}": ${Json.toJsonString(t._2)} """.trim
    }
    val unfiltered: String = jsonStringList.foldLeft("")(_ + ", " + _)
    if (unfiltered.startsWith(", ")) unfiltered.drop(2) else unfiltered //TODO: Stop making myself want to cry...
  }

}

final case class JsonString(value: String) extends Json

final case class JsonNumber(value: Double) extends Json

final case class JsonBool(value: Boolean) extends Json

final case class JsonArray(value: List[Json]) extends Json

final case class JsonObject(entries: Map[String, Json]) extends Json

final case class JsonNull(value: Null) extends Json

object Main extends App {
  // TODO: Create and stringify some JsValues
  val name: JsonString = JsonString("Arthur")
  println(name)
  println(Json.toJsonString(name))
  val jsonObject = JsonObject(Map("name" -> name))
  println(Json.toJsonString(jsonObject))

  println(
    Circle.circleToJson.toJson(Circle(7)).toJsonString
  )


  println(
    Rect.rectToJson.toJson(Rect(4, 9)).toJsonString
  )

  val shapeList: List[Shape] = List(Circle(1), Circle(2), Circle(3), Rect(3, 4))
  val hj: Json = JsonAble.jsonify(shapeList)
  println(hj)

  val jsonShapeArray: Json = JsonAble.listToJson[Shape].toJson(shapeList)

  val jsonShapes: Json = JsonObject(Map("shapes" -> jsonShapeArray))

  println(jsonShapes.toJsonString)

}
