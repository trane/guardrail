package swagger
package protocols

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{ClassDefinition, EnumDefinition, ProtocolGenerator, CodegenApplication, Target}
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class BigObjectSpec extends FunSuite with Matchers {

  val swagger = new SwaggerParser().parse(s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  BigObject:
    |    type: object
    |    properties:
    |      v1:
    |        type: integer
    |        format: int32
    |        readOnly: true
    |      v2:
    |        type: integer
    |        format: int32
    |      v3:
    |        type: integer
    |        format: int32
    |      v4:
    |        type: integer
    |        format: int32
    |      v5:
    |        type: integer
    |        format: int32
    |      v6:
    |        type: integer
    |        format: int32
    |      v7:
    |        type: integer
    |        format: int32
    |      v8:
    |        type: integer
    |        format: int32
    |      v9:
    |        type: integer
    |        format: int32
    |      v10:
    |        type: integer
    |        format: int32
    |      v11:
    |        type: integer
    |        format: int32
    |      v12:
    |        type: integer
    |        format: int32
    |      v13:
    |        type: integer
    |        format: int32
    |      v14:
    |        type: integer
    |        format: int32
    |      v15:
    |        type: integer
    |        format: int32
    |      v16:
    |        type: integer
    |        format: int32
    |      v17:
    |        type: integer
    |        format: int32
    |      v18:
    |        type: integer
    |        format: int32
    |      v19:
    |        type: integer
    |        format: int32
    |      v20:
    |        type: integer
    |        format: int32
    |      v21:
    |        type: integer
    |        format: int32
    |      v22:
    |        type: integer
    |        format: int32
    |      v23:
    |        type: integer
    |        format: int32
    |      v24:
    |        type: integer
    |        format: int32
    |      v25:
    |        type: integer
    |        format: int32
    |      v26:
    |        type: integer
    |        format: int32
    |      v27:
    |        type: integer
    |        format: int32
    |      v28:
    |        type: integer
    |        format: int32
    |      v29:
    |        type: integer
    |        format: int32
    |      v30:
    |        type: string
    |        x-scala-empty-is-null: true
    |""".stripMargin)

  test("Big objects can be generated") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    val ClassDefinition(_, _, cls, cmp) :: _ = definitions

    val definition = q"""
      case class BigObject(v1: Option[Int] = None, v2: Option[Int] = None, v3: Option[Int] = None, v4: Option[Int] = None, v5: Option[Int] = None, v6: Option[Int] = None, v7: Option[Int] = None, v8: Option[Int] = None, v9: Option[Int] = None, v10: Option[Int] = None, v11: Option[Int] = None, v12: Option[Int] = None, v13: Option[Int] = None, v14: Option[Int] = None, v15: Option[Int] = None, v16: Option[Int] = None, v17: Option[Int] = None, v18: Option[Int] = None, v19: Option[Int] = None, v20: Option[Int] = None, v21: Option[Int] = None, v22: Option[Int] = None, v23: Option[Int] = None, v24: Option[Int] = None, v25: Option[Int] = None, v26: Option[Int] = None, v27: Option[Int] = None, v28: Option[Int] = None, v29: Option[Int] = None, v30: Option[String] = None)
    """
    val companion = q"""
      object BigObject {
        implicit val encodeBigObject = {
          val readOnlyKeys = Set[String]("v1")
          new ObjectEncoder[BigObject] { final def encodeObject(o: BigObject): JsonObject = JsonObject.fromIterable(Vector(("v1", o.v1.asJson), ("v2", o.v2.asJson), ("v3", o.v3.asJson), ("v4", o.v4.asJson), ("v5", o.v5.asJson), ("v6", o.v6.asJson), ("v7", o.v7.asJson), ("v8", o.v8.asJson), ("v9", o.v9.asJson), ("v10", o.v10.asJson), ("v11", o.v11.asJson), ("v12", o.v12.asJson), ("v13", o.v13.asJson), ("v14", o.v14.asJson), ("v15", o.v15.asJson), ("v16", o.v16.asJson), ("v17", o.v17.asJson), ("v18", o.v18.asJson), ("v19", o.v19.asJson), ("v20", o.v20.asJson), ("v21", o.v21.asJson), ("v22", o.v22.asJson), ("v23", o.v23.asJson), ("v24", o.v24.asJson), ("v25", o.v25.asJson), ("v26", o.v26.asJson), ("v27", o.v27.asJson), ("v28", o.v28.asJson), ("v29", o.v29.asJson), ("v30", o.v30.asJson))) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeBigObject = new Decoder[BigObject] {
          final def apply(c: HCursor): Decoder.Result[BigObject] = for {
            v0 <- c.downField("v1").as[Option[Int]]
            v1 <- c.downField("v2").as[Option[Int]]
            v2 <- c.downField("v3").as[Option[Int]]
            v3 <- c.downField("v4").as[Option[Int]]
            v4 <- c.downField("v5").as[Option[Int]]
            v5 <- c.downField("v6").as[Option[Int]]
            v6 <- c.downField("v7").as[Option[Int]]
            v7 <- c.downField("v8").as[Option[Int]]
            v8 <- c.downField("v9").as[Option[Int]]
            v9 <- c.downField("v10").as[Option[Int]]
            v10 <- c.downField("v11").as[Option[Int]]
            v11 <- c.downField("v12").as[Option[Int]]
            v12 <- c.downField("v13").as[Option[Int]]
            v13 <- c.downField("v14").as[Option[Int]]
            v14 <- c.downField("v15").as[Option[Int]]
            v15 <- c.downField("v16").as[Option[Int]]
            v16 <- c.downField("v17").as[Option[Int]]
            v17 <- c.downField("v18").as[Option[Int]]
            v18 <- c.downField("v19").as[Option[Int]]
            v19 <- c.downField("v20").as[Option[Int]]
            v20 <- c.downField("v21").as[Option[Int]]
            v21 <- c.downField("v22").as[Option[Int]]
            v22 <- c.downField("v23").as[Option[Int]]
            v23 <- c.downField("v24").as[Option[Int]]
            v24 <- c.downField("v25").as[Option[Int]]
            v25 <- c.downField("v26").as[Option[Int]]
            v26 <- c.downField("v27").as[Option[Int]]
            v27 <- c.downField("v28").as[Option[Int]]
            v28 <- c.downField("v29").as[Option[Int]]
            v29 <- c.downField("v30").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[String]]
          } yield BigObject(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29)
        }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
