package tests.generators.AkkaHttp

import cats.instances.all._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{ClassDefinition, Client, Context, ClientGenerator, ProtocolGenerator, CodegenApplication, Target}
import io.swagger.parser.SwaggerParser
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class PropertyExtractors extends FunSuite with Matchers {
  val swagger = new SwaggerParser().parse(s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  Something:
    |    type: object
    |    required:
    |      - map
    |    properties:
    |      boolean_value:
    |        type: boolean
    |      string_value:
    |        type: string
    |      date_property:
    |        type: date
    |      date_time_property:
    |        type: date-time
    |      long_property:
    |        type: integer
    |        format: int64
    |      int_property:
    |        type: integer
    |        format: int32
    |      integer_property:
    |        type: integer
    |      float_property:
    |        type: number
    |        format: float
    |      double_property:
    |        type: number
    |        format: double
    |      number_property:
    |        type: number
    |      property:
    |        default: "what"
    |      object_property:
    |        type: object
    |""".stripMargin)
/*
    |      ref_property:
    |        "$$ref": "#/definitions/ref_target_property"
    |      ref_target_property:
    |        type: string
    |      array_property:
    |        type: array
    |        items:
    |          "$$ref": "#/definitions/ref_target_property"
*/

  test("Render all primitive types correctly") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    val ClassDefinition(_, _, cls, cmp) :: _ = definitions

    val definition = q"""
      case class Something(
        booleanValue: Option[Boolean] = None, stringValue: Option[String] = None,
        longProperty: Option[Long] = None, intProperty: Option[Int] = None,
        integerProperty: Option[BigInt] = None, floatProperty: Option[Float] = None,
        doubleProperty: Option[Double] = None, numberProperty: Option[BigDecimal] = None,
        objectProperty: Option[io.circe.Json] = None
        /*, refProperty: Option[ref_target_property] = None, refTargetProperty: Option[String] = None,
        arrayProperty: Option[IndexedSeq[ref_target_property]] = Option(IndexedSeq.empty)
        */
      )
    """

    val companion = q"""
      object Something {
        implicit val encodeSomething = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct9(
              "boolean_value", "string_value", "long_property", "int_property", "integer_property", "float_property",
              "double_property", "number_property", "object_property"
              /*, "ref_property", "ref_target_property", "array_property" */
            )( (o: Something) => (
              o.booleanValue, o.stringValue, o.longProperty, o.intProperty, o.integerProperty, o.floatProperty,
              o.doubleProperty, o.numberProperty, o.objectProperty
              /* , o.refProperty, o.refTargetProperty, o.arrayProperty */
            )
          ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeSomething = Decoder.forProduct9(
          "boolean_value", "string_value", "long_property", "int_property", "integer_property", "float_property",
          "double_property", "number_property", "object_property"
          /*, "ref_property", "ref_target_property", "array_property" */
        )(Something.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
