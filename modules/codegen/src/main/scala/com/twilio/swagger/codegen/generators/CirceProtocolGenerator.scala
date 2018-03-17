package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models.{ArrayModel, Model, ModelImpl, RefModel}
import _root_.io.swagger.models.properties._
import cats.implicits._
import cats.~>
import com.twilio.swagger.codegen.extract.{Default, ScalaEmptyIsNull, ScalaType, ScalaWrapper}
import com.twilio.swagger.codegen.terms.protocol._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._

object CirceProtocolGenerator {
  import ProtocolGenerator._

  def suffixClsName(prefix: String, clsName: String) = Pat.Var(Term.Name(s"${prefix}${clsName}"))

  def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta])(f: Type => Type): Option[Type] = {
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)
  }

  object EnumProtocolTermInterp extends (EnumProtocolTerm ~> Target) {
    def apply[T](term: EnumProtocolTerm[T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getEnum()).map(_.asScala.to[List]), "Model has no enumerations"))

      case ExtractType(swagger) =>
        // Default to `string` for untyped enums.
        // Currently, only plain strings are correctly supported anyway, so no big loss.
        val tpeName = Option(swagger.getType()).getOrElse("string")
        Target.pure(Either.right(SwaggerUtil.typeName(tpeName, Option(swagger.getFormat()), ScalaType(swagger))))

      case RenderMembers(clsName, elems) =>
        Target.pure(q"""
          object members {
            ..${elems.map({ case (value, termName, defaultTerm) =>
              q"""case object ${termName} extends ${Type.Name(clsName)}(${Lit.String(value)})"""
            }).to[List]}
          }
        """)

      case EncodeEnum(clsName) =>
        Target.pure(q"""
          implicit val ${suffixClsName("encode", clsName)}: Encoder[${Type.Name(clsName)}] =
            Encoder[String].contramap(_.value)
        """)

      case DecodeEnum(clsName) =>
        Target.pure(q"""
          implicit val ${suffixClsName("decode", clsName)}: Decoder[${Type.Name(clsName)}] =
            Decoder[String].emap(value => parse(value).toRight(${Term.Interpolate(Term.Name("s"), List(Lit.String(""), Lit.String(s" not a member of ${clsName}")), List(Term.Name("value")))}))
        """)

      case RenderClass(clsName, tpe) =>
        Target.pure(q"""
          sealed abstract class ${Type.Name(clsName)}(val value: ${tpe}) {
            override def toString: String = value.toString
          }
        """)

      case RenderCompanion(clsName, members, accessors, values, encoder, decoder) =>
        Target.pure(q"""
          object ${Term.Name(clsName)} {
            ..${
              List(members) ++
              accessors ++
              List(values) ++
              List(q"def parse(value: String): Option[${Type.Name(clsName)}] = values.find(_.value == value)") ++
              List(encoder) ++
              List(decoder) ++
              List(q"implicit val ${Pat.Var(Term.Name(s"addPath${clsName}"))}: AddPath[${Type.Name(clsName)}] = AddPath.build(_.value)") ++
              List(q"implicit val ${Pat.Var(Term.Name(s"show${clsName}"))}: Show[${Type.Name(clsName)}] = Show.build(_.value)")
            }
          }
        """)
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm ~> Target) {
    def apply[T](term: ModelProtocolTerm[T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        Target.pure(Either.fromOption(Option(swagger.getProperties()).map(_.asScala.toList), "Model has no properties"))

      case TransformProperty(clsName, name, property, needCamelSnakeConversion, concreteTypes) =>
        def toCamelCase(s: String): String = "[_\\.]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))

        for {
          _ <- Target.log.debug("definitions", "circe", "modelProtocolTerm")(s"Generated ProtocolParameter(${term}, ${name}, ...)")

          argName = if (needCamelSnakeConversion) toCamelCase(name) else name
          meta <- SwaggerUtil.propMeta(property)

          (defaultValue, scalaWrapper) = property match {
            case p: MapProperty =>
              (Option(q"Map.empty"), ScalaWrapper(p))
            case p: ArrayProperty =>
              (Option(q"IndexedSeq.empty"), ScalaWrapper(p))
            case p: BooleanProperty =>
              (Default(p).extract[Boolean].map(Lit.Boolean(_)), ScalaWrapper(p))
            case p: DoubleProperty =>
              (Default(p).extract[Double].map(Lit.Double(_)), ScalaWrapper(p))
            case p: FloatProperty =>
              (Default(p).extract[Float].map(Lit.Float(_)), ScalaWrapper(p))
            case p: IntegerProperty =>
              (Default(p).extract[Int].map(Lit.Int(_)), ScalaWrapper(p))
            case p: LongProperty =>
              (Default(p).extract[Long].map(Lit.Long(_)), ScalaWrapper(p))
            case p: StringProperty =>
              (Default(p).extract[String].map(Lit.String(_)), ScalaWrapper(p))
            case _ =>
              (None, None)
          }

          readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
          needsEmptyToNull = property match {
            case d: DateProperty => ScalaEmptyIsNull(d)
            case dt: DateTimeProperty => ScalaEmptyIsNull(dt)
            case s: StringProperty => ScalaEmptyIsNull(s)
            case _ => None
          }
          emptyToNullKey = needsEmptyToNull.filter(_ == true).map(_ => argName)

          (tpe, rawDep) = meta match {
            case SwaggerUtil.Resolved(declType, rawDep, _) =>
              (declType, rawDep)
            case SwaggerUtil.Deferred(tpeName) =>
              val tpe = concreteTypes.find(_.clsName == tpeName).map(_.tpe).getOrElse {
                println(s"Unable to find definition for ${tpeName}, just inlining")
                Type.Name(tpeName)
              }
              (tpe, Option.empty)
            case SwaggerUtil.DeferredArray(tpeName) =>
              (t"IndexedSeq[${Type.Name(tpeName)}]", Option.empty)
            case SwaggerUtil.DeferredMap(tpeName) =>
              (t"Map[String, ${Type.Name(tpeName)}]", Option.empty)
            }

          (finalDeclType, finalDefaultValue) =
            Option(property.getRequired)
              .filterNot(_ == false)
              .fold[(Type, Option[Term])](
                (t"Option[${tpe}]", Some(defaultValue.fold[Term](q"None")(t => q"Option($t)")))
              )(Function.const((tpe, defaultValue)) _)
          term = param"${Term.Name(argName)}: ${finalDeclType}".copy(default=finalDefaultValue)
          dep = rawDep.filterNot(_.value == clsName) // Filter out our own class name
        } yield ProtocolParameter(term, name, dep, readOnlyKey, emptyToNullKey, scalaWrapper)

      case RenderDTOClass(clsName, terms) =>
        Target.pure(q"""
          case class ${Type.Name(clsName)}(..${terms})
        """)

      case EncodeModel(clsName, needCamelSnakeConversion, params) =>
        val readOnlyKeys: List[String] = params.flatMap(_.readOnlyKey).toList
        val paramCount = params.length
        val typeName = Type.Name(clsName)

        val (names, tupleFields): (List[Lit], List[Term]) = (for {
          param <- params
          jsonKey = Lit.String(param.name)
          baseAccessor = Term.Name(param.term.name.value)
          accessor = param.scalaWrapper.fold[Term](q"o.${baseAccessor}")(Function.const(q"o.${baseAccessor}.value"))
        } yield (jsonKey, accessor)).to[List].unzip

        val encVal = if (paramCount == 1) {
          val List(name) = names
          val List(field) = tupleFields
          q"""
            Encoder.forProduct1(${name})((o: ${Type.Name(clsName)}) => ${field})
          """
        } else if (paramCount <= 22) {

          val unapply: Term.Function = Term.Function(
            List(param"o: ${Type.Name(clsName)}"),
            Term.Tuple(tupleFields)
          )
          q"""
            Encoder.${Term.Name(s"forProduct${paramCount}")}(..${names})(${unapply})
          """
        } else {
          val pairs: List[Term.Tuple] = names.zip(tupleFields).map({ case (name, field) => q"(${name}, ${field}.asJson)" }).to[List]
          q"""
            new ObjectEncoder[${Type.Name(clsName)}] {
              final def encodeObject(o: ${Type.Name(clsName)}): JsonObject = JsonObject.fromIterable(Vector(..${pairs}))
            }
          """
        }
        Target.pure(q"""
          implicit val ${suffixClsName("encode", clsName)} = {
            val readOnlyKeys = Set[String](..${readOnlyKeys.map(Lit.String(_))})
            $encVal.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
        """)

      case DecodeModel(clsName, needCamelSnakeConversion, params) =>
        val emptyToNullKeys: List[String] = params.flatMap(_.emptyToNullKey).toList
        val paramCount = params.length
        val (names, accessorTpes, bindingNames) = (for {
          (param, idx) <- params.toList.zipWithIndex
          bindingName = Term.Name(s"v${idx}")
          name = Lit.String(param.name)
          accessor = param.scalaWrapper.fold[Term](bindingName)(wrapperName => q"${Term.Name(wrapperName)}(${bindingName})")
          tpe = param.term.decltpe.flatMap({
            case tpe: Type => Some(tpe)
            case x =>
              println(s"Unsure how to map ${x.structure}, please report this bug!")
              None
          })
        } yield (name, (accessor, tpe), bindingName)).unzip3
        val bindingParams = bindingNames.map(Term.Param(List.empty, _, None, None))
        val (accessors, tpes) = accessorTpes.unzip
        val decVal = if (paramCount <= 22 && emptyToNullKeys.isEmpty) {
          q"""
            Decoder.${Term.Name(s"forProduct${paramCount}")}(..${names})((..${bindingParams}) => ${Term.Name(clsName)}(..${accessors}))
          """
        } else {
          val enumerators: List[Enumerator.Generator] = (for {
            ((name, term), tpe) <- names.zip(bindingNames).zip(tpes)
            enum = if (emptyToNullKeys contains name.value) {
              enumerator"""
                ${Pat.Var(term)} <- c.downField(${name}).withFocus(j => j.asString.fold(j)(s => if(s.isEmpty) Json.Null else j)).as[${tpe.get}]
              """
            } else {
              enumerator"""
                ${Pat.Var(term)} <- c.downField(${name}).as[${tpe.get}]
              """
            }
          } yield enum).to[List]
          q"""
          new Decoder[${Type.Name(clsName)}] {
            final def apply(c: HCursor): Decoder.Result[${Type.Name(clsName)}] =
              for {
                ..${enumerators}
              } yield ${Term.Name(clsName)}(..${accessors})
          }
          """
        }
        Target.pure(q"""
          implicit val ${suffixClsName("decode", clsName)} = $decVal
        """)

      case RenderDTOCompanion(clsName, deps, encoder, decoder) =>
        val extraImports: List[Import] = deps.map { term =>
          q"import ${term}._"
        }
        Target.pure(
          q"""object ${Term.Name(clsName)} {
            ..${
              extraImports :+
              encoder :+
              decoder
            }
          }
          """)
    }
  }

  object AliasProtocolTermInterp extends (AliasProtocolTerm ~> Target) {
    def apply[T](term: AliasProtocolTerm[T]): Target[T] = term match {
      case _ => ???
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm ~> Target) {
    def apply[T](term: ArrayProtocolTerm[T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        SwaggerUtil.modelMetaType(arr).flatMap {
          case SwaggerUtil.Resolved(tpe, dep, default) => Target.pure(tpe)
          case SwaggerUtil.Deferred(tpeName) => Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), s"Unresolved reference ${tpeName}")
          case SwaggerUtil.DeferredArray(tpeName) => Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[${tpe}]"), s"Unresolved reference ${tpeName}")
          case SwaggerUtil.DeferredMap(tpeName) => Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => t"IndexedSeq[Map[String, ${tpe}]]"), s"Unresolved reference ${tpeName}")
        }
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm ~> Target) {
    def apply[T](term: ProtocolSupportTerm[T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) => {
        for {
          entries <- definitions.map({
            case (clsName, impl: ModelImpl) if (Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined) =>
              Target.pure((clsName, SwaggerUtil.Resolved(Type.Name(clsName), None, None): SwaggerUtil.ResolvedType))
            case (clsName, definition) =>
              SwaggerUtil.modelMetaType(definition)
                .map(x => (clsName, x))
          }).sequenceU
          result <- SwaggerUtil.ResolvedType.resolve_(entries)
        } yield result.map { case (clsName, SwaggerUtil.Resolved(tpe, _, _)) =>
          PropMeta(clsName, tpe)
        }
      }

      case ProtocolImports() =>
        Target.pure(List(
          q"import io.circe._"
        , q"import io.circe.syntax._"
        , q"import io.circe.generic.semiauto._"
        , q"import cats.syntax.either._"
        ))

      case PackageObjectImports() =>
        Target.pure(List(
          q"import java.time._"
        , q"import io.circe.java8.{ time => j8time }"
        ))

      case PackageObjectContents() =>
        Target.pure(q"""
          val decodeLong = implicitly[Decoder[Long]]

          implicit def decodeInstant: Decoder[Instant] = j8time.decodeInstant.or(decodeLong.map(Instant.ofEpochMilli))
          implicit def decodeLocalDate: Decoder[LocalDate] = j8time.decodeLocalDateDefault.or(decodeInstant.map(_.atZone(ZoneOffset.UTC).toLocalDate))
          implicit def decodeOffsetDateTime: Decoder[OffsetDateTime] = j8time.decodeOffsetDateTimeDefault.or(decodeInstant.map(_.atZone(ZoneOffset.UTC).toOffsetDateTime))

          // Unused
          //implicit def decodeLocalDateTime: Decoder[Instant] = ???
          //implicit def decodeLocalTime: Decoder[Instant] = ???
          // implicit def decodeZonedDateTime: Decoder[Instant] = ???

          // Mirror
          implicit val encodeInstant = j8time.encodeInstant
          implicit val encodeLocalDateDefault = j8time.encodeLocalDateDefault
          implicit val encodeLocalDateTimeDefault = j8time.encodeLocalDateTimeDefault
          implicit val encodeLocalTimeDefault = j8time.encodeLocalTimeDefault
          implicit val encodeOffsetDateTimeDefault = j8time.encodeOffsetDateTimeDefault
          implicit val encodeZonedDateTimeDefault = j8time.encodeZonedDateTimeDefault
        """.stats)
    }
  }
}
