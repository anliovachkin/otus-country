package example

import io.circe.syntax._
import io.circe.{Decoder, Encoder, _}

import java.io.{FileOutputStream, PrintStream}
import scala.io.Source

object CountryApp extends App {

  case class Country(name: String, capital: String, region: String, area: Double)

  implicit val countryDecoder: Decoder[Country] = (cursor: HCursor) => {
    for {
      name <- cursor.downField("name").downField("official").as[String]
      capital <- cursor.downField("capital").as[List[String]].map {
        case head :: _ => head
        case Nil => ""
      }
      region <- cursor.downField("region").as[String]
      area <- cursor.downField("area").as[Double]
    } yield Country(name, capital, region, area)
  }

  implicit val countryEncoder: Encoder[Country] = (country: Country) => Json.obj(
    ("name", Json.fromString(country.name)),
    ("capital", Json.fromString(country.capital)),
    ("area", Json.fromDoubleOrNull(country.area))
  )

  def source = Source.fromURL(
    "https://raw.githubusercontent.com/mledoze/countries/master/countries.json"
  )

  val output = parser.decode[List[Country]](source.mkString) match {
    case Right(countries) =>
      val value = countries
        .filter(_.region == "Africa")
        .sortBy(_.area)(Ordering[Double].reverse)
        .take(10)
        .toArray
      value.asJson
    case Left(ex) => s"Can not parse JSON: $ex"
  }
  val outputFile = args(0)
  val fileOutputStream = new FileOutputStream(outputFile)
  val printStream = new PrintStream(fileOutputStream)
  printStream.println(output)
}