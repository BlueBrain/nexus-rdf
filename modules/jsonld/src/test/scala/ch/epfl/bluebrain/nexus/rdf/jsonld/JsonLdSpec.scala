package ch.epfl.bluebrain.nexus.rdf.jsonld

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.RdfSpec
import ch.epfl.bluebrain.nexus.rdf.jsonld.JsonLd._
import ch.epfl.bluebrain.nexus.rdf.jsonld.syntax._
import ch.epfl.bluebrain.nexus.rdf.syntax.all._
import io.circe.literal._

import scala.util.{Failure, Success, Try}

class JsonLdSpec extends RdfSpec {

  "JsonLd" should {
    "resolve contexts" when {
      "no @context is present" in {
        val json = json"""{"someKey": "value"}"""

        json
          .resolveContext[Try](_ => Failure(new IllegalArgumentException))
          .value
          .success
          .value
          .rightValue shouldEqual
          json"""
          {
            "@context": {},
            "someKey": "value"
          }
          """
      }

      "context is an IRI" in {
        val contextUri = url"http://context.example.com"

        val json =
          json"""
          {
            "@context": ${contextUri.asString}
          }
          """

        val contextValue =
          json"""
          {
            "@context": {
              "schema": "http://schema.org/"
            }
          }       
          """

        json
          .resolveContext[Try] {
            case `contextUri` => Success(Some(contextValue))
            case _            => Failure(new IllegalArgumentException)
          }
          .value
          .success
          .value
          .rightValue shouldEqual (json deepMerge contextValue)
      }

      "context is a JSON array containing IRI" in {
        val contextUri = url"http://context.example.com"

        val json =
          json"""
          {
            "@context": [
              {
                "xsd": "http://www.w3.org/2001/XMLSchema#"
              },
              ${contextUri.asString}
            ]
          }                
          """

        val contextValue =
          json"""
          {
            "@context": {
              "schema": "http://schema.org/"
            }
          }       
          """

        val expected =
          json"""
          {
            "@context": {
              "xsd": "http://www.w3.org/2001/XMLSchema#",
              "schema": "http://schema.org/"
            }
          }               
          """

        json
          .resolveContext[Try] {
            case `contextUri` => Success(Some(contextValue))
            case _            => Failure(new IllegalArgumentException)
          }
          .value
          .success
          .value
          .rightValue shouldEqual expected
      }
      "contexts are nested" in {
        val contextUri1 = url"http://context1.example.com"
        val contextUri2 = url"http://context2.example.com"
        val json =
          json"""
          {
            "@context": [
              {
                "xsd": "http://www.w3.org/2001/XMLSchema#"
              },
              ${contextUri1.asString}
            ]
          }                
          """

        val contextValue1 =
          json"""
          {
            "@context": [
              {
                "schema": "http://schema.org/"
              },
              ${contextUri2.asString}
            ]
          }                
          """

        val contextValue2 =
          json"""
          {
            "@context": {
              "nxv": "https://bluebrain.github.io/nexus/vocabulary/"
            }
          }       
          """

        val expected =
          json"""
          {
            "@context" : {
              "xsd": "http://www.w3.org/2001/XMLSchema#",
              "schema": "http://schema.org/",
              "nxv": "https://bluebrain.github.io/nexus/vocabulary/"
            }
          }
          """

        json
          .resolveContext[Try] {
            case `contextUri1` => Success(Some(contextValue1))
            case `contextUri2` => Success(Some(contextValue2))
            case _             => Failure(new IllegalArgumentException)
          }
          .value
          .success
          .value
          .rightValue shouldEqual expected
      }

      "there are scoped contexts" in {
        val contextUri = url"http://context.example.com"
        val json =
          json"""
           {
             "@context": {
               "xsd": "http://www.default-xsd.com#"
             },
             "key": {
               "@context": {
                 "xsd": "http://www.w3.org/2001/XMLSchema#"
               },
               "key-value1": "value1",
               "key2": {
                 "@context": ${contextUri.asString},
                 "key-value2": "value2"
               }
             }
           }
           """

        val contextValue =
          json"""
          {
            "@context": {
              "schema": "http://schema.org/"
            }
          }
          """
        val expected =
          json"""
          {
            "@context": {
              "schema": "http://schema.org/",
              "xsd": "http://www.w3.org/2001/XMLSchema#"
            },
            "key": {
              "key-value1": "value1",
              "key2": {
                "key-value2": "value2"
              }
            }
          }
          """

        json
          .resolveContext[Try] {
            case `contextUri` => Success(Some(contextValue))
            case _            => Failure(new IllegalArgumentException)
          }
          .value
          .success
          .value
          .rightValue shouldEqual expected
      }
    }

    "fail to resolve" when {
      "there are circular dependencies in contexts" in {
        val contextUri1 = url"http://context1.example.com"
        val contextUri2 = url"http://context2.example.com"
        val json =
          json"""
          {
            "@context": [
              {
                "xsd": "http://www.w3.org/2001/XMLSchema#"
              },
              ${contextUri1.asString}
            ]
          }                
          """

        val contextValue1 =
          json"""
          {
            "@context": [
              {
                "schema": "http://schema.org/"
              },
              ${contextUri2.asString}
            ]
          }                
          """

        val contextValue2 =
          json"""
          {
            "@context": ${contextUri1.asString}
          }       
          """

        json
          .resolveContext[Try] {
            case `contextUri1` => Success(Some(contextValue1))
            case `contextUri2` => Success(Some(contextValue2))
            case _             => Failure(new IllegalArgumentException)
          }
          .value
          .success
          .value
          .leftValue shouldEqual CircularContextDependency(List(contextUri1, contextUri2, contextUri1))
      }

      "context reference is not a string" in {
        val json =
          json"""
          {
            "@context": 1
          }
          """

        json
          .resolveContext[Try](_ => Failure(new IllegalArgumentException))
          .value
          .success
          .value
          .leftValue shouldEqual IllegalContextValue("1")

      }
      "context reference is an invalid IRI" in {
        val json =
          json"""
          {
            "@context": "notAContext"
          }
          """

        json
          .resolveContext[Try](_ => Failure(new IllegalArgumentException))
          .value
          .success
          .value
          .leftValue shouldEqual IllegalContextValue("notAContext")

      }
      "context is not found" in {
        val contextUri = url"http://context.example.com"

        val json =
          json"""
          {
            "@context": ${contextUri.asString}
          }
          """
        json
          .resolveContext[Try] {
            case `contextUri` => Success(None)
            case _            => Failure(new IllegalArgumentException)
          }
          .value
          .success
          .value
          .leftValue shouldEqual ContextNotFound(contextUri)
      }
    }

    "return context value" when {
      "json is an array" in {
        json"""
         [
           {
             "@context": {
               "xsd": "http://www.w3.org/2001/XMLSchema#"
             }
           },
           {
             "@context": {
               "schema": "http://schema.org/"
             }
           }
         ]
         """.contextValue shouldEqual
          json"""
          {
            "xsd": "http://www.w3.org/2001/XMLSchema#",
            "schema": "http://schema.org/"
          }
          """
      }
    }
    "remove nested keys" when {
      "json contains nested keys" in {
        removeNestedKeys(
          json"""
          {
            "@context": "http://example.com/1",
            "field1": {
              "@context": "http://example.com/2",
              "field2": {
                "field3": "Other text"
              }
            },
            "field3": [
              {
                "@context": {
                  "xsd": "http://www.w3.org/2001/XMLSchema#"
                }
              },
              {
                "@context": "http://example.com/3",
                "field4": 2
              }
            ],
            "field5": "Some text"
          }
         """,
          "@context"
        ) shouldEqual
          json"""
          {
            "field1" : {
              "field2" : {
                "field3" : "Other text"
              }
            },
            "field3" : [
              {
                "field4" : 2
              }
            ],
            "field5" : "Some text"
          }
          """
      }
    }
  }
}
