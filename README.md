# Macro Visit 

A macro-based generic visitor generator

[![Build Status](https://travis-ci.org/sangria-graphql-org/macro-visit.svg?branch=master)](https://travis-ci.org/sangria-graphql-org/macro-visit) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.sangria-graphql/macro-visit_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.sangria-graphql/macro-visit_2.11) [![License](http://img.shields.io/:license-Apache%202-brightgreen.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt) [![Join the chat at https://gitter.im/sangria-graphql/sangria](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/sangria-graphql/sangria?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

SBT Configuration:

```scala
libraryDependencies += "org.sangria-graphql" %% "macro-visit" % "0.1.2"
```

## Introduction

Writing visitor code can be quite tedious, especially if some nodes need to be transformed (in immutable way) along the way. This becomes even harder if performance is a concern and data structures are deeply recursive, so non tail-recursive approach is not an option. **macro-visit** provides very simple way to create type-safe visitor code for arbitrary sealed case class hierarchies. Generated visitor provides following features:

* **Non-recursive traversal**, which means all state is managed in the heap and you will not run into stack 
  overflow errors with deep recursive data structures. 
* **Optimised for performance and memory footprint**. Generated code for class hierarchy traversal is compiled into tight `while` loop.
* **Allows to transform** traversed object in immutable way. It generates code that uses case class's `copy` method to get updated 
  instance of object in most efficient way (if object has several changes, it would be copied only once)
* **Allows to break traversal at any given node and skip nodes**
* **Supports `List`, `Vector`, `Seq` and `Option` traversal**
  
Generated visitors can be very useful for traversing and transforming AST (Abstract Syntax Tree).        

Assuming that the base type is called `Ast`, following visitors are supported:

* `Visit(enter, leave)`, where `enter` and `leave` are `Ast ⇒ VisitorCommand` functions - it recursively visits all instances of `Ast` type
* `VisitAnyField(fn)` - it visits all fields with special type `S` (which is different from `Ast`) along the way 
* `VisitAnyFieldByName(fieldName, fn)` - it visits all fields with provided name and special type `S` (which is different from `Ast`) along the way
 
Following visitor commands are supported:
 
* `Skip`
* `Continue`
* `Break`
* `Transform(newValue, controlCommand)`
* `Delete`
* `DeleteAndBreak` 

## Example

Here is how basic usage looks like:

```scala
import sangria.visitor._

val res = visit[Ast](root,
  Visit[Field](f ⇒ if (f.name == "hex") VisitorCommand.Delete else VisitorCommand.Continue),
  Visit[IntValue](v ⇒ VisitorCommand.Transform(IntValue(v.value + 1))))
```

Macro will look for all subtypes of `Ast` and then will generate traversal code for it with provided visitors. `Visit` takes 2 functions 
as an argument `enter` and `leave` which both have `Ast ⇒ VisitorCommand` type.
 
Given following AST definitions:
 
```scala
sealed trait Ast

case class Vertex(name: String, edges: List[Edge], fields: Vector[Field] = Vector.empty) extends Ast
case class Edge(weight: Int, toVertex: Vertex) extends Ast
case class Field(name: String, value: Option[Value]) extends Ast

sealed trait Value extends Ast

case class StringValue(value: String) extends Value
case class IntValue(value: Int) extends Value
``` 

and sample data:

```scala
val root =
  Vertex("start", List(
    Edge(1, Vertex("colors", List(
      Edge(2, Vertex("RED", Nil, Vector(
        Field("intensity", Some(IntValue(123))),
        Field("hex", Some(StringValue("#FF0000")))
      ))),
      Edge(100, Vertex("GREEN", Nil, Vector(
        Field("hex", Some(StringValue("#00FF00")))
      )))
    ))),
    Edge(42, Vertex("books", List(
      Edge(1, Vertex("The Hobbit", Nil, Vector(
        Field("pages", Some(IntValue(320)))
      )))
    )))
  ))
```

The result of the transformation will look like this:

```scala
Vertex("start", List(
  Edge(1, Vertex("colors", List(
    Edge(2,Vertex("RED", Nil, Vector(
      Field("intensity", Some(IntValue(124)))))),
    Edge(100, Vertex("GREEN", Nil, Vector.empty))), Vector.empty)),
  Edge(42, Vertex("books", List(
    Edge(1, Vertex("The Hobbit", Nil, Vector(
      Field("pages", Some(IntValue(321))))))),
    Vector.empty))),
  Vector.empty)
```

## License

**macro-visit** is licensed under [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
