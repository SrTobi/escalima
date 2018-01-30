package escalima.ast

import upickle.Js


sealed abstract class Node(val loc: Option[SourceLocation]) {
	def toJSON: Js.Value
}

object Node {
	def unapply(node: Node): Option[Option[SourceLocation]] = Some(node.loc)

	def from(src: Js.Value): Node = src("type").str match {
		case "Program" => Program.from(src)
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "FunctionExpression" => FunctionExpression.from(src)
		case "ExpressionStatement" if !src.obj.contains("directive") => ExpressionStatement.from(src)
		case "ExpressionStatement" if src.obj.contains("directive") => Directive.from(src)
		case "BlockStatement" => BlockStatement.from(src)
		case "EmptyStatement" => EmptyStatement.from(src)
		case "DebuggerStatement" => DebuggerStatement.from(src)
		case "WithStatement" => WithStatement.from(src)
		case "ReturnStatement" => ReturnStatement.from(src)
		case "LabeledStatement" => LabeledStatement.from(src)
		case "BreakStatement" => BreakStatement.from(src)
		case "ContinueStatement" => ContinueStatement.from(src)
		case "IfStatement" => IfStatement.from(src)
		case "SwitchStatement" => SwitchStatement.from(src)
		case "ThrowStatement" => ThrowStatement.from(src)
		case "TryStatement" => TryStatement.from(src)
		case "WhileStatement" => WhileStatement.from(src)
		case "DoWhileStatement" => DoWhileStatement.from(src)
		case "ForStatement" => ForStatement.from(src)
		case "ForInStatement" => ForInStatement.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "SwitchCase" => SwitchCase.from(src)
		case "CatchClause" => CatchClause.from(src)
		case "VariableDeclarator" => VariableDeclarator.from(src)
		case "Identifier" => Identifier.from(src)
		case "Literal" if src.obj.contains("regex") => RegExpLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.True.type] || src.obj("value").isInstanceOf[Js.False.type] => BooleanLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Num] => NumberLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Str] && !src.obj.contains("regex") => StringLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Null.type] => NullLiteral.from(src)
		case "ThisExpression" => ThisExpression.from(src)
		case "ArrayExpression" => ArrayExpression.from(src)
		case "ObjectExpression" => ObjectExpression.from(src)
		case "UnaryExpression" => UnaryExpression.from(src)
		case "UpdateExpression" => UpdateExpression.from(src)
		case "BinaryExpression" => BinaryExpression.from(src)
		case "AssignmentExpression" => AssignmentExpression.from(src)
		case "LogicalExpression" => LogicalExpression.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case "ConditionalExpression" => ConditionalExpression.from(src)
		case "CallExpression" => CallExpression.from(src)
		case "NewExpression" => NewExpression.from(src)
		case "SequenceExpression" => SequenceExpression.from(src)
		case "Property" => Property.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Node")
	}
}

sealed class SourceLocation(val source: Option[String], val start: Position, val end: Position) {
	def toJSON: Js.Value = Js.Obj(
			"source" -> this.source.map(inner => Js.Str(inner)).getOrElse(Js.Null),
			"start" -> this.start.toJSON,
			"end" -> this.end.toJSON
		)
}

object SourceLocation {
	def apply(source: Option[String], start: Position, end: Position): SourceLocation = new SourceLocation(source, start, end)
	def unapply(sourceLocation: SourceLocation): Option[(Option[String], Position, Position)] = Some((sourceLocation.source, sourceLocation.start, sourceLocation.end))

	def from(src: Js.Value): SourceLocation = {
		val _obj = src.obj

		new SourceLocation(
			_obj.get("source").map(inner => inner.str.toString),
			Position.from(_obj("start")),
			Position.from(_obj("end"))
		)
	}
}

sealed class Position(val line: Int, val column: Int) {
	def toJSON: Js.Value = Js.Obj(
			"line" -> Js.Num(this.line),
			"column" -> Js.Num(this.column)
		)
}

object Position {
	def apply(line: Int, column: Int): Position = new Position(line, column)
	def unapply(position: Position): Option[(Int, Int)] = Some((position.line, position.column))

	def from(src: Js.Value): Position = {
		val _obj = src.obj

		new Position(
			_obj("line").num.toInt,
			_obj("column").num.toInt
		)
	}
}

sealed class Identifier(val name: String, loc: Option[SourceLocation]) extends Node(loc) with Expression with Pattern with PropertyKey {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Identifier"),
			"name" -> Js.Str(this.name),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Identifier {
	def apply(name: String, loc: Option[SourceLocation]): Identifier = new Identifier(name, loc)
	def unapply(identifier: Identifier): Option[String] = Some(identifier.name)

	def from(src: Js.Value): Identifier = {
		val _obj = src.obj
		assert(_obj("type").str == "Identifier")

		new Identifier(
			_obj("name").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class Literal(val raw: String, loc: Option[SourceLocation]) extends Node(loc) with Expression with PropertyKey {
	def toJSON: Js.Value
}

object Literal {
	def unapply(literal: Literal): Option[String] = Some(literal.raw)

	def from(src: Js.Value): Literal = src("type").str match {
		case "Literal" if src.obj.contains("regex") => RegExpLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.True.type] || src.obj("value").isInstanceOf[Js.False.type] => BooleanLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Num] => NumberLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Str] && !src.obj.contains("regex") => StringLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Null.type] => NullLiteral.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Literal")
	}
}

sealed class Regex(val pattern: String, val flags: String) {
	def toJSON: Js.Value = Js.Obj(
			"pattern" -> Js.Str(this.pattern),
			"flags" -> Js.Str(this.flags)
		)
}

object Regex {
	def apply(pattern: String, flags: String): Regex = new Regex(pattern, flags)
	def unapply(regex: Regex): Option[(String, String)] = Some((regex.pattern, regex.flags))

	def from(src: Js.Value): Regex = {
		val _obj = src.obj

		new Regex(
			_obj("pattern").str.toString,
			_obj("flags").str.toString
		)
	}
}

sealed class RegExpLiteral(val regex: Regex, raw: String, loc: Option[SourceLocation]) extends Literal(raw, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Literal"),
			"regex" -> this.regex.toJSON,
			"raw" -> Js.Str(this.raw),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object RegExpLiteral {
	def apply(regex: Regex, raw: String, loc: Option[SourceLocation]): RegExpLiteral = new RegExpLiteral(regex, raw, loc)
	def unapply(regExpLiteral: RegExpLiteral): Option[Regex] = Some(regExpLiteral.regex)

	def from(src: Js.Value): RegExpLiteral = {
		val _obj = src.obj
		assert(_obj("type").str == "Literal")
		assert(_obj.contains("regex"))

		new RegExpLiteral(
			Regex.from(_obj("regex")),
			_obj("raw").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Program(val body: Seq[Statement], loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Program"),
			"body" -> Js.Arr(this.body.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Program {
	def apply(body: Seq[Statement], loc: Option[SourceLocation]): Program = new Program(body, loc)
	def unapply(program: Program): Option[Seq[Statement]] = Some(program.body)

	def from(src: Js.Value): Program = {
		val _obj = src.obj
		assert(_obj("type").str == "Program")

		new Program(
			_obj("body").arr.map(elem => Statement.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Function(val id: Option[Identifier], val params: Seq[Pattern], val body: FunctionBody, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Function {
	def apply(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, loc: Option[SourceLocation]): Function = new Function(id, params, body, loc)
	def unapply(function: Function): Option[(Option[Identifier], Seq[Pattern], FunctionBody)] = Some((function.id, function.params, function.body))

	def from(src: Js.Value): Function = {
		val _obj = src.obj

		new Function(
			_obj.get("id").map(inner => Identifier.from(inner)),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			FunctionBody.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Statement {
	this: Node =>

	def toJSON: Js.Value
}

object Statement {
	def from(src: Js.Value): Statement = src("type").str match {
		case "ExpressionStatement" if !src.obj.contains("directive") => ExpressionStatement.from(src)
		case "ExpressionStatement" if src.obj.contains("directive") => Directive.from(src)
		case "BlockStatement" => BlockStatement.from(src)
		case "EmptyStatement" => EmptyStatement.from(src)
		case "DebuggerStatement" => DebuggerStatement.from(src)
		case "WithStatement" => WithStatement.from(src)
		case "ReturnStatement" => ReturnStatement.from(src)
		case "LabeledStatement" => LabeledStatement.from(src)
		case "BreakStatement" => BreakStatement.from(src)
		case "ContinueStatement" => ContinueStatement.from(src)
		case "IfStatement" => IfStatement.from(src)
		case "SwitchStatement" => SwitchStatement.from(src)
		case "ThrowStatement" => ThrowStatement.from(src)
		case "TryStatement" => TryStatement.from(src)
		case "WhileStatement" => WhileStatement.from(src)
		case "DoWhileStatement" => DoWhileStatement.from(src)
		case "ForStatement" => ForStatement.from(src)
		case "ForInStatement" => ForInStatement.from(src)
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Statement")
	}
}

sealed class ExpressionStatement(val expression: Expression, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ExpressionStatement"),
			"expression" -> this.expression.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ExpressionStatement {
	def apply(expression: Expression, loc: Option[SourceLocation]): ExpressionStatement = new ExpressionStatement(expression, loc)
	def unapply(expressionStatement: ExpressionStatement): Option[Expression] = Some(expressionStatement.expression)

	def from(src: Js.Value): ExpressionStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ExpressionStatement")
		assert(!_obj.contains("directive"))

		new ExpressionStatement(
			Expression.from(_obj("expression")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Directive(val expression: Literal, val directive: String, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ExpressionStatement"),
			"expression" -> this.expression.toJSON,
			"directive" -> Js.Str(this.directive),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Directive {
	def apply(expression: Literal, directive: String, loc: Option[SourceLocation]): Directive = new Directive(expression, directive, loc)
	def unapply(directive: Directive): Option[(Literal, String)] = Some((directive.expression, directive.directive))

	def from(src: Js.Value): Directive = {
		val _obj = src.obj
		assert(_obj("type").str == "ExpressionStatement")
		assert(_obj.contains("directive"))

		new Directive(
			Literal.from(_obj("expression")),
			_obj("directive").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class BlockStatement(val body: Seq[Statement], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("BlockStatement"),
			"body" -> Js.Arr(this.body.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object BlockStatement {
	def apply(body: Seq[Statement], loc: Option[SourceLocation]): BlockStatement = new BlockStatement(body, loc)
	def unapply(blockStatement: BlockStatement): Option[Seq[Statement]] = Some(blockStatement.body)

	def from(src: Js.Value): BlockStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "BlockStatement")

		new BlockStatement(
			_obj("body").arr.map(elem => Statement.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class FunctionBody(val body: Seq[Statement]) {
	def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("BlockStatement"),
			"body" -> Js.Arr(this.body.map(inner => inner.toJSON): _*)
		)
}

object FunctionBody {
	def apply(body: Seq[Statement]): FunctionBody = new FunctionBody(body)
	def unapply(functionBody: FunctionBody): Option[Seq[Statement]] = Some(functionBody.body)

	def from(src: Js.Value): FunctionBody = {
		val _obj = src.obj
		assert(_obj("type").str == "BlockStatement")

		new FunctionBody(
			_obj("body").arr.map(elem => Statement.from(elem))
		)
	}
}

sealed class EmptyStatement(loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("EmptyStatement"),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object EmptyStatement {
	def apply(loc: Option[SourceLocation]): EmptyStatement = new EmptyStatement(loc)

	def from(src: Js.Value): EmptyStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "EmptyStatement")

		new EmptyStatement(
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class DebuggerStatement(loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("DebuggerStatement"),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object DebuggerStatement {
	def apply(loc: Option[SourceLocation]): DebuggerStatement = new DebuggerStatement(loc)

	def from(src: Js.Value): DebuggerStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "DebuggerStatement")

		new DebuggerStatement(
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class WithStatement(val obj: Expression, val body: Statement, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("WithStatement"),
			"object" -> this.obj.toJSON,
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object WithStatement {
	def apply(obj: Expression, body: Statement, loc: Option[SourceLocation]): WithStatement = new WithStatement(obj, body, loc)
	def unapply(withStatement: WithStatement): Option[(Expression, Statement)] = Some((withStatement.obj, withStatement.body))

	def from(src: Js.Value): WithStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "WithStatement")

		new WithStatement(
			Expression.from(_obj("object")),
			Statement.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ReturnStatement(val argument: Option[Expression], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ReturnStatement"),
			"argument" -> this.argument.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ReturnStatement {
	def apply(argument: Option[Expression], loc: Option[SourceLocation]): ReturnStatement = new ReturnStatement(argument, loc)
	def unapply(returnStatement: ReturnStatement): Option[Option[Expression]] = Some(returnStatement.argument)

	def from(src: Js.Value): ReturnStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ReturnStatement")

		new ReturnStatement(
			_obj.get("argument").map(inner => Expression.from(inner)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class LabeledStatement(val label: Identifier, val body: Statement, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("LabeledStatement"),
			"label" -> this.label.toJSON,
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object LabeledStatement {
	def apply(label: Identifier, body: Statement, loc: Option[SourceLocation]): LabeledStatement = new LabeledStatement(label, body, loc)
	def unapply(labeledStatement: LabeledStatement): Option[(Identifier, Statement)] = Some((labeledStatement.label, labeledStatement.body))

	def from(src: Js.Value): LabeledStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "LabeledStatement")

		new LabeledStatement(
			Identifier.from(_obj("label")),
			Statement.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class BreakStatement(val label: Option[Identifier], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("BreakStatement"),
			"label" -> this.label.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object BreakStatement {
	def apply(label: Option[Identifier], loc: Option[SourceLocation]): BreakStatement = new BreakStatement(label, loc)
	def unapply(breakStatement: BreakStatement): Option[Option[Identifier]] = Some(breakStatement.label)

	def from(src: Js.Value): BreakStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "BreakStatement")

		new BreakStatement(
			_obj.get("label").map(inner => Identifier.from(inner)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ContinueStatement(val label: Option[Identifier], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ContinueStatement"),
			"label" -> this.label.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ContinueStatement {
	def apply(label: Option[Identifier], loc: Option[SourceLocation]): ContinueStatement = new ContinueStatement(label, loc)
	def unapply(continueStatement: ContinueStatement): Option[Option[Identifier]] = Some(continueStatement.label)

	def from(src: Js.Value): ContinueStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ContinueStatement")

		new ContinueStatement(
			_obj.get("label").map(inner => Identifier.from(inner)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class IfStatement(val test: Expression, val consequent: Statement, val alternate: Option[Statement], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("IfStatement"),
			"test" -> this.test.toJSON,
			"consequent" -> this.consequent.toJSON,
			"alternate" -> this.alternate.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object IfStatement {
	def apply(test: Expression, consequent: Statement, alternate: Option[Statement], loc: Option[SourceLocation]): IfStatement = new IfStatement(test, consequent, alternate, loc)
	def unapply(ifStatement: IfStatement): Option[(Expression, Statement, Option[Statement])] = Some((ifStatement.test, ifStatement.consequent, ifStatement.alternate))

	def from(src: Js.Value): IfStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "IfStatement")

		new IfStatement(
			Expression.from(_obj("test")),
			Statement.from(_obj("consequent")),
			_obj.get("alternate").map(inner => Statement.from(inner)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class SwitchStatement(val discriminant: Expression, val cases: Seq[SwitchCase], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("SwitchStatement"),
			"discriminant" -> this.discriminant.toJSON,
			"cases" -> Js.Arr(this.cases.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object SwitchStatement {
	def apply(discriminant: Expression, cases: Seq[SwitchCase], loc: Option[SourceLocation]): SwitchStatement = new SwitchStatement(discriminant, cases, loc)
	def unapply(switchStatement: SwitchStatement): Option[(Expression, Seq[SwitchCase])] = Some((switchStatement.discriminant, switchStatement.cases))

	def from(src: Js.Value): SwitchStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "SwitchStatement")

		new SwitchStatement(
			Expression.from(_obj("discriminant")),
			_obj("cases").arr.map(elem => SwitchCase.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class SwitchCase(val test: Option[Expression], val consequent: Seq[Statement], loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("SwitchCase"),
			"test" -> this.test.map(inner => inner.toJSON).getOrElse(Js.Null),
			"consequent" -> Js.Arr(this.consequent.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object SwitchCase {
	def apply(test: Option[Expression], consequent: Seq[Statement], loc: Option[SourceLocation]): SwitchCase = new SwitchCase(test, consequent, loc)
	def unapply(switchCase: SwitchCase): Option[(Option[Expression], Seq[Statement])] = Some((switchCase.test, switchCase.consequent))

	def from(src: Js.Value): SwitchCase = {
		val _obj = src.obj
		assert(_obj("type").str == "SwitchCase")

		new SwitchCase(
			_obj.get("test").map(inner => Expression.from(inner)),
			_obj("consequent").arr.map(elem => Statement.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ThrowStatement(val argument: Expression, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ThrowStatement"),
			"argument" -> this.argument.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ThrowStatement {
	def apply(argument: Expression, loc: Option[SourceLocation]): ThrowStatement = new ThrowStatement(argument, loc)
	def unapply(throwStatement: ThrowStatement): Option[Expression] = Some(throwStatement.argument)

	def from(src: Js.Value): ThrowStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ThrowStatement")

		new ThrowStatement(
			Expression.from(_obj("argument")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class TryStatement(val block: BlockStatement, val handler: Option[CatchClause], val finalizer: Option[BlockStatement], loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("TryStatement"),
			"block" -> this.block.toJSON,
			"handler" -> this.handler.map(inner => inner.toJSON).getOrElse(Js.Null),
			"finalizer" -> this.finalizer.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object TryStatement {
	def apply(block: BlockStatement, handler: Option[CatchClause], finalizer: Option[BlockStatement], loc: Option[SourceLocation]): TryStatement = new TryStatement(block, handler, finalizer, loc)
	def unapply(tryStatement: TryStatement): Option[(BlockStatement, Option[CatchClause], Option[BlockStatement])] = Some((tryStatement.block, tryStatement.handler, tryStatement.finalizer))

	def from(src: Js.Value): TryStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "TryStatement")

		new TryStatement(
			BlockStatement.from(_obj("block")),
			_obj.get("handler").map(inner => CatchClause.from(inner)),
			_obj.get("finalizer").map(inner => BlockStatement.from(inner)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class CatchClause(val param: Pattern, val body: BlockStatement, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("CatchClause"),
			"param" -> this.param.toJSON,
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object CatchClause {
	def apply(param: Pattern, body: BlockStatement, loc: Option[SourceLocation]): CatchClause = new CatchClause(param, body, loc)
	def unapply(catchClause: CatchClause): Option[(Pattern, BlockStatement)] = Some((catchClause.param, catchClause.body))

	def from(src: Js.Value): CatchClause = {
		val _obj = src.obj
		assert(_obj("type").str == "CatchClause")

		new CatchClause(
			Pattern.from(_obj("param")),
			BlockStatement.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class WhileStatement(val test: Expression, val body: Statement, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("WhileStatement"),
			"test" -> this.test.toJSON,
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object WhileStatement {
	def apply(test: Expression, body: Statement, loc: Option[SourceLocation]): WhileStatement = new WhileStatement(test, body, loc)
	def unapply(whileStatement: WhileStatement): Option[(Expression, Statement)] = Some((whileStatement.test, whileStatement.body))

	def from(src: Js.Value): WhileStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "WhileStatement")

		new WhileStatement(
			Expression.from(_obj("test")),
			Statement.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class DoWhileStatement(val body: Statement, val test: Expression, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("DoWhileStatement"),
			"body" -> this.body.toJSON,
			"test" -> this.test.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object DoWhileStatement {
	def apply(body: Statement, test: Expression, loc: Option[SourceLocation]): DoWhileStatement = new DoWhileStatement(body, test, loc)
	def unapply(doWhileStatement: DoWhileStatement): Option[(Statement, Expression)] = Some((doWhileStatement.body, doWhileStatement.test))

	def from(src: Js.Value): DoWhileStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "DoWhileStatement")

		new DoWhileStatement(
			Statement.from(_obj("body")),
			Expression.from(_obj("test")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ForStatement(val init: Option[ForInit], val test: Option[Expression], val update: Option[Expression], val body: Statement, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ForStatement"),
			"init" -> this.init.map(inner => inner.toJSON).getOrElse(Js.Null),
			"test" -> this.test.map(inner => inner.toJSON).getOrElse(Js.Null),
			"update" -> this.update.map(inner => inner.toJSON).getOrElse(Js.Null),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ForStatement {
	def apply(init: Option[ForInit], test: Option[Expression], update: Option[Expression], body: Statement, loc: Option[SourceLocation]): ForStatement = new ForStatement(init, test, update, body, loc)
	def unapply(forStatement: ForStatement): Option[(Option[ForInit], Option[Expression], Option[Expression], Statement)] = Some((forStatement.init, forStatement.test, forStatement.update, forStatement.body))

	def from(src: Js.Value): ForStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ForStatement")

		new ForStatement(
			_obj.get("init").map(inner => ForInit.from(inner)),
			_obj.get("test").map(inner => Expression.from(inner)),
			_obj.get("update").map(inner => Expression.from(inner)),
			Statement.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ForInStatement(val left: ForInTarget, val right: Expression, val body: Statement, loc: Option[SourceLocation]) extends Node(loc) with Statement {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ForInStatement"),
			"left" -> this.left.toJSON,
			"right" -> this.right.toJSON,
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ForInStatement {
	def apply(left: ForInTarget, right: Expression, body: Statement, loc: Option[SourceLocation]): ForInStatement = new ForInStatement(left, right, body, loc)
	def unapply(forInStatement: ForInStatement): Option[(ForInTarget, Expression, Statement)] = Some((forInStatement.left, forInStatement.right, forInStatement.body))

	def from(src: Js.Value): ForInStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ForInStatement")

		new ForInStatement(
			ForInTarget.from(_obj("left")),
			Expression.from(_obj("right")),
			Statement.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Declaration extends Statement {
	this: Node =>

	def toJSON: Js.Value
}

object Declaration {
	def from(src: Js.Value): Declaration = src("type").str match {
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Declaration")
	}
}

sealed class FunctionDeclaration(val actualId: Identifier, params: Seq[Pattern], body: FunctionBody, loc: Option[SourceLocation]) extends Function(Some(actualId), params, body, loc) with Declaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("FunctionDeclaration"),
			"id" -> this.actualId.toJSON,
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object FunctionDeclaration {
	def apply(actualId: Identifier, params: Seq[Pattern], body: FunctionBody, loc: Option[SourceLocation]): FunctionDeclaration = new FunctionDeclaration(actualId, params, body, loc)
	def unapply(functionDeclaration: FunctionDeclaration): Option[Identifier] = Some(functionDeclaration.actualId)

	def from(src: Js.Value): FunctionDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "FunctionDeclaration")

		new FunctionDeclaration(
			Identifier.from(_obj("id")),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			FunctionBody.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class VariableDeclaration(val declarations: Seq[VariableDeclarator], loc: Option[SourceLocation]) extends Node(loc) with Declaration with ForInit with ForInTarget {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("VariableDeclaration"),
			"declarations" -> Js.Arr(this.declarations.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object VariableDeclaration {
	def apply(declarations: Seq[VariableDeclarator], loc: Option[SourceLocation]): VariableDeclaration = new VariableDeclaration(declarations, loc)
	def unapply(variableDeclaration: VariableDeclaration): Option[Seq[VariableDeclarator]] = Some(variableDeclaration.declarations)

	def from(src: Js.Value): VariableDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "VariableDeclaration")

		new VariableDeclaration(
			_obj("declarations").arr.map(elem => VariableDeclarator.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class VariableDeclarator(val id: Pattern, val init: Option[Expression], loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("VariableDeclarator"),
			"id" -> this.id.toJSON,
			"init" -> this.init.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object VariableDeclarator {
	def apply(id: Pattern, init: Option[Expression], loc: Option[SourceLocation]): VariableDeclarator = new VariableDeclarator(id, init, loc)
	def unapply(variableDeclarator: VariableDeclarator): Option[(Pattern, Option[Expression])] = Some((variableDeclarator.id, variableDeclarator.init))

	def from(src: Js.Value): VariableDeclarator = {
		val _obj = src.obj
		assert(_obj("type").str == "VariableDeclarator")

		new VariableDeclarator(
			Pattern.from(_obj("id")),
			_obj.get("init").map(inner => Expression.from(inner)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Expression extends ForInit with AssignmentTarget {
	this: Node =>

	def toJSON: Js.Value
}

object Expression {
	def from(src: Js.Value): Expression = src("type").str match {
		case "Identifier" => Identifier.from(src)
		case "Literal" if src.obj.contains("regex") => RegExpLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.True.type] || src.obj("value").isInstanceOf[Js.False.type] => BooleanLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Num] => NumberLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Str] && !src.obj.contains("regex") => StringLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Null.type] => NullLiteral.from(src)
		case "ThisExpression" => ThisExpression.from(src)
		case "ArrayExpression" => ArrayExpression.from(src)
		case "ObjectExpression" => ObjectExpression.from(src)
		case "FunctionExpression" => FunctionExpression.from(src)
		case "UnaryExpression" => UnaryExpression.from(src)
		case "UpdateExpression" => UpdateExpression.from(src)
		case "BinaryExpression" => BinaryExpression.from(src)
		case "AssignmentExpression" => AssignmentExpression.from(src)
		case "LogicalExpression" => LogicalExpression.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case "ConditionalExpression" => ConditionalExpression.from(src)
		case "CallExpression" => CallExpression.from(src)
		case "NewExpression" => NewExpression.from(src)
		case "SequenceExpression" => SequenceExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Expression")
	}
}

sealed class ThisExpression(loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ThisExpression"),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ThisExpression {
	def apply(loc: Option[SourceLocation]): ThisExpression = new ThisExpression(loc)

	def from(src: Js.Value): ThisExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ThisExpression")

		new ThisExpression(
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ArrayExpression(val elements: Seq[Option[Expression]], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ArrayExpression"),
			"elements" -> Js.Arr(this.elements.map(inner => inner.map(inner => inner.toJSON).getOrElse(Js.Null)): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ArrayExpression {
	def apply(elements: Seq[Option[Expression]], loc: Option[SourceLocation]): ArrayExpression = new ArrayExpression(elements, loc)
	def unapply(arrayExpression: ArrayExpression): Option[Seq[Option[Expression]]] = Some(arrayExpression.elements)

	def from(src: Js.Value): ArrayExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ArrayExpression")

		new ArrayExpression(
			_obj("elements").arr.map(elem => Option(elem).map(inner => Expression.from(inner))),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ObjectExpression(val properties: Seq[Property], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ObjectExpression"),
			"properties" -> Js.Arr(this.properties.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ObjectExpression {
	def apply(properties: Seq[Property], loc: Option[SourceLocation]): ObjectExpression = new ObjectExpression(properties, loc)
	def unapply(objectExpression: ObjectExpression): Option[Seq[Property]] = Some(objectExpression.properties)

	def from(src: Js.Value): ObjectExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ObjectExpression")

		new ObjectExpression(
			_obj("properties").arr.map(elem => Property.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Property(val key: PropertyKey, val value: Expression, val kind: PropertyKind, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Property"),
			"key" -> this.key.toJSON,
			"value" -> this.value.toJSON,
			"kind" -> this.kind.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Property {
	def apply(key: PropertyKey, value: Expression, kind: PropertyKind, loc: Option[SourceLocation]): Property = new Property(key, value, kind, loc)
	def unapply(property: Property): Option[(PropertyKey, Expression, PropertyKind)] = Some((property.key, property.value, property.kind))

	def from(src: Js.Value): Property = {
		val _obj = src.obj
		assert(_obj("type").str == "Property")

		new Property(
			PropertyKey.from(_obj("key")),
			Expression.from(_obj("value")),
			PropertyKind.from(_obj("kind")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class FunctionExpression(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, loc: Option[SourceLocation]) extends Function(id, params, body, loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("FunctionExpression"),
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object FunctionExpression {
	def apply(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, loc: Option[SourceLocation]): FunctionExpression = new FunctionExpression(id, params, body, loc)

	def from(src: Js.Value): FunctionExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "FunctionExpression")

		new FunctionExpression(
			_obj.get("id").map(inner => Identifier.from(inner)),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			FunctionBody.from(_obj("body")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class UnaryExpression(val operator: UnaryOperator, val prefix: Boolean, val argument: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("UnaryExpression"),
			"operator" -> this.operator.toJSON,
			"prefix" -> (if (this.prefix) Js.True else Js.False),
			"argument" -> this.argument.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object UnaryExpression {
	def apply(operator: UnaryOperator, prefix: Boolean, argument: Expression, loc: Option[SourceLocation]): UnaryExpression = new UnaryExpression(operator, prefix, argument, loc)
	def unapply(unaryExpression: UnaryExpression): Option[(UnaryOperator, Boolean, Expression)] = Some((unaryExpression.operator, unaryExpression.prefix, unaryExpression.argument))

	def from(src: Js.Value): UnaryExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "UnaryExpression")

		new UnaryExpression(
			UnaryOperator.from(_obj("operator")),
			_obj("prefix").isInstanceOf[Js.True.type],
			Expression.from(_obj("argument")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class UnaryOperator {
	def toJSON: Js.Str
}

object UnaryOperator {
	final case object `-` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("-")
		override def toString: String ="UnaryOperator[-]"
	}

	final case object `+` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("+")
		override def toString: String ="UnaryOperator[+]"
	}

	final case object `!` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("!")
		override def toString: String ="UnaryOperator[!]"
	}

	final case object `~` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("~")
		override def toString: String ="UnaryOperator[~]"
	}

	final case object `typeof` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("typeof")
		override def toString: String ="UnaryOperator[typeof]"
	}

	final case object `void` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("void")
		override def toString: String ="UnaryOperator[void]"
	}

	final case object `delete` extends UnaryOperator {
		override def toJSON: Js.Str = Js.Str("delete")
		override def toString: String ="UnaryOperator[delete]"
	}


	def from(src: Js.Value): UnaryOperator = src.str match {
		case "-" => `-`
		case "+" => `+`
		case "!" => `!`
		case "~" => `~`
		case "typeof" => `typeof`
		case "void" => `void`
		case "delete" => `delete`
	}
}

sealed class UpdateExpression(val operator: UpdateOperator, val argument: Expression, val prefix: Boolean, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("UpdateExpression"),
			"operator" -> this.operator.toJSON,
			"argument" -> this.argument.toJSON,
			"prefix" -> (if (this.prefix) Js.True else Js.False),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object UpdateExpression {
	def apply(operator: UpdateOperator, argument: Expression, prefix: Boolean, loc: Option[SourceLocation]): UpdateExpression = new UpdateExpression(operator, argument, prefix, loc)
	def unapply(updateExpression: UpdateExpression): Option[(UpdateOperator, Expression, Boolean)] = Some((updateExpression.operator, updateExpression.argument, updateExpression.prefix))

	def from(src: Js.Value): UpdateExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "UpdateExpression")

		new UpdateExpression(
			UpdateOperator.from(_obj("operator")),
			Expression.from(_obj("argument")),
			_obj("prefix").isInstanceOf[Js.True.type],
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class UpdateOperator {
	def toJSON: Js.Str
}

object UpdateOperator {
	final case object `++` extends UpdateOperator {
		override def toJSON: Js.Str = Js.Str("++")
		override def toString: String ="UpdateOperator[++]"
	}

	final case object `--` extends UpdateOperator {
		override def toJSON: Js.Str = Js.Str("--")
		override def toString: String ="UpdateOperator[--]"
	}


	def from(src: Js.Value): UpdateOperator = src.str match {
		case "++" => `++`
		case "--" => `--`
	}
}

sealed class BinaryExpression(val operator: BinaryOperator, val left: Expression, val right: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("BinaryExpression"),
			"operator" -> this.operator.toJSON,
			"left" -> this.left.toJSON,
			"right" -> this.right.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object BinaryExpression {
	def apply(operator: BinaryOperator, left: Expression, right: Expression, loc: Option[SourceLocation]): BinaryExpression = new BinaryExpression(operator, left, right, loc)
	def unapply(binaryExpression: BinaryExpression): Option[(BinaryOperator, Expression, Expression)] = Some((binaryExpression.operator, binaryExpression.left, binaryExpression.right))

	def from(src: Js.Value): BinaryExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "BinaryExpression")

		new BinaryExpression(
			BinaryOperator.from(_obj("operator")),
			Expression.from(_obj("left")),
			Expression.from(_obj("right")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class BinaryOperator {
	def toJSON: Js.Str
}

object BinaryOperator {
	final case object `==` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("==")
		override def toString: String ="BinaryOperator[==]"
	}

	final case object `!=` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("!=")
		override def toString: String ="BinaryOperator[!=]"
	}

	final case object `===` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("===")
		override def toString: String ="BinaryOperator[===]"
	}

	final case object `!==` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("!==")
		override def toString: String ="BinaryOperator[!==]"
	}

	final case object `<` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("<")
		override def toString: String ="BinaryOperator[<]"
	}

	final case object `<=` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("<=")
		override def toString: String ="BinaryOperator[<=]"
	}

	final case object `>` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str(">")
		override def toString: String ="BinaryOperator[>]"
	}

	final case object `>=` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str(">=")
		override def toString: String ="BinaryOperator[>=]"
	}

	final case object `<<` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("<<")
		override def toString: String ="BinaryOperator[<<]"
	}

	final case object `>>` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str(">>")
		override def toString: String ="BinaryOperator[>>]"
	}

	final case object `>>>` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str(">>>")
		override def toString: String ="BinaryOperator[>>>]"
	}

	final case object `+` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("+")
		override def toString: String ="BinaryOperator[+]"
	}

	final case object `-` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("-")
		override def toString: String ="BinaryOperator[-]"
	}

	final case object `*` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("*")
		override def toString: String ="BinaryOperator[*]"
	}

	final case object `/` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("/")
		override def toString: String ="BinaryOperator[/]"
	}

	final case object `%` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("%")
		override def toString: String ="BinaryOperator[%]"
	}

	final case object `|` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("|")
		override def toString: String ="BinaryOperator[|]"
	}

	final case object `^` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("^")
		override def toString: String ="BinaryOperator[^]"
	}

	final case object `&` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("&")
		override def toString: String ="BinaryOperator[&]"
	}

	final case object `in` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("in")
		override def toString: String ="BinaryOperator[in]"
	}

	final case object `instanceof` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("instanceof")
		override def toString: String ="BinaryOperator[instanceof]"
	}


	def from(src: Js.Value): BinaryOperator = src.str match {
		case "==" => `==`
		case "!=" => `!=`
		case "===" => `===`
		case "!==" => `!==`
		case "<" => `<`
		case "<=" => `<=`
		case ">" => `>`
		case ">=" => `>=`
		case "<<" => `<<`
		case ">>" => `>>`
		case ">>>" => `>>>`
		case "+" => `+`
		case "-" => `-`
		case "*" => `*`
		case "/" => `/`
		case "%" => `%`
		case "|" => `|`
		case "^" => `^`
		case "&" => `&`
		case "in" => `in`
		case "instanceof" => `instanceof`
	}
}

sealed class AssignmentExpression(val operator: AssignmentOperator, val left: AssignmentTarget, val right: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("AssignmentExpression"),
			"operator" -> this.operator.toJSON,
			"left" -> this.left.toJSON,
			"right" -> this.right.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object AssignmentExpression {
	def apply(operator: AssignmentOperator, left: AssignmentTarget, right: Expression, loc: Option[SourceLocation]): AssignmentExpression = new AssignmentExpression(operator, left, right, loc)
	def unapply(assignmentExpression: AssignmentExpression): Option[(AssignmentOperator, AssignmentTarget, Expression)] = Some((assignmentExpression.operator, assignmentExpression.left, assignmentExpression.right))

	def from(src: Js.Value): AssignmentExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "AssignmentExpression")

		new AssignmentExpression(
			AssignmentOperator.from(_obj("operator")),
			AssignmentTarget.from(_obj("left")),
			Expression.from(_obj("right")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class AssignmentOperator {
	def toJSON: Js.Str
}

object AssignmentOperator {
	final case object `=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("=")
		override def toString: String ="AssignmentOperator[=]"
	}

	final case object `+=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("+=")
		override def toString: String ="AssignmentOperator[+=]"
	}

	final case object `-=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("-=")
		override def toString: String ="AssignmentOperator[-=]"
	}

	final case object `*=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("*=")
		override def toString: String ="AssignmentOperator[*=]"
	}

	final case object `/=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("/=")
		override def toString: String ="AssignmentOperator[/=]"
	}

	final case object `%=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("%=")
		override def toString: String ="AssignmentOperator[%=]"
	}

	final case object `<<=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("<<=")
		override def toString: String ="AssignmentOperator[<<=]"
	}

	final case object `>>=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str(">>=")
		override def toString: String ="AssignmentOperator[>>=]"
	}

	final case object `>>>=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str(">>>=")
		override def toString: String ="AssignmentOperator[>>>=]"
	}

	final case object `|=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("|=")
		override def toString: String ="AssignmentOperator[|=]"
	}

	final case object `^=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("^=")
		override def toString: String ="AssignmentOperator[^=]"
	}

	final case object `&=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("&=")
		override def toString: String ="AssignmentOperator[&=]"
	}


	def from(src: Js.Value): AssignmentOperator = src.str match {
		case "=" => `=`
		case "+=" => `+=`
		case "-=" => `-=`
		case "*=" => `*=`
		case "/=" => `/=`
		case "%=" => `%=`
		case "<<=" => `<<=`
		case ">>=" => `>>=`
		case ">>>=" => `>>>=`
		case "|=" => `|=`
		case "^=" => `^=`
		case "&=" => `&=`
	}
}

sealed class LogicalExpression(val operator: LogicalOperator, val left: Expression, val right: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("LogicalExpression"),
			"operator" -> this.operator.toJSON,
			"left" -> this.left.toJSON,
			"right" -> this.right.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object LogicalExpression {
	def apply(operator: LogicalOperator, left: Expression, right: Expression, loc: Option[SourceLocation]): LogicalExpression = new LogicalExpression(operator, left, right, loc)
	def unapply(logicalExpression: LogicalExpression): Option[(LogicalOperator, Expression, Expression)] = Some((logicalExpression.operator, logicalExpression.left, logicalExpression.right))

	def from(src: Js.Value): LogicalExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "LogicalExpression")

		new LogicalExpression(
			LogicalOperator.from(_obj("operator")),
			Expression.from(_obj("left")),
			Expression.from(_obj("right")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class LogicalOperator {
	def toJSON: Js.Str
}

object LogicalOperator {
	final case object `||` extends LogicalOperator {
		override def toJSON: Js.Str = Js.Str("||")
		override def toString: String ="LogicalOperator[||]"
	}

	final case object `&&` extends LogicalOperator {
		override def toJSON: Js.Str = Js.Str("&&")
		override def toString: String ="LogicalOperator[&&]"
	}


	def from(src: Js.Value): LogicalOperator = src.str match {
		case "||" => `||`
		case "&&" => `&&`
	}
}

sealed class MemberExpression(val obj: Expression, val property: Expression, val computed: Boolean, loc: Option[SourceLocation]) extends Node(loc) with Expression with Pattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("MemberExpression"),
			"object" -> this.obj.toJSON,
			"property" -> this.property.toJSON,
			"computed" -> (if (this.computed) Js.True else Js.False),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object MemberExpression {
	def apply(obj: Expression, property: Expression, computed: Boolean, loc: Option[SourceLocation]): MemberExpression = new MemberExpression(obj, property, computed, loc)
	def unapply(memberExpression: MemberExpression): Option[(Expression, Expression, Boolean)] = Some((memberExpression.obj, memberExpression.property, memberExpression.computed))

	def from(src: Js.Value): MemberExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "MemberExpression")

		new MemberExpression(
			Expression.from(_obj("object")),
			Expression.from(_obj("property")),
			_obj("computed").isInstanceOf[Js.True.type],
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ConditionalExpression(val test: Expression, val alternate: Expression, val consequent: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ConditionalExpression"),
			"test" -> this.test.toJSON,
			"alternate" -> this.alternate.toJSON,
			"consequent" -> this.consequent.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ConditionalExpression {
	def apply(test: Expression, alternate: Expression, consequent: Expression, loc: Option[SourceLocation]): ConditionalExpression = new ConditionalExpression(test, alternate, consequent, loc)
	def unapply(conditionalExpression: ConditionalExpression): Option[(Expression, Expression, Expression)] = Some((conditionalExpression.test, conditionalExpression.alternate, conditionalExpression.consequent))

	def from(src: Js.Value): ConditionalExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ConditionalExpression")

		new ConditionalExpression(
			Expression.from(_obj("test")),
			Expression.from(_obj("alternate")),
			Expression.from(_obj("consequent")),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class CallExpression(val callee: Expression, val arguments: Seq[Expression], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("CallExpression"),
			"callee" -> this.callee.toJSON,
			"arguments" -> Js.Arr(this.arguments.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object CallExpression {
	def apply(callee: Expression, arguments: Seq[Expression], loc: Option[SourceLocation]): CallExpression = new CallExpression(callee, arguments, loc)
	def unapply(callExpression: CallExpression): Option[(Expression, Seq[Expression])] = Some((callExpression.callee, callExpression.arguments))

	def from(src: Js.Value): CallExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "CallExpression")

		new CallExpression(
			Expression.from(_obj("callee")),
			_obj("arguments").arr.map(elem => Expression.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class NewExpression(val callee: Expression, val arguments: Seq[Expression], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("NewExpression"),
			"callee" -> this.callee.toJSON,
			"arguments" -> Js.Arr(this.arguments.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object NewExpression {
	def apply(callee: Expression, arguments: Seq[Expression], loc: Option[SourceLocation]): NewExpression = new NewExpression(callee, arguments, loc)
	def unapply(newExpression: NewExpression): Option[(Expression, Seq[Expression])] = Some((newExpression.callee, newExpression.arguments))

	def from(src: Js.Value): NewExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "NewExpression")

		new NewExpression(
			Expression.from(_obj("callee")),
			_obj("arguments").arr.map(elem => Expression.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class SequenceExpression(val expressions: Seq[Expression], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("SequenceExpression"),
			"expressions" -> Js.Arr(this.expressions.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object SequenceExpression {
	def apply(expressions: Seq[Expression], loc: Option[SourceLocation]): SequenceExpression = new SequenceExpression(expressions, loc)
	def unapply(sequenceExpression: SequenceExpression): Option[Seq[Expression]] = Some(sequenceExpression.expressions)

	def from(src: Js.Value): SequenceExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "SequenceExpression")

		new SequenceExpression(
			_obj("expressions").arr.map(elem => Expression.from(elem)),
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Pattern extends ForInTarget with AssignmentTarget {
	this: Node =>

	def toJSON: Js.Value
}

object Pattern {
	def from(src: Js.Value): Pattern = src("type").str match {
		case "Identifier" => Identifier.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Pattern")
	}
}

sealed class BooleanLiteral(val value: Boolean, raw: String, loc: Option[SourceLocation]) extends Literal(raw, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Literal"),
			"value" -> (if (this.value) Js.True else Js.False),
			"raw" -> Js.Str(this.raw),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object BooleanLiteral {
	def apply(value: Boolean, raw: String, loc: Option[SourceLocation]): BooleanLiteral = new BooleanLiteral(value, raw, loc)
	def unapply(booleanLiteral: BooleanLiteral): Option[Boolean] = Some(booleanLiteral.value)

	def from(src: Js.Value): BooleanLiteral = {
		val _obj = src.obj
		assert(_obj("type").str == "Literal")
		assert(_obj("value").isInstanceOf[Js.True.type] || _obj("value").isInstanceOf[Js.False.type])

		new BooleanLiteral(
			_obj("value").isInstanceOf[Js.True.type],
			_obj("raw").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class NumberLiteral(raw: String, loc: Option[SourceLocation]) extends Literal(raw, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Literal"),
			"raw" -> Js.Str(this.raw),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object NumberLiteral {
	def apply(raw: String, loc: Option[SourceLocation]): NumberLiteral = new NumberLiteral(raw, loc)

	def from(src: Js.Value): NumberLiteral = {
		val _obj = src.obj
		assert(_obj("type").str == "Literal")
		assert(_obj("value").isInstanceOf[Js.Num])

		new NumberLiteral(
			_obj("raw").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class StringLiteral(val value: String, raw: String, loc: Option[SourceLocation]) extends Literal(raw, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Literal"),
			"value" -> Js.Str(this.value),
			"raw" -> Js.Str(this.raw),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object StringLiteral {
	def apply(value: String, raw: String, loc: Option[SourceLocation]): StringLiteral = new StringLiteral(value, raw, loc)
	def unapply(stringLiteral: StringLiteral): Option[String] = Some(stringLiteral.value)

	def from(src: Js.Value): StringLiteral = {
		val _obj = src.obj
		assert(_obj("type").str == "Literal")
		assert(_obj("value").isInstanceOf[Js.Str] && !_obj.contains("regex"))

		new StringLiteral(
			_obj("value").str.toString,
			_obj("raw").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class NullLiteral(raw: String, loc: Option[SourceLocation]) extends Literal(raw, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Literal"),
			"raw" -> Js.Str(this.raw),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object NullLiteral {
	def apply(raw: String, loc: Option[SourceLocation]): NullLiteral = new NullLiteral(raw, loc)

	def from(src: Js.Value): NullLiteral = {
		val _obj = src.obj
		assert(_obj("type").str == "Literal")
		assert(_obj("value").isInstanceOf[Js.Null.type])

		new NullLiteral(
			_obj("raw").str.toString,
			_obj.get("loc").map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class PropertyKind {
	def toJSON: Js.Str
}

object PropertyKind {
	final case object `init` extends PropertyKind {
		override def toJSON: Js.Str = Js.Str("init")
		override def toString: String ="PropertyKind[init]"
	}

	final case object `get` extends PropertyKind {
		override def toJSON: Js.Str = Js.Str("get")
		override def toString: String ="PropertyKind[get]"
	}

	final case object `set` extends PropertyKind {
		override def toJSON: Js.Str = Js.Str("set")
		override def toString: String ="PropertyKind[set]"
	}


	def from(src: Js.Value): PropertyKind = src.str match {
		case "init" => `init`
		case "get" => `get`
		case "set" => `set`
	}
}

sealed trait PropertyKey {
	this: Node =>

	def toJSON: Js.Value
}

object PropertyKey {
	def from(src: Js.Value): PropertyKey = src("type").str match {
		case "Identifier" => Identifier.from(src)
		case "Literal" if src.obj.contains("regex") => RegExpLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.True.type] || src.obj("value").isInstanceOf[Js.False.type] => BooleanLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Num] => NumberLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Str] && !src.obj.contains("regex") => StringLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Null.type] => NullLiteral.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for PropertyKey")
	}
}

sealed trait ForInit {
	this: Node =>

	def toJSON: Js.Value
}

object ForInit {
	def from(src: Js.Value): ForInit = src("type").str match {
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "Identifier" => Identifier.from(src)
		case "Literal" if src.obj.contains("regex") => RegExpLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.True.type] || src.obj("value").isInstanceOf[Js.False.type] => BooleanLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Num] => NumberLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Str] && !src.obj.contains("regex") => StringLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Null.type] => NullLiteral.from(src)
		case "ThisExpression" => ThisExpression.from(src)
		case "ArrayExpression" => ArrayExpression.from(src)
		case "ObjectExpression" => ObjectExpression.from(src)
		case "FunctionExpression" => FunctionExpression.from(src)
		case "UnaryExpression" => UnaryExpression.from(src)
		case "UpdateExpression" => UpdateExpression.from(src)
		case "BinaryExpression" => BinaryExpression.from(src)
		case "AssignmentExpression" => AssignmentExpression.from(src)
		case "LogicalExpression" => LogicalExpression.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case "ConditionalExpression" => ConditionalExpression.from(src)
		case "CallExpression" => CallExpression.from(src)
		case "NewExpression" => NewExpression.from(src)
		case "SequenceExpression" => SequenceExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ForInit")
	}
}

sealed trait ForInTarget {
	this: Node =>

	def toJSON: Js.Value
}

object ForInTarget {
	def from(src: Js.Value): ForInTarget = src("type").str match {
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "Identifier" => Identifier.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ForInTarget")
	}
}

sealed trait AssignmentTarget {
	this: Node =>

	def toJSON: Js.Value
}

object AssignmentTarget {
	def from(src: Js.Value): AssignmentTarget = src("type").str match {
		case "Identifier" => Identifier.from(src)
		case "Literal" if src.obj.contains("regex") => RegExpLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.True.type] || src.obj("value").isInstanceOf[Js.False.type] => BooleanLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Num] => NumberLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Str] && !src.obj.contains("regex") => StringLiteral.from(src)
		case "Literal" if src.obj("value").isInstanceOf[Js.Null.type] => NullLiteral.from(src)
		case "ThisExpression" => ThisExpression.from(src)
		case "ArrayExpression" => ArrayExpression.from(src)
		case "ObjectExpression" => ObjectExpression.from(src)
		case "FunctionExpression" => FunctionExpression.from(src)
		case "UnaryExpression" => UnaryExpression.from(src)
		case "UpdateExpression" => UpdateExpression.from(src)
		case "BinaryExpression" => BinaryExpression.from(src)
		case "AssignmentExpression" => AssignmentExpression.from(src)
		case "LogicalExpression" => LogicalExpression.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case "ConditionalExpression" => ConditionalExpression.from(src)
		case "CallExpression" => CallExpression.from(src)
		case "NewExpression" => NewExpression.from(src)
		case "SequenceExpression" => SequenceExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for AssignmentTarget")
	}
}

