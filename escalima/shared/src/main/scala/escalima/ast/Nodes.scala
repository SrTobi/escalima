package escalima.ast

import upickle.Js


sealed abstract class Node(val loc: Option[SourceLocation]) {
	def toJSON: Js.Value
}

object Node {
	def unapply(node: Node): Option[Option[SourceLocation]] = Some(node.loc)
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
			_obj.get("source").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => inner.str.toString),
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

sealed class Identifier(val name: String, loc: Option[SourceLocation]) extends Node(loc) with Expression with Pattern {
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed abstract class Literal(val raw: String, loc: Option[SourceLocation]) extends Node(loc) with Expression {
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Program(val body: Seq[ModuleStatement], loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"body" -> Js.Arr(this.body.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Program {
	def apply(body: Seq[ModuleStatement], loc: Option[SourceLocation]): Program = new Program(body, loc)
	def unapply(program: Program): Option[Seq[ModuleStatement]] = Some(program.body)

	def from(src: Js.Value): Program = {
		val _obj = src.obj

		new Program(
			_obj("body").arr.map(elem => ModuleStatement.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Function(val id: Option[Identifier], val params: Seq[Pattern], val body: FunctionBody, val generator: Boolean, val async: Boolean, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"generator" -> Js.Bool(this.generator),
			"async" -> Js.Bool(this.async),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Function {
	def apply(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, generator: Boolean, async: Boolean, loc: Option[SourceLocation]): Function = new Function(id, params, body, generator, async, loc)
	def unapply(function: Function): Option[(Option[Identifier], Seq[Pattern], FunctionBody, Boolean, Boolean)] = Some((function.id, function.params, function.body, function.generator, function.async))

	def from(src: Js.Value): Function = {
		val _obj = src.obj

		new Function(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			FunctionBody.from(_obj("body")),
			_obj("generator").bool,
			_obj("async").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Statement extends Node with ModuleStatement {
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
		case "ForOfStatement" => ForOfStatement.from(src)
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "ClassDeclaration" => ClassDeclaration.from(src)
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class FunctionBody(val body: Seq[Statement], loc: Option[SourceLocation]) extends Node(loc) with ArrowFunctionBody {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("BlockStatement"),
			"body" -> Js.Arr(this.body.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object FunctionBody {
	def apply(body: Seq[Statement], loc: Option[SourceLocation]): FunctionBody = new FunctionBody(body, loc)
	def unapply(functionBody: FunctionBody): Option[Seq[Statement]] = Some(functionBody.body)

	def from(src: Js.Value): FunctionBody = {
		val _obj = src.obj
		assert(_obj("type").str == "BlockStatement")

		new FunctionBody(
			_obj("body").arr.map(elem => Statement.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("argument").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("label").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("label").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("alternate").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Statement.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("test").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			_obj("consequent").arr.map(elem => Statement.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("handler").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => CatchClause.from(inner)),
			_obj.get("finalizer").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => BlockStatement.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("init").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => ForInit.from(inner)),
			_obj.get("test").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			_obj.get("update").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			Statement.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
		_obj("type").str match {
			case "ForOfStatement" => ForOfStatement.from(src)
			case _ =>
		}
		assert(_obj("type").str == "ForInStatement")

		new ForInStatement(
			ForInTarget.from(_obj("left")),
			Expression.from(_obj("right")),
			Statement.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Declaration extends Node with Statement with Exportable {
	def toJSON: Js.Value
}

object Declaration {
	def from(src: Js.Value): Declaration = src("type").str match {
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "ClassDeclaration" => ClassDeclaration.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Declaration")
	}
}

sealed class FunctionDeclaration(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, generator: Boolean, async: Boolean, loc: Option[SourceLocation]) extends Function(id, params, body, generator, async, loc) with Declaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("FunctionDeclaration"),
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"generator" -> Js.Bool(this.generator),
			"async" -> Js.Bool(this.async),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object FunctionDeclaration {
	def apply(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, generator: Boolean, async: Boolean, loc: Option[SourceLocation]): FunctionDeclaration = new FunctionDeclaration(id, params, body, generator, async, loc)

	def from(src: Js.Value): FunctionDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "FunctionDeclaration")

		new FunctionDeclaration(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			FunctionBody.from(_obj("body")),
			_obj("generator").bool,
			_obj("async").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class VariableDeclaration(val declarations: Seq[VariableDeclarator], val kind: VariableDeclarationKind, loc: Option[SourceLocation]) extends Node(loc) with Declaration with ForInit with ForInTarget {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("VariableDeclaration"),
			"declarations" -> Js.Arr(this.declarations.map(inner => inner.toJSON): _*),
			"kind" -> this.kind.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object VariableDeclaration {
	def apply(declarations: Seq[VariableDeclarator], kind: VariableDeclarationKind, loc: Option[SourceLocation]): VariableDeclaration = new VariableDeclaration(declarations, kind, loc)
	def unapply(variableDeclaration: VariableDeclaration): Option[(Seq[VariableDeclarator], VariableDeclarationKind)] = Some((variableDeclaration.declarations, variableDeclaration.kind))

	def from(src: Js.Value): VariableDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "VariableDeclaration")

		new VariableDeclaration(
			_obj("declarations").arr.map(elem => VariableDeclarator.from(elem)),
			VariableDeclarationKind.from(_obj("kind")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("init").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Expression extends Node with ForInit with AssignmentTarget with SpreadableExpression with Callee with ArrowFunctionBody with Exportable {
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ArrayExpression(val elements: Seq[Option[SpreadableExpression]], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ArrayExpression"),
			"elements" -> Js.Arr(this.elements.map(inner => inner.map(inner => inner.toJSON).getOrElse(Js.Null)): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ArrayExpression {
	def apply(elements: Seq[Option[SpreadableExpression]], loc: Option[SourceLocation]): ArrayExpression = new ArrayExpression(elements, loc)
	def unapply(arrayExpression: ArrayExpression): Option[Seq[Option[SpreadableExpression]]] = Some(arrayExpression.elements)

	def from(src: Js.Value): ArrayExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ArrayExpression")

		new ArrayExpression(
			_obj("elements").arr.map(elem => (elem match { case Js.Null => None; case some => Some(some)}).map(inner => SpreadableExpression.from(inner))),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ObjectExpression(val properties: Seq[SpreadableProperty], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ObjectExpression"),
			"properties" -> Js.Arr(this.properties.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ObjectExpression {
	def apply(properties: Seq[SpreadableProperty], loc: Option[SourceLocation]): ObjectExpression = new ObjectExpression(properties, loc)
	def unapply(objectExpression: ObjectExpression): Option[Seq[SpreadableProperty]] = Some(objectExpression.properties)

	def from(src: Js.Value): ObjectExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ObjectExpression")

		new ObjectExpression(
			_obj("properties").arr.map(elem => SpreadableProperty.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Property(val key: Expression, val value: Expression, val kind: PropertyKind, val method: Boolean, val shorthand: Boolean, val computed: Boolean, loc: Option[SourceLocation]) extends Node(loc) with SpreadableProperty {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Property"),
			"key" -> this.key.toJSON,
			"value" -> this.value.toJSON,
			"kind" -> this.kind.toJSON,
			"method" -> Js.Bool(this.method),
			"shorthand" -> Js.Bool(this.shorthand),
			"computed" -> Js.Bool(this.computed),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Property {
	def apply(key: Expression, value: Expression, kind: PropertyKind, method: Boolean, shorthand: Boolean, computed: Boolean, loc: Option[SourceLocation]): Property = new Property(key, value, kind, method, shorthand, computed, loc)
	def unapply(property: Property): Option[(Expression, Expression, PropertyKind, Boolean, Boolean, Boolean)] = Some((property.key, property.value, property.kind, property.method, property.shorthand, property.computed))

	def from(src: Js.Value): Property = {
		val _obj = src.obj
		assert(_obj("type").str == "Property")

		new Property(
			Expression.from(_obj("key")),
			Expression.from(_obj("value")),
			PropertyKind.from(_obj("kind")),
			_obj("method").bool,
			_obj("shorthand").bool,
			_obj("computed").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class FunctionExpression(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, generator: Boolean, async: Boolean, loc: Option[SourceLocation]) extends Function(id, params, body, generator, async, loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("FunctionExpression"),
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"generator" -> Js.Bool(this.generator),
			"async" -> Js.Bool(this.async),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object FunctionExpression {
	def apply(id: Option[Identifier], params: Seq[Pattern], body: FunctionBody, generator: Boolean, async: Boolean, loc: Option[SourceLocation]): FunctionExpression = new FunctionExpression(id, params, body, generator, async, loc)

	def from(src: Js.Value): FunctionExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "FunctionExpression")

		new FunctionExpression(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			FunctionBody.from(_obj("body")),
			_obj("generator").bool,
			_obj("async").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class UnaryExpression(val operator: UnaryOperator, val prefix: Boolean, val argument: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("UnaryExpression"),
			"operator" -> this.operator.toJSON,
			"prefix" -> Js.Bool(this.prefix),
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
			_obj("prefix").bool,
			Expression.from(_obj("argument")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			"prefix" -> Js.Bool(this.prefix),
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
			_obj("prefix").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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

	final case object `**` extends BinaryOperator {
		override def toJSON: Js.Str = Js.Str("**")
		override def toString: String ="BinaryOperator[**]"
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
		case "**" => `**`
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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

	final case object `**=` extends AssignmentOperator {
		override def toJSON: Js.Str = Js.Str("**=")
		override def toString: String ="AssignmentOperator[**=]"
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
		case "**=" => `**=`
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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

sealed class MemberExpression(val obj: Callee, val property: Expression, val computed: Boolean, loc: Option[SourceLocation]) extends Node(loc) with Expression with Pattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("MemberExpression"),
			"object" -> this.obj.toJSON,
			"property" -> this.property.toJSON,
			"computed" -> Js.Bool(this.computed),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object MemberExpression {
	def apply(obj: Callee, property: Expression, computed: Boolean, loc: Option[SourceLocation]): MemberExpression = new MemberExpression(obj, property, computed, loc)
	def unapply(memberExpression: MemberExpression): Option[(Callee, Expression, Boolean)] = Some((memberExpression.obj, memberExpression.property, memberExpression.computed))

	def from(src: Js.Value): MemberExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "MemberExpression")

		new MemberExpression(
			Callee.from(_obj("object")),
			Expression.from(_obj("property")),
			_obj("computed").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ConditionalExpression(val test: Expression, val consequent: Expression, val alternate: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ConditionalExpression"),
			"test" -> this.test.toJSON,
			"consequent" -> this.consequent.toJSON,
			"alternate" -> this.alternate.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ConditionalExpression {
	def apply(test: Expression, consequent: Expression, alternate: Expression, loc: Option[SourceLocation]): ConditionalExpression = new ConditionalExpression(test, consequent, alternate, loc)
	def unapply(conditionalExpression: ConditionalExpression): Option[(Expression, Expression, Expression)] = Some((conditionalExpression.test, conditionalExpression.consequent, conditionalExpression.alternate))

	def from(src: Js.Value): ConditionalExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ConditionalExpression")

		new ConditionalExpression(
			Expression.from(_obj("test")),
			Expression.from(_obj("consequent")),
			Expression.from(_obj("alternate")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class CallExpression(val callee: Callee, val arguments: Seq[SpreadableExpression], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("CallExpression"),
			"callee" -> this.callee.toJSON,
			"arguments" -> Js.Arr(this.arguments.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object CallExpression {
	def apply(callee: Callee, arguments: Seq[SpreadableExpression], loc: Option[SourceLocation]): CallExpression = new CallExpression(callee, arguments, loc)
	def unapply(callExpression: CallExpression): Option[(Callee, Seq[SpreadableExpression])] = Some((callExpression.callee, callExpression.arguments))

	def from(src: Js.Value): CallExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "CallExpression")

		new CallExpression(
			Callee.from(_obj("callee")),
			_obj("arguments").arr.map(elem => SpreadableExpression.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class NewExpression(val callee: Expression, val arguments: Seq[SpreadableExpression], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("NewExpression"),
			"callee" -> this.callee.toJSON,
			"arguments" -> Js.Arr(this.arguments.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object NewExpression {
	def apply(callee: Expression, arguments: Seq[SpreadableExpression], loc: Option[SourceLocation]): NewExpression = new NewExpression(callee, arguments, loc)
	def unapply(newExpression: NewExpression): Option[(Expression, Seq[SpreadableExpression])] = Some((newExpression.callee, newExpression.arguments))

	def from(src: Js.Value): NewExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "NewExpression")

		new NewExpression(
			Expression.from(_obj("callee")),
			_obj("arguments").arr.map(elem => SpreadableExpression.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait Pattern extends Node with ForInTarget with AssignmentTarget {
	def toJSON: Js.Value
}

object Pattern {
	def from(src: Js.Value): Pattern = src("type").str match {
		case "Identifier" => Identifier.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case "ObjectPattern" => ObjectPattern.from(src)
		case "ArrayPattern" => ArrayPattern.from(src)
		case "RestElement" => RestElement.from(src)
		case "AssignmentPattern" => AssignmentPattern.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Pattern")
	}
}

sealed class ForOfStatement(left: ForInTarget, right: Expression, body: Statement, loc: Option[SourceLocation]) extends ForInStatement(left, right, body, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ForOfStatement"),
			"left" -> this.left.toJSON,
			"right" -> this.right.toJSON,
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ForOfStatement {
	def apply(left: ForInTarget, right: Expression, body: Statement, loc: Option[SourceLocation]): ForOfStatement = new ForOfStatement(left, right, body, loc)

	def from(src: Js.Value): ForOfStatement = {
		val _obj = src.obj
		assert(_obj("type").str == "ForOfStatement")

		new ForOfStatement(
			ForInTarget.from(_obj("left")),
			Expression.from(_obj("right")),
			Statement.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Super(loc: Option[SourceLocation]) extends Node(loc) with Callee {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Super"),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Super {
	def apply(loc: Option[SourceLocation]): Super = new Super(loc)

	def from(src: Js.Value): Super = {
		val _obj = src.obj
		assert(_obj("type").str == "Super")

		new Super(
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class SpreadElement(val argument: Expression, loc: Option[SourceLocation]) extends Node(loc) with SpreadableExpression with SpreadableProperty {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("SpreadElement"),
			"argument" -> this.argument.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object SpreadElement {
	def apply(argument: Expression, loc: Option[SourceLocation]): SpreadElement = new SpreadElement(argument, loc)
	def unapply(spreadElement: SpreadElement): Option[Expression] = Some(spreadElement.argument)

	def from(src: Js.Value): SpreadElement = {
		val _obj = src.obj
		assert(_obj("type").str == "SpreadElement")

		new SpreadElement(
			Expression.from(_obj("argument")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ArrowFunctionExpression(val id: Option[Identifier], val params: Seq[Pattern], val body: ArrowFunctionBody, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ArrowFunctionExpression"),
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"params" -> Js.Arr(this.params.map(inner => inner.toJSON): _*),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ArrowFunctionExpression {
	def apply(id: Option[Identifier], params: Seq[Pattern], body: ArrowFunctionBody, loc: Option[SourceLocation]): ArrowFunctionExpression = new ArrowFunctionExpression(id, params, body, loc)
	def unapply(arrowFunctionExpression: ArrowFunctionExpression): Option[(Option[Identifier], Seq[Pattern], ArrowFunctionBody)] = Some((arrowFunctionExpression.id, arrowFunctionExpression.params, arrowFunctionExpression.body))

	def from(src: Js.Value): ArrowFunctionExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ArrowFunctionExpression")

		new ArrowFunctionExpression(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj("params").arr.map(elem => Pattern.from(elem)),
			ArrowFunctionBody.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class YieldExpression(val argument: Option[Expression], val delegate: Boolean, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("YieldExpression"),
			"argument" -> this.argument.map(inner => inner.toJSON).getOrElse(Js.Null),
			"delegate" -> Js.Bool(this.delegate),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object YieldExpression {
	def apply(argument: Option[Expression], delegate: Boolean, loc: Option[SourceLocation]): YieldExpression = new YieldExpression(argument, delegate, loc)
	def unapply(yieldExpression: YieldExpression): Option[(Option[Expression], Boolean)] = Some((yieldExpression.argument, yieldExpression.delegate))

	def from(src: Js.Value): YieldExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "YieldExpression")

		new YieldExpression(
			_obj.get("argument").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			_obj("delegate").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class TemplateLiteral(val quasis: Seq[TemplateElement], val expressions: Seq[Expression], loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("TemplateLiteral"),
			"quasis" -> Js.Arr(this.quasis.map(inner => inner.toJSON): _*),
			"expressions" -> Js.Arr(this.expressions.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object TemplateLiteral {
	def apply(quasis: Seq[TemplateElement], expressions: Seq[Expression], loc: Option[SourceLocation]): TemplateLiteral = new TemplateLiteral(quasis, expressions, loc)
	def unapply(templateLiteral: TemplateLiteral): Option[(Seq[TemplateElement], Seq[Expression])] = Some((templateLiteral.quasis, templateLiteral.expressions))

	def from(src: Js.Value): TemplateLiteral = {
		val _obj = src.obj
		assert(_obj("type").str == "TemplateLiteral")

		new TemplateLiteral(
			_obj("quasis").arr.map(elem => TemplateElement.from(elem)),
			_obj("expressions").arr.map(elem => Expression.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class TaggedTemplateExpression(val tag: Expression, val quasi: TemplateLiteral, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("TaggedTemplateExpression"),
			"tag" -> this.tag.toJSON,
			"quasi" -> this.quasi.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object TaggedTemplateExpression {
	def apply(tag: Expression, quasi: TemplateLiteral, loc: Option[SourceLocation]): TaggedTemplateExpression = new TaggedTemplateExpression(tag, quasi, loc)
	def unapply(taggedTemplateExpression: TaggedTemplateExpression): Option[(Expression, TemplateLiteral)] = Some((taggedTemplateExpression.tag, taggedTemplateExpression.quasi))

	def from(src: Js.Value): TaggedTemplateExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "TaggedTemplateExpression")

		new TaggedTemplateExpression(
			Expression.from(_obj("tag")),
			TemplateLiteral.from(_obj("quasi")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Value(val cooked: String, val raw: String) {
	def toJSON: Js.Value = Js.Obj(
			"cooked" -> Js.Str(this.cooked),
			"raw" -> Js.Str(this.raw)
		)
}

object Value {
	def apply(cooked: String, raw: String): Value = new Value(cooked, raw)
	def unapply(value: Value): Option[(String, String)] = Some((value.cooked, value.raw))

	def from(src: Js.Value): Value = {
		val _obj = src.obj

		new Value(
			_obj("cooked").str.toString,
			_obj("raw").str.toString
		)
	}
}

sealed class TemplateElement(val tail: Boolean, val value: Value, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("TemplateElement"),
			"tail" -> Js.Bool(this.tail),
			"value" -> this.value.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object TemplateElement {
	def apply(tail: Boolean, value: Value, loc: Option[SourceLocation]): TemplateElement = new TemplateElement(tail, value, loc)
	def unapply(templateElement: TemplateElement): Option[(Boolean, Value)] = Some((templateElement.tail, templateElement.value))

	def from(src: Js.Value): TemplateElement = {
		val _obj = src.obj
		assert(_obj("type").str == "TemplateElement")

		new TemplateElement(
			_obj("tail").bool,
			Value.from(_obj("value")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class AssignmentProperty(val value: Pattern, loc: Option[SourceLocation]) extends Node(loc) with PropertyPattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Property"),
			"value" -> this.value.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object AssignmentProperty {
	def apply(value: Pattern, loc: Option[SourceLocation]): AssignmentProperty = new AssignmentProperty(value, loc)
	def unapply(assignmentProperty: AssignmentProperty): Option[Pattern] = Some(assignmentProperty.value)

	def from(src: Js.Value): AssignmentProperty = {
		val _obj = src.obj
		assert(_obj("type").str == "Property")

		new AssignmentProperty(
			Pattern.from(_obj("value")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ObjectPattern(val properties: Seq[PropertyPattern], loc: Option[SourceLocation]) extends Node(loc) with Pattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ObjectPattern"),
			"properties" -> Js.Arr(this.properties.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ObjectPattern {
	def apply(properties: Seq[PropertyPattern], loc: Option[SourceLocation]): ObjectPattern = new ObjectPattern(properties, loc)
	def unapply(objectPattern: ObjectPattern): Option[Seq[PropertyPattern]] = Some(objectPattern.properties)

	def from(src: Js.Value): ObjectPattern = {
		val _obj = src.obj
		assert(_obj("type").str == "ObjectPattern")

		new ObjectPattern(
			_obj("properties").arr.map(elem => PropertyPattern.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ArrayPattern(val elements: Seq[Option[Pattern]], loc: Option[SourceLocation]) extends Node(loc) with Pattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ArrayPattern"),
			"elements" -> Js.Arr(this.elements.map(inner => inner.map(inner => inner.toJSON).getOrElse(Js.Null)): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ArrayPattern {
	def apply(elements: Seq[Option[Pattern]], loc: Option[SourceLocation]): ArrayPattern = new ArrayPattern(elements, loc)
	def unapply(arrayPattern: ArrayPattern): Option[Seq[Option[Pattern]]] = Some(arrayPattern.elements)

	def from(src: Js.Value): ArrayPattern = {
		val _obj = src.obj
		assert(_obj("type").str == "ArrayPattern")

		new ArrayPattern(
			_obj("elements").arr.map(elem => (elem match { case Js.Null => None; case some => Some(some)}).map(inner => Pattern.from(inner))),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class RestElement(val argument: Pattern, loc: Option[SourceLocation]) extends Node(loc) with Pattern with PropertyPattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("RestElement"),
			"argument" -> this.argument.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object RestElement {
	def apply(argument: Pattern, loc: Option[SourceLocation]): RestElement = new RestElement(argument, loc)
	def unapply(restElement: RestElement): Option[Pattern] = Some(restElement.argument)

	def from(src: Js.Value): RestElement = {
		val _obj = src.obj
		assert(_obj("type").str == "RestElement")

		new RestElement(
			Pattern.from(_obj("argument")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class AssignmentPattern(val left: Pattern, val right: Expression, loc: Option[SourceLocation]) extends Node(loc) with Pattern {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("AssignmentPattern"),
			"left" -> this.left.toJSON,
			"right" -> this.right.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object AssignmentPattern {
	def apply(left: Pattern, right: Expression, loc: Option[SourceLocation]): AssignmentPattern = new AssignmentPattern(left, right, loc)
	def unapply(assignmentPattern: AssignmentPattern): Option[(Pattern, Expression)] = Some((assignmentPattern.left, assignmentPattern.right))

	def from(src: Js.Value): AssignmentPattern = {
		val _obj = src.obj
		assert(_obj("type").str == "AssignmentPattern")

		new AssignmentPattern(
			Pattern.from(_obj("left")),
			Expression.from(_obj("right")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class Class(val id: Option[Identifier], val superClass: Option[Expression], val body: ClassBody, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"superClass" -> this.superClass.map(inner => inner.toJSON).getOrElse(Js.Null),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object Class {
	def apply(id: Option[Identifier], superClass: Option[Expression], body: ClassBody, loc: Option[SourceLocation]): Class = new Class(id, superClass, body, loc)
	def unapply(clazz: Class): Option[(Option[Identifier], Option[Expression], ClassBody)] = Some((clazz.id, clazz.superClass, clazz.body))

	def from(src: Js.Value): Class = {
		val _obj = src.obj

		new Class(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj.get("superClass").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			ClassBody.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ClassBody(val body: Seq[MethodDefinition], loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ClassBody"),
			"body" -> Js.Arr(this.body.map(inner => inner.toJSON): _*),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ClassBody {
	def apply(body: Seq[MethodDefinition], loc: Option[SourceLocation]): ClassBody = new ClassBody(body, loc)
	def unapply(classBody: ClassBody): Option[Seq[MethodDefinition]] = Some(classBody.body)

	def from(src: Js.Value): ClassBody = {
		val _obj = src.obj
		assert(_obj("type").str == "ClassBody")

		new ClassBody(
			_obj("body").arr.map(elem => MethodDefinition.from(elem)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class MethodDefinition(val key: Expression, val value: FunctionExpression, val kind: MethodKind, val computed: Boolean, val static: Boolean, loc: Option[SourceLocation]) extends Node(loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("MethodDefinition"),
			"key" -> this.key.toJSON,
			"value" -> this.value.toJSON,
			"kind" -> this.kind.toJSON,
			"computed" -> Js.Bool(this.computed),
			"static" -> Js.Bool(this.static),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object MethodDefinition {
	def apply(key: Expression, value: FunctionExpression, kind: MethodKind, computed: Boolean, static: Boolean, loc: Option[SourceLocation]): MethodDefinition = new MethodDefinition(key, value, kind, computed, static, loc)
	def unapply(methodDefinition: MethodDefinition): Option[(Expression, FunctionExpression, MethodKind, Boolean, Boolean)] = Some((methodDefinition.key, methodDefinition.value, methodDefinition.kind, methodDefinition.computed, methodDefinition.static))

	def from(src: Js.Value): MethodDefinition = {
		val _obj = src.obj
		assert(_obj("type").str == "MethodDefinition")

		new MethodDefinition(
			Expression.from(_obj("key")),
			FunctionExpression.from(_obj("value")),
			MethodKind.from(_obj("kind")),
			_obj("computed").bool,
			_obj("static").bool,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ClassDeclaration(id: Option[Identifier], superClass: Option[Expression], body: ClassBody, loc: Option[SourceLocation]) extends Class(id, superClass, body, loc) with Declaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ClassDeclaration"),
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"superClass" -> this.superClass.map(inner => inner.toJSON).getOrElse(Js.Null),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ClassDeclaration {
	def apply(id: Option[Identifier], superClass: Option[Expression], body: ClassBody, loc: Option[SourceLocation]): ClassDeclaration = new ClassDeclaration(id, superClass, body, loc)

	def from(src: Js.Value): ClassDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "ClassDeclaration")

		new ClassDeclaration(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj.get("superClass").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			ClassBody.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ClassExpression(id: Option[Identifier], superClass: Option[Expression], body: ClassBody, loc: Option[SourceLocation]) extends Class(id, superClass, body, loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ClassExpression"),
			"id" -> this.id.map(inner => inner.toJSON).getOrElse(Js.Null),
			"superClass" -> this.superClass.map(inner => inner.toJSON).getOrElse(Js.Null),
			"body" -> this.body.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ClassExpression {
	def apply(id: Option[Identifier], superClass: Option[Expression], body: ClassBody, loc: Option[SourceLocation]): ClassExpression = new ClassExpression(id, superClass, body, loc)

	def from(src: Js.Value): ClassExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "ClassExpression")

		new ClassExpression(
			_obj.get("id").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Identifier.from(inner)),
			_obj.get("superClass").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Expression.from(inner)),
			ClassBody.from(_obj("body")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class MetaProperty(val meta: Identifier, val property: Identifier, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("MetaProperty"),
			"meta" -> this.meta.toJSON,
			"property" -> this.property.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object MetaProperty {
	def apply(meta: Identifier, property: Identifier, loc: Option[SourceLocation]): MetaProperty = new MetaProperty(meta, property, loc)
	def unapply(metaProperty: MetaProperty): Option[(Identifier, Identifier)] = Some((metaProperty.meta, metaProperty.property))

	def from(src: Js.Value): MetaProperty = {
		val _obj = src.obj
		assert(_obj("type").str == "MetaProperty")

		new MetaProperty(
			Identifier.from(_obj("meta")),
			Identifier.from(_obj("property")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed trait ModuleDeclaration extends Node with ModuleStatement {
	def toJSON: Js.Value
}

object ModuleDeclaration {
	def from(src: Js.Value): ModuleDeclaration = src("type").str match {
		case "ImportDeclaration" => ImportDeclaration.from(src)
		case "ExportNamedDeclaration" => ExportNamedDeclaration.from(src)
		case "ExportDefaultDeclaration" => ExportDefaultDeclaration.from(src)
		case "ExportAllDeclaration" => ExportAllDeclaration.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ModuleDeclaration")
	}
}

sealed abstract class ModuleSpecifier(val local: Identifier, loc: Option[SourceLocation]) extends Node(loc) {
	def toJSON: Js.Value
}

object ModuleSpecifier {
	def unapply(moduleSpecifier: ModuleSpecifier): Option[Identifier] = Some(moduleSpecifier.local)

	def from(src: Js.Value): ModuleSpecifier = src("type").str match {
		case "ExportSpecifier" => ExportSpecifier.from(src)
		case "ImportSpecifier" => ImportSpecifier.from(src)
		case "ImportDefaultSpecifier" => ImportDefaultSpecifier.from(src)
		case "ImportNamespaceSpecifier" => ImportNamespaceSpecifier.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ModuleSpecifier")
	}
}

sealed class ImportDeclaration(val specifiers: Seq[ModuleImportSpecifier], val source: Literal, loc: Option[SourceLocation]) extends Node(loc) with ModuleDeclaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ImportDeclaration"),
			"specifiers" -> Js.Arr(this.specifiers.map(inner => inner.toJSON): _*),
			"source" -> this.source.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ImportDeclaration {
	def apply(specifiers: Seq[ModuleImportSpecifier], source: Literal, loc: Option[SourceLocation]): ImportDeclaration = new ImportDeclaration(specifiers, source, loc)
	def unapply(importDeclaration: ImportDeclaration): Option[(Seq[ModuleImportSpecifier], Literal)] = Some((importDeclaration.specifiers, importDeclaration.source))

	def from(src: Js.Value): ImportDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "ImportDeclaration")

		new ImportDeclaration(
			_obj("specifiers").arr.map(elem => ModuleImportSpecifier.from(elem)),
			Literal.from(_obj("source")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ImportSpecifier(val imported: Identifier, local: Identifier, loc: Option[SourceLocation]) extends ModuleSpecifier(local, loc) with ModuleImportSpecifier {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ImportSpecifier"),
			"imported" -> this.imported.toJSON,
			"local" -> this.local.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ImportSpecifier {
	def apply(imported: Identifier, local: Identifier, loc: Option[SourceLocation]): ImportSpecifier = new ImportSpecifier(imported, local, loc)
	def unapply(importSpecifier: ImportSpecifier): Option[Identifier] = Some(importSpecifier.imported)

	def from(src: Js.Value): ImportSpecifier = {
		val _obj = src.obj
		assert(_obj("type").str == "ImportSpecifier")

		new ImportSpecifier(
			Identifier.from(_obj("imported")),
			Identifier.from(_obj("local")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ImportDefaultSpecifier(local: Identifier, loc: Option[SourceLocation]) extends ModuleSpecifier(local, loc) with ModuleImportSpecifier {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ImportDefaultSpecifier"),
			"local" -> this.local.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ImportDefaultSpecifier {
	def apply(local: Identifier, loc: Option[SourceLocation]): ImportDefaultSpecifier = new ImportDefaultSpecifier(local, loc)

	def from(src: Js.Value): ImportDefaultSpecifier = {
		val _obj = src.obj
		assert(_obj("type").str == "ImportDefaultSpecifier")

		new ImportDefaultSpecifier(
			Identifier.from(_obj("local")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ImportNamespaceSpecifier(local: Identifier, loc: Option[SourceLocation]) extends ModuleSpecifier(local, loc) with ModuleImportSpecifier {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ImportNamespaceSpecifier"),
			"local" -> this.local.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ImportNamespaceSpecifier {
	def apply(local: Identifier, loc: Option[SourceLocation]): ImportNamespaceSpecifier = new ImportNamespaceSpecifier(local, loc)

	def from(src: Js.Value): ImportNamespaceSpecifier = {
		val _obj = src.obj
		assert(_obj("type").str == "ImportNamespaceSpecifier")

		new ImportNamespaceSpecifier(
			Identifier.from(_obj("local")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ExportNamedDeclaration(val declaration: Option[Declaration], val specifiers: Seq[ExportSpecifier], val source: Option[Literal], loc: Option[SourceLocation]) extends Node(loc) with ModuleDeclaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ExportNamedDeclaration"),
			"declaration" -> this.declaration.map(inner => inner.toJSON).getOrElse(Js.Null),
			"specifiers" -> Js.Arr(this.specifiers.map(inner => inner.toJSON): _*),
			"source" -> this.source.map(inner => inner.toJSON).getOrElse(Js.Null),
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ExportNamedDeclaration {
	def apply(declaration: Option[Declaration], specifiers: Seq[ExportSpecifier], source: Option[Literal], loc: Option[SourceLocation]): ExportNamedDeclaration = new ExportNamedDeclaration(declaration, specifiers, source, loc)
	def unapply(exportNamedDeclaration: ExportNamedDeclaration): Option[(Option[Declaration], Seq[ExportSpecifier], Option[Literal])] = Some((exportNamedDeclaration.declaration, exportNamedDeclaration.specifiers, exportNamedDeclaration.source))

	def from(src: Js.Value): ExportNamedDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "ExportNamedDeclaration")

		new ExportNamedDeclaration(
			_obj.get("declaration").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Declaration.from(inner)),
			_obj("specifiers").arr.map(elem => ExportSpecifier.from(elem)),
			_obj.get("source").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => Literal.from(inner)),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ExportSpecifier(val exported: Identifier, local: Identifier, loc: Option[SourceLocation]) extends ModuleSpecifier(local, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ExportSpecifier"),
			"exported" -> this.exported.toJSON,
			"local" -> this.local.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ExportSpecifier {
	def apply(exported: Identifier, local: Identifier, loc: Option[SourceLocation]): ExportSpecifier = new ExportSpecifier(exported, local, loc)
	def unapply(exportSpecifier: ExportSpecifier): Option[Identifier] = Some(exportSpecifier.exported)

	def from(src: Js.Value): ExportSpecifier = {
		val _obj = src.obj
		assert(_obj("type").str == "ExportSpecifier")

		new ExportSpecifier(
			Identifier.from(_obj("exported")),
			Identifier.from(_obj("local")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ExportDefaultDeclaration(val declaration: Exportable, loc: Option[SourceLocation]) extends Node(loc) with ModuleDeclaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ExportDefaultDeclaration"),
			"declaration" -> this.declaration.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ExportDefaultDeclaration {
	def apply(declaration: Exportable, loc: Option[SourceLocation]): ExportDefaultDeclaration = new ExportDefaultDeclaration(declaration, loc)
	def unapply(exportDefaultDeclaration: ExportDefaultDeclaration): Option[Exportable] = Some(exportDefaultDeclaration.declaration)

	def from(src: Js.Value): ExportDefaultDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "ExportDefaultDeclaration")

		new ExportDefaultDeclaration(
			Exportable.from(_obj("declaration")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class ExportAllDeclaration(val source: Literal, loc: Option[SourceLocation]) extends Node(loc) with ModuleDeclaration {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("ExportAllDeclaration"),
			"source" -> this.source.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object ExportAllDeclaration {
	def apply(source: Literal, loc: Option[SourceLocation]): ExportAllDeclaration = new ExportAllDeclaration(source, loc)
	def unapply(exportAllDeclaration: ExportAllDeclaration): Option[Literal] = Some(exportAllDeclaration.source)

	def from(src: Js.Value): ExportAllDeclaration = {
		val _obj = src.obj
		assert(_obj("type").str == "ExportAllDeclaration")

		new ExportAllDeclaration(
			Literal.from(_obj("source")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class AwaitExpression(val argument: Expression, loc: Option[SourceLocation]) extends Node(loc) with Expression {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("AwaitExpression"),
			"argument" -> this.argument.toJSON,
			"loc" -> this.loc.map(inner => inner.toJSON).getOrElse(Js.Null)
		)
}

object AwaitExpression {
	def apply(argument: Expression, loc: Option[SourceLocation]): AwaitExpression = new AwaitExpression(argument, loc)
	def unapply(awaitExpression: AwaitExpression): Option[Expression] = Some(awaitExpression.argument)

	def from(src: Js.Value): AwaitExpression = {
		val _obj = src.obj
		assert(_obj("type").str == "AwaitExpression")

		new AwaitExpression(
			Expression.from(_obj("argument")),
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
		)
	}
}

sealed class BooleanLiteral(val value: Boolean, raw: String, loc: Option[SourceLocation]) extends Literal(raw, loc) {
	override def toJSON: Js.Value = Js.Obj(
			"type" -> Js.Str("Literal"),
			"value" -> Js.Bool(this.value),
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
			_obj("value").bool,
			_obj("raw").str.toString,
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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
			_obj.get("loc").flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => SourceLocation.from(inner))
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

sealed trait ForInit extends Node {
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ForInit")
	}
}

sealed trait ForInTarget extends Node {
	def toJSON: Js.Value
}

object ForInTarget {
	def from(src: Js.Value): ForInTarget = src("type").str match {
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "Identifier" => Identifier.from(src)
		case "MemberExpression" => MemberExpression.from(src)
		case "ObjectPattern" => ObjectPattern.from(src)
		case "ArrayPattern" => ArrayPattern.from(src)
		case "RestElement" => RestElement.from(src)
		case "AssignmentPattern" => AssignmentPattern.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ForInTarget")
	}
}

sealed trait AssignmentTarget extends Node {
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
		case "ObjectPattern" => ObjectPattern.from(src)
		case "ArrayPattern" => ArrayPattern.from(src)
		case "RestElement" => RestElement.from(src)
		case "AssignmentPattern" => AssignmentPattern.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for AssignmentTarget")
	}
}

sealed trait ModuleStatement extends Node {
	def toJSON: Js.Value
}

object ModuleStatement {
	def from(src: Js.Value): ModuleStatement = src("type").str match {
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
		case "ForOfStatement" => ForOfStatement.from(src)
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "ClassDeclaration" => ClassDeclaration.from(src)
		case "ImportDeclaration" => ImportDeclaration.from(src)
		case "ExportNamedDeclaration" => ExportNamedDeclaration.from(src)
		case "ExportDefaultDeclaration" => ExportDefaultDeclaration.from(src)
		case "ExportAllDeclaration" => ExportAllDeclaration.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ModuleStatement")
	}
}

sealed abstract class VariableDeclarationKind {
	def toJSON: Js.Str
}

object VariableDeclarationKind {
	final case object `var` extends VariableDeclarationKind {
		override def toJSON: Js.Str = Js.Str("var")
		override def toString: String ="VariableDeclarationKind[var]"
	}

	final case object `let` extends VariableDeclarationKind {
		override def toJSON: Js.Str = Js.Str("let")
		override def toString: String ="VariableDeclarationKind[let]"
	}

	final case object `const` extends VariableDeclarationKind {
		override def toJSON: Js.Str = Js.Str("const")
		override def toString: String ="VariableDeclarationKind[const]"
	}


	def from(src: Js.Value): VariableDeclarationKind = src.str match {
		case "var" => `var`
		case "let" => `let`
		case "const" => `const`
	}
}

sealed trait SpreadableExpression extends Node {
	def toJSON: Js.Value
}

object SpreadableExpression {
	def from(src: Js.Value): SpreadableExpression = src("type").str match {
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
		case "SpreadElement" => SpreadElement.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for SpreadableExpression")
	}
}

sealed trait Callee extends Node {
	def toJSON: Js.Value
}

object Callee {
	def from(src: Js.Value): Callee = src("type").str match {
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
		case "Super" => Super.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Callee")
	}
}

sealed trait ArrowFunctionBody extends Node {
	def toJSON: Js.Value
}

object ArrowFunctionBody {
	def from(src: Js.Value): ArrowFunctionBody = src("type").str match {
		case "BlockStatement" => FunctionBody.from(src)
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ArrowFunctionBody")
	}
}

sealed abstract class MethodKind {
	def toJSON: Js.Str
}

object MethodKind {
	final case object `constructor` extends MethodKind {
		override def toJSON: Js.Str = Js.Str("constructor")
		override def toString: String ="MethodKind[constructor]"
	}

	final case object `method` extends MethodKind {
		override def toJSON: Js.Str = Js.Str("method")
		override def toString: String ="MethodKind[method]"
	}

	final case object `get` extends MethodKind {
		override def toJSON: Js.Str = Js.Str("get")
		override def toString: String ="MethodKind[get]"
	}

	final case object `set` extends MethodKind {
		override def toJSON: Js.Str = Js.Str("set")
		override def toString: String ="MethodKind[set]"
	}


	def from(src: Js.Value): MethodKind = src.str match {
		case "constructor" => `constructor`
		case "method" => `method`
		case "get" => `get`
		case "set" => `set`
	}
}

sealed trait ModuleImportSpecifier extends ModuleSpecifier {
	def toJSON: Js.Value
}

object ModuleImportSpecifier {
	def from(src: Js.Value): ModuleImportSpecifier = src("type").str match {
		case "ImportSpecifier" => ImportSpecifier.from(src)
		case "ImportDefaultSpecifier" => ImportDefaultSpecifier.from(src)
		case "ImportNamespaceSpecifier" => ImportNamespaceSpecifier.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for ModuleImportSpecifier")
	}
}

sealed trait Exportable extends Node {
	def toJSON: Js.Value
}

object Exportable {
	def from(src: Js.Value): Exportable = src("type").str match {
		case "FunctionDeclaration" => FunctionDeclaration.from(src)
		case "VariableDeclaration" => VariableDeclaration.from(src)
		case "ClassDeclaration" => ClassDeclaration.from(src)
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
		case "ArrowFunctionExpression" => ArrowFunctionExpression.from(src)
		case "YieldExpression" => YieldExpression.from(src)
		case "TemplateLiteral" => TemplateLiteral.from(src)
		case "TaggedTemplateExpression" => TaggedTemplateExpression.from(src)
		case "ClassExpression" => ClassExpression.from(src)
		case "MetaProperty" => MetaProperty.from(src)
		case "AwaitExpression" => AwaitExpression.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for Exportable")
	}
}

sealed trait SpreadableProperty {
	def toJSON: Js.Value
}

object SpreadableProperty {
	def from(src: Js.Value): SpreadableProperty = src("type").str match {
		case "Property" => Property.from(src)
		case "SpreadElement" => SpreadElement.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for SpreadableProperty")
	}
}

sealed trait PropertyPattern {
	def toJSON: Js.Value
}

object PropertyPattern {
	def from(src: Js.Value): PropertyPattern = src("type").str match {
		case "Property" => AssignmentProperty.from(src)
		case "RestElement" => RestElement.from(src)
		case discriminant => throw new IllegalArgumentException(s"Unknown type '$discriminant' for PropertyPattern")
	}
}

