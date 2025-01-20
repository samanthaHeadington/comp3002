//
//  SampleTranslator.swift
//  SampleTranslator
//
//  Created by Wilf Lalonde on 2022-12-19
//  and Jeeheon Kim (for Wilf Lalonde) on 2022-1-10
//


import Foundation

typealias compileClosure = (VirtualTree) -> Void
typealias evaluateClosure = (VirtualTree) -> Int

public final class SampleTranslator: Translator {
  var parser: Parser?
  var tree: VirtualTree? = nil
  var variableDictionary: [String : Int] = [:];
  var codeIfCompiler: InputStream = InputStream()
    
  public final class InputStream {
    public var content: String = ""
    
    final func write(_ text: String){
      content += "\n\(text)"
    }
    
    final func show() {
      Swift.print(content)
    }
  }
  var compilationOperatorMap: [String: compileClosure]?
  var evaluationOperatorMap: [String: evaluateClosure]?
    
  public func canPerformAction(_ :String) -> Bool {return false}
  public func performAction(_ :String, _ :[Any]) -> Void {}

  init() {
    resetParser();
    // codeIfCompiler <- not sure what this does
    // codeIfCompiler = TextOutputStream()
    compilationOperatorMap = [
      "+": compilePlus,
      "*": compileMultiply,
      "-": compileMinus,
      "/": compileDivide,
      "<-": compileAssign,
      "Identifier": compileIdentifier,
      "Integer": compileInteger,
      "where": compileWhere,
      "send": compileFunctionCall,
    ]
    evaluationOperatorMap = [
      "+": evaluatePlus,
      "*": evaluateMultiply,
      "-": evaluateMinus,
      "/": evaluateDivide,
      "<-": evaluateAssign,
      "Identifier": evaluateIdentifier,
      "Integer": evaluateInteger,
      "where": evaluateWhere
      // "send": "evaluateFunctionCall",
    ]
  }

  // reinitializes the parser after each compilation/evaluation to prevent crashing
  func resetParser(){
    parser = Parser(sponsor: self, parserTables: parserTables, scannerTables: scannerTables)
  }

  func compile(text: String) -> String {
      tree = parser!.parse(text)
      guard tree != nil else {
          error("CompilationError: tree failed to build")
        return ""
      }
      compileExpressionFor(tree!)

      resetParser();

      return codeIfCompiler.content
  }

  func compileExpressionFor(_ tree: VirtualTree) {
    if let operatorMapFunction = compilationOperatorMap?[tree.label] {
      operatorMapFunction(tree)
    }
  }


  func compilePlus(_ tree: VirtualTree) -> Void {
    let t = tree as! Tree
    compileExpressionFor(t.children[0])
    compileExpressionFor(t.children[1])
    generate(instruction: "PLUS")
  }

  func compileMultiply(_ tree: VirtualTree) -> Void{
    let t = tree as! Tree
    compileExpressionFor(t.children[0])
    compileExpressionFor(t.children[1])
    generate(instruction: "MULTIPLY")
  }

  func compileMinus(_ tree: VirtualTree) -> Void {
    let t = tree as! Tree
    compileExpressionFor(t.children[0])
    compileExpressionFor(t.children[1])
    generate(instruction: "MINUS")
  }

  func compileDivide(_ tree: VirtualTree) -> Void{
    let t = tree as! Tree
    compileExpressionFor(t.children[0])
    compileExpressionFor(t.children[1])
    generate(instruction: "DIVIDE")
  }

  func compileAssign(_ tree: VirtualTree) -> Void {
    let t = tree as! Tree
    for index in t.children.indices {
      compileExpressionFor(t.children[index])
      generate(instruction: "POP")
    }
  }

  func compileWhere(_ tree: VirtualTree) -> Void{
    let t: Tree = tree as! Tree;

    compileExpressionFor(t.children[1]);

    compileExpressionFor(t.children[0]);
  }

  func compileIdentifier(_ token: VirtualTree) -> Void {
    generate(instruction: "PUSH", with: (token as! Token).symbol)
  }

  func compileInteger(_ token: VirtualTree) -> Void {
    generate(instruction: "PUSH", with: Int((token as! Token).symbol) ?? -1 )
  }

  func compileFunctionCall(_ tree: VirtualTree) -> Void {
    let t = tree as! Tree
    let childrenIndices: CountableClosedRange = 1...t.children.count
    for index in childrenIndices {
      compileExpressionFor(t.children[index])
    }
    let aToken = t.children[0] as! Token
    generate(instruction: "FUNCTION_CALL", with: aToken.symbol)
  }

  func generate(instruction: String) {
    codeIfCompiler.write("\(instruction)")
  }

  func generate(instruction: String, with: String) {
    let string = "\(instruction) \(with)"
    codeIfCompiler.write(string)
  }
  
  func generate(instruction: String, with: Int) {
    let string = "\(instruction) \(with)\n"
    codeIfCompiler.write(string)
  }

  func evaluate(text: String) -> Any? {
    // If no variables are set up, just return the expression
    // Otherwise, return a dictionary of variables
    tree = parser!.parse(text)
    guard tree != nil else {
        error("CompilationError: tree failed to build")
        return nil
      }

      print("\(tree)");

      // reset the parser so it can handle the next request
      resetParser();

      let result = evaluateExpressionFor(tree!) // see result and variable dictionary
      if variableDictionary.count == 0 {
        return result
      } else {
        return (result, variableDictionary);
      }
  }

  func evaluateExpressionFor(_ tree: VirtualTree) -> Int {
    return evaluationOperatorMap![tree.label]!(tree)
  }

  func evaluatePlus(_ tree: VirtualTree) -> Int {
    let t = tree as! Tree
    let exp1 = evaluateExpressionFor(t.children[0])
    let exp2 = evaluateExpressionFor(t.children[1])
    return exp1 + exp2
  }

  func evaluateMultiply(_ tree: VirtualTree) -> Int {
    let t = tree as! Tree
    let exp1 = evaluateExpressionFor(t.children[0])
    let exp2 = evaluateExpressionFor(t.children[1])
    return exp1 * exp2
  }

  func evaluateMinus(_ tree: VirtualTree) -> Int{
    let t = tree as! Tree
    let exp1 = evaluateExpressionFor(t.children[0])
    let exp2 = evaluateExpressionFor(t.children[1])
    return exp1 - exp2
  }

  func evaluateDivide(_ tree: VirtualTree) -> Int{
    let t = tree as! Tree
    let exp1 = evaluateExpressionFor(t.children[0])
    let exp2 = evaluateExpressionFor(t.children[1])
    return exp1 / exp2
  }

  func evaluateIdentifier(_ token: VirtualTree) -> Int {
    let t = token as! Token

    let identifier = t.symbol

    return variableDictionary[identifier]!;
  }

  func evaluateInteger(_ token: VirtualTree) -> Int {
    let t = token as! Token
    return Int(t.symbol)!
  }

  func evaluateAssign(_ tree: VirtualTree) -> Int {
    let t = tree as! Tree;
    var token: Token = t.children[0] as! Token;

    //(1..<t.children.count).filter {!$0.odd()}

    // range of i is filtered since the identifiers are the even children of the assignment token
    for i in (0..<t.children.count).filter({!$0.odd()}){
      token = t.children[i] as! Token;
      variableDictionary[token.symbol] = evaluateExpressionFor(t.children[i+1]);
    }

    return variableDictionary[token.symbol]!;
  }


  func evaluateWhere(_ tree: VirtualTree) -> Int { 
    let t = tree as! Tree;

    // evaluate right child of where statement (variable assignment) first
    _ = evaluateExpressionFor(t.children[1]);

    // once variable assignment is performed, evaluate left child (expression)
    return evaluateExpressionFor(t.children[0]);
  }
    

var scannerTables: Array<Any> = [
   ["ScannerReadaheadTable", 1, ([256], "L", 7), ("ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 4), (")", "RK", 9), ("*", "RK", 10), ("+", "RK", 11), (",", "RK", 12), ("-", "RK", 13), ("(", "RK", 8), ("/", "RK", 2), ("0123456789", "RK", 3), ([9,10,12,13,32], "R", 5), (";", "RK", 16), ("=", "RK", 17)],
   ["ScannerReadaheadTable", 2, ([9,10,12,13,32,256], "L", 14), ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789+-*=[]{}()^;#:.$'\"", "L", 14), ("/", "R", 6)],
   ["ScannerReadaheadTable", 3, ([9,10,12,13,32,256], "L", 15), ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_+-*/=[]{}()^;#:.$'\"", "L", 15), ("0123456789", "RK", 3)],
   ["ScannerReadaheadTable", 4, ([9,10,12,13,32,256], "L", 18), ("+-*/=[]{}()^;#:.$'\"", "L", 18), ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 4)],
   ["ScannerReadaheadTable", 5, ([256], "L", 1), ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789+-*/=[]{}()^;#:.$'\"", "L", 1), ([9,10,12,13,32], "R", 5)],
   ["ScannerReadaheadTable", 6, ([9,12,13,32,256], "R", 6), ("=ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijklmnopqrstuvwxyz{}\"#$'()*+-./0123456789:;", "R", 6), ([10], "R", 1)],
   ["SemanticTable", 7, "buildToken", ["-|"], 1],
   ["SemanticTable", 8, "buildToken", ["("], 1],
   ["SemanticTable", 9, "buildToken", [")"], 1],
   ["SemanticTable", 10, "buildToken", ["*"], 1],
   ["SemanticTable", 11, "buildToken", ["+"], 1],
   ["SemanticTable", 12, "buildToken", [","], 1],
   ["SemanticTable", 13, "buildToken", ["-"], 1],
   ["SemanticTable", 14, "buildToken", ["/"], 1],
   ["SemanticTable", 15, "buildToken", ["Integer"], 1],
   ["SemanticTable", 16, "buildToken", [";"], 1],
   ["SemanticTable", 17, "buildToken", ["="], 1],
   ["SemanticTable", 18, "buildToken", ["Identifier"], 1]];

var parserTables: Array<Any> = [
   ["keywords", "where"],
   ["ReadaheadTable", 1, ("Assignments", "RSN", 32), ("Expression", "RSN", 2), ("Term", "RSN", 3), ("Primary", "RSN", 34), ("Integer", "RSN", 35), ("Identifier", "RSN", 4), ("Grammar", "RSN", 59), ("(", "RS", 5)],
   ["ReadaheadTable", 2, ("+", "RS", 6), ("-", "RS", 7), ("where", "RS", 8), ("-|", "L", 32)],
   ["ReadaheadTable", 3, ("*", "RS", 9), ("/", "RS", 10), (")", "L", 33), (",", "L", 33), ("where", "L", 33), ("-|", "L", 33), (";", "L", 33), ("+", "L", 33), ("-", "L", 33)],
   ["ReadaheadTable", 4, ("(", "RS", 11), ("=", "RS", 12), ("*", "L", 35), ("/", "L", 35), (")", "L", 35), (",", "L", 35), ("where", "L", 35), ("-|", "L", 35), (";", "L", 35), ("+", "L", 35), ("-", "L", 35)],
   ["ReadaheadTable", 5, ("Expression", "RSN", 13), ("Primary", "RSN", 34), ("Term", "RSN", 3), ("Integer", "RSN", 35), ("Identifier", "RSN", 14), ("(", "RS", 5)],
   ["ReadaheadTable", 6, ("Identifier", "RSN", 14), ("(", "RS", 5), ("Integer", "RSN", 35), ("Primary", "RSN", 34), ("Term", "RSN", 15)],
   ["ReadaheadTable", 7, ("Identifier", "RSN", 14), ("(", "RS", 5), ("Integer", "RSN", 35), ("Primary", "RSN", 34), ("Term", "RSN", 16)],
   ["ReadaheadTable", 8, ("Identifier", "RSN", 17), ("Assignments", "RSN", 39)],
   ["ReadaheadTable", 9, ("Identifier", "RSN", 14), ("(", "RS", 5), ("Integer", "RSN", 35), ("Primary", "RSN", 40)],
   ["ReadaheadTable", 10, ("Identifier", "RSN", 14), ("(", "RS", 5), ("Integer", "RSN", 35), ("Primary", "RSN", 41)],
   ["ReadaheadTable", 11, ("Expression", "RSN", 18), (")", "RS", 42), ("Primary", "RSN", 34), ("Term", "RSN", 3), ("Integer", "RSN", 35), ("Identifier", "RSN", 14), ("(", "RS", 5)],
   ["ReadaheadTable", 12, ("Expression", "RSN", 19), ("Primary", "RSN", 34), ("Term", "RSN", 3), ("Integer", "RSN", 35), ("Identifier", "RSN", 14), ("(", "RS", 5)],
   ["ReadaheadTable", 13, ("+", "RS", 6), ("-", "RS", 7), (")", "RS", 36)],
   ["ReadaheadTable", 14, ("(", "RS", 11), ("*", "L", 35), ("/", "L", 35), (")", "L", 35), (",", "L", 35), ("where", "L", 35), ("-|", "L", 35), (";", "L", 35), ("+", "L", 35), ("-", "L", 35)],
   ["ReadaheadTable", 15, ("*", "RS", 9), ("/", "RS", 10), (")", "L", 37), (",", "L", 37), ("where", "L", 37), ("-|", "L", 37), (";", "L", 37), ("+", "L", 37), ("-", "L", 37)],
   ["ReadaheadTable", 16, ("*", "RS", 9), ("/", "RS", 10), (")", "L", 38), (",", "L", 38), ("where", "L", 38), ("-|", "L", 38), (";", "L", 38), ("+", "L", 38), ("-", "L", 38)],
   ["ReadaheadTable", 17, ("=", "RS", 12)],
   ["ReadaheadTable", 18, (",", "RS", 20), (")", "RS", 42), ("+", "RS", 6), ("-", "RS", 7)],
   ["ReadaheadTable", 19, (";", "RS", 21), ("+", "RS", 6), ("-", "RS", 7)],
   ["ReadaheadTable", 20, ("Expression", "RSN", 22), ("Primary", "RSN", 34), ("Term", "RSN", 3), ("Integer", "RSN", 35), ("Identifier", "RSN", 14), ("(", "RS", 5)],
   ["ReadaheadTable", 21, ("Identifier", "RSN", 17), ("-|", "L", 43)],
   ["ReadaheadTable", 22, (",", "RS", 20), (")", "RS", 42), ("+", "RS", 6), ("-", "RS", 7)],
   ["ReadbackTable", 23, (("Expression", 18), "RSN", 45), (("(", 11), "RS", 28), (("Expression", 22), "RSN", 46)],
   ["ReadbackTable", 24, (("Expression", 2), "RSN", 52), (("Expression", 13), "RSN", 52), (("Expression", 18), "RSN", 52), (("Expression", 19), "RSN", 52), (("Expression", 22), "RSN", 52)],
   ["ReadbackTable", 25, (("Expression", 2), "RSN", 53), (("Expression", 13), "RSN", 53), (("Expression", 18), "RSN", 53), (("Expression", 19), "RSN", 53), (("Expression", 22), "RSN", 53)],
   ["ReadbackTable", 26, (("Term", 3), "RSN", 55), (("Term", 15), "RSN", 55), (("Term", 16), "RSN", 55)],
   ["ReadbackTable", 27, (("Term", 3), "RSN", 56), (("Term", 15), "RSN", 56), (("Term", 16), "RSN", 56)],
   ["ReadbackTable", 28, (("Identifier", 4), "RSN", 57), (("Identifier", 14), "RSN", 57)],
   ["ReadbackTable", 29, (("Expression", 18), "RSN", 45), (("Expression", 22), "RSN", 46)],
   ["ReadbackTable", 30, (("Identifier", 4), "RSN", 58), (("Identifier", 17), "RSN", 31)],
   ["ReadbackTable", 31, ((";", 21), "RS", 44), (("where", 8), "L", 58)],
   ["ShiftbackTable", 32, 1, 50],
   ["ShiftbackTable", 33, 1, 48],
   ["ShiftbackTable", 34, 1, 49],
   ["ShiftbackTable", 35, 1, 47],
   ["ShiftbackTable", 36, 3, 47],
   ["ShiftbackTable", 37, 2, 24],
   ["ShiftbackTable", 38, 2, 25],
   ["ShiftbackTable", 39, 3, 54],
   ["ShiftbackTable", 40, 2, 26],
   ["ShiftbackTable", 41, 2, 27],
   ["ShiftbackTable", 42, 1, 23],
   ["ShiftbackTable", 43, 3, 30],
   ["ShiftbackTable", 44, 2, 30],
   ["ShiftbackTable", 45, 1, 28],
   ["ShiftbackTable", 46, 1, 29],
   ["ReduceTable", 47, "Primary", (1, "RSN", 34), (5, "RSN", 34), (6, "RSN", 34), (7, "RSN", 34), (9, "RSN", 40), (10, "RSN", 41), (11, "RSN", 34), (12, "RSN", 34), (20, "RSN", 34)],
   ["ReduceTable", 48, "Expression", (1, "RSN", 2), (5, "RSN", 13), (11, "RSN", 18), (12, "RSN", 19), (20, "RSN", 22)],
   ["ReduceTable", 49, "Term", (1, "RSN", 3), (5, "RSN", 3), (6, "RSN", 15), (7, "RSN", 16), (11, "RSN", 3), (12, "RSN", 3), (20, "RSN", 3)],
   ["ReduceTable", 50, "Grammar", (1, "RSN", 59)],
   ["ReduceTable", 51, "Assignments", (1, "RSN", 32), (8, "RSN", 39)],
   ["SemanticTable", 52, "buildTree", ["+"], 48],
   ["SemanticTable", 53, "buildTree", ["-"], 48],
   ["SemanticTable", 54, "buildTree", ["where"], 50],
   ["SemanticTable", 55, "buildTree", ["*"], 49],
   ["SemanticTable", 56, "buildTree", ["/"], 49],
   ["SemanticTable", 57, "buildTree", ["send"], 47],
   ["SemanticTable", 58, "buildTree", ["<-"], 51],
   ["AcceptTable", 59]];
}