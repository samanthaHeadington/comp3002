import Foundation

let parserInput: String = try String(contentsOf: URL(fileURLWithPath: "Sources/FSMBuilder/Assets.xcassets/parserFSMs.txt"), encoding: .utf8)
let scannerInput: String = try String(contentsOf: URL(fileURLWithPath: "Sources/FSMBuilder/Assets.xcassets/scannerFSMs.txt"), encoding: .utf8)

// part 1

let testGrammar : Grammar = Grammar();

testGrammar.addNonterminal("#A");

Grammar.activeGrammar = testGrammar;

print(Grammar.defaultsFor("#A"));

testGrammar.type = "parser";

print(Grammar.defaultsFor("#A"));

// part 2

print(FSMBuilder.example1(parserFSM: parserInput, scannerFSMs: scannerInput));
