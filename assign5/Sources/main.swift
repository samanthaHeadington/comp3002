import Foundation

let input: String = try String(
    contentsOf: URL(fileURLWithPath: "Sources/Grammars/toyLispGrammar.txt"), encoding: .utf8)

// part 1

// let testGrammar : Grammar = Grammar();

// testGrammar.addNonterminal("#A");

// Grammar.activeGrammar = testGrammar;

// print(Grammar.defaultsFor("#A"));

// testGrammar.type = "parser";

// print(Grammar.defaultsFor("#A"));

// part 2 //

print(Constructor.example1(grammar_text: input))

// Relation<Int, String>.example4();
