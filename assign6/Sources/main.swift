import Foundation

let input: String = try String(
    contentsOf: URL(fileURLWithPath: "Sources/Grammars/LISPGrammarWithInvisibles.txt"),
    encoding: .utf8)

// first follow
// let input : String = try String(
//     contentsOf: URL(fileURLWithPath: "Sources/Grammars/toyParserGrammarToTestFollowSets.txt"),
//     encoding: .utf8)

// scanner
//let input: String = try String(contentsOf: URL(fileURLWithPath: "Sources/Grammars/toyScannerGrammar.txt"), encoding: .utf8)

// part 1

// let testGrammar : Grammar = Grammar();

// testGrammar.addNonterminal("#A");

// Grammar.activeGrammar = testGrammar;

// print(Grammar.defaultsFor("#A"));

// testGrammar.type = "parser";

// print(Grammar.defaultsFor("#A"));

// part 2 //

print(Constructor.example1(grammar_text: input))

//Constructor.example1(grammar_text: input_scanner)

//Constructor.exampleWithFirstFollow(grammar_text: input_first_follow)

// Relation<Int, String>.example4();
