import Foundation


let parser_input: String = try String(
    contentsOf: URL(fileURLWithPath: "Sources/Grammars/realParserGrammar.txt"),
        encoding: .utf8)

let scanner_input: String = try String(
    contentsOf: URL(fileURLWithPath: "Sources/Grammars/realScannerGrammar.txt"),
        encoding: .utf8)

print(Constructor.example1(grammar_text: parser_input))

print(Constructor.example1(grammar_text: scanner_input))
