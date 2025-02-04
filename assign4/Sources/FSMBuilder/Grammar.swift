//
//  Grammar.swift
//  FSMBuilder
//
//  Created by Wilf Lalonde on 2023-01-24.
//

import Foundation
    
public final class Grammar {
    var type: String;
    var nonterminals: Array <String>
    var macros: Dictionary <String,Any>
    
    init () {
        type = "scanner"
        nonterminals = ["A", "b", "C"];
        macros = [:]
    }
    //finiteStateMachine:
    func addMacro (_ macro: String, _ fsm: Any) -> Void {
        macros [macro] = fsm
    }
    
    func addNonterminal (_ name: String) -> Void {
        nonterminals.append (name)
    }
    
    func isNonterminal (_ symbol: String) -> Bool {
        return nonterminals.contains (symbol)
    }
    
    func isParser () -> Bool {
        return type == "parser"
    }
    
    func isScanner () -> Bool {
        return type == "scanner"
    }
    
    nonisolated(unsafe) static var activeGrammar: Grammar?
    
    static func lookDefaults () -> [String] {
        return ["look"]
    }
    
    static func scannerDefaults () -> AttributeList {
        return AttributeList ().set (["read", "keep", "noStack", "noNode"])
    }
    
    static func parserTerminalDefaults () -> AttributeList {
        return AttributeList ().set(["read", "noKeep", "stack", "noNode"])
    }
    
    static func parserNonterminalDefaults () -> AttributeList {
        return AttributeList ().set (["read", "noKeep", "stack", "node"])
    }
    
    static func defaultsFor (_ name: String) -> AttributeList {
        let grammar = activeGrammar
        if (grammar == nil) {return scannerDefaults()}
        if (grammar!.isScanner()) {return scannerDefaults ()}
        if (grammar!.isNonterminal (name)) {
            return parserNonterminalDefaults()
        } else {
            return parserTerminalDefaults()
        }
    }
            
    static func isPrintable(_ value: Int) -> Bool {
        // Printable ASCII characters range from 32 (space) to 126 (~)
        // Int ("a".first!.asciiValue!) gives you 97.
        // String(Character(UnicodeScalar(97)) gives you "a"
         return (32...126).contains(value)
    }
}