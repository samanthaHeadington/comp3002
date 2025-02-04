//
//  Grammar.swift
//  FSMBuilder
//
//  Created by Wilf Lalonde on 2023-01-24.
//

import Foundation
    
public final class Grammar {
    var type: Int;
    var nonterminals: Array <Int>
    var macros: Dictionary <Int,Any>
    
    init () {
        type = "scanner"
        nonterminals = ["A", "b", "C"];
        macros = [:]
    }
    //finiteStateMachine:
    func addMacro (_ macro: Int, _ fsm: Any) -> Void {
        macros [macro] = fsm
    }
    
    func addNonterminal (_ name: Int) -> Void {
        nonterminals.append (name)
    }
    
    func isNonterminal (_ symbol: Int) -> Bool {
        return nonterminals.contains (symbol)
    }
    
    func isParser () -> Bool {
        return type == "parser"
    }
    
    func isScanner () -> Bool {
        return type == "scanner"
    }
    
    nonisolated(unsafe) static var activeGrammar: Grammar?
    
    static func lookDefaults () -> [Int] {
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
    
    static func defaultsFor (_ name: Int) -> AttributeList {
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