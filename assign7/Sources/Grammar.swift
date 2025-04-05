//Start by creating a copy of FSMBuilder and call it GrammarBuilder

//
// CODE YOU WILL HAVE TO ADD TO YOUR EXISTING CODE.
//

import Foundation

//My code uses these routines.
extension Array where Element: Equatable {
    mutating func addIfAbsentAdded(_ object: Element) -> Bool {
        if contains(object) { return false }
        append(object)
        return true
    }
    mutating func addAllIfAbsentAdded(_ collection: [Element]) -> Bool {
        var changed: Bool = false
        for item in collection {
            if addIfAbsentAdded(item) { changed = true }
        }
        return changed
    }
}

class Production: CustomStringConvertible {
    var leftPart: String = ""
    var lookahead: [String]?
    var fsm: FiniteStateMachine = FiniteStateMachine()
    var generatesE: Bool = false
    var firstSet: [String] = []
    var followSet: [String] = []

    func name(_ newName: String) { leftPart = newName }
    func rightPart() -> FiniteStateMachine { return fsm }
    func get_fsm() -> FiniteStateMachine { return fsm }
    public var description: String {
        var string = leftPart
        if lookahead != nil {
            string += " {"
            var index = 0
            for symbol in lookahead! {
                if index > 0 { string += " " }
                index += 1
                string += symbol
            }
            string += "}"
        }
        string += " -> " + rightPart().description
        return string
    }

    func isGoal() -> Bool { return lookahead != nil }

}

//For the following, just add the code you are missing...

class Grammar: CustomStringConvertible {
    var type: String = ""
    var nonterminals: [String] = []
    var macros: [String: FiniteStateMachine] = [:]
    var productions: [String: Production] = [:]
    var keywords: [String] = []
    nonisolated(unsafe) static var activeGrammar: Grammar?

    func productionFor(_ name: String) -> Production {
        return (productions[name])!
    }
    func isNonterminal(_ name: String) -> Bool {
        return nonterminals.contains(name)
    }
    func totalStates() -> Int {
        var return_val = 0
        for production in productions {
            return_val += production.value.fsm.states.count
        }
        return return_val
    }

    func isScanner() -> Bool { return type == "scanner" }
    func isParser() -> Bool { return type == "grammar" || type == "parser" }
    func addMacro(_ str: String, _ fsm: FiniteStateMachine) { macros[str] = fsm }
    func addProduction(_ str: String, _ production: Production) { productions[str] = production }

    static func scannerDefaults() -> AttributeList {
        return AttributeList().set(["read", "keep", "noStack", "noNode"])
    }

    static func parserTerminalDefaults() -> AttributeList {
        return AttributeList().set(["read", "noKeep", "stack", "noNode"])
    }

    static func parserNonterminalDefaults() -> AttributeList {
        return AttributeList().set(["read", "noKeep", "stack", "node"])
    }

    static func defaultsFor(_ name: String) -> AttributeList {
        let grammar = activeGrammar
        if grammar == nil { return scannerDefaults() }
        if grammar!.isScanner() { return scannerDefaults() }
        if grammar!.isNonterminal(name) {
            return parserNonterminalDefaults()
        } else {
            return parserTerminalDefaults()
        }
    }

    func initialStateOfGoals() -> [FiniteStateMachineState] {
        var return_val: [FiniteStateMachineState] = []
        for (key, value) in productions {
            return_val.appendIf(
                contentsOf: value.fsm.states.filter { $0.isInitial }, value.isGoal())
        }
        return return_val
    }

    func isGoal(_ nonterminal: String) -> Bool {
        for (key, value) in productions {
            if key == nonterminal && value.isGoal() {
                return true
            }
        }

        return false
    }

    func isReadTerminalTransition(_ transition: Transition) -> Bool {
        if transition.label.hasAction() { return false }  //Otherwise, it has attributes"
        if isNonterminal(transition.label.terseDescription) { return false }
        return transition.label.attributes.isRead
    }

    func isNonterminalTransition(_ transition: Transition) -> Bool {
        if transition.label.hasAction() { return false }  //Otherwise, it has attributes"
        if isNonterminal(transition.label.terseDescription) { return true }
        return false
    }

    func isETransitionLabel(_ label: Label) -> Bool {
        if label.hasAction() { return true }
        //So it must be a name with attributes...
        let name = label.terseDescription
        if isNonterminal(name) { return productionFor(name).generatesE }
        //So it must be a nonterminal.
        if !label.attributes.isRead { return true }  //because its a look
        //None of the 3 cases apply, so...
        return false
    }

    func goalProductions() -> [Production] {
        var goalProductions: [Production] = []
        for (_, production) in productions {
            if production.isGoal() { goalProductions.append(production) }
        }
        return goalProductions
    }

    func eSuccessors(_ fsmStates: [FiniteStateMachineState]) -> [FiniteStateMachineState] {
        //Revised by Eric Leblanc.
        var result: [FiniteStateMachineState] = []
        for state in fsmStates { result.append(state) }
        //for state in result {//Swift won't allow state to encounter results that were recently added.
        var i = 0
        while i < result.count {
            let state = result[i]
            state.transitionsDo {
                if isETransitionLabel($0.label){ result.appendIfAbsent($0.goto) }
            }
            i += 1
        }
        return result
    }

    func eSuccessors(_ raStates: [ReadaheadState]) -> [ReadaheadState] {
        //Revised by Eric Leblanc.
        var result: [ReadaheadState] = []
        for state in raStates { result.append(state) }
        //for state in result {//Swift won't allow state to encounter results that were recently added.
        var i = 0
        while i < result.count {
            let state = result[i]
            state.transitionsDo {
                if isETransitionLabel($0.label) && $0.goto as? ReadaheadState != nil{ result.appendIfAbsent($0.goto as! ReadaheadState) }
            }
            i += 1
        }
        return result
    }

    public var description: String {
        return "\(type)\n" + "\(nonterminals)\n" + "\(macros)\n" + "\(productions)\n"
            + "\(keywords)"
    }

    func computeEGeneratingNonterminals() {
        var changed: Bool = true
        while changed {
            changed = false
            for (A, production) in productions {  //A is left part (for information only)
                if !production.generatesE {
                    for state in eSuccessors(production.fsm.states.filter { $0.isInitial }) {  //of A
                        if state.isFinal {
                            production.generatesE = true
                            changed = true
                        }
                    }
                }
            }
        }
    }

    func computeFirstSets() {
        //Need to invent a method so I can say 'aCollection addIfAbsentAdded (anObject) and addAllIf...'.

        var changed: Bool = true
        while changed {
            changed = false
            for (A, production) in productions {  //A is for information only
                for state in eSuccessors(production.fsm.initialStates()) {
                    for transition in state.transitions {
                        if isReadTerminalTransition(transition) {
                            if production.firstSet.addIfAbsentAdded(
                                transition.label.terseDescription)
                            {
                                changed = true
                            }
                            //if (!production.firstSet.contains)
                        }
                        if isNonterminalTransition(transition) {
                            var M = transition.label.terseDescription  //NOT for information only
                            if production.firstSet.addAllIfAbsentAdded(
                                productionFor(M).firstSet)
                            {
                                changed = true
                            }
                        }
                    }
                }
            }
        }
    }

    func computeFollowSets() {
        //    A copy of the diagram in text form that the notes used as an aid to build follow sets...
        //
        //    A -> ... p via B to q ... e-successor ... r via a or C
        //        a => add a to Follow(B)
        //        C => add First(C) to Follow(B)
        //        if r is final => add Follow(A) to Follow(B)
        //
        //Needed to invent a method so I can say 'if anArray.addAllIfAbsentAdded (collection) {... do something ...}.
        //Note: added variable names match the diagram in the notes (replicated in text up above).

        var changed: Bool = false

        //Start off by adding the lookahead to the follow set of the goal...
        for (_, production) in productions {
            if production.lookahead != nil {
                production.followSet.append(contentsOf: production.lookahead!)  //Shouldn't have duplicates
            }
        }

        changed = true
        while changed {
            changed = false
            for (A, production) in productions {  //A is for information only not to be confuxed with B or C
                production.fsm.transitionsDo { (_ transition: Transition) -> Void in
                    if isNonterminalTransition(transition) {
                        let B = transition.label.terseDescription
                        let Bproduction = productionFor(B)
                        let q = transition.goto

                        for r in eSuccessors([q]) {
                            for rTransition in r.transitions {
                                if isReadTerminalTransition(rTransition) {
                                    let a = rTransition.label
                                    if Bproduction.followSet.addIfAbsentAdded(a.terseDescription) {
                                        changed = true
                                    }
                                } else if isNonterminalTransition(rTransition) {
                                    let C = rTransition.label
                                    if Bproduction.followSet.addAllIfAbsentAdded(
                                        productionFor(C.terseDescription).firstSet)
                                    {
                                        changed = true
                                    }
                                }
                            }
                            if r.isFinal {
                                if Bproduction.followSet.addAllIfAbsentAdded(
                                    production.followSet)
                                {
                                    changed = true
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    func getFollow(_ raState: ReadaheadState) -> [String] {
        var return_val: [String] = []

        eSuccessors([raState]).do {
            $0.transitions.do { transition in
                if !transition.label.hasAction() {
                    if isNonterminalTransition(transition) {
                        return_val.appendIfAbsent(productionFor(transition.label.name!).firstSet)
                    } else {
                        return_val.appendIfAbsent(transition.label.name!)
                    }
                }
            }

            let finals = ($0 as! ReadaheadState).items.filter { state in state.isFinal }
            finals.map { state in state.leftPart }.do { nonterminal in
                return_val.appendIfAbsent(productionFor(nonterminal).followSet)
            }
        }

        return return_val
    }

    func printEGeneratingFirstAndFollowSets() {
        //Should really print to output...
        print("For grammar...")

        print("")
        for nonterminal in nonterminals.sorted(by: <) {
            print(
                "//e-Generating(\(nonterminal)) = \((productionFor (nonterminal)).generatesE)")
        }

        print("")
        for nonterminal in nonterminals.sorted(by: <) {
            print(
                "//First(\(nonterminal)) = \((productionFor (nonterminal)).firstSet.sorted (by: <))"
            )
        }

        print("")
        for nonterminal in nonterminals.sorted(by: <) {
            print(
                "//Follow(\(nonterminal)) = \((productionFor (nonterminal)).followSet.sorted (by: <))"
            )
        }
    }

    func finalize() {
        for (key, value) in productions {
            value.fsm.states.do {
                $0.leftPart = key
            }
        }

        renumber()

        computeEGeneratingNonterminals()
        computeFirstSets()
        computeFollowSets()
        printEGeneratingFirstAndFollowSets()
    }

    func renumber() {
        var next_start: Int = 1
        for production in productions {
            production.value.fsm.renumberFrom(next_start)
            next_start += production.value.get_fsm().states.count
        }
    }
}

//======================= THESE ARE CONSTRUCTOR WALK ROUTINES THAT MOSTLY DO NOTHING ====
//Only the first 2 do something. However, they must be in your constructor and in the
//canPerformAction and performAction routines.
/*

func processAndDiscardDefaultsNow (_ tree: VirtualTree) {
    //Pick up the tree just built containing either the attributes, keywords, optimize, and output tree,
    //process it with walkTree, and remove it from the tree stack... by replacing the entry by nil..."
    var tree: Tree = parser.treeStack.last; walkTree (tree)
    parser.treeStack.removeLast; parser.treeStack.addLast: nil
}

 func walkKeywords (_ tree: VirtualTree) {
     "Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
     //eliminates the tree to prevent generic tree walking later...|
     //All it does is give the grammar the keywords and prints them..."
     keywords := aTree children collect: [:child | child symbol].
     Grammar activeGrammar keywordsForParser: keywords.
  }


func walkAttributeTerminalDefaults (_ tree: VirtualTree) {
     //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
     //eliminates the tree to prevent generic tree walking later...
}

func walkAttributeNonterminalDefaults (_ tree: VirtualTree) {
    //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
    //eliminates the tree to prevent generic tree walking later...
 }

func walkOutput (_ tree: VirtualTree) {
    //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
    //eliminates the tree to prevent generic tree walking later...

    "All it does is print the output language. We commented out code that records the
    output language in the grammar since the student version will currently output
    in the format their tool is written in; i.e., Smalltalk for Smalltalk users versus
    Swift for Swift users."
 }

func walkAttributeDefaults (_ tree: VirtualTree) {
    //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
    //eliminates the tree to prevent generic tree walking later...
 }

func walkOptimize (_ tree: VirtualTree) {
    //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
    //eliminates the tree to prevent generic tree walking later...

    //All it does is allow 'chain reductions' and 'keep nonterminal transitions' to be used
    //by Wilf's parser constructor. It does so by telling the grammar what the optimization is
    //and the more advanced constructor he has to perform the optimizations. They are
    //of no concern to the student constructor... so that code is commented out..."
 }
 */