//
//  FSMBuilder.swift
//  FSMBuilder
//
//  Created by Wilf Lalonde on 2023-01-24.
//

import Foundation

typealias treeClosure = (VirtualTree) -> Any  //Ultimately FSM

public final class Constructor: Translator {
    var parser: Parser?
    var tree: VirtualTree? = nil
    var symbolOnly: Bool = false  // when symbolOnly is true, the builder stops constructing FSMs and starts returning token symbols instead

    var readaheadStates: [ReadaheadState] = []
    var readbackStates: [ReadbackState] = []
    var shiftStates: [ShiftState] = []
    var reduceStates: [String: ReduceState] = [:]
    var semanticStates: [SemanticState] = []
    var acceptState: AcceptState = AcceptState()

    var right = Relation<FiniteStateMachineState, Label>()
    var left = Relation<Pair, Pair>()
    var down = Relation<FiniteStateMachineState, Label>()
    var up = Relation<Pair, Label>()
    var invisible_left = Relation<Pair, Pair>()
    var visible_left = Relation<Pair, Pair>()

    init() {
        resetParser()
    }

    func inSymbolOnlyMode(operation: () -> Void) {
        symbolOnly = true
        operation()
        symbolOnly = false
    }

    func resetParser() {
        parser = Parser(sponsor: self, parserTables: parserTables, scannerTables: scannerTables)
    }

    func process(_ text: String) {
        tree = parser!.parse(text)
        print(tree)
        walkTree(tree!)
        resetParser()
    }

    func walkTree(_ tree: VirtualTree) -> Any {
        let action = tree.label as String
        switch action {
        case "walkList":
            return walkList(tree)
        case "walkIdentifier":
            return walkIdentifier(tree)
        case "walkCharacter":
            return walkCharacter(tree)
        case "walkString", "walkSymbol":
            return walkString(tree)
        case "walkInteger":
            return walkInteger(tree)
        case "walkEpsilon":
            return walkEpsilon(tree)
        case "walkQuestionMark":
            return walkPostfix(tree, ^)
        case "walkPlus":
            return walkPostfix(tree, +)
        case "walkDotDot":
            return walkDotDot(tree)
        case "walkStar":
            return walkPostfix(tree, *)
        case "walkOr":
            return walkInfix(tree, |)
        case "walkConcatenation":
            return walkInfix(tree, ..)
        case "walkMinus":
            return walkInfix(tree, -)
        case "walkAnd":
            return walkInfix(tree, &)
        case "walkAttributes":
            return walkAttributes(tree)
        case "walkGrammar":
            return walkGrammar(tree)
        case "walkMacro":
            return walkMacro(tree)
        case "walkProduction":
            return walkProduction(tree)
        case "walkLeftPartWithLookahead":
            return walkLeftPartWithLookahead(tree)
        case "walkLeftPart":
            return walkLeftPart(tree)
        case "walkBuildTreeOrTokenFromName":
            return walkBuildTreeOrTokenFromName(tree)
        case "walkBuildTreeFromLeftIndex":
            return walkBuildTreeFromLeftIndex(tree)
        case "walkBuildTreeFromRightIndex":
            return walkBuildTreeFromRightIndex(tree)
        case "walkTreeBuildingSemanticAction":
            return walkTreeBuildingSemanticAction(tree)
        case "walkNonTreeBuildingSemanticAction":
            return walkNonTreeBuildingSemanticAction(tree)
        case "walkLook":
            return walkLook(tree)
        default:
            if canPerformAction(action) {
                return performAction(action, [tree])
            } else {
                error("Attempt to perform unknown walkTree routine \(action)")
                return 0
            }
        }
    }

    public func canPerformAction(_ action: String) -> Bool {
        var action_without_last = action
        action_without_last.removeLast()
        let actions = [
            "processTypeNow",
            "processAndDiscardDefaultsNow",
            "walkKeywords",
            "walkAttributeTerminalDefaults",
            "walkAttributeNonterminalDefaults",
            "walkOutput",
            "walkAttributeDefaults",
            "walkOptimize",
        ]
        if actions.contains(action) || actions.contains(action_without_last) {
            return true
        }
        return false
    }

    public func performAction(_ action: String, _ parameters: [Any]) {
        var tree: VirtualTree?
        print(action)
        print(parameters)
        if parameters.count >= 1 {
            tree = parameters[0] as? VirtualTree
        }
        switch action {
        case "processTypeNow":
            processTypeNow(parameters)
        case "processAndDiscardDefaultsNow":
            processAndDiscardDefaultsNow()
        case "walkKeywords":
            walkKeywords(tree!)
        case "walkAttributeTerminalDefaults":
            walkAttributeTerminalDefaults(tree)
        case "walkAttributeNonterminalDefaults":
            walkAttributeNonterminalDefaults(tree)
        case "walkOutput":
            walkOutput(tree)
        case "walkAttributeDefaults":
            walkAttributeDefaults(tree)
        case "walkOptimize":
            walkOptimize(tree)
        default:
            return
        }
    }

    static public func exampleWithFirstFollow(grammar_text: String) {
        let grammar = Grammar()
        Grammar.activeGrammar = grammar

        let builder: Constructor = Constructor()

        do {
            builder.process(grammar_text)
            print(grammar)
            grammar.finalize()
        } catch {
            print("File not found")
        }
    }

    static public func example1(grammar_text: String) -> String {  //Returns a string to please ContentView
        let grammar = Grammar()
        Grammar.activeGrammar = grammar

        //Wilf: I've placed scannerFSMs.txt in all the subfolders of FSMBuilder
        //but it can never find the file... Don't know what the problem is.
        //Also, the do-catch seems unable to catch the error when an attempt
        //is made to get the text. Instead, it just crashes when attempting to
        //get the contents of the nil path. What's the point of do-catch down below???

        //An alternative work around would be to manually let text = "contents of scannerFSMs.txt"
        //but you'll have to replace internal uses of character $" by \". I've done it below
        //with a subset of the file...

        let builder: Constructor = Constructor()
        let givenUp = false
        if givenUp {
            //The following is a subset of the scannerFSMs.txt file.
            let text = """
                scanner
                    fsm1 = $+; //Should default to "RK"
                    fsm2 = $a [noKeep]; //Should have attribute "R"
                """
            builder.process(text)
            print("Finished building scanner FSMs")
        } else {
            do {
                builder.process(grammar_text)
                //print(grammar)
            } catch {
                print("File not found")
            }
        }
        return "Done"
    }

    //Walk routines...
    func processTypeNow(_ parameters: [Any]) {
        //The child will be a walkString with "scanner" or "parser"
        let type = parameters[0] as? String
        if type != nil {
            Grammar.activeGrammar!.type = type!.trimmingCharacters(in: .whitespacesAndNewlines)
        }
    }

    func renumber() {
        var count = 1  // Grammar.activeGrammar!.totalStates()
        readaheadStates.do {
            $0.stateNumber = count
            count += 1
        }

        readbackStates.do {
            $0.stateNumber = count
            count += 1
        }

        shiftStates.do {
            $0.stateNumber = count
            count += 1
        }

        for state in reduceStates {
            state.value.stateNumber = count
            count += 1
        }

        semanticStates.do {
            $0.stateNumber = count
            count += 1
        }

        acceptState.stateNumber = count
    }

    func walkList(_ tree: VirtualTree) -> Any {
        let treeList = (tree as? Tree)!
        var index = 0
        print("\(tree)")
        while index < treeList.children.count {
            let child0 = treeList.child(index)
            let child1 = treeList.child(index + 1)

            let name = (child0 as? Token)!.symbol
            let fsm = walkTree(child1)

            print("FSM for \(name) = \n\(fsm)\n")
            if fsm is FiniteStateMachine {
                //fsmMap[name] = fsm as! FiniteStateMachine
            }
            Grammar.activeGrammar!.addMacro(name, fsm as! FiniteStateMachine)
            index += 2
        }

        // print("\n\n\n ~~~ POST LOOP FSMs ~~~");

        // // moved print to post loop to check that copied fsms are not altered
        // for (key, value) in fsmMap{
        //     print ("FSM for \(key) = \n\(value)")
        // }

        return 0
    }

    func walkGrammar(_ tree: VirtualTree) {
        firstPassWalkTree(tree)

        (tree as! Tree).children.do {
            walkTree($0)
        }

        Grammar.activeGrammar!.finalize()

        Grammar.activeGrammar!.nonterminals.do {
            if Grammar.activeGrammar!.productionFor($0).isGoal()
                && Grammar.activeGrammar!.isParser()
            {
                acceptState = AcceptState()
            } else {
                reduceStates[$0] = ReduceState($0)
            }
        }

        buildRightAndDown()

        buildReadaheadStates()

        buildReadbackStateBridges()

        if Grammar.activeGrammar!.isParser() {
            finishBuildingReadbackStates()
        }

        renumber()

        finalizeReduceTables()

        replaceSemanticTransitions()

        if Grammar.activeGrammar!.isParser() {
            // remove initial readahead state from parsers (left goalpost transition)
            readaheadStates.remove(at: 0)
        }

        optimize()

        renumber()

        // printStates()

        outputParserTables()
        outputScannerTables()
    }

    func buildRightAndDown() {
        for (key, value) in Grammar.activeGrammar!.productions {
            value.fsm.states.do { state in
                state.transitionsDo { transition in
                    if Grammar.activeGrammar!.isNonterminal(transition.label.terseDescription) {
                        for goto in Grammar.activeGrammar!.productionFor(
                            transition.label.terseDescription
                        ).fsm.states where goto.isInitial {
                            down.add(state, and: transition.label, and: goto)
                        }
                    }

                    right.add(state, and: transition.label, and: transition.goto)
                }
            }
        }
    }

    func printStates() {
        print("\n\nreadahead states:\n\(readaheadStates)")
        print("\n\nsemantic states:\n\(semanticStates)")
        print("\n\nreadback states:\n\(readbackStates)")
        print("\n\nreduce states:\n\(reduceStates)")
        print("\n\naccept states:\n\(acceptState)")
    }

    func buildReadaheadStates() {
        readaheadStates.append(ReadaheadState(Grammar.activeGrammar!.initialStateOfGoals()))

        readaheadStates[0].isInitial = true

        var i = 0

        while i < readaheadStates.count {
            let raState = readaheadStates[i]
            let localDown = down.performRelationStar(raState.items)

            // print("\n\n    ~~~~~ \(i) ~~~~~    \n\n")

            localDown.do {
                up.add(Pair($2, raState), and: $1, and: Pair($0, raState))
            }

            raState.items.append(contentsOf: localDown.allTo())

            // print("\(i), \(localDown)\n")
            // print(raState.items)

            right.from(raState.items) { M, localRight in
                // print(localRight.allTo())
                let candidate = ReadaheadState(localRight.allTo())
                var successor = readaheadStates.first {
                    Set($0.items).contains(candidate.items)
                }
                //print(candidate)
                if successor == nil {
                    readaheadStates.append(candidate)
                    successor = candidate
                }
                raState.addTransition(
                    Transition(label: M, goto: successor!))
                localRight.do { from, relationship, to in
                    left.add(
                        Pair(to, successor!),
                        and: Pair(relationship, successor!),
                        and: Pair(from, raState))
                }
            }

            i += 1
        }

        visible_left = Relation(
            from: left.triples.filter { ($0.relationship.first() as! Label).isVisible() })
        invisible_left = Relation(
            from: left.triples.filter { !($0.relationship.first() as! Label).isVisible() })
    }

    func buildReadbackStateBridges() {
        var i = 0
        while i < readaheadStates.count {
            let raState = readaheadStates[i]
            let finalItems = raState.items.filter { $0.isFinal }
            let partition = finalItems.partitionUsing { $0.leftPart }

            for (key, value) in partition {
                var new_state: FiniteStateMachineState?

                if Grammar.activeGrammar!.isScanner() {
                    new_state = readaheadStates[0]
                } else if Grammar.activeGrammar!.isGoal(key) {
                    new_state = acceptState
                } else {
                    new_state = ReadbackState(
                        items: finalItems.map {
                            Pair($0, raState)
                        })

                    new_state!.isInitial = true

                    readbackStates.append(new_state as! ReadbackState)
                }

                Grammar.activeGrammar!.productionFor(key).followSet.do {
                    raState.addTransition(
                        Transition(
                            label: Label(name: $0).asLook(),
                            goto: new_state!
                        ))
                }
            }

            i += 1
        }
    }

    func finishBuildingReadbackStates() {
        var i = 0
        while i < readbackStates.count {
            let rbState: ReadbackState = readbackStates[i]
            let more_items = invisible_left.performStar(rbState.items)

            visible_left.from(more_items) { Mp, local_left in
                let candidate = ReadbackState(items: local_left.allTo())
                // print(candidate)
                var successor = readbackStates.first {
                    $0.items.elementsEquivalent(candidate.items)
                }
                if successor == nil {
                    readbackStates.append(candidate)
                    successor = candidate
                }

                rbState.addTransition(
                    Transition(
                        label: Label(
                            label: Mp.first() as! Label,
                            predecessor: Mp.second() as? FiniteStateMachineState), goto: successor!)
                )
            }

            let initial_items = more_items.filter {
                ($0.first() as! FiniteStateMachineState).isInitial
            }

            let lookbacks =
                lookbackFor(
                    initial_items)

            for Mp in lookbacks {
                var goto: FiniteStateMachineState? = reduceStates[
                    (initial_items[0].first() as! FiniteStateMachineState).leftPart]
                if goto == nil {
                    goto = acceptState
                }
                rbState.addTransition(
                    Transition(
                        label: Label(
                            label: Mp.first() as! Label,
                            predecessor: Mp.second() as? FiniteStateMachineState), goto: goto!
                    ))
            }

            i += 1

        }
    }

    func lookbackFor(_ items: [Pair]) -> Set<Pair> {
        var result = up.performOnce(items)

        var i = 0

        while i < result.count {
            let pair = result[i]
            let up_items = up.performStar([pair])
            let left_items = left.performStar([pair])

            result.appendIfAbsent(up_items.filter { result.firstIndex(of: $0) == nil })
            result.appendIfAbsent(left_items.filter { result.firstIndex(of: $0) == nil })

            i += 1
        }

        var lookbacks = Set<Pair>()

        visible_left.from(result) { Mp, relation in
            lookbacks.insert(Pair((Mp.first() as! Label).asLook(), Mp.second()))
        }

        return lookbacks
    }

    func replaceSemanticTransitions() {
        readaheadStates.do {
            for sem_transition in $0.transitions where sem_transition.hasAction() {
                buildSemanticState($0, transition: sem_transition)
                $0.transitions.removeAll { transition in transition == sem_transition }
            }
        }
    }

    func buildSemanticState(_ state: ReadaheadState, transition: Transition) {
        // get follow set for goto
        let follow = Grammar.activeGrammar!.getFollow(state)

        // build semantic state based on transition
        let new_state = SemanticState(transition.label, goto: transition.goto)

        // add follow to transitions from state to new semantic state
        for char in Array(follow) {
            state.addTransition(Transition(label: Label(name: char).asLook(), goto: new_state))
        }

        if semanticStates.first(where: { $0.label == new_state.label && $0.goto == new_state.goto })
            == nil
        {
            semanticStates.append(new_state)
        }
    }

    func finalizeReduceTables() {

        readaheadStates.do { raState in
            raState.transitionsDo { transition in
                if Grammar.activeGrammar!.isNonterminal(transition.label.terseDescription) {
                    if reduceStates[transition.label.name!]!.restarts[transition.goto] == nil {
                        reduceStates[transition.label.name!]!.restarts[transition.goto] = []
                    }

                    reduceStates[transition.label.name!]!.restarts[transition.goto]!.append(raState)  // may need to change to append
                }
            }
        }

        var stackable_states: [FiniteStateMachineState] = []

        // get all stackable states
        readaheadStates.do { raState in
            raState.transitions.do { transition in
                if transition.label.isVisible()
                    || Grammar.activeGrammar!.isNonterminal(transition.label.terseDescription)
                {
                    // print(raState)
                    stackable_states.append(transition.goto)
                }
            }
        }

        // print(stackable_states)

        // build invisible_left for readahead states
        let ra_invisible_left = Relation<FiniteStateMachineState, Label>()
        readaheadStates.do { raState in
            raState.transitionsDo {
                if $0.label.isInvisible() {
                    ra_invisible_left.add($0.goto, and: $0.label, and: raState)
                }
            }
        }

        print(ra_invisible_left)

        for rdState in reduceStates {
            for (key, value) in rdState.value.restarts {
                value.do { state in
                    // print(raState.terseDescription)
                    // print(
                    //     ra_invisible_left.performStar([raState]).filter {
                    //         stackable_states.contains($0)
                    //     }.map { $0.terseDescription }.joined(separator: ", "))

                    let alt_restarts = ra_invisible_left.performStar([state]).filter {
                        stackable_states.contains($0)
                    }

                    rdState.value.restarts[key]!.removeAll { $0 == state}

                    rdState.value.restarts[key]!.appendIfAbsent(alt_restarts)
                }
            }
        }

        // print(reduceStates)
    }

    func optimize() {
        eliminateStates()
        // readbackToShift()
    }

    func eliminateStates() {
        var dead_states: [FiniteStateMachineState] = []

        readaheadStates.do { raState in
            if (raState.transitions.allSatisfy {
                $0.goto == raState.transitions[0].goto && !$0.label.isVisible()
            }) {
                replaceState(raState, with: raState.transitions[0].goto)
                dead_states.append(raState)
            }
        }

        readaheadStates.removeAll { dead_states.contains($0) }

        dead_states.removeAll()

        readbackStates.do { rbState in
            if (rbState.transitions.allSatisfy {
                $0.goto == rbState.transitions[0].goto && !$0.label.isVisible()
            }) {
                replaceState(rbState, with: rbState.transitions[0].goto)
                dead_states.append(rbState)
            }
        }

        readbackStates.removeAll { dead_states.contains($0) }
    }

    func readbackToShift() {
        var dead_states: [FiniteStateMachineState] = []
        readbackStates.do { rbState in
            if (rbState.transitions.allSatisfy {
                $0.goto == rbState.transitions[0].goto
            }) {
                shiftStates.append(ShiftState(1, rbState.transitions[0].goto))
                replaceState(
                    rbState, with: shiftStates.last!)
                dead_states.append(rbState)
            }
        }

        readbackStates.removeAll { dead_states.contains($0) }

        dead_states.removeAll()

        // merge consecutive shift states
        shiftStates.do { shState in
            if shState.goto as? ShiftState != nil {
                dead_states.append(shState.goto)
                replaceState(shState.goto, with: shState)

                shState.shiftVal += (dead_states.last! as! ShiftState).shiftVal
                shState.goto = (dead_states.last! as! ShiftState).goto
            }
        }

        shiftStates.removeAll { dead_states.contains($0) }
    }

    func replaceState(_ old: FiniteStateMachineState, with new_state: FiniteStateMachineState) {
        readaheadStates.do { state in
            state.transitionsDo {
                if $0.goto == old {
                    $0.goto = new_state
                }
            }
        }

        readbackStates.do { state in
            state.transitionsDo {
                if $0.goto == old {
                    $0.goto = new_state
                }

                if $0.label.predecessor == old {
                    $0.label.predecessor = new_state
                }
            }
        }

        semanticStates.do { state in
            if state.goto == old {
                state.goto = new_state
            }
        }

        shiftStates.do { state in
            if state.goto == old {
                state.goto = new_state
            }
        }

        for rdState in reduceStates {
            for (key, value) in rdState.value.restarts {
                var new_value = value
                new_value.removeAll{$0 == old}
                if(new_value.count < value.count){
                    new_value.appendIfAbsent(new_state)
                    rdState.value.restarts[key] = new_value
                }
            }
        }
    }

    func outputParserTables() {
        var tables: String = ""

        print("\n\n\n~~~~~~~~~~~~~~ BEGIN PARSER TABLES ~~~~~~~~~~~~~~\n\n\n")
        tables.append(
            "[[\"keywords\",\(Grammar.activeGrammar!.keywords.map{"\"\($0)\""}.joined(separator: ", "))],\n"
        )
        readaheadStates.do { raState in
            tables.append(
                "[\"ReadaheadTable\", \(raState.stateNumber), \(raState.transitions.map{"(\"\($0.label.name!)\", \"\($0.label.attributes)\", \($0.goto.stateNumber))"}.joined(separator: ", "))],\n"
            )
        }

        readbackStates.do { rbState in
            tables.append(
                "[\"ReadbackTable\", \(rbState.stateNumber), \(rbState.transitions.map{"((\"\($0.label.name!)\", \($0.label.predecessor!.stateNumber)), \"\($0.label.attributes)\", \($0.goto.stateNumber))"}.joined(separator: ", "))],\n"
            )
        }

        shiftStates.do { shState in
            tables.append(
                "[\"ShiftbackTable\", \(shState.stateNumber), \(shState.shiftVal), \(shState.goto.terseDescription)],\n"
            )
        }

        for rdState in reduceStates {
            tables.append(
                "[\"ReduceTable\", \(rdState.value.stateNumber), \"\(rdState.key)\", \(rdState.value.restarts.map{(key, value) in value.map{"(\($0.stateNumber), \"RSN\", \(key.stateNumber))"}.joined(separator: ", ")}.joined(separator: ", "))],\n"
            )
        }

        semanticStates.do { smState in
            tables.append(
                "[\"SemanticTable\", \(smState.stateNumber), \"\(smState.label.action)\", [\(smState.label.parameters.map{"\"\($0)\""}.joined(separator: ", "))], \(smState.goto.stateNumber)],\n"
            )
        }
        tables.append("[\"AcceptTable\", \(acceptState.stateNumber)]]")

        do {
            try tables.write(
                to: URL(fileURLWithPath: "Sources/Constructor/bootstrapParserTables.txt"),
                atomically: true, encoding: String.Encoding.utf8)
        } catch {
            print("parser table write failed")
            // failed to write file – bad permissions, bad filename, missing permissions, or more likely it can't be converted to the encoding
        }

        print("\n\n\n~~~~~~~~~~~~~~ END PARSER TABLES ~~~~~~~~~~~~~~\n\n\n")
    }

    func outputScannerTables() {
        print("\n\n\n~~~~~~~~~~~~~~ BEGIN SCANNER TABLES ~~~~~~~~~~~~~~\n\n\n")
        print("[")

        print("]")
        print("\n\n\n~~~~~~~~~~~~~~ END SCANNER TABLES ~~~~~~~~~~~~~~\n\n\n")
    }

    func firstPassWalkTree(_ tree: VirtualTree) {
        if ["walkLeftPart", "walkLeftPartWithLookahead"].contains(tree.label) {
            Grammar.activeGrammar!.nonterminals.append(((tree as! Tree).child(0) as! Token).symbol)
        } else if ["walkGrammar", "walkProduction"].contains(tree.label) {
            (tree as! Tree).children.do {
                firstPassWalkTree($0)
            }
        }
    }

    func walkMacro(_ tree: VirtualTree) {
        var label: String = ""
        inSymbolOnlyMode {
            label = walkTree((tree as! Tree).child(0)) as! String
        }
        Grammar.activeGrammar?.addMacro(
            label, walkTree((tree as! Tree).child(1)) as! FiniteStateMachine)
    }

    func walkProduction(_ tree: VirtualTree) {
        let new_production: Production = walkTree((tree as! Tree).child(0)) as! Production

        new_production.fsm = walkTree((tree as! Tree).child(1)) as! FiniteStateMachine

        Grammar.activeGrammar!.addProduction(new_production.leftPart, new_production)
    }

    func walkLeftPartWithLookahead(_ tree: VirtualTree) -> Any {
        let return_val = walkLeftPart(tree) as! Production

        return_val.lookahead = (walkTree((tree as! Tree).child(1)) as! FiniteStateMachine)
            .transitionNames()

        return return_val
    }

    func walkLeftPart(_ tree: VirtualTree) -> Any {
        let return_val = Production()

        inSymbolOnlyMode {
            return_val.leftPart = walkTree((tree as! Tree).child(0)) as! String
        }

        return return_val
    }

    func walkIdentifier(_ tree: VirtualTree) -> Any {
        let symbol: String = (tree as! Token).symbol

        if symbolOnly {
            return symbol
        }

        // if(symbol.capitalized == symbol){print(symbol)}

        // if Grammar.activeGrammar!.productions[symbol] != nil{
        //     print(Grammar.activeGrammar!.productions[symbol]!);
        //     return FiniteStateMachine(fsm: Grammar.activeGrammar!.productions[symbol]!.fsm)
        // }else
        if Grammar.activeGrammar!.macros[symbol] != nil {
            return FiniteStateMachine(fsm: Grammar.activeGrammar!.macros[symbol]!)
        } else {
            return FiniteStateMachine.forString(symbol)
        }
    }
    func walkEpsilon(_ tree: VirtualTree) -> Any {
        return FiniteStateMachine.empty()
    }
    func walkCharacter(_ tree: VirtualTree) -> Any {
        if symbolOnly { return Int(Character((tree as! Token).symbol).asciiValue!) }
        return FiniteStateMachine.forInteger(Int(Character((tree as! Token).symbol).asciiValue!))
    }
    func walkString(_ tree: VirtualTree) -> Any {
        if symbolOnly { return (tree as! Token).symbol }
        return FiniteStateMachine.forString((tree as! Token).symbol)
    }
    func walkSymbol(_ tree: VirtualTree) -> Any {
        return walkString(tree)
    }
    func walkInteger(_ tree: VirtualTree) -> Any {
        if symbolOnly { return Int((tree as! Token).symbol)! }
        return FiniteStateMachine.forInteger(Int((tree as! Token).symbol)!)
    }
    func walkAttributes(_ tree: VirtualTree) -> Any {
        let t = tree as! Tree
        var return_val: FiniteStateMachine = walkTree(t.child(0)) as! FiniteStateMachine

        var attributes: [String] = []
        inSymbolOnlyMode {
            t.children.doWithoutFirst {
                attributes.append(($0 as! Token).symbol)
            }
        }

        return_val.override(attributes)

        return return_val
    }

    func walkPostfix(_ tree: VirtualTree, _ postfix: (FiniteStateMachine) -> FiniteStateMachine)
        -> Any
    {
        return postfix(walkTree((tree as! Tree).child(0)) as! FiniteStateMachine)
    }

    func walkInfix(
        _ tree: VirtualTree, _ infix: (FiniteStateMachine, FiniteStateMachine) -> FiniteStateMachine
    ) -> Any {
        var return_val = (walkTree((tree as! Tree).child(0))) as! FiniteStateMachine
        (tree as! Tree).children.doWithoutFirst {
            return_val = infix(return_val, (walkTree($0) as! FiniteStateMachine))
        }
        return return_val
    }

    func walkDotDot(_ tree: VirtualTree) -> Any {
        var return_val = FiniteStateMachine()
        inSymbolOnlyMode {
            return_val = FiniteStateMachine.forDotDot(
                walkTree((tree as! Tree).child(0)) as! Int,
                walkTree((tree as! Tree).child(1)) as! Int)
        }
        return return_val
    }

    func walkBuildTreeOrTokenFromName(_ tree: VirtualTree) -> Any {
        (tree as! Tree).children.insert(
            Token(
                label: "walkSymbol",
                symbol: (Grammar.activeGrammar!.isScanner()) ? "buildToken" : "buildTree"), at: 0)
        return walkSemanticAction(
            tree as! Tree,
            treeBuilding: true)
    }

    func walkBuildTreeFromLeftIndex(_ tree: VirtualTree) -> Any {
        (tree as! Tree).children.insert(
            Token(label: "walkSymbol", symbol: "buildTreeFromIndex"), at: 0)
        return walkSemanticAction(
            tree as! Tree, treeBuilding: true)
    }

    func walkBuildTreeFromRightIndex(_ tree: VirtualTree) -> Any {
        (tree as! Tree).children.insert(
            Token(label: "walkSymbol", symbol: "buildTreeFromIndex"), at: 0)

        (tree as! Tree).children.doWithoutFirst {
            ($0 as! Token).symbol.insert(
                contentsOf: "-", at: ($0 as! Token).label.startIndex)
        }

        return walkSemanticAction(tree as! Tree, treeBuilding: true)
    }

    func walkTreeBuildingSemanticAction(_ tree: VirtualTree) -> Any {
        return walkSemanticAction((tree as! Tree).child(0) as! Tree, treeBuilding: true)
    }
    func walkNonTreeBuildingSemanticAction(_ tree: VirtualTree) -> Any {
        return walkSemanticAction((tree as! Tree).child(0) as! Tree, treeBuilding: false)
    }
    func walkSemanticAction(_ tree: Tree, treeBuilding: Bool) -> FiniteStateMachine {
        var parameters: [AnyHashable] = []
        var action = ""

        inSymbolOnlyMode {
            tree.children.doWithoutFirst {
                parameters.append(
                    walkTree($0) as! AnyHashable)
            }

            action = walkTree(tree.child(0)) as! String
        }

        return FiniteStateMachine.forAction(
            action, parameters: parameters, isRootBuilding: treeBuilding)
    }

    func walkLook(_ tree: VirtualTree) -> Any {
        var return_val = walkTree((tree as! Tree).child(0)) as! FiniteStateMachine

        return_val.override(["look"])

        return return_val
    }

    func processAndDiscardDefaultsNow() {
        //Pick up the tree just built containing either the attributes, keywords, optimize, and output tree,
        //process it with walkTree, and remove it from the tree stack... by replacing the entry by nil..."
        var tree: Tree = parser!.treeStack.last as! Tree
        walkTree(tree)
        parser!.treeStack.removeLast()
        parser!.treeStack.append(nil)
    }

    func walkKeywords(_ tree: VirtualTree) {
        // Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
        //eliminates the tree to prevent generic tree walking later...|
        //All it does is give the grammar the keywords and prints them..."
        print(tree)
        let keywords = (tree as! Tree).children.map { ($0 as! Token).symbol }
        Grammar.activeGrammar!.keywords.append(contentsOf: keywords)
    }

    func walkAttributeTerminalDefaults(_ tree: VirtualTree?) {
        //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
        //eliminates the tree to prevent generic tree walking later...
    }

    func walkAttributeNonterminalDefaults(_ tree: VirtualTree?) {
        //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
        //eliminates the tree to prevent generic tree walking later...
    }

    func walkOutput(_ tree: VirtualTree?) {
        //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
        //eliminates the tree to prevent generic tree walking later...

        // All it does is print the output language. We commented out code that records the
        // output language in the grammar since the student version will currently output
        // in the format their tool is written in; i.e., Smalltalk for Smalltalk users versus
        // Swift for Swift users.
    }

    func walkAttributeDefaults(_ tree: VirtualTree?) {
        //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
        //eliminates the tree to prevent generic tree walking later...
    }

    func walkOptimize(_ tree: VirtualTree?) {
        //Note: This walk routine is initiated by #processAndDiscardDefaultsNow which subsequently
        //eliminates the tree to prevent generic tree walking later...

        //All it does is allow 'chain reductions' and 'keep nonterminal transitions' to be used
        //by Wilf's parser constructor. It does so by telling the grammar what the optimization is
        //and the more advanced constructor he has to perform the optimizations. They are
        //of no concern to the student constructor... so that code is commented out..."
    }

    var scannerTables: [Any] =
        [
            [
                "ScannerReadaheadTable", 1, ("]", "RK", 35), ("/", "R", 10), ("{", "RK", 36),
                ("}", "RK", 37), ("\"", "R", 11), ("$", "R", 12), ([256], "L", 21),
                ("ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 6), ("[", "RK", 33),
                ("(", "RK", 23), (")", "RK", 24), ("*", "RK", 25), ("+", "RK", 26), ("-", "RK", 2),
                ("&", "RK", 22), (".", "RK", 3), ([9, 10, 12, 13, 32], "R", 7),
                ("0123456789", "RK", 4), ("=", "RK", 5), ("?", "RK", 31), ("#", "R", 8),
                ("|", "RK", 34), ("'", "R", 9),
            ],
            [
                "ScannerReadaheadTable", 2, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 27),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<[]{}()^;#:.$'\"",
                    "L", 27
                ), (">", "RK", 38),
            ],
            [
                "ScannerReadaheadTable", 3, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 28),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:$'\"",
                    "L", 28
                ), (".", "RK", 39),
            ],
            [
                "ScannerReadaheadTable", 4, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 29),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!,+-/\\*~=@%&?|<>[]{}()^;#:.$'\"",
                    "L", 29
                ), ("0123456789", "RK", 4),
            ],
            [
                "ScannerReadaheadTable", 5, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 30),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<[]{}()^;#:.$'\"",
                    "L", 30
                ), (">", "RK", 40),
            ],
            [
                "ScannerReadaheadTable", 6, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 32),
                ("!,+-/\\*~=@%&?|<>[]{}()^;#.$'\"", "L", 32),
                ("0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 6),
            ],
            [
                "ScannerReadaheadTable", 7, ([96, 147, 148, 256], "L", 1),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$'\"",
                    "L", 1
                ), ([9, 10, 12, 13, 32], "R", 7),
            ],
            [
                "ScannerReadaheadTable", 8, ("\"", "R", 14), ("'", "R", 15),
                ("ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 13),
            ],
            [
                "ScannerReadaheadTable", 9, ([256], "LK", 42), ("'", "R", 16),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 9),
                (
                    "!\"#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 9
                ),
            ],
            [
                "ScannerReadaheadTable", 10, ([9, 10, 12, 13, 32], "L", 44),
                ([96, 147, 148, 256], "LK", 44),
                (
                    "=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_\\abcdefghijklmnopqrstuvwxyz{|}~!\"#$%&'()*+,-.0123456789:;<",
                    "LK", 44
                ), ("/", "R", 17),
            ],
            [
                "ScannerReadaheadTable", 11, ([256], "LK", 45), ("\"", "R", 18),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 11),
                (
                    "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 11
                ),
            ],
            [
                "ScannerReadaheadTable", 12, ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 46),
                (
                    "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 46
                ),
            ],
            [
                "ScannerReadaheadTable", 13, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 41),
                ("!,+-/\\*~=@%&?|<>[]{}()^;#.$'\"", "L", 41),
                ("0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 13),
            ],
            [
                "ScannerReadaheadTable", 14, ([256], "LK", 47), ("\"", "R", 19),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 14),
                (
                    "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 14
                ),
            ],
            [
                "ScannerReadaheadTable", 15, ([256], "LK", 48), ("'", "R", 20),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 15),
                (
                    "!\"#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 15
                ),
            ],
            [
                "ScannerReadaheadTable", 16, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 43),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$\"",
                    "L", 43
                ), ("'", "RK", 9),
            ],
            [
                "ScannerReadaheadTable", 17, ([9, 32, 96, 147, 148], "R", 17),
                (
                    "=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~!\"#$%&'()*+,-./0123456789:;<",
                    "R", 17
                ), ([256], "LK", 1), ([10, 12, 13], "R", 1),
            ],
            [
                "ScannerReadaheadTable", 18, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 43),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$'",
                    "L", 43
                ), ("\"", "RK", 11),
            ],
            [
                "ScannerReadaheadTable", 19, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 41),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$'",
                    "L", 41
                ), ("\"", "RK", 14),
            ],
            [
                "ScannerReadaheadTable", 20, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 41),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$\"",
                    "L", 41
                ), ("'", "RK", 15),
            ],
            ["SemanticTable", 21, "buildToken", ["-|"], 1],
            ["SemanticTable", 22, "buildToken", ["And"], 1],
            ["SemanticTable", 23, "buildToken", ["OpenRound"], 1],
            ["SemanticTable", 24, "buildToken", ["CloseRound"], 1],
            ["SemanticTable", 25, "buildToken", ["Star"], 1],
            ["SemanticTable", 26, "buildToken", ["Plus"], 1],
            ["SemanticTable", 27, "buildToken", ["Minus"], 1],
            ["SemanticTable", 28, "buildToken", ["Dot"], 1],
            ["SemanticTable", 29, "buildToken", ["walkInteger"], 1],
            ["SemanticTable", 30, "buildToken", ["Equals"], 1],
            ["SemanticTable", 31, "buildToken", ["QuestionMark"], 1],
            ["SemanticTable", 32, "buildToken", ["walkIdentifier"], 1],
            ["SemanticTable", 33, "buildToken", ["OpenSquare"], 1],
            ["SemanticTable", 34, "buildToken", ["Or"], 1],
            ["SemanticTable", 35, "buildToken", ["CloseSquare"], 1],
            ["SemanticTable", 36, "buildToken", ["OpenCurly"], 1],
            ["SemanticTable", 37, "buildToken", ["CloseCurly"], 1],
            ["SemanticTable", 38, "buildToken", ["RightArrow"], 1],
            ["SemanticTable", 39, "buildToken", ["DotDot"], 1],
            ["SemanticTable", 40, "buildToken", ["FatRightArrow"], 1],
            ["SemanticTable", 41, "buildToken", ["walkSymbol"], 1],
            [
                "SemanticTable", 42, "syntaxError", ["missing end quote for single quoted string"],
                43,
            ],
            ["SemanticTable", 43, "buildToken", ["walkString"], 1],
            ["SemanticTable", 44, "syntaxError", ["// is a comment, / alone is not valid"], 1],
            [
                "SemanticTable", 45, "syntaxError", ["missing end quote for double quoted string"],
                43,
            ],
            ["SemanticTable", 46, "buildToken", ["walkCharacter"], 1],
            [
                "SemanticTable", 47, "syntaxError", ["missing end quote for double quoted string"],
                41,
            ],
            [
                "SemanticTable", 48, "syntaxError", ["missing end quote for single quoted string"],
                41,
            ],
        ]

    var parserTables: [Any] = [["keywords","stack", "noStack", "read", "look", "node", "noNode", "keep", "noKeep", "parser", "scanner", "super", "superScanner", "attribute", "defaults", "keywords", "output", "optimize", "terminal", "nonterminal"],
["ReadaheadTable", 1, ("superScanner", "RS", 204), ("GrammarType", "RSN", 2), ("super", "RS", 3), ("scanner", "RS", 205), ("parser", "RS", 206)],
["ReadaheadTable", 2, ("walkString", "RSN", 65), ("Rules", "RSN", 240), ("keywords", "RS", 4), ("output", "RS", 5), ("LeftPart", "RSN", 6), ("optimize", "RS", 7), ("attribute", "RS", 8), ("Name", "RSN", 9), ("walkIdentifier", "RSN", 66), ("Production", "RSN", 10), ("Defaults", "RSN", 209), ("Macro", "RSN", 11)],
["ReadaheadTable", 3, ("scanner", "RS", 210)],
["ReadaheadTable", 4, ("Name", "RSN", 12), ("walkIdentifier", "RSN", 66), ("walkString", "RSN", 65)],
["ReadaheadTable", 5, ("walkString", "RSN", 65), ("walkIdentifier", "RSN", 66), ("Name", "RSN", 13)],
["ReadaheadTable", 6, ("RightArrow", "RS", 14), ("RightParts", "RSN", 15), ("RightPart", "RSN", 16)],
["ReadaheadTable", 7, ("Name", "RSN", 17), ("walkIdentifier", "RSN", 66), ("walkString", "RSN", 65)],
["ReadaheadTable", 8, ("nonterminal", "RS", 18), ("defaults", "RS", 19), ("terminal", "RS", 20)],
["ReadaheadTable", 9, ("Equals", "RS", 21), ("OpenCurly", "RS", 22), ("Equals", "L", 207), ("OpenCurly", "L", 207), ("RightArrow", "L", 207), ("walkIdentifier", "L", 207), ("walkString", "L", 207)],
["ReadaheadTable", 10, ("walkString", "RSN", 65), ("LeftPart", "RSN", 6), ("Name", "RSN", 23), ("walkIdentifier", "RSN", 66), ("Production", "RSN", 10), ("Macro", "RSN", 11), ("walkString", "L", 208), ("walkIdentifier", "L", 208), ("-|", "L", 208)],
["ReadaheadTable", 11, ("walkString", "RSN", 65), ("LeftPart", "RSN", 6), ("Name", "RSN", 9), ("walkIdentifier", "RSN", 66), ("Production", "RSN", 10), ("Macro", "RSN", 11), ("walkString", "L", 0), ("walkIdentifier", "L", 0), ("-|", "L", 0)],
["ReadaheadTable", 12, ("Dot", "RS", 213), ("Name", "RSN", 12), ("walkString", "RSN", 65), ("walkIdentifier", "RSN", 66)],
["ReadaheadTable", 13, ("Dot", "RS", 214)],
["ReadaheadTable", 14, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 28), ("Expression", "RSN", 29), ("Alternation", "RSN", 30), ("AndExpression", "RSN", 31), ("OpenRound", "RS", 32), ("Primary", "RSN", 33), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 211), ("walkIdentifier", "L", 211), ("walkCharacter", "L", 211), ("walkInteger", "L", 211), ("walkSymbol", "L", 211), ("OpenRound", "L", 211), ("walkString", "L", 211), ("And", "L", 211), ("Minus", "L", 211), ("FatRightArrow", "L", 211), ("RightArrow", "L", 211), ("Dot", "L", 211), ("CloseCurly", "L", 211), ("CloseRound", "L", 211), ("parser", "L", 211), ("scanner", "L", 211), ("super", "L", 211), ("superScanner", "L", 211), ("|-", "L", 211), ("Or", "L", 211), ("attribute", "L", 211), ("keywords", "L", 211), ("output", "L", 211), ("optimize", "L", 211), ("Equals", "L", 211)],
["ReadaheadTable", 15, ("Dot", "RS", 217)],
["ReadaheadTable", 16, ("RightArrow", "RS", 36), ("RightPart", "RSN", 16), ("RightArrow", "L", 212), ("Dot", "L", 212)],
["ReadaheadTable", 17, ("Dot", "RS", 218)],
["ReadaheadTable", 18, ("defaults", "RS", 37)],
["ReadaheadTable", 19, ("Name", "RSN", 38), ("walkIdentifier", "RSN", 66), ("walkString", "RSN", 65)],
["ReadaheadTable", 20, ("defaults", "RS", 39)],
["ReadaheadTable", 21, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 28), ("Alternation", "RSN", 30), ("Expression", "RSN", 40), ("AndExpression", "RSN", 31), ("OpenRound", "RS", 32), ("Primary", "RSN", 41), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("Minus", "L", 0), ("RightArrow", "L", 0), ("FatRightArrow", "L", 0), ("CloseCurly", "L", 0), ("Dot", "L", 0), ("CloseRound", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 22, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 28), ("Expression", "RSN", 42), ("Alternation", "RSN", 30), ("AndExpression", "RSN", 31), ("OpenRound", "RS", 32), ("Primary", "RSN", 33), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("Minus", "L", 0), ("CloseCurly", "L", 0), ("RightArrow", "L", 0), ("FatRightArrow", "L", 0), ("Dot", "L", 0), ("CloseRound", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 23, ("Equals", "RS", 21), ("OpenCurly", "RS", 22), ("Equals", "L", 0), ("OpenCurly", "L", 0), ("RightArrow", "L", 0), ("walkIdentifier", "L", 0), ("walkString", "L", 0)],
["ReadaheadTable", 24, ("walkString", "RSN", 65), ("Rules", "RSN", 240), ("keywords", "RS", 4), ("output", "RS", 5), ("LeftPart", "RSN", 6), ("optimize", "RS", 7), ("attribute", "RS", 8), ("Name", "RSN", 23), ("walkIdentifier", "RSN", 66), ("Defaults", "RSN", 209), ("Production", "RSN", 10), ("Macro", "RSN", 11)],
["ReadaheadTable", 25, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 28), ("Expression", "RSN", 43), ("Alternation", "RSN", 30), ("AndExpression", "RSN", 31), ("OpenRound", "RS", 32), ("Primary", "RSN", 44), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("Minus", "L", 0), ("CloseCurly", "L", 0), ("RightArrow", "L", 0), ("FatRightArrow", "L", 0), ("Dot", "L", 0), ("CloseRound", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 26, ("DotDot", "RS", 45), ("OpenSquare", "L", 73), ("OpenRound", "L", 73), ("OpenCurly", "L", 73), ("walkIdentifier", "L", 73), ("walkString", "L", 73), ("walkSymbol", "L", 73), ("walkCharacter", "L", 73), ("walkInteger", "L", 73), ("Star", "L", 73), ("QuestionMark", "L", 73), ("Plus", "L", 73), ("And", "L", 73), ("Or", "L", 73), ("RightArrow", "L", 73), ("FatRightArrow", "L", 73), ("CloseCurly", "L", 73), ("Dot", "L", 73), ("CloseRound", "L", 73), ("Minus", "L", 73)],
["ReadaheadTable", 27, ("OpenSquare", "RS", 46), ("OpenRound", "L", 75), ("OpenCurly", "L", 75), ("walkIdentifier", "L", 75), ("walkString", "L", 75), ("walkSymbol", "L", 75), ("walkCharacter", "L", 75), ("walkInteger", "L", 75), ("Star", "L", 75), ("QuestionMark", "L", 75), ("Plus", "L", 75), ("And", "L", 75), ("Or", "L", 75), ("RightArrow", "L", 75), ("FatRightArrow", "L", 75), ("CloseCurly", "L", 75), ("Dot", "L", 75), ("CloseRound", "L", 75), ("Minus", "L", 75)],
["ReadaheadTable", 28, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("SemanticAction", "RSN", 215), ("Byte", "RSN", 26), ("walkCharacter", "RSN", 72), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 47), ("OpenRound", "RS", 32), ("Primary", "RSN", 33), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("And", "L", 76), ("Or", "L", 76), ("RightArrow", "L", 76), ("FatRightArrow", "L", 76), ("CloseCurly", "L", 76), ("Dot", "L", 76), ("CloseRound", "L", 76), ("Minus", "L", 76)],
["ReadaheadTable", 29, ("FatRightArrow", "RS", 48), ("RightArrow", "L", 77), ("Dot", "L", 77)],
["ReadaheadTable", 30, ("And", "RS", 49), ("RightArrow", "L", 78), ("FatRightArrow", "L", 78), ("CloseCurly", "L", 78), ("Dot", "L", 78), ("CloseRound", "L", 78), ("Minus", "L", 78)],
["ReadaheadTable", 31, ("Minus", "RS", 50), ("RightArrow", "L", 79), ("FatRightArrow", "L", 79), ("CloseCurly", "L", 79), ("Dot", "L", 79), ("CloseRound", "L", 79)],
["ReadaheadTable", 32, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 51), ("Alternation", "RSN", 30), ("Expression", "RSN", 52), ("AndExpression", "RSN", 31), ("OpenRound", "RS", 32), ("Primary", "RSN", 44), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("Minus", "L", 0), ("RightArrow", "L", 0), ("FatRightArrow", "L", 0), ("CloseCurly", "L", 0), ("Dot", "L", 0), ("CloseRound", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 33, ("Plus", "RS", 220), ("QuestionMark", "RS", 221), ("Star", "RS", 222), ("OpenRound", "L", 80), ("OpenCurly", "L", 80), ("walkIdentifier", "L", 80), ("walkString", "L", 80), ("walkSymbol", "L", 80), ("walkCharacter", "L", 80), ("walkInteger", "L", 80), ("And", "L", 80), ("Or", "L", 80), ("RightArrow", "L", 80), ("FatRightArrow", "L", 80), ("CloseCurly", "L", 80), ("Dot", "L", 80), ("CloseRound", "L", 80), ("Minus", "L", 80)],
["ReadaheadTable", 34, ("OpenSquare", "RS", 53), ("OpenSquare", "L", 216), ("RightArrow", "L", 216), ("OpenRound", "L", 216), ("OpenCurly", "L", 216), ("walkIdentifier", "L", 216), ("walkString", "L", 216), ("walkSymbol", "L", 216), ("walkCharacter", "L", 216), ("walkInteger", "L", 216), ("Star", "L", 216), ("QuestionMark", "L", 216), ("Plus", "L", 216), ("Dot", "L", 216), ("And", "L", 216), ("Or", "L", 216), ("FatRightArrow", "L", 216), ("CloseCurly", "L", 216), ("CloseRound", "L", 216), ("Minus", "L", 216)],
["ReadaheadTable", 35, ("Or", "RS", 54), ("And", "L", 82), ("RightArrow", "L", 82), ("FatRightArrow", "L", 82), ("CloseCurly", "L", 82), ("Dot", "L", 82), ("CloseRound", "L", 82), ("Minus", "L", 82)],
["ReadaheadTable", 36, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 51), ("Expression", "RSN", 29), ("Alternation", "RSN", 30), ("AndExpression", "RSN", 31), ("OpenRound", "RS", 32), ("Primary", "RSN", 44), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("Minus", "L", 0), ("FatRightArrow", "L", 0), ("RightArrow", "L", 0), ("Dot", "L", 0), ("CloseCurly", "L", 0), ("CloseRound", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 37, ("Name", "RSN", 55), ("walkString", "RSN", 65), ("walkIdentifier", "RSN", 66)],
["ReadaheadTable", 38, ("Dot", "RS", 223), ("Name", "RSN", 38), ("walkString", "RSN", 65), ("walkIdentifier", "RSN", 66)],
["ReadaheadTable", 39, ("Name", "RSN", 56), ("walkString", "RSN", 65), ("walkIdentifier", "RSN", 66)],
["ReadaheadTable", 40, ("Dot", "RS", 224)],
["ReadaheadTable", 41, ("Plus", "RS", 220), ("QuestionMark", "RS", 221), ("Star", "RS", 222), ("OpenRound", "L", 84), ("OpenCurly", "L", 84), ("walkIdentifier", "L", 84), ("walkString", "L", 84), ("walkSymbol", "L", 84), ("walkCharacter", "L", 84), ("walkInteger", "L", 84), ("And", "L", 84), ("Or", "L", 84), ("RightArrow", "L", 84), ("FatRightArrow", "L", 84), ("CloseCurly", "L", 84), ("Dot", "L", 84), ("CloseRound", "L", 84), ("Minus", "L", 84)],
["ReadaheadTable", 42, ("CloseCurly", "RS", 225)],
["ReadaheadTable", 43, ("CloseCurly", "RS", 226)],
["ReadaheadTable", 44, ("Plus", "RS", 220), ("QuestionMark", "RS", 221), ("Star", "RS", 222), ("OpenRound", "L", 87), ("OpenCurly", "L", 87), ("walkIdentifier", "L", 87), ("walkString", "L", 87), ("walkSymbol", "L", 87), ("walkCharacter", "L", 87), ("walkInteger", "L", 87), ("And", "L", 87), ("Or", "L", 87), ("RightArrow", "L", 87), ("FatRightArrow", "L", 87), ("CloseCurly", "L", 87), ("Dot", "L", 87), ("CloseRound", "L", 87), ("Minus", "L", 87)],
["ReadaheadTable", 45, ("Byte", "RSN", 227), ("walkCharacter", "RSN", 72), ("walkInteger", "RSN", 74)],
["ReadaheadTable", 46, ("keep", "RSN", 93), ("stack", "RSN", 94), ("Attribute", "RSN", 57), ("noNode", "RSN", 95), ("noKeep", "RSN", 96), ("noStack", "RSN", 97), ("look", "RSN", 98), ("read", "RSN", 99), ("node", "RSN", 100), ("CloseSquare", "RS", 228)],
["ReadaheadTable", 47, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("SemanticAction", "RSN", 215), ("Byte", "RSN", 26), ("walkCharacter", "RSN", 72), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 47), ("OpenRound", "RS", 32), ("Primary", "RSN", 41), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("OpenCurly", "L", 219), ("walkIdentifier", "L", 219), ("walkSymbol", "L", 219), ("walkCharacter", "L", 219), ("walkInteger", "L", 219), ("OpenRound", "L", 219), ("walkString", "L", 219), ("And", "L", 219), ("Or", "L", 219), ("RightArrow", "L", 219), ("FatRightArrow", "L", 219), ("CloseCurly", "L", 219), ("Dot", "L", 219), ("CloseRound", "L", 219), ("Minus", "L", 219)],
["ReadaheadTable", 48, ("walkString", "RSN", 65), ("TreeBuildingOptions", "RSN", 229), ("SemanticAction", "RSN", 230), ("Minus", "RS", 58), ("walkInteger:", "RSN", 231), ("walkSymbol", "RSN", 34), ("Name", "RSN", 232), ("walkIdentifier", "RSN", 66), ("Plus", "RS", 59)],
["ReadaheadTable", 49, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 51), ("Alternation", "RSN", 233), ("OpenRound", "RS", 32), ("Primary", "RSN", 41), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("RightArrow", "L", 0), ("FatRightArrow", "L", 0), ("CloseCurly", "L", 0), ("Dot", "L", 0), ("CloseRound", "L", 0), ("Minus", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 50, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("walkCharacter", "RSN", 72), ("Byte", "RSN", 26), ("SemanticAction", "RSN", 215), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 51), ("Alternation", "RSN", 30), ("AndExpression", "RSN", 234), ("OpenRound", "RS", 32), ("Primary", "RSN", 41), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 35), ("OpenCurly", "L", 0), ("walkIdentifier", "L", 0), ("walkCharacter", "L", 0), ("walkInteger", "L", 0), ("walkSymbol", "L", 0), ("OpenRound", "L", 0), ("walkString", "L", 0), ("And", "L", 0), ("RightArrow", "L", 0), ("FatRightArrow", "L", 0), ("CloseCurly", "L", 0), ("Dot", "L", 0), ("CloseRound", "L", 0), ("Minus", "L", 0), ("parser", "L", 0), ("scanner", "L", 0), ("super", "L", 0), ("superScanner", "L", 0), ("|-", "L", 0), ("Or", "L", 0), ("attribute", "L", 0), ("keywords", "L", 0), ("output", "L", 0), ("optimize", "L", 0), ("Equals", "L", 0)],
["ReadaheadTable", 51, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("SemanticAction", "RSN", 215), ("Byte", "RSN", 26), ("walkCharacter", "RSN", 72), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 47), ("OpenRound", "RS", 32), ("Primary", "RSN", 44), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("And", "L", 89), ("Or", "L", 89), ("RightArrow", "L", 89), ("FatRightArrow", "L", 89), ("CloseCurly", "L", 89), ("Dot", "L", 89), ("CloseRound", "L", 89), ("Minus", "L", 89)],
["ReadaheadTable", 52, ("CloseRound", "RS", 102)],
["ReadaheadTable", 53, ("walkString", "RSN", 65), ("walkInteger", "RSN", 74), ("Byte", "RSN", 106), ("walkCharacter", "RSN", 72), ("SemanticActionParameter", "RSN", 60), ("walkSymbol", "RSN", 107), ("Name", "RSN", 108), ("walkIdentifier", "RSN", 66), ("CloseSquare", "RS", 235)],
["ReadaheadTable", 54, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("SemanticAction", "RSN", 215), ("Byte", "RSN", 26), ("walkCharacter", "RSN", 72), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 61), ("OpenRound", "RS", 32), ("Primary", "RSN", 44), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("Concatenation", "RSN", 62)],
["ReadaheadTable", 55, ("Dot", "RS", 237), ("walkIdentifier", "RSN", 66), ("walkString", "RSN", 65), ("Name", "RSN", 55)],
["ReadaheadTable", 56, ("Dot", "RS", 238), ("Name", "RSN", 56), ("walkString", "RSN", 65), ("walkIdentifier", "RSN", 66)],
["ReadaheadTable", 57, ("Attribute", "RSN", 57), ("keep", "RSN", 93), ("stack", "RSN", 94), ("noNode", "RSN", 95), ("noStack", "RSN", 97), ("noKeep", "RSN", 96), ("look", "RSN", 98), ("read", "RSN", 99), ("node", "RSN", 100), ("CloseSquare", "RS", 228)],
["ReadaheadTable", 58, ("walkInteger:", "RSN", 239)],
["ReadaheadTable", 59, ("walkInteger:", "RSN", 231)],
["ReadaheadTable", 60, ("walkString", "RSN", 65), ("walkInteger", "RSN", 74), ("Byte", "RSN", 106), ("walkCharacter", "RSN", 72), ("SemanticActionParameter", "RSN", 60), ("walkSymbol", "RSN", 107), ("Name", "RSN", 108), ("walkIdentifier", "RSN", 66), ("CloseSquare", "RS", 235)],
["ReadaheadTable", 61, ("OpenCurly", "RS", 25), ("walkIdentifier", "RSN", 66), ("SemanticAction", "RSN", 215), ("Byte", "RSN", 63), ("walkCharacter", "RSN", 72), ("walkInteger", "RSN", 74), ("Secondary", "RSN", 27), ("RepetitionOption", "RSN", 47), ("OpenRound", "RS", 32), ("Primary", "RSN", 41), ("walkSymbol", "RSN", 34), ("Name", "RSN", 81), ("walkString", "RSN", 65), ("And", "L", 109), ("Or", "L", 109), ("RightArrow", "L", 109), ("FatRightArrow", "L", 109), ("CloseCurly", "L", 109), ("Dot", "L", 109), ("CloseRound", "L", 109), ("Minus", "L", 109)],
["ReadaheadTable", 62, ("Or", "RS", 54), ("Or", "L", 236), ("And", "L", 236), ("RightArrow", "L", 236), ("FatRightArrow", "L", 236), ("CloseCurly", "L", 236), ("Dot", "L", 236), ("CloseRound", "L", 236), ("Minus", "L", 236), ("OpenRound", "L", 236), ("OpenCurly", "L", 236), ("walkIdentifier", "L", 236), ("walkString", "L", 236), ("walkSymbol", "L", 236), ("walkCharacter", "L", 236), ("walkInteger", "L", 236)],
["ReadaheadTable", 63, ("DotDot", "RS", 45), ("OpenSquare", "L", 123), ("OpenRound", "L", 123), ("OpenCurly", "L", 123), ("walkIdentifier", "L", 123), ("walkString", "L", 123), ("walkSymbol", "L", 123), ("walkCharacter", "L", 123), ("walkInteger", "L", 123), ("Star", "L", 123), ("QuestionMark", "L", 123), ("Plus", "L", 123), ("And", "L", 123), ("Or", "L", 123), ("RightArrow", "L", 123), ("FatRightArrow", "L", 123), ("CloseCurly", "L", 123), ("Dot", "L", 123), ("CloseRound", "L", 123), ("Minus", "L", 123)],
["ReadbackTable", 64, (("superScanner", 204), "RS", 199)],
["ReadbackTable", 65, (("walkString", 65), "RSN", 189)],
["ReadbackTable", 66, (("walkIdentifier", 66), "RSN", 189)],
["ReadbackTable", 67, (("scanner", 205), "RS", 199)],
["ReadbackTable", 68, (("parser", 206), "RS", 199)],
["ReadbackTable", 69, (("Name", 23), "RSN", 187)],
["ReadbackTable", 70, (("Macro", 11), "RSN", 128), (("Production", 10), "RSN", 129)],
["ReadbackTable", 71, (("scanner", 210), "RS", 130)],
["ReadbackTable", 72, (("walkCharacter", 72), "RSN", 191)],
["ReadbackTable", 73, (("Byte", 26), "RSN", 185)],
["ReadbackTable", 74, (("walkInteger", 74), "RSN", 191)],
["ReadbackTable", 75, (("Secondary", 27), "RSN", 197)],
["ReadbackTable", 76, (("RepetitionOption", 28), "RSN", 200)],
["ReadbackTable", 77, (("Expression", 29), "RSN", 131)],
["ReadbackTable", 78, (("Alternation", 30), "RSN", 188)],
["ReadbackTable", 79, (("AndExpression", 31), "RSN", 186)],
["ReadbackTable", 80, (("Primary", 33), "RSN", 203)],
["ReadbackTable", 81, (("Name", 81), "RSN", 185)],
["ReadbackTable", 82, (("Concatenation", 35), "RSN", 193)],
["ReadbackTable", 83, (("RightPart", 16), "RSN", 132)],
["ReadbackTable", 84, (("Primary", 41), "RSN", 203)],
["ReadbackTable", 85, (("Dot", 213), "RS", 133)],
["ReadbackTable", 86, (("Dot", 214), "RS", 134)],
["ReadbackTable", 87, (("Primary", 44), "RSN", 203)],
["ReadbackTable", 88, (("SemanticAction", 215), "RSN", 197)],
["ReadbackTable", 89, (("RepetitionOption", 51), "RSN", 200)],
["ReadbackTable", 90, (("walkSymbol", 34), "RSN", 196)],
["ReadbackTable", 91, (("Dot", 217), "RS", 135)],
["ReadbackTable", 92, (("Dot", 218), "RS", 136)],
["ReadbackTable", 93, (("keep", 93), "RSN", 192)],
["ReadbackTable", 94, (("stack", 94), "RSN", 192)],
["ReadbackTable", 95, (("noNode", 95), "RSN", 192)],
["ReadbackTable", 96, (("noKeep", 96), "RSN", 192)],
["ReadbackTable", 97, (("noStack", 97), "RSN", 192)],
["ReadbackTable", 98, (("look", 98), "RSN", 192)],
["ReadbackTable", 99, (("read", 99), "RSN", 192)],
["ReadbackTable", 100, (("node", 100), "RSN", 192)],
["ReadbackTable", 101, (("RepetitionOption", 47), "RSN", 137)],
["ReadbackTable", 102, (("CloseRound", 102), "RS", 138)],
["ReadbackTable", 103, (("Plus", 220), "RS", 139)],
["ReadbackTable", 104, (("QuestionMark", 221), "RS", 140)],
["ReadbackTable", 105, (("Star", 222), "RS", 141)],
["ReadbackTable", 106, (("Byte", 106), "RSN", 183)],
["ReadbackTable", 107, (("walkSymbol", 107), "RSN", 183)],
["ReadbackTable", 108, (("Name", 108), "RSN", 183)],
["ReadbackTable", 109, (("RepetitionOption", 61), "RSN", 200)],
["ReadbackTable", 110, (("Dot", 223), "RS", 142)],
["ReadbackTable", 111, (("Dot", 224), "RS", 143)],
["ReadbackTable", 112, (("CloseCurly", 225), "RS", 144)],
["ReadbackTable", 113, (("CloseCurly", 226), "RS", 145)],
["ReadbackTable", 114, (("Byte", 227), "RSN", 146)],
["ReadbackTable", 115, (("CloseSquare", 228), "RS", 147)],
["ReadbackTable", 116, (("TreeBuildingOptions", 229), "RSN", 148)],
["ReadbackTable", 117, (("SemanticAction", 230), "RSN", 201)],
["ReadbackTable", 118, (("walkInteger:", 231), "RSN", 149)],
["ReadbackTable", 119, (("Name", 232), "RSN", 201)],
["ReadbackTable", 120, (("Alternation", 233), "RSN", 150)],
["ReadbackTable", 121, (("AndExpression", 234), "RSN", 151)],
["ReadbackTable", 122, (("CloseSquare", 235), "RS", 152)],
["ReadbackTable", 123, (("Byte", 63), "RSN", 185)],
["ReadbackTable", 124, (("Concatenation", 62), "RSN", 153)],
["ReadbackTable", 125, (("Dot", 237), "RS", 154)],
["ReadbackTable", 126, (("Dot", 238), "RS", 155)],
["ReadbackTable", 127, (("walkInteger:", 239), "RSN", 156)],
["ReadbackTable", 128, (("Production", 10), "RSN", 129), (("Macro", 11), "RSN", 128), (("GrammarType", 2), "L", 202), (("|-", 1), "L", 202), (("Defaults", 209), "L", 202)],
["ReadbackTable", 129, (("Production", 10), "RSN", 129), (("Macro", 11), "RSN", 128), (("Defaults", 209), "L", 202), (("|-", 1), "L", 202), (("GrammarType", 2), "L", 202)],
["ReadbackTable", 130, (("super", 3), "RS", 199)],
["ReadbackTable", 131, (("RightArrow", 14), "RS", 190)],
["ReadbackTable", 132, (("RightPart", 16), "RSN", 132), (("|-", 1), "L", 195), (("LeftPart", 6), "L", 195), (("Macro", 11), "L", 195), (("GrammarType", 2), "L", 195), (("Defaults", 209), "L", 195), (("Production", 10), "L", 195)],
["ReadbackTable", 133, (("Name", 12), "RSN", 157)],
["ReadbackTable", 134, (("Name", 13), "RSN", 158)],
["ReadbackTable", 135, (("RightParts", 15), "RSN", 159)],
["ReadbackTable", 136, (("Name", 17), "RSN", 160)],
["ReadbackTable", 137, (("RepetitionOption", 51), "RSN", 200)],
["ReadbackTable", 138, (("Expression", 52), "RSN", 161)],
["ReadbackTable", 139, (("Primary", 41), "RSN", 203)],
["ReadbackTable", 140, (("Primary", 41), "RSN", 203)],
["ReadbackTable", 141, (("Primary", 41), "RSN", 203)],
["ReadbackTable", 142, (("Name", 38), "RSN", 162)],
["ReadbackTable", 143, (("Expression", 40), "RSN", 163)],
["ReadbackTable", 144, (("Expression", 42), "RSN", 164)],
["ReadbackTable", 145, (("Expression", 43), "RSN", 165)],
["ReadbackTable", 146, (("DotDot", 45), "RS", 166)],
["ReadbackTable", 147, (("OpenSquare", 46), "RS", 167), (("Attribute", 57), "RSN", 147)],
["ReadbackTable", 148, (("FatRightArrow", 48), "RS", 168)],
["ReadbackTable", 149, (("Plus", 59), "RS", 201), (("GrammarType", 2), "L", 201), (("RightArrow", 36), "L", 201), (("RightPart", 16), "L", 201), (("LeftPart", 6), "L", 201), (("Production", 10), "L", 201), (("FatRightArrow", 48), "L", 201), (("|-", 1), "L", 201), (("Expression", 29), "L", 201), (("Macro", 11), "L", 201), (("Defaults", 209), "L", 201)],
["ReadbackTable", 150, (("And", 49), "RS", 169)],
["ReadbackTable", 151, (("Minus", 50), "RS", 170)],
["ReadbackTable", 152, (("SemanticActionParameter", 60), "RSN", 152), (("OpenSquare", 53), "RS", 171)],
["ReadbackTable", 153, (("Or", 54), "RS", 172)],
["ReadbackTable", 154, (("Name", 55), "RSN", 173)],
["ReadbackTable", 155, (("Name", 56), "RSN", 174)],
["ReadbackTable", 156, (("Minus", 58), "RS", 201)],
["ReadbackTable", 157, (("Name", 12), "RSN", 157), (("keywords", 4), "RS", 184)],
["ReadbackTable", 158, (("output", 5), "RS", 184)],
["ReadbackTable", 159, (("LeftPart", 6), "RSN", 198)],
["ReadbackTable", 160, (("optimize", 7), "RS", 184)],
["ReadbackTable", 161, (("OpenRound", 32), "RS", 185)],
["ReadbackTable", 162, (("Name", 38), "RSN", 162), (("defaults", 19), "RS", 175)],
["ReadbackTable", 163, (("Equals", 21), "RS", 176)],
["ReadbackTable", 164, (("OpenCurly", 22), "RS", 177)],
["ReadbackTable", 165, (("OpenCurly", 25), "RS", 185)],
["ReadbackTable", 166, (("Byte", 26), "RSN", 185)],
["ReadbackTable", 167, (("Secondary", 27), "RSN", 197)],
["ReadbackTable", 168, (("Expression", 29), "RSN", 178)],
["ReadbackTable", 169, (("Alternation", 30), "RSN", 188)],
["ReadbackTable", 170, (("AndExpression", 31), "RSN", 186)],
["ReadbackTable", 171, (("walkSymbol", 34), "RSN", 196)],
["ReadbackTable", 172, (("Concatenation", 62), "RSN", 153)],
["ReadbackTable", 173, (("defaults", 37), "RS", 179), (("Name", 55), "RSN", 173)],
["ReadbackTable", 174, (("defaults", 39), "RS", 180), (("Name", 56), "RSN", 174)],
["ReadbackTable", 175, (("attribute", 8), "RS", 184)],
["ReadbackTable", 176, (("Name", 9), "RSN", 194)],
["ReadbackTable", 177, (("Name", 23), "RSN", 187)],
["ReadbackTable", 178, (("RightArrow", 36), "RS", 190)],
["ReadbackTable", 179, (("nonterminal", 18), "RS", 181)],
["ReadbackTable", 180, (("terminal", 20), "RS", 182)],
["ReadbackTable", 181, (("attribute", 8), "RS", 184)],
["ReadbackTable", 182, (("attribute", 8), "RS", 184)],
["ReduceTable", 183, "SemanticActionParameter", (53, "RSN", 60), (60, "RSN", 60)],
["ReduceTable", 184, "Defaults", (2, "RSN", 19), (209, "RSN", 19)],
["ReduceTable", 185, "Secondary", (14, "RSN", 27), (21, "RSN", 27), (22, "RSN", 27), (25, "RSN", 27), (28, "RSN", 27), (32, "RSN", 27), (36, "RSN", 27), (47, "RSN", 27), (49, "RSN", 27), (50, "RSN", 27), (51, "RSN", 27), (54, "RSN", 27), (61, "RSN", 27)],
["ReduceTable", 186, "Expression", (21, "RSN", 40), (32, "RSN", 52), (25, "RSN", 43), (22, "RSN", 42), (14, "RSN", 29), (36, "RSN", 29)],
["ReduceTable", 187, "LeftPart", (2, "RSN", 6), (10, "RSN", 6), (11, "RSN", 6), (209, "RSN", 6)],
["ReduceTable", 188, "AndExpression", (14, "RSN", 31), (21, "RSN", 31), (22, "RSN", 31), (25, "RSN", 31), (32, "RSN", 31), (36, "RSN", 31), (50, "RSN", 114)],
["ReduceTable", 189, "Name", (2, "RSN", 9), (11, "RSN", 9), (10, "RSN", 23), (209, "RSN", 23), (4, "RSN", 12), (12, "RSN", 12), (7, "RSN", 17), (39, "RSN", 56), (56, "RSN", 56), (19, "RSN", 38), (38, "RSN", 38), (53, "RSN", 122), (60, "RSN", 122), (48, "RSN", 111), (37, "RSN", 55), (55, "RSN", 55), (14, "RSN", 56), (21, "RSN", 56), (22, "RSN", 56), (25, "RSN", 56), (28, "RSN", 56), (32, "RSN", 56), (36, "RSN", 56), (47, "RSN", 56), (49, "RSN", 56), (50, "RSN", 56), (51, "RSN", 56), (54, "RSN", 56), (61, "RSN", 56), (5, "RSN", 13)],
["ReduceTable", 190, "RightPart", (6, "RSN", 16), (16, "RSN", 16)],
["ReduceTable", 191, "Byte", (45, "RSN", 95), (53, "RSN", 119), (60, "RSN", 119), (14, "RSN", 26), (21, "RSN", 26), (22, "RSN", 26), (25, "RSN", 26), (28, "RSN", 26), (32, "RSN", 26), (36, "RSN", 26), (47, "RSN", 26), (49, "RSN", 26), (50, "RSN", 26), (51, "RSN", 26), (54, "RSN", 26), (61, "RSN", 63)],
["ReduceTable", 192, "Attribute", (46, "RSN", 57), (57, "RSN", 57)],
["ReduceTable", 193, "Alternation", (49, "RSN", 113), (14, "RSN", 30), (21, "RSN", 30), (22, "RSN", 30), (25, "RSN", 30), (32, "RSN", 30), (36, "RSN", 30), (50, "RSN", 30)],
["ReduceTable", 194, "Macro", (2, "RSN", 11), (10, "RSN", 11), (11, "RSN", 11), (209, "RSN", 11)],
["ReduceTable", 195, "RightParts", (6, "RSN", 15)],
["ReduceTable", 196, "SemanticAction", (14, "RSN", 45), (21, "RSN", 45), (22, "RSN", 45), (25, "RSN", 45), (28, "RSN", 45), (32, "RSN", 45), (36, "RSN", 45), (47, "RSN", 45), (49, "RSN", 45), (50, "RSN", 45), (51, "RSN", 45), (54, "RSN", 45), (61, "RSN", 45), (48, "RSN", 108)],
["ReduceTable", 197, "Primary", (25, "RSN", 44), (32, "RSN", 44), (36, "RSN", 44), (51, "RSN", 44), (54, "RSN", 44), (14, "RSN", 33), (22, "RSN", 33), (28, "RSN", 33), (21, "RSN", 41), (47, "RSN", 41), (49, "RSN", 41), (50, "RSN", 41), (61, "RSN", 41)],
["ReduceTable", 198, "Production", (2, "RSN", 10), (10, "RSN", 10), (11, "RSN", 10), (209, "RSN", 10)],
["ReduceTable", 199, "GrammarType", (1, "RSN", 2)],
["ReduceTable", 200, "Concatenation", (14, "RSN", 35), (21, "RSN", 35), (22, "RSN", 35), (25, "RSN", 35), (32, "RSN", 35), (36, "RSN", 35), (49, "RSN", 35), (50, "RSN", 35), (54, "RSN", 62)],
["ReduceTable", 201, "TreeBuildingOptions", (48, "RSN", 107)],
["ReduceTable", 202, "Rules", (2, "RSN", 10), (209, "RSN", 10)],
["ReduceTable", 203, "RepetitionOption", (14, "RSN", 28), (21, "RSN", 28), (22, "RSN", 28), (25, "RSN", 28), (54, "RSN", 61), (28, "RSN", 47), (47, "RSN", 47), (51, "RSN", 47), (61, "RSN", 47), (32, "RSN", 51), (36, "RSN", 51), (49, "RSN", 51), (50, "RSN", 51)],
["SemanticTable", 204, "processTypeNow:", ["superScanner"], 64],
["SemanticTable", 205, "processTypeNow:", ["scanner"], 67],
["SemanticTable", 206, "processTypeNow:", ["parser"], 68],
["SemanticTable", 207, "buildTree", ["walkLeftPart:"], 69],
["SemanticTable", 208, "buildTree", ["walkGrammar:"], 70],
["SemanticTable", 209, "processAndDiscardDefaultsNow", [], 24],
["SemanticTable", 210, "processTypeNow:", ["superScanner"], 71],
["SemanticTable", 211, "buildTree", ["walkEpsilon:"], 193],
["SemanticTable", 212, "buildTree", ["walkOr:"], 83],
["SemanticTable", 213, "buildTree", ["walkKeywords:"], 85],
["SemanticTable", 214, "buildTree", ["walkOutput:"], 86],
["SemanticTable", 215, "buildTree", ["walkNonTreeBuildingSemanticAction:"], 88],
["SemanticTable", 216, "buildTree", ["walkSemanticAction:"], 90],
["SemanticTable", 217, "buildTree", ["walkProduction:"], 91],
["SemanticTable", 218, "buildTree", ["walkOptimize:"], 92],
["SemanticTable", 219, "buildTree", ["walkConcatenation:"], 101],
["SemanticTable", 220, "buildTree", ["walkPlus:"], 103],
["SemanticTable", 221, "buildTree", ["walkQuestionMark:"], 104],
["SemanticTable", 222, "buildTree", ["walkStar:"], 105],
["SemanticTable", 223, "buildTree", ["walkAttributeDefaults:"], 110],
["SemanticTable", 224, "buildTree", ["walkMacro:"], 111],
["SemanticTable", 225, "buildTree", ["walkLeftPartWithLookahead:"], 112],
["SemanticTable", 226, "buildTree", ["walkLook:"], 113],
["SemanticTable", 227, "buildTree", ["walkDotDot:"], 114],
["SemanticTable", 228, "buildTree", ["walkAttributes:"], 115],
["SemanticTable", 229, "buildTree", ["walkConcatenation:"], 116],
["SemanticTable", 230, "buildTree", ["walkTreeBuildingSemanticAction:"], 117],
["SemanticTable", 231, "buildTree", ["walkBuildTreeFromLeftIndex:"], 118],
["SemanticTable", 232, "buildTree", ["walkBuildTreeOrTokenFromName:"], 119],
["SemanticTable", 233, "buildTree", ["walkAnd:"], 120],
["SemanticTable", 234, "buildTree", ["walkMinus:"], 121],
["SemanticTable", 235, "buildTree", ["walkSemanticAction:"], 122],
["SemanticTable", 236, "buildTree", ["walkOr:"], 124],
["SemanticTable", 237, "buildTree", ["walkAttributeNonterminalDefaults:"], 125],
["SemanticTable", 238, "buildTree", ["walkAttributeTerminalDefaults:"], 126],
["SemanticTable", 239, "buildTree", ["walkBuildTreeFromRightIndex:"], 127],
["AcceptTable", 240]]

    var realparserTables: [Any] =
        [
            [
                "keywords", "stack", "noStack", "read", "look", "node", "noNode", "keep", "noKeep",
                "parser", "scanner", "super", "superScanner", "attribute", "defaults", "keywords",
                "output", "optimize", "terminal", "nonterminal",
            ],
            [
                "ReadaheadTable", 1, ("GrammarType", "RSN", 2), ("scanner", "RS", 172),
                ("superScanner", "RS", 173), ("super", "RS", 3), ("parser", "RS", 174),
                ("Grammar", "RSN", 177),
            ],
            [
                "ReadaheadTable", 2, ("walkString", "RSN", 69), ("Macro", "RSN", 4),
                ("keywords", "RS", 5), ("attribute", "RS", 6), ("optimize", "RS", 7),
                ("Name", "RSN", 8), ("output", "RS", 9), ("Rules", "RSN", 70),
                ("walkIdentifier", "RSN", 69), ("LeftPart", "RSN", 10), ("Defaults", "RSN", 175),
                ("Production", "RSN", 11),
            ],
            ["ReadaheadTable", 3, ("scanner", "RS", 176)],
            [
                "ReadaheadTable", 4, ("walkString", "RSN", 69), ("Macro", "RSN", 4),
                ("Name", "RSN", 8), ("walkIdentifier", "RSN", 69), ("LeftPart", "RSN", 10),
                ("Production", "RSN", 11), ("-|", "L", 72),
            ],
            [
                "ReadaheadTable", 5, ("walkString", "RSN", 69), ("Name", "RSN", 12),
                ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 6, ("defaults", "RS", 13), ("terminal", "RS", 14),
                ("nonterminal", "RS", 15),
            ],
            [
                "ReadaheadTable", 7, ("walkString", "RSN", 69), ("Name", "RSN", 16),
                ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 8, ("OpenCurly", "RS", 17), ("Equals", "RS", 18),
                ("RightArrow", "L", 73),
            ],
            [
                "ReadaheadTable", 9, ("walkString", "RSN", 69), ("Name", "RSN", 19),
                ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 10, ("RightArrow", "RS", 20), ("RightParts", "RSN", 21),
                ("RightPart", "RSN", 22),
            ],
            [
                "ReadaheadTable", 11, ("walkString", "RSN", 69), ("Macro", "RSN", 4),
                ("Name", "RSN", 8), ("walkIdentifier", "RSN", 69), ("LeftPart", "RSN", 10),
                ("Production", "RSN", 11), ("-|", "L", 72),
            ],
            [
                "ReadaheadTable", 12, ("walkString", "RSN", 69), ("Name", "RSN", 12),
                ("Dot", "RS", 85), ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 13, ("walkString", "RSN", 69), ("Name", "RSN", 24),
                ("walkIdentifier", "RSN", 69),
            ],
            ["ReadaheadTable", 14, ("defaults", "RS", 25)],
            ["ReadaheadTable", 15, ("defaults", "RS", 26)],
            ["ReadaheadTable", 16, ("Dot", "RS", 86)],
            [
                "ReadaheadTable", 17, ("AndExpression", "RSN", 27), ("Primary", "RSN", 28),
                ("Alternation", "RSN", 29), ("Secondary", "RSN", 30), ("Byte", "RSN", 31),
                ("OpenCurly", "RS", 32), ("walkString", "RSN", 69), ("walkSymbol", "RSN", 33),
                ("walkInteger", "RSN", 80), ("SemanticAction", "RSN", 88),
                ("walkCharacter", "RSN", 80), ("walkIdentifier", "RSN", 69),
                ("Expression", "RSN", 34), ("Concatenation", "RSN", 35),
                ("RepetitionOption", "RSN", 36), ("Name", "RSN", 79), ("OpenRound", "RS", 37),
                ("And", "L", 144), ("Minus", "L", 144), ("CloseCurly", "L", 144), ("Dot", "L", 144),
                ("CloseRound", "L", 144), ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            [
                "ReadaheadTable", 18, ("AndExpression", "RSN", 27), ("Primary", "RSN", 28),
                ("Alternation", "RSN", 29), ("Secondary", "RSN", 30), ("Byte", "RSN", 31),
                ("OpenCurly", "RS", 32), ("walkString", "RSN", 69), ("walkSymbol", "RSN", 33),
                ("walkInteger", "RSN", 80), ("SemanticAction", "RSN", 88),
                ("walkCharacter", "RSN", 80), ("walkIdentifier", "RSN", 69),
                ("Expression", "RSN", 38), ("Concatenation", "RSN", 35),
                ("RepetitionOption", "RSN", 36), ("Name", "RSN", 79), ("OpenRound", "RS", 37),
                ("And", "L", 144), ("Minus", "L", 144), ("CloseCurly", "L", 144), ("Dot", "L", 144),
                ("CloseRound", "L", 144), ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            ["ReadaheadTable", 19, ("Dot", "RS", 89)],
            [
                "ReadaheadTable", 20, ("AndExpression", "RSN", 27), ("Primary", "RSN", 28),
                ("walkString", "RSN", 69), ("Alternation", "RSN", 29), ("Secondary", "RSN", 30),
                ("Byte", "RSN", 31), ("OpenCurly", "RS", 32), ("walkSymbol", "RSN", 33),
                ("walkInteger", "RSN", 80), ("SemanticAction", "RSN", 88),
                ("walkCharacter", "RSN", 80), ("walkIdentifier", "RSN", 69),
                ("Expression", "RSN", 39), ("Concatenation", "RSN", 35),
                ("RepetitionOption", "RSN", 36), ("OpenRound", "RS", 37), ("Name", "RSN", 79),
                ("And", "L", 144), ("Minus", "L", 144), ("CloseCurly", "L", 144), ("Dot", "L", 144),
                ("CloseRound", "L", 144), ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            ["ReadaheadTable", 21, ("Dot", "RS", 90)],
            [
                "ReadaheadTable", 22, ("RightArrow", "RS", 20), ("RightPart", "RSN", 22),
                ("Dot", "L", 84),
            ],
            [
                "ReadaheadTable", 23, ("walkString", "RSN", 69), ("Macro", "RSN", 4),
                ("keywords", "RS", 5), ("attribute", "RS", 6), ("Name", "RSN", 8),
                ("walkIdentifier", "RSN", 69), ("Rules", "RSN", 70), ("optimize", "RS", 7),
                ("output", "RS", 9), ("LeftPart", "RSN", 10), ("Defaults", "RSN", 175),
                ("Production", "RSN", 11),
            ],
            [
                "ReadaheadTable", 24, ("walkString", "RSN", 69), ("Name", "RSN", 24),
                ("Dot", "RS", 91), ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 25, ("walkString", "RSN", 69), ("Name", "RSN", 40),
                ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 26, ("walkString", "RSN", 69), ("Name", "RSN", 41),
                ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 27, ("Minus", "RS", 42), ("CloseCurly", "L", 75),
                ("Dot", "L", 75), ("CloseRound", "L", 75), ("FatRightArrow", "L", 75),
                ("RightArrow", "L", 75),
            ],
            [
                "ReadaheadTable", 28, ("QuestionMark", "RS", 92), ("Star", "RS", 93),
                ("Plus", "RS", 94), ("walkSymbol", "L", 76), ("OpenRound", "L", 76),
                ("OpenCurly", "L", 76), ("walkIdentifier", "L", 76), ("walkString", "L", 76),
                ("walkCharacter", "L", 76), ("walkInteger", "L", 76), ("Or", "L", 76),
                ("And", "L", 76), ("Minus", "L", 76), ("CloseCurly", "L", 76), ("Dot", "L", 76),
                ("CloseRound", "L", 76), ("FatRightArrow", "L", 76), ("RightArrow", "L", 76),
            ],
            [
                "ReadaheadTable", 29, ("And", "RS", 43), ("Minus", "L", 77),
                ("CloseCurly", "L", 77), ("Dot", "L", 77), ("CloseRound", "L", 77),
                ("FatRightArrow", "L", 77), ("RightArrow", "L", 77),
            ],
            [
                "ReadaheadTable", 30, ("OpenSquare", "RS", 44), ("Star", "L", 78),
                ("QuestionMark", "L", 78), ("Plus", "L", 78), ("walkSymbol", "L", 78),
                ("OpenRound", "L", 78), ("OpenCurly", "L", 78), ("walkIdentifier", "L", 78),
                ("walkString", "L", 78), ("walkCharacter", "L", 78), ("walkInteger", "L", 78),
                ("Or", "L", 78), ("And", "L", 78), ("Minus", "L", 78), ("CloseCurly", "L", 78),
                ("Dot", "L", 78), ("CloseRound", "L", 78), ("FatRightArrow", "L", 78),
                ("RightArrow", "L", 78),
            ],
            [
                "ReadaheadTable", 31, ("DotDot", "RS", 45), ("OpenSquare", "L", 79),
                ("Star", "L", 79), ("QuestionMark", "L", 79), ("Plus", "L", 79),
                ("walkSymbol", "L", 79), ("OpenRound", "L", 79), ("OpenCurly", "L", 79),
                ("walkIdentifier", "L", 79), ("walkString", "L", 79), ("walkCharacter", "L", 79),
                ("walkInteger", "L", 79), ("Or", "L", 79), ("And", "L", 79), ("Minus", "L", 79),
                ("CloseCurly", "L", 79), ("Dot", "L", 79), ("CloseRound", "L", 79),
                ("FatRightArrow", "L", 79), ("RightArrow", "L", 79),
            ],
            [
                "ReadaheadTable", 32, ("AndExpression", "RSN", 27), ("Primary", "RSN", 28),
                ("Alternation", "RSN", 29), ("Secondary", "RSN", 30), ("Byte", "RSN", 31),
                ("OpenCurly", "RS", 32), ("walkString", "RSN", 69), ("walkSymbol", "RSN", 33),
                ("walkInteger", "RSN", 80), ("SemanticAction", "RSN", 88),
                ("walkCharacter", "RSN", 80), ("walkIdentifier", "RSN", 69),
                ("Expression", "RSN", 46), ("Concatenation", "RSN", 35),
                ("RepetitionOption", "RSN", 36), ("Name", "RSN", 79), ("OpenRound", "RS", 37),
                ("And", "L", 144), ("Minus", "L", 144), ("CloseCurly", "L", 144), ("Dot", "L", 144),
                ("CloseRound", "L", 144), ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            [
                "ReadaheadTable", 33, ("OpenSquare", "RS", 47), ("Star", "L", 87),
                ("QuestionMark", "L", 87), ("Plus", "L", 87), ("walkSymbol", "L", 87),
                ("OpenRound", "L", 87), ("OpenCurly", "L", 87), ("walkIdentifier", "L", 87),
                ("walkString", "L", 87), ("walkCharacter", "L", 87), ("walkInteger", "L", 87),
                ("Or", "L", 87), ("And", "L", 87), ("Minus", "L", 87), ("RightArrow", "L", 87),
                ("Dot", "L", 87), ("CloseCurly", "L", 87), ("CloseRound", "L", 87),
                ("FatRightArrow", "L", 87),
            ],
            ["ReadaheadTable", 34, ("CloseCurly", "RS", 97)],
            [
                "ReadaheadTable", 35, ("Or", "RS", 48), ("And", "L", 81), ("Minus", "L", 81),
                ("CloseCurly", "L", 81), ("Dot", "L", 81), ("CloseRound", "L", 81),
                ("FatRightArrow", "L", 81), ("RightArrow", "L", 81),
            ],
            [
                "ReadaheadTable", 36, ("Primary", "RSN", 28), ("walkString", "RSN", 69),
                ("Secondary", "RSN", 30), ("Byte", "RSN", 31), ("OpenCurly", "RS", 32),
                ("walkSymbol", "RSN", 33), ("walkInteger", "RSN", 80),
                ("SemanticAction", "RSN", 88), ("walkCharacter", "RSN", 80),
                ("walkIdentifier", "RSN", 69), ("RepetitionOption", "RSN", 49), ("Name", "RSN", 79),
                ("OpenRound", "RS", 37), ("Or", "L", 82), ("And", "L", 82), ("Minus", "L", 82),
                ("CloseCurly", "L", 82), ("Dot", "L", 82), ("CloseRound", "L", 82),
                ("FatRightArrow", "L", 82), ("RightArrow", "L", 82),
            ],
            [
                "ReadaheadTable", 37, ("AndExpression", "RSN", 27), ("Primary", "RSN", 28),
                ("walkString", "RSN", 69), ("Secondary", "RSN", 30), ("Byte", "RSN", 31),
                ("OpenCurly", "RS", 32), ("Alternation", "RSN", 29), ("walkSymbol", "RSN", 33),
                ("walkInteger", "RSN", 80), ("SemanticAction", "RSN", 88),
                ("walkCharacter", "RSN", 80), ("walkIdentifier", "RSN", 69),
                ("Expression", "RSN", 50), ("Concatenation", "RSN", 35), ("OpenRound", "RS", 37),
                ("Name", "RSN", 79), ("RepetitionOption", "RSN", 36), ("And", "L", 144),
                ("Minus", "L", 144), ("CloseCurly", "L", 144), ("Dot", "L", 144),
                ("CloseRound", "L", 144), ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            ["ReadaheadTable", 38, ("Dot", "RS", 100)],
            [
                "ReadaheadTable", 39, ("FatRightArrow", "RS", 51), ("RightArrow", "L", 83),
                ("Dot", "L", 83),
            ],
            [
                "ReadaheadTable", 40, ("walkString", "RSN", 69), ("Name", "RSN", 40),
                ("Dot", "RS", 101), ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 41, ("walkString", "RSN", 69), ("Name", "RSN", 41),
                ("Dot", "RS", 102), ("walkIdentifier", "RSN", 69),
            ],
            [
                "ReadaheadTable", 42, ("AndExpression", "RSN", 103), ("Primary", "RSN", 28),
                ("walkString", "RSN", 69), ("Alternation", "RSN", 29), ("Byte", "RSN", 31),
                ("OpenCurly", "RS", 32), ("Secondary", "RSN", 30), ("walkSymbol", "RSN", 33),
                ("walkInteger", "RSN", 80), ("SemanticAction", "RSN", 88),
                ("walkCharacter", "RSN", 80), ("walkIdentifier", "RSN", 69),
                ("Concatenation", "RSN", 35), ("OpenRound", "RS", 37), ("Name", "RSN", 79),
                ("RepetitionOption", "RSN", 36), ("And", "L", 144), ("Minus", "L", 144),
                ("CloseCurly", "L", 144), ("Dot", "L", 144), ("CloseRound", "L", 144),
                ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            [
                "ReadaheadTable", 43, ("Primary", "RSN", 28), ("Alternation", "RSN", 104),
                ("walkString", "RSN", 69), ("Byte", "RSN", 31), ("OpenCurly", "RS", 32),
                ("Secondary", "RSN", 30), ("walkSymbol", "RSN", 33), ("walkInteger", "RSN", 80),
                ("SemanticAction", "RSN", 88), ("walkCharacter", "RSN", 80),
                ("walkIdentifier", "RSN", 69), ("Concatenation", "RSN", 35),
                ("RepetitionOption", "RSN", 36), ("Name", "RSN", 79), ("OpenRound", "RS", 37),
                ("And", "L", 144), ("Minus", "L", 144), ("CloseCurly", "L", 144), ("Dot", "L", 144),
                ("CloseRound", "L", 144), ("FatRightArrow", "L", 144), ("RightArrow", "L", 144),
            ],
            [
                "ReadaheadTable", 44, ("Attribute", "RSN", 52), ("keep", "RSN", 95),
                ("noNode", "RSN", 95), ("noStack", "RSN", 95), ("CloseSquare", "RS", 105),
                ("read", "RSN", 95), ("look", "RSN", 95), ("stack", "RSN", 95), ("node", "RSN", 95),
                ("noKeep", "RSN", 95),
            ],
            [
                "ReadaheadTable", 45, ("Byte", "RSN", 106), ("walkInteger", "RSN", 80),
                ("walkCharacter", "RSN", 80),
            ],
            ["ReadaheadTable", 46, ("CloseCurly", "RS", 107)],
            [
                "ReadaheadTable", 47, ("walkString", "RSN", 69), ("walkSymbol", "RSN", 96),
                ("SemanticActionParameter", "RSN", 53), ("walkCharacter", "RSN", 80),
                ("walkIdentifier", "RSN", 69), ("CloseSquare", "RS", 108), ("Byte", "RSN", 96),
                ("Name", "RSN", 96), ("walkInteger", "RSN", 80),
            ],
            [
                "ReadaheadTable", 48, ("Primary", "RSN", 28), ("walkString", "RSN", 69),
                ("Secondary", "RSN", 30), ("Byte", "RSN", 31), ("OpenCurly", "RS", 32),
                ("walkSymbol", "RSN", 33), ("walkInteger", "RSN", 80),
                ("SemanticAction", "RSN", 88), ("walkCharacter", "RSN", 80),
                ("walkIdentifier", "RSN", 69), ("Concatenation", "RSN", 54),
                ("RepetitionOption", "RSN", 36), ("Name", "RSN", 79), ("OpenRound", "RS", 37),
            ],
            [
                "ReadaheadTable", 49, ("Primary", "RSN", 28), ("walkString", "RSN", 69),
                ("Secondary", "RSN", 30), ("Byte", "RSN", 31), ("OpenCurly", "RS", 32),
                ("walkSymbol", "RSN", 33), ("walkInteger", "RSN", 80),
                ("SemanticAction", "RSN", 88), ("walkCharacter", "RSN", 80),
                ("walkIdentifier", "RSN", 69), ("RepetitionOption", "RSN", 49), ("Name", "RSN", 79),
                ("OpenRound", "RS", 37), ("Or", "L", 98), ("And", "L", 98), ("Minus", "L", 98),
                ("CloseCurly", "L", 98), ("Dot", "L", 98), ("CloseRound", "L", 98),
                ("FatRightArrow", "L", 98), ("RightArrow", "L", 98),
            ],
            ["ReadaheadTable", 50, ("CloseRound", "RS", 99)],
            [
                "ReadaheadTable", 51, ("Minus", "RS", 55), ("walkSymbol", "RSN", 33),
                ("walkString", "RSN", 69), ("Name", "RSN", 110), ("walkIdentifier", "RSN", 69),
                ("Plus", "RS", 56), ("TreeBuildingOptions", "RSN", 111),
                ("SemanticAction", "RSN", 112), ("walkInteger", "RSN", 113),
            ],
            [
                "ReadaheadTable", 52, ("Attribute", "RSN", 52), ("keep", "RSN", 95),
                ("noNode", "RSN", 95), ("noStack", "RSN", 95), ("CloseSquare", "RS", 105),
                ("read", "RSN", 95), ("look", "RSN", 95), ("stack", "RSN", 95), ("node", "RSN", 95),
                ("noKeep", "RSN", 95),
            ],
            [
                "ReadaheadTable", 53, ("walkString", "RSN", 69), ("walkSymbol", "RSN", 96),
                ("Name", "RSN", 96), ("walkIdentifier", "RSN", 69), ("Byte", "RSN", 96),
                ("walkCharacter", "RSN", 80), ("CloseSquare", "RS", 108),
                ("SemanticActionParameter", "RSN", 53), ("walkInteger", "RSN", 80),
            ],
            [
                "ReadaheadTable", 54, ("Or", "RS", 48), ("And", "L", 109), ("Minus", "L", 109),
                ("CloseCurly", "L", 109), ("Dot", "L", 109), ("CloseRound", "L", 109),
                ("FatRightArrow", "L", 109), ("RightArrow", "L", 109),
            ],
            ["ReadaheadTable", 55, ("walkInteger", "RSN", 114)],
            ["ReadaheadTable", 56, ("walkInteger", "RSN", 113)],
            [
                "ReadbackTable", 57, (("GrammarType", 2), "RSN", 129),
                (("Defaults", 175), "RSN", 57),
            ],
            [
                "ReadbackTable", 58, (("Macro", 4), "RSN", 58), (("Production", 11), "RSN", 58),
                (("GrammarType", 2), "L", 142), (("Defaults", 175), "L", 142),
            ],
            ["ReadbackTable", 59, (("RightPart", 22), "RSN", 59), (("LeftPart", 10), "L", 145)],
            [
                "ReadbackTable", 60, (("RepetitionOption", 49), "RSN", 60),
                (("RepetitionOption", 36), "RSN", 157),
            ],
            ["ReadbackTable", 61, (("Attribute", 52), "RSN", 61), (("OpenSquare", 44), "RS", 116)],
            [
                "ReadbackTable", 62, (("SemanticActionParameter", 53), "RSN", 62),
                (("OpenSquare", 47), "RS", 87),
            ],
            ["ReadbackTable", 63, (("Plus", 56), "RS", 170), (("FatRightArrow", 51), "L", 170)],
            ["ReadbackTable", 64, (("keywords", 5), "RS", 146), (("Name", 12), "RSN", 64)],
            ["ReadbackTable", 65, (("defaults", 13), "RS", 117), (("Name", 24), "RSN", 65)],
            ["ReadbackTable", 66, (("defaults", 25), "RS", 118), (("Name", 40), "RSN", 66)],
            ["ReadbackTable", 67, (("defaults", 26), "RS", 119), (("Name", 41), "RSN", 67)],
            [
                "ReadbackTable", 68, (("Concatenation", 54), "RSN", 115),
                (("Concatenation", 35), "RSN", 166),
            ],
            ["ShiftbackTable", 69, 1, 126],
            ["ShiftbackTable", 70, 1, 57],
            ["ShiftbackTable", 71, 1, 133],
            ["ShiftbackTable", 72, 1, 58],
            ["ShiftbackTable", 73, 1, 143],
            ["ShiftbackTable", 74, 2, 133],
            ["ShiftbackTable", 75, 1, 121],
            ["ShiftbackTable", 76, 1, 125],
            ["ShiftbackTable", 77, 1, 131],
            ["ShiftbackTable", 78, 1, 135],
            ["ShiftbackTable", 79, 1, 138],
            ["ShiftbackTable", 80, 1, 139],
            ["ShiftbackTable", 81, 1, 137],
            ["ShiftbackTable", 82, 1, 122],
            ["ShiftbackTable", 83, 2, 140],
            ["ShiftbackTable", 84, 1, 59],
            ["ShiftbackTable", 85, 2, 64],
            ["ShiftbackTable", 86, 3, 147],
            ["ShiftbackTable", 87, 1, 148],
            ["ShiftbackTable", 88, 1, 149],
            ["ShiftbackTable", 89, 3, 150],
            ["ShiftbackTable", 90, 3, 151],
            ["ShiftbackTable", 91, 2, 65],
            ["ShiftbackTable", 92, 2, 153],
            ["ShiftbackTable", 93, 2, 154],
            ["ShiftbackTable", 94, 2, 155],
            ["ShiftbackTable", 95, 1, 136],
            ["ShiftbackTable", 96, 1, 128],
            ["ShiftbackTable", 97, 4, 156],
            ["ShiftbackTable", 98, 1, 60],
            ["ShiftbackTable", 99, 3, 138],
            ["ShiftbackTable", 100, 4, 158],
            ["ShiftbackTable", 101, 2, 66],
            ["ShiftbackTable", 102, 2, 67],
            ["ShiftbackTable", 103, 3, 161],
            ["ShiftbackTable", 104, 3, 162],
            ["ShiftbackTable", 105, 1, 61],
            ["ShiftbackTable", 106, 3, 164],
            ["ShiftbackTable", 107, 3, 165],
            ["ShiftbackTable", 108, 1, 62],
            ["ShiftbackTable", 109, 2, 68],
            ["ShiftbackTable", 110, 1, 167],
            ["ShiftbackTable", 111, 4, 168],
            ["ShiftbackTable", 112, 1, 169],
            ["ShiftbackTable", 113, 1, 63],
            ["ShiftbackTable", 114, 2, 171],
            ["ShiftbackTable", 115, 1, 68],
            ["ShiftbackTable", 116, 1, 163],
            ["ShiftbackTable", 117, 1, 152],
            ["ShiftbackTable", 118, 2, 159],
            ["ShiftbackTable", 119, 2, 160],
            [
                "ReduceTable", 120, "SemanticAction", (17, "RSN", 88), (18, "RSN", 88),
                (20, "RSN", 88), (32, "RSN", 88), (36, "RSN", 88), (37, "RSN", 88), (42, "RSN", 88),
                (43, "RSN", 88), (48, "RSN", 88), (49, "RSN", 88), (51, "RSN", 112),
            ],
            [
                "ReduceTable", 121, "Expression", (17, "RSN", 34), (18, "RSN", 38), (20, "RSN", 39),
                (32, "RSN", 46), (37, "RSN", 50),
            ],
            [
                "ReduceTable", 122, "Concatenation", (17, "RSN", 35), (18, "RSN", 35),
                (20, "RSN", 35), (32, "RSN", 35), (37, "RSN", 35), (42, "RSN", 35), (43, "RSN", 35),
                (48, "RSN", 54),
            ],
            [
                "ReduceTable", 123, "LeftPart", (2, "RSN", 10), (4, "RSN", 10), (11, "RSN", 10),
                (175, "RSN", 10),
            ],
            [
                "ReduceTable", 124, "Macro", (2, "RSN", 4), (4, "RSN", 4), (11, "RSN", 4),
                (175, "RSN", 4),
            ],
            [
                "ReduceTable", 125, "RepetitionOption", (17, "RSN", 36), (18, "RSN", 36),
                (20, "RSN", 36), (32, "RSN", 36), (36, "RSN", 49), (37, "RSN", 36), (42, "RSN", 36),
                (43, "RSN", 36), (48, "RSN", 36), (49, "RSN", 49),
            ],
            [
                "ReduceTable", 126, "Name", (2, "RSN", 8), (4, "RSN", 8), (5, "RSN", 12),
                (7, "RSN", 16), (9, "RSN", 19), (11, "RSN", 8), (12, "RSN", 12), (13, "RSN", 24),
                (17, "RSN", 79), (18, "RSN", 79), (20, "RSN", 79), (175, "RSN", 8), (24, "RSN", 24),
                (25, "RSN", 40), (26, "RSN", 41), (32, "RSN", 79), (36, "RSN", 79), (37, "RSN", 79),
                (40, "RSN", 40), (41, "RSN", 41), (42, "RSN", 79), (43, "RSN", 79), (47, "RSN", 96),
                (48, "RSN", 79), (49, "RSN", 79), (51, "RSN", 110), (53, "RSN", 96),
            ],
            ["ReduceTable", 127, "Defaults", (2, "RSN", 175), (175, "RSN", 175)],
            ["ReduceTable", 128, "SemanticActionParameter", (47, "RSN", 53), (53, "RSN", 53)],
            ["ReduceTable", 129, "Grammar", (1, "RSN", 177)],
            ["ReduceTable", 130, "TreeBuildingOptions", (51, "RSN", 111)],
            [
                "ReduceTable", 131, "AndExpression", (17, "RSN", 27), (18, "RSN", 27),
                (20, "RSN", 27), (32, "RSN", 27), (37, "RSN", 27), (42, "RSN", 103),
            ],
            [
                "ReduceTable", 132, "Production", (2, "RSN", 11), (4, "RSN", 11), (11, "RSN", 11),
                (175, "RSN", 11),
            ],
            ["ReduceTable", 133, "GrammarType", (1, "RSN", 2)],
            ["ReduceTable", 134, "Rules", (2, "RSN", 70), (175, "RSN", 70)],
            [
                "ReduceTable", 135, "Primary", (17, "RSN", 28), (18, "RSN", 28), (20, "RSN", 28),
                (32, "RSN", 28), (36, "RSN", 28), (37, "RSN", 28), (42, "RSN", 28), (43, "RSN", 28),
                (48, "RSN", 28), (49, "RSN", 28),
            ],
            ["ReduceTable", 136, "Attribute", (44, "RSN", 52), (52, "RSN", 52)],
            [
                "ReduceTable", 137, "Alternation", (17, "RSN", 29), (18, "RSN", 29),
                (20, "RSN", 29), (32, "RSN", 29), (37, "RSN", 29), (42, "RSN", 29),
                (43, "RSN", 104),
            ],
            [
                "ReduceTable", 138, "Secondary", (17, "RSN", 30), (18, "RSN", 30), (20, "RSN", 30),
                (32, "RSN", 30), (36, "RSN", 30), (37, "RSN", 30), (42, "RSN", 30), (43, "RSN", 30),
                (48, "RSN", 30), (49, "RSN", 30),
            ],
            [
                "ReduceTable", 139, "Byte", (17, "RSN", 31), (18, "RSN", 31), (20, "RSN", 31),
                (32, "RSN", 31), (36, "RSN", 31), (37, "RSN", 31), (42, "RSN", 31), (43, "RSN", 31),
                (45, "RSN", 106), (47, "RSN", 96), (48, "RSN", 31), (49, "RSN", 31),
                (53, "RSN", 96),
            ],
            ["ReduceTable", 140, "RightPart", (10, "RSN", 22), (22, "RSN", 22)],
            ["ReduceTable", 141, "RightParts", (10, "RSN", 21)],
            ["SemanticTable", 142, "buildTree", ["walkGrammar"], 134],
            ["SemanticTable", 143, "buildTree", ["walkLeftPart"], 123],
            ["SemanticTable", 144, "buildTree", ["walkEpsilon"], 137],
            ["SemanticTable", 145, "buildTree", ["walkOr"], 141],
            ["SemanticTable", 146, "buildTree", ["walkKeywords"], 127],
            ["SemanticTable", 147, "buildTree", ["walkOptimize"], 127],
            ["SemanticTable", 148, "buildTree", ["walkSemanticAction"], 120],
            ["SemanticTable", 149, "buildTree", ["walkNonTreeBuildingSemanticAction"], 135],
            ["SemanticTable", 150, "buildTree", ["walkOutput"], 127],
            ["SemanticTable", 151, "buildTree", ["walkProduction"], 132],
            ["SemanticTable", 152, "buildTree", ["walkAttributeDefaults"], 127],
            ["SemanticTable", 153, "buildTree", ["walkQuestionMark"], 125],
            ["SemanticTable", 154, "buildTree", ["walkStar"], 125],
            ["SemanticTable", 155, "buildTree", ["walkPlus"], 125],
            ["SemanticTable", 156, "buildTree", ["walkLeftPartWithLookahead"], 123],
            ["SemanticTable", 157, "buildTree", ["walkConcatenation"], 122],
            ["SemanticTable", 158, "buildTree", ["walkMacro"], 124],
            ["SemanticTable", 159, "buildTree", ["walkAttributeTerminalDefaults"], 127],
            ["SemanticTable", 160, "buildTree", ["walkAttributeNonterminalDefaults"], 127],
            ["SemanticTable", 161, "buildTree", ["walkMinus"], 121],
            ["SemanticTable", 162, "buildTree", ["walkAnd"], 131],
            ["SemanticTable", 163, "buildTree", ["walkAttributes"], 135],
            ["SemanticTable", 164, "buildTree", ["walkDotDot"], 138],
            ["SemanticTable", 165, "buildTree", ["walkLook"], 138],
            ["SemanticTable", 166, "buildTree", ["walkOr"], 137],
            ["SemanticTable", 167, "buildTree", ["walkBuildTreeOrTokenFromName"], 130],
            ["SemanticTable", 168, "buildTree", ["walkConcatenation"], 140],
            ["SemanticTable", 169, "buildTree", ["walkTreeBuildingSemanticAction"], 130],
            ["SemanticTable", 170, "buildTree", ["walkBuildTreeFromLeftIndex"], 130],
            ["SemanticTable", 171, "buildTree", ["walkBuildTreeFromRightIndex"], 130],
            ["SemanticTable", 172, "processTypeNow", ["scanner"], 71],
            ["SemanticTable", 173, "processTypeNow", ["superScanner"], 71],
            ["SemanticTable", 174, "processTypeNow", ["parser"], 71],
            ["SemanticTable", 175, "processAndDiscardDefaultsNow", [], 23],
            ["SemanticTable", 176, "processTypeNow", ["superScanner"], 74],
            ["AcceptTable", 177],
        ]
}
