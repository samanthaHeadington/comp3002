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
        case "walkIdentifier", "walkSymbol":
            return walkIdentifier(tree)
        case "walkCharacter":
            return walkCharacter(tree)
        case "walkString":
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

        // printStates()

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
            let localDown = down.performRelationStar(raState.initialItems)

            // print("\n\n    ~~~~~ \(i) ~~~~~    \n\n")

            localDown.do {
                up.add(Pair($2, raState), and: $1, and: Pair($0, raState))
            }

            raState.items.append(contentsOf: raState.initialItems)
            raState.items.append(contentsOf: localDown.allTo())

            // print("\(i), \(localDown)\n")
            // print(raState.items)

            right.from(raState.items) { M, localRight in
                // print(localRight.allTo())
                let candidate = ReadaheadState(localRight.allTo())
                var successor = readaheadStates.first {
                    Set($0.initialItems).contains(candidate.initialItems)
                }
                //print(candidate)
                if successor == nil {
                    readaheadStates.append(candidate)
                    successor = candidate
                }
                raState.addTransition(
                    Transition(label: M, goto: successor!))
                localRight.do { from, M, to in
                    left.add(
                        Pair(to, successor!),
                        and: Pair(M, successor!),
                        and: Pair(from, raState))
                }
            }

            i += 1
        }

        visible_left = Relation(
            from: left.triples.filter { $0.relationship.isVisible() })
        invisible_left = Relation(
            from: left.triples.filter { !$0.relationship.isVisible() })
    }

    func buildReadbackStateBridges() {
        var i = 0
        while i < readaheadStates.count {
            let raState = readaheadStates[i]
            let finalItems = raState.initialItems.filter { $0.isFinal }
            let partition = finalItems.partitionUsing { $0.leftPart }

            for (key, value) in partition {
                var new_state: FiniteStateMachineState = FiniteStateMachineState()

                if Grammar.activeGrammar!.isScanner() {
                    new_state = readaheadStates[0]
                } else if Grammar.activeGrammar!.isGoal(key) {
                    new_state = acceptState
                } else {
                    new_state = ReadbackState(
                        items: finalItems.map {
                            Pair($0, raState)
                        })

                    new_state.isInitial = true

                    readbackStates.append(new_state as! ReadbackState)
                }

                Grammar.activeGrammar!.productionFor(key).followSet.do {
                    raState.addTransition(
                        Transition(
                            label: Label(name: $0).asLook(),
                            goto: new_state
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
            let more_items = invisible_left.performStar(rbState.initialItems)

            rbState.items.append(contentsOf: rbState.initialItems)
            rbState.items.append(contentsOf: more_items)

            visible_left.from(more_items) { Mp, local_left in
                let candidate = ReadbackState(items: local_left.allTo())
                // print(candidate)
                var successor = readbackStates.first {
                    $0.initialItems.elementsEquivalent(candidate.initialItems)
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

    func lookbackFor(_ items: [Pair]) -> [Pair] {
        var result = up.performOnce(items)

        var i = 0

        while i < result.count {
            let pair = result[i]
            let up_items = up.performStar([pair])
            let left_items = invisible_left.performStar([pair])

            result.appendIfAbsent(up_items)
            result.appendIfAbsent(left_items)

            i += 1
        }

        var lookbacks: [Pair] = []

        visible_left.from(result) { Mp, relation in
            lookbacks.appendIfAbsent(Mp)
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
        let follow = Grammar.activeGrammar!.getFollow(transition.goto as! ReadaheadState)

        // build semantic state based on transition
        var new_state = SemanticState(transition.label, goto: transition.goto)
        let id_state = semanticStates.first(where: { $0.label == new_state.label && $0.goto == new_state.goto })

        if id_state != nil{
            new_state = id_state!
        }else{
            semanticStates.append(new_state)
        }

        // add follow to transitions from state to new semantic state
        Array(follow).do { name in
            if state.transitions.first(where: {$0.label.terseDescription == name}) == nil{
                state.addTransition(Transition(label: Label(name: name).asLook(), goto: new_state))
            }
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

        // print(ra_invisible_left)

        for rdState in reduceStates {
            for (key, value) in rdState.value.restarts {
                
                var i = 0

                while i < value.count { let state = value[i]
                    i += 1

                    // print(raState.terseDescription)
                    // print(
                    //     ra_invisible_left.performStar([raState]).filter {
                    //         stackable_states.contains($0)
                    //     }.map { $0.terseDescription }.joined(separator: ", "))

                    if(stackable_states.contains(state)){
                        continue
                    }

                    let alt_restarts = ra_invisible_left.performStar([state]).filter {
                        stackable_states.contains($0)
                    }

                    rdState.value.restarts[key]!.removeAll { $0 == state }

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
            if (rbState.transitions.partitionUsing{$0.goto}.count == 1) {
                let new_state = ShiftState(1, rbState.transitions[0].goto)
                shiftStates.append(new_state)
                
                replaceState(
                    rbState, with: new_state)
                dead_states.append(rbState)
            }
        }

        readbackStates.removeAll { dead_states.contains($0) }

        dead_states.removeAll()

        shiftStates.do{state in
            if(!dead_states.contains(state)){
                shiftStates.filter {$0 != state && $0.shiftVal == state.shiftVal && $0.goto == state.goto}.do{
                    replaceState($0, with: state)
                    dead_states.append($0)
                }
            }
        }

        shiftStates.removeAll { dead_states.contains($0) }

        dead_states.removeAll()

        // merge consecutive shift states
        mergeShifts()
    }

    func mergeShifts(){
        shiftStates.do { shState in
            if shState.goto as? ShiftState != nil {
                shState.shiftVal += (shState.goto as! ShiftState).shiftVal
                shState.goto = (shState.goto as! ShiftState).goto
            }
        }
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
                var new_key = key

                if(key == old){
                    new_key = new_state
                }

                var new_value = value
                new_value.removeAll { $0 == old }
                if new_value.count < value.count || new_key != key{
                    new_value.appendIfAbsent(new_state)
                    rdState.value.restarts[new_key] = new_value
                }
            }

            rdState.value.restarts.removeValue(forKey: old)
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
                "[\"ShiftbackTable\", \(shState.stateNumber), \(shState.shiftVal), \(shState.goto.stateNumber)],\n"
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
        var symbol: String = (tree as! Token).symbol

        if symbol.last! == ":"{
            symbol.removeLast()
        }

        print(symbol)

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
        let return_val = walkTree((tree as! Tree).child(0)) as! FiniteStateMachine

        return_val.override(["look"])

        return return_val
    }

    func processAndDiscardDefaultsNow() {
        //Pick up the tree just built containing either the attributes, keywords, optimize, and output tree,
        //process it with walkTree, and remove it from the tree stack... by replacing the entry by nil..."
        let tree: Tree = parser!.treeStack.last as! Tree
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
["ReadaheadTable", 1, ("super", "RS", 2), ("scanner", "RS", 290), ("superScanner", "RS", 291), ("GrammarType", "RSN", 3), ("parser", "RS", 292)],
["ReadaheadTable", 2, ("scanner", "RS", 293)],
["ReadaheadTable", 3, ("attribute", "RS", 4), ("Rules", "RSN", 326), ("output", "RS", 5), ("Name", "RSN", 6), ("Macro", "RSN", 7), ("walkString", "RSN", 67), ("optimize", "RS", 8), ("walkIdentifier", "RSN", 68), ("Defaults", "RSN", 296), ("LeftPart", "RSN", 9), ("keywords", "RS", 10), ("Production", "RSN", 11)],
["ReadaheadTable", 4, ("defaults", "RS", 12), ("terminal", "RS", 13), ("nonterminal", "RS", 14)],
["ReadaheadTable", 5, ("walkIdentifier", "RSN", 68), ("Name", "RSN", 15), ("walkString", "RSN", 67)],
["ReadaheadTable", 6, ("OpenCurly", "RS", 16), ("Equals", "RS", 17), ("RightArrow", "L", 294), ("walkIdentifier", "L", 294), ("walkString", "L", 294)],
["ReadaheadTable", 7, ("walkString", "RSN", 67), ("Name", "RSN", 18), ("Macro", "RSN", 7), ("walkIdentifier", "RSN", 68), ("LeftPart", "RSN", 9), ("Production", "RSN", 11), ("-|", "L", 295)],
["ReadaheadTable", 8, ("Name", "RSN", 19), ("walkString", "RSN", 67), ("walkIdentifier", "RSN", 68)],
["ReadaheadTable", 9, ("RightParts", "RSN", 21), ("RightPart", "RSN", 22), ("RightArrow", "RS", 23)],
["ReadaheadTable", 10, ("Name", "RSN", 24), ("walkString", "RSN", 67), ("walkIdentifier", "RSN", 68)],
["ReadaheadTable", 11, ("walkString", "RSN", 67), ("Name", "RSN", 25), ("Macro", "RSN", 7), ("walkIdentifier", "RSN", 68), ("LeftPart", "RSN", 9), ("Production", "RSN", 11), ("-|", "L", 295)],
["ReadaheadTable", 12, ("Name", "RSN", 26), ("walkIdentifier", "RSN", 68), ("walkString", "RSN", 67)],
["ReadaheadTable", 13, ("defaults", "RS", 27)],
["ReadaheadTable", 14, ("defaults", "RS", 28)],
["ReadaheadTable", 15, ("Dot", "RS", 299)],
["ReadaheadTable", 16, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("AndExpression", "RSN", 29), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 33), ("Expression", "RSN", 34), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 37), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 17, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("AndExpression", "RSN", 29), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 33), ("Expression", "RSN", 40), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 41), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 18, ("OpenCurly", "RS", 16), ("Equals", "RS", 17), ("RightArrow", "L", 294), ("walkIdentifier", "L", 294), ("walkString", "L", 294)],
["ReadaheadTable", 19, ("Dot", "RS", 302)],
["ReadaheadTable", 20, ("attribute", "RS", 42), ("output", "RS", 5), ("walkString", "RSN", 67), ("Name", "RSN", 43), ("Macro", "RSN", 7), ("optimize", "RS", 8), ("Rules", "RSN", 326), ("walkIdentifier", "RSN", 68), ("Defaults", "RSN", 296), ("LeftPart", "RSN", 9), ("keywords", "RS", 10), ("Production", "RSN", 11)],
["ReadaheadTable", 21, ("Dot", "RS", 303)],
["ReadaheadTable", 22, ("RightArrow", "RS", 23), ("RightPart", "RSN", 22), ("Dot", "L", 298)],
["ReadaheadTable", 23, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("AndExpression", "RSN", 29), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 33), ("Expression", "RSN", 44), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 45), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 24, ("Name", "RSN", 24), ("walkString", "RSN", 67), ("Dot", "RS", 304), ("walkIdentifier", "RSN", 68)],
["ReadaheadTable", 25, ("OpenCurly", "RS", 16), ("Equals", "RS", 17), ("RightArrow", "L", 294), ("walkIdentifier", "L", 294), ("walkString", "L", 294)],
["ReadaheadTable", 26, ("Name", "RSN", 26), ("walkString", "RSN", 67), ("Dot", "RS", 305), ("walkIdentifier", "RSN", 68)],
["ReadaheadTable", 27, ("walkIdentifier", "RSN", 68), ("walkString", "RSN", 67), ("Name", "RSN", 46)],
["ReadaheadTable", 28, ("walkIdentifier", "RSN", 68), ("Name", "RSN", 47), ("walkString", "RSN", 67)],
["ReadaheadTable", 29, ("Minus", "RS", 48), ("CloseRound", "L", 74), ("CloseCurly", "L", 74), ("Dot", "L", 74), ("RightArrow", "L", 74), ("FatRightArrow", "L", 74)],
["ReadaheadTable", 30, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("RepetitionOption", "RSN", 49), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 41), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("walkSymbol", "RSN", 39), ("And", "L", 75), ("Or", "L", 75), ("CloseRound", "L", 75), ("CloseCurly", "L", 75), ("Dot", "L", 75), ("RightArrow", "L", 75), ("FatRightArrow", "L", 75), ("Minus", "L", 75)],
["ReadaheadTable", 31, ("OpenSquare", "RS", 50), ("walkSymbol", "L", 76), ("OpenRound", "L", 76), ("OpenCurly", "L", 76), ("walkIdentifier", "L", 76), ("walkString", "L", 76), ("walkCharacter", "L", 76), ("walkInteger", "L", 76), ("Star", "L", 76), ("QuestionMark", "L", 76), ("Plus", "L", 76), ("And", "L", 76), ("Or", "L", 76), ("CloseRound", "L", 76), ("CloseCurly", "L", 76), ("Dot", "L", 76), ("RightArrow", "L", 76), ("FatRightArrow", "L", 76), ("Minus", "L", 76)],
["ReadaheadTable", 32, ("DotDot", "RS", 51), ("OpenSquare", "L", 78), ("walkSymbol", "L", 78), ("OpenRound", "L", 78), ("OpenCurly", "L", 78), ("walkIdentifier", "L", 78), ("walkString", "L", 78), ("walkCharacter", "L", 78), ("walkInteger", "L", 78), ("Star", "L", 78), ("QuestionMark", "L", 78), ("Plus", "L", 78), ("And", "L", 78), ("Or", "L", 78), ("CloseRound", "L", 78), ("CloseCurly", "L", 78), ("Dot", "L", 78), ("RightArrow", "L", 78), ("FatRightArrow", "L", 78), ("Minus", "L", 78)],
["ReadaheadTable", 33, ("And", "RS", 52), ("CloseRound", "L", 80), ("CloseCurly", "L", 80), ("Dot", "L", 80), ("RightArrow", "L", 80), ("FatRightArrow", "L", 80), ("Minus", "L", 80)],
["ReadaheadTable", 34, ("CloseCurly", "RS", 307)],
["ReadaheadTable", 35, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("AndExpression", "RSN", 29), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 33), ("Expression", "RSN", 53), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 45), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 36, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("AndExpression", "RSN", 29), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 33), ("Expression", "RSN", 54), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 55), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 37, ("Plus", "RS", 308), ("QuestionMark", "RS", 309), ("Star", "RS", 310), ("walkSymbol", "L", 81), ("OpenRound", "L", 81), ("OpenCurly", "L", 81), ("walkIdentifier", "L", 81), ("walkString", "L", 81), ("walkCharacter", "L", 81), ("walkInteger", "L", 81), ("And", "L", 81), ("Or", "L", 81), ("CloseRound", "L", 81), ("CloseCurly", "L", 81), ("Dot", "L", 81), ("RightArrow", "L", 81), ("FatRightArrow", "L", 81), ("Minus", "L", 81)],
["ReadaheadTable", 38, ("Or", "RS", 56), ("And", "L", 83), ("CloseRound", "L", 83), ("CloseCurly", "L", 83), ("Dot", "L", 83), ("RightArrow", "L", 83), ("FatRightArrow", "L", 83), ("Minus", "L", 83)],
["ReadaheadTable", 39, ("OpenSquare", "RS", 57), ("walkSymbol", "L", 301), ("OpenRound", "L", 301), ("OpenCurly", "L", 301), ("walkIdentifier", "L", 301), ("walkString", "L", 301), ("walkCharacter", "L", 301), ("walkInteger", "L", 301), ("Star", "L", 301), ("QuestionMark", "L", 301), ("Plus", "L", 301), ("RightArrow", "L", 301), ("And", "L", 301), ("Or", "L", 301), ("Dot", "L", 301), ("CloseRound", "L", 301), ("CloseCurly", "L", 301), ("FatRightArrow", "L", 301), ("Minus", "L", 301)],
["ReadaheadTable", 40, ("Dot", "RS", 311)],
["ReadaheadTable", 41, ("Plus", "RS", 308), ("QuestionMark", "RS", 309), ("Star", "RS", 310), ("walkSymbol", "L", 84), ("OpenRound", "L", 84), ("OpenCurly", "L", 84), ("walkIdentifier", "L", 84), ("walkString", "L", 84), ("walkCharacter", "L", 84), ("walkInteger", "L", 84), ("And", "L", 84), ("Or", "L", 84), ("CloseRound", "L", 84), ("CloseCurly", "L", 84), ("Dot", "L", 84), ("RightArrow", "L", 84), ("FatRightArrow", "L", 84), ("Minus", "L", 84)],
["ReadaheadTable", 42, ("defaults", "RS", 12), ("terminal", "RS", 13), ("nonterminal", "RS", 14)],
["ReadaheadTable", 43, ("OpenCurly", "RS", 16), ("Equals", "RS", 17), ("RightArrow", "L", 294), ("walkIdentifier", "L", 294), ("walkString", "L", 294)],
["ReadaheadTable", 44, ("FatRightArrow", "RS", 58), ("RightArrow", "L", 86), ("Dot", "L", 86)],
["ReadaheadTable", 45, ("Plus", "RS", 308), ("QuestionMark", "RS", 309), ("Star", "RS", 310), ("walkSymbol", "L", 87), ("OpenRound", "L", 87), ("OpenCurly", "L", 87), ("walkIdentifier", "L", 87), ("walkString", "L", 87), ("walkCharacter", "L", 87), ("walkInteger", "L", 87), ("And", "L", 87), ("Or", "L", 87), ("CloseRound", "L", 87), ("CloseCurly", "L", 87), ("Dot", "L", 87), ("RightArrow", "L", 87), ("FatRightArrow", "L", 87), ("Minus", "L", 87)],
["ReadaheadTable", 46, ("Dot", "RS", 312), ("Name", "RSN", 46), ("walkString", "RSN", 67), ("walkIdentifier", "RSN", 68)],
["ReadaheadTable", 47, ("Dot", "RS", 313), ("Name", "RSN", 47), ("walkString", "RSN", 67), ("walkIdentifier", "RSN", 68)],
["ReadaheadTable", 48, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("AndExpression", "RSN", 314), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 33), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 55), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 49, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("RepetitionOption", "RSN", 49), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 59), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("walkSymbol", "RSN", 39), ("And", "L", 306), ("Or", "L", 306), ("CloseRound", "L", 306), ("CloseCurly", "L", 306), ("Dot", "L", 306), ("RightArrow", "L", 306), ("FatRightArrow", "L", 306), ("Minus", "L", 306)],
["ReadaheadTable", 50, ("look", "RSN", 98), ("stack", "RSN", 99), ("CloseSquare", "RS", 315), ("noStack", "RSN", 100), ("noNode", "RSN", 101), ("read", "RSN", 102), ("node", "RSN", 103), ("keep", "RSN", 104), ("Attribute", "RSN", 60), ("noKeep", "RSN", 105)],
["ReadaheadTable", 51, ("Byte", "RSN", 316), ("walkCharacter", "RSN", 73), ("walkInteger", "RSN", 79)],
["ReadaheadTable", 52, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("Alternation", "RSN", 317), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 55), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 38), ("walkSymbol", "RSN", 39), ("And", "L", 297), ("CloseRound", "L", 297), ("CloseCurly", "L", 297), ("Dot", "L", 297), ("RightArrow", "L", 297), ("FatRightArrow", "L", 297), ("Minus", "L", 297), ("Equals", "L", 297)],
["ReadaheadTable", 53, ("CloseRound", "RS", 107)],
["ReadaheadTable", 54, ("CloseCurly", "RS", 318)],
["ReadaheadTable", 55, ("Plus", "RS", 308), ("QuestionMark", "RS", 309), ("Star", "RS", 310), ("walkSymbol", "L", 89), ("OpenRound", "L", 89), ("OpenCurly", "L", 89), ("walkIdentifier", "L", 89), ("walkString", "L", 89), ("walkCharacter", "L", 89), ("walkInteger", "L", 89), ("And", "L", 89), ("Or", "L", 89), ("CloseRound", "L", 89), ("CloseCurly", "L", 89), ("Dot", "L", 89), ("RightArrow", "L", 89), ("FatRightArrow", "L", 89), ("Minus", "L", 89)],
["ReadaheadTable", 56, ("walkIdentifier", "RSN", 68), ("walkCharacter", "RSN", 73), ("RepetitionOption", "RSN", 30), ("Secondary", "RSN", 31), ("Byte", "RSN", 32), ("walkInteger", "RSN", 79), ("OpenRound", "RS", 35), ("OpenCurly", "RS", 36), ("SemanticAction", "RSN", 300), ("Primary", "RSN", 55), ("walkString", "RSN", 67), ("Name", "RSN", 82), ("Concatenation", "RSN", 61), ("walkSymbol", "RSN", 39)],
["ReadaheadTable", 57, ("walkString", "RSN", 67), ("CloseSquare", "RS", 320), ("Name", "RSN", 111), ("walkSymbol", "RSN", 112), ("walkIdentifier", "RSN", 68), ("walkInteger", "RSN", 79), ("SemanticActionParameter", "RSN", 62), ("Byte", "RSN", 113), ("walkCharacter", "RSN", 73)],
["ReadaheadTable", 58, ("walkString", "RSN", 67), ("TreeBuildingOptions", "RSN", 321), ("SemanticAction", "RSN", 322), ("Name", "RSN", 323), ("Plus", "RS", 63), ("walkSymbol", "RSN", 39), ("walkInteger", "RSN", 324), ("walkIdentifier", "RSN", 68), ("Minus", "RS", 64)],
["ReadaheadTable", 59, ("Plus", "RS", 308), ("QuestionMark", "RS", 309), ("Star", "RS", 310), ("walkSymbol", "L", 97), ("OpenRound", "L", 97), ("OpenCurly", "L", 97), ("walkIdentifier", "L", 97), ("walkString", "L", 97), ("walkCharacter", "L", 97), ("walkInteger", "L", 97), ("And", "L", 97), ("Or", "L", 97), ("CloseRound", "L", 97), ("CloseCurly", "L", 97), ("Dot", "L", 97), ("RightArrow", "L", 97), ("FatRightArrow", "L", 97), ("Minus", "L", 97)],
["ReadaheadTable", 60, ("Attribute", "RSN", 60), ("stack", "RSN", 99), ("CloseSquare", "RS", 315), ("noStack", "RSN", 100), ("noNode", "RSN", 101), ("read", "RSN", 102), ("node", "RSN", 103), ("keep", "RSN", 104), ("noKeep", "RSN", 105), ("look", "RSN", 98)],
["ReadaheadTable", 61, ("Or", "RS", 56), ("And", "L", 319), ("CloseRound", "L", 319), ("CloseCurly", "L", 319), ("Dot", "L", 319), ("RightArrow", "L", 319), ("FatRightArrow", "L", 319), ("Minus", "L", 319), ("walkSymbol", "L", 319), ("OpenRound", "L", 319), ("OpenCurly", "L", 319), ("walkIdentifier", "L", 319), ("walkString", "L", 319), ("walkCharacter", "L", 319), ("walkInteger", "L", 319)],
["ReadaheadTable", 62, ("CloseSquare", "RS", 320), ("walkString", "RSN", 67), ("Name", "RSN", 111), ("walkSymbol", "RSN", 112), ("walkIdentifier", "RSN", 68), ("walkInteger", "RSN", 79), ("SemanticActionParameter", "RSN", 62), ("Byte", "RSN", 113), ("walkCharacter", "RSN", 73)],
["ReadaheadTable", 63, ("walkInteger", "RSN", 324)],
["ReadaheadTable", 64, ("walkInteger", "RSN", 325)],
["ReadbackTable", 65, (("scanner", 290), "RS", 129)],
["ReadbackTable", 66, (("superScanner", 291), "RS", 130)],
["ReadbackTable", 67, (("walkString", 67), "RSN", 131)],
["ReadbackTable", 68, (("walkIdentifier", 68), "RSN", 132)],
["ReadbackTable", 69, (("parser", 292), "RS", 133)],
["ReadbackTable", 70, (("scanner", 293), "RS", 134)],
["ReadbackTable", 71, (("Name", 43), "RSN", 135)],
["ReadbackTable", 72, (("Production", 11), "RSN", 139), (("Macro", 7), "RSN", 140)],
["ReadbackTable", 73, (("walkCharacter", 73), "RSN", 141)],
["ReadbackTable", 74, (("AndExpression", 29), "RSN", 142)],
["ReadbackTable", 75, (("RepetitionOption", 30), "RSN", 143)],
["ReadbackTable", 76, (("Secondary", 31), "RSN", 144)],
["ReadbackTable", 77, (("OpenRound", 35), "RS", 285), (("OpenCurly", 16), "RS", 285), (("And", 52), "RS", 285), (("Equals", 17), "RS", 285), (("RightArrow", 23), "RS", 285), (("Minus", 48), "RS", 285)],
["ReadbackTable", 78, (("Byte", 32), "RSN", 145)],
["ReadbackTable", 79, (("walkInteger", 79), "RSN", 146)],
["ReadbackTable", 80, (("Alternation", 33), "RSN", 147)],
["ReadbackTable", 81, (("Primary", 37), "RSN", 148)],
["ReadbackTable", 82, (("Name", 82), "RSN", 149)],
["ReadbackTable", 83, (("Concatenation", 38), "RSN", 150)],
["ReadbackTable", 84, (("Primary", 41), "RSN", 151)],
["ReadbackTable", 85, (("RightPart", 22), "RSN", 152)],
["ReadbackTable", 86, (("Expression", 44), "RSN", 153)],
["ReadbackTable", 87, (("Primary", 45), "RSN", 154)],
["ReadbackTable", 88, (("Dot", 299), "RS", 155)],
["ReadbackTable", 89, (("Primary", 55), "RSN", 156)],
["ReadbackTable", 90, (("SemanticAction", 300), "RSN", 157)],
["ReadbackTable", 91, (("walkSymbol", 39), "RSN", 158)],
["ReadbackTable", 92, (("Dot", 302), "RS", 159)],
["ReadbackTable", 93, (("Dot", 303), "RS", 160)],
["ReadbackTable", 94, (("Dot", 304), "RS", 161)],
["ReadbackTable", 95, (("Dot", 305), "RS", 162)],
["ReadbackTable", 96, (("RepetitionOption", 49), "RSN", 163)],
["ReadbackTable", 97, (("Primary", 59), "RSN", 164)],
["ReadbackTable", 98, (("look", 98), "RSN", 165)],
["ReadbackTable", 99, (("stack", 99), "RSN", 166)],
["ReadbackTable", 100, (("noStack", 100), "RSN", 167)],
["ReadbackTable", 101, (("noNode", 101), "RSN", 168)],
["ReadbackTable", 102, (("read", 102), "RSN", 169)],
["ReadbackTable", 103, (("node", 103), "RSN", 170)],
["ReadbackTable", 104, (("keep", 104), "RSN", 171)],
["ReadbackTable", 105, (("noKeep", 105), "RSN", 172)],
["ReadbackTable", 106, (("CloseCurly", 307), "RS", 173)],
["ReadbackTable", 107, (("CloseRound", 107), "RS", 174)],
["ReadbackTable", 108, (("Plus", 308), "RS", 175)],
["ReadbackTable", 109, (("QuestionMark", 309), "RS", 176)],
["ReadbackTable", 110, (("Star", 310), "RS", 177)],
["ReadbackTable", 111, (("Name", 111), "RSN", 178)],
["ReadbackTable", 112, (("walkSymbol", 112), "RSN", 179)],
["ReadbackTable", 113, (("Byte", 113), "RSN", 180)],
["ReadbackTable", 114, (("Dot", 311), "RS", 181)],
["ReadbackTable", 115, (("Dot", 312), "RS", 182)],
["ReadbackTable", 116, (("Dot", 313), "RS", 183)],
["ReadbackTable", 117, (("AndExpression", 314), "RSN", 184)],
["ReadbackTable", 118, (("CloseSquare", 315), "RS", 185)],
["ReadbackTable", 119, (("Byte", 316), "RSN", 186)],
["ReadbackTable", 120, (("Alternation", 317), "RSN", 187)],
["ReadbackTable", 121, (("CloseCurly", 318), "RS", 188)],
["ReadbackTable", 122, (("Concatenation", 61), "RSN", 189)],
["ReadbackTable", 123, (("CloseSquare", 320), "RS", 190)],
["ReadbackTable", 124, (("TreeBuildingOptions", 321), "RSN", 191)],
["ReadbackTable", 125, (("SemanticAction", 322), "RSN", 192)],
["ReadbackTable", 126, (("Name", 323), "RSN", 193)],
["ReadbackTable", 127, (("walkInteger", 324), "RSN", 194)],
["ReadbackTable", 128, (("walkInteger", 325), "RSN", 195)],
["ReadbackTable", 129, (("|-", 1), "RS", 288)],
["ReadbackTable", 130, (("|-", 1), "RS", 288)],
["ReadbackTable", 131, (("keywords", 10), "RS", 278), (("OpenCurly", 16), "RS", 278), (("GrammarType", 3), "RSN", 278), (("OpenRound", 35), "RS", 278), (("Name", 26), "RSN", 278), (("SemanticActionParameter", 62), "RSN", 278), (("OpenSquare", 57), "RS", 278), (("Minus", 48), "RS", 278), (("defaults", 28), "RS", 278), (("optimize", 8), "RS", 278), (("Or", 56), "RS", 278), (("RepetitionOption", 49), "RSN", 278), (("Macro", 7), "RSN", 278), (("FatRightArrow", 58), "RS", 278), (("RightArrow", 23), "RS", 278), (("Equals", 17), "RS", 278), (("And", 52), "RS", 278), (("output", 5), "RS", 278), (("Defaults", 296), "RSN", 278), (("Production", 11), "RSN", 278)],
["ReadbackTable", 132, (("Defaults", 296), "RSN", 278), (("Equals", 17), "RS", 278), (("Name", 46), "RSN", 278), (("OpenCurly", 36), "RS", 278), (("SemanticActionParameter", 62), "RSN", 278), (("GrammarType", 3), "RSN", 278), (("defaults", 27), "RS", 278), (("RepetitionOption", 30), "RSN", 278), (("Macro", 7), "RSN", 278), (("optimize", 8), "RS", 278), (("And", 52), "RS", 278), (("Or", 56), "RS", 278), (("keywords", 10), "RS", 278), (("output", 5), "RS", 278), (("OpenSquare", 57), "RS", 278), (("RightArrow", 23), "RS", 278), (("OpenRound", 35), "RS", 278), (("Minus", 48), "RS", 278), (("Production", 11), "RSN", 278), (("FatRightArrow", 58), "RS", 278)],
["ReadbackTable", 133, (("|-", 1), "RS", 288)],
["ReadbackTable", 134, (("super", 2), "RS", 196)],
["ReadbackTable", 135, (("Defaults", 296), "RSN", 282)],
["ReadbackTable", 136, (("GrammarType", 3), "RSN", 282)],
["ReadbackTable", 137, (("Macro", 7), "RSN", 282)],
["ReadbackTable", 138, (("Production", 11), "RSN", 282)],
["ReadbackTable", 139, (("Production", 11), "RSN", 139), (("Macro", 7), "RSN", 140), (("GrammarType", 3), "RSN", 269), (("Defaults", 296), "RSN", 269)],
["ReadbackTable", 140, (("Production", 11), "RSN", 139), (("Macro", 7), "RSN", 140), (("GrammarType", 3), "RSN", 269), (("Defaults", 296), "RSN", 269)],
["ReadbackTable", 141, (("OpenCurly", 16), "RS", 289), (("RepetitionOption", 49), "RSN", 289), (("OpenSquare", 57), "RS", 289), (("Equals", 17), "RS", 289), (("OpenRound", 35), "RS", 289), (("SemanticActionParameter", 62), "RSN", 289), (("And", 52), "RS", 289), (("Minus", 48), "RS", 289), (("RightArrow", 23), "RS", 289), (("Or", 56), "RS", 289), (("DotDot", 51), "RS", 289)],
["ReadbackTable", 142, (("OpenCurly", 36), "RS", 275), (("OpenRound", 35), "RS", 275), (("Equals", 17), "RS", 275), (("RightArrow", 23), "RS", 275)],
["ReadbackTable", 143, (("OpenRound", 35), "RS", 279), (("OpenCurly", 16), "RS", 279), (("And", 52), "RS", 279), (("Equals", 17), "RS", 279), (("RightArrow", 23), "RS", 279), (("Minus", 48), "RS", 279), (("Or", 56), "RS", 279)],
["ReadbackTable", 144, (("OpenRound", 35), "RS", 283), (("OpenCurly", 16), "RS", 283), (("RepetitionOption", 49), "RSN", 283), (("Equals", 17), "RS", 283), (("RightArrow", 23), "RS", 283), (("And", 52), "RS", 283), (("Or", 56), "RS", 283), (("Minus", 48), "RS", 283)],
["ReadbackTable", 145, (("OpenRound", 35), "RS", 272), (("OpenCurly", 16), "RS", 272), (("RepetitionOption", 49), "RSN", 272), (("Equals", 17), "RS", 272), (("RightArrow", 23), "RS", 272), (("And", 52), "RS", 272), (("Or", 56), "RS", 272), (("Minus", 48), "RS", 272)],
["ReadbackTable", 146, (("OpenCurly", 16), "RS", 289), (("RepetitionOption", 49), "RSN", 289), (("OpenSquare", 57), "RS", 289), (("Equals", 17), "RS", 289), (("OpenRound", 35), "RS", 289), (("SemanticActionParameter", 62), "RSN", 289), (("And", 52), "RS", 289), (("Minus", 48), "RS", 289), (("RightArrow", 23), "RS", 289), (("Or", 56), "RS", 289), (("DotDot", 51), "RS", 289)],
["ReadbackTable", 147, (("OpenCurly", 36), "RS", 270), (("OpenRound", 35), "RS", 270), (("Equals", 17), "RS", 270), (("Minus", 48), "RS", 270), (("RightArrow", 23), "RS", 270)],
["ReadbackTable", 148, (("OpenCurly", 16), "RS", 274)],
["ReadbackTable", 149, (("OpenRound", 35), "RS", 272), (("OpenCurly", 16), "RS", 272), (("RepetitionOption", 49), "RSN", 272), (("Equals", 17), "RS", 272), (("RightArrow", 23), "RS", 272), (("And", 52), "RS", 272), (("Or", 56), "RS", 272), (("Minus", 48), "RS", 272)],
["ReadbackTable", 150, (("OpenRound", 35), "RS", 285), (("OpenCurly", 16), "RS", 285), (("And", 52), "RS", 285), (("Equals", 17), "RS", 285), (("RightArrow", 23), "RS", 285), (("Minus", 48), "RS", 285)],
["ReadbackTable", 151, (("Equals", 17), "RS", 274), (("RepetitionOption", 30), "RSN", 274)],
["ReadbackTable", 152, (("RightPart", 22), "RSN", 152), (("LeftPart", 9), "RSN", 280)],
["ReadbackTable", 153, (("RightArrow", 23), "RS", 197)],
["ReadbackTable", 154, (("OpenRound", 35), "RS", 274), (("RightArrow", 23), "RS", 274)],
["ReadbackTable", 155, (("Name", 15), "RSN", 198)],
["ReadbackTable", 156, (("Minus", 48), "RS", 274), (("OpenCurly", 36), "RS", 274), (("And", 52), "RS", 274), (("Or", 56), "RS", 274)],
["ReadbackTable", 157, (("OpenRound", 35), "RS", 283), (("OpenCurly", 16), "RS", 283), (("RepetitionOption", 49), "RSN", 283), (("Equals", 17), "RS", 283), (("RightArrow", 23), "RS", 283), (("And", 52), "RS", 283), (("Or", 56), "RS", 283), (("Minus", 48), "RS", 283)],
["ReadbackTable", 158, (("OpenRound", 35), "RS", 276), (("OpenCurly", 16), "RS", 276), (("RepetitionOption", 49), "RSN", 276), (("Equals", 17), "RS", 276), (("RightArrow", 23), "RS", 276), (("FatRightArrow", 58), "RS", 276), (("Or", 56), "RS", 276), (("Minus", 48), "RS", 276), (("And", 52), "RS", 276)],
["ReadbackTable", 159, (("Name", 19), "RSN", 199)],
["ReadbackTable", 160, (("RightParts", 21), "RSN", 200)],
["ReadbackTable", 161, (("Name", 24), "RSN", 201)],
["ReadbackTable", 162, (("Name", 26), "RSN", 202)],
["ReadbackTable", 163, (("RepetitionOption", 30), "RSN", 203)],
["ReadbackTable", 164, (("RepetitionOption", 49), "RSN", 274)],
["ReadbackTable", 165, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 166, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 167, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 168, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 169, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 170, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 171, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 172, (("OpenSquare", 50), "RS", 273), (("Attribute", 60), "RSN", 273)],
["ReadbackTable", 173, (("Expression", 34), "RSN", 204)],
["ReadbackTable", 174, (("Expression", 53), "RSN", 205)],
["ReadbackTable", 175, (("Primary", 45), "RSN", 206)],
["ReadbackTable", 176, (("Primary", 45), "RSN", 211)],
["ReadbackTable", 177, (("Primary", 45), "RSN", 216)],
["ReadbackTable", 178, (("OpenSquare", 57), "RS", 281), (("SemanticActionParameter", 62), "RSN", 281)],
["ReadbackTable", 179, (("OpenSquare", 57), "RS", 281), (("SemanticActionParameter", 62), "RSN", 281)],
["ReadbackTable", 180, (("OpenSquare", 57), "RS", 281), (("SemanticActionParameter", 62), "RSN", 281)],
["ReadbackTable", 181, (("Expression", 40), "RSN", 221)],
["ReadbackTable", 182, (("Name", 46), "RSN", 222)],
["ReadbackTable", 183, (("Name", 47), "RSN", 223)],
["ReadbackTable", 184, (("Minus", 48), "RS", 224)],
["ReadbackTable", 185, (("OpenSquare", 50), "RS", 225), (("Attribute", 60), "RSN", 185)],
["ReadbackTable", 186, (("DotDot", 51), "RS", 226)],
["ReadbackTable", 187, (("And", 52), "RS", 227)],
["ReadbackTable", 188, (("Expression", 54), "RSN", 228)],
["ReadbackTable", 189, (("Or", 56), "RS", 229)],
["ReadbackTable", 190, (("OpenSquare", 57), "RS", 230), (("SemanticActionParameter", 62), "RSN", 190)],
["ReadbackTable", 191, (("FatRightArrow", 58), "RS", 231)],
["ReadbackTable", 192, (("FatRightArrow", 58), "RS", 286)],
["ReadbackTable", 193, (("FatRightArrow", 58), "RS", 286)],
["ReadbackTable", 194, (("Plus", 63), "RS", 232), (("FatRightArrow", 58), "RS", 286)],
["ReadbackTable", 195, (("Minus", 64), "RS", 233)],
["ReadbackTable", 196, (("|-", 1), "RS", 288)],
["ReadbackTable", 197, (("LeftPart", 9), "RSN", 287), (("RightPart", 22), "RSN", 287)],
["ReadbackTable", 198, (("output", 5), "RS", 234)],
["ReadbackTable", 199, (("optimize", 8), "RS", 235)],
["ReadbackTable", 200, (("LeftPart", 9), "RSN", 236)],
["ReadbackTable", 201, (("keywords", 10), "RS", 237), (("Name", 24), "RSN", 201)],
["ReadbackTable", 202, (("Name", 26), "RSN", 202), (("defaults", 12), "RS", 238)],
["ReadbackTable", 203, (("OpenRound", 35), "RS", 279), (("OpenCurly", 16), "RS", 279), (("And", 52), "RS", 279), (("Equals", 17), "RS", 279), (("RightArrow", 23), "RS", 279), (("Minus", 48), "RS", 279), (("Or", 56), "RS", 279)],
["ReadbackTable", 204, (("OpenCurly", 16), "RS", 239)],
["ReadbackTable", 205, (("OpenRound", 35), "RS", 240)],
["ReadbackTable", 206, (("OpenRound", 35), "RS", 274), (("RightArrow", 23), "RS", 274)],
["ReadbackTable", 207, (("OpenCurly", 36), "RS", 274), (("Minus", 48), "RS", 274), (("And", 52), "RS", 274), (("Or", 56), "RS", 274)],
["ReadbackTable", 208, (("RepetitionOption", 49), "RSN", 274)],
["ReadbackTable", 209, (("OpenCurly", 16), "RS", 274)],
["ReadbackTable", 210, (("RepetitionOption", 30), "RSN", 274), (("Equals", 17), "RS", 274)],
["ReadbackTable", 211, (("RightArrow", 23), "RS", 274), (("OpenRound", 35), "RS", 274)],
["ReadbackTable", 212, (("Minus", 48), "RS", 274), (("OpenCurly", 36), "RS", 274), (("And", 52), "RS", 274), (("Or", 56), "RS", 274)],
["ReadbackTable", 213, (("RepetitionOption", 49), "RSN", 274)],
["ReadbackTable", 214, (("OpenCurly", 16), "RS", 274)],
["ReadbackTable", 215, (("Equals", 17), "RS", 274), (("RepetitionOption", 30), "RSN", 274)],
["ReadbackTable", 216, (("OpenRound", 35), "RS", 274), (("RightArrow", 23), "RS", 274)],
["ReadbackTable", 217, (("Minus", 48), "RS", 274), (("OpenCurly", 36), "RS", 274), (("And", 52), "RS", 274), (("Or", 56), "RS", 274)],
["ReadbackTable", 218, (("RepetitionOption", 49), "RSN", 274)],
["ReadbackTable", 219, (("OpenCurly", 16), "RS", 274)],
["ReadbackTable", 220, (("RepetitionOption", 30), "RSN", 274), (("Equals", 17), "RS", 274)],
["ReadbackTable", 221, (("Equals", 17), "RS", 241)],
["ReadbackTable", 222, (("Name", 46), "RSN", 222), (("defaults", 27), "RS", 242)],
["ReadbackTable", 223, (("defaults", 28), "RS", 243), (("Name", 47), "RSN", 223)],
["ReadbackTable", 224, (("AndExpression", 29), "RSN", 244)],
["ReadbackTable", 225, (("Secondary", 31), "RSN", 245)],
["ReadbackTable", 226, (("Byte", 32), "RSN", 246)],
["ReadbackTable", 227, (("Alternation", 33), "RSN", 247)],
["ReadbackTable", 228, (("OpenCurly", 36), "RS", 248)],
["ReadbackTable", 229, (("Concatenation", 38), "RSN", 249)],
["ReadbackTable", 230, (("walkSymbol", 39), "RSN", 250)],
["ReadbackTable", 231, (("Expression", 44), "RSN", 251)],
["ReadbackTable", 232, (("FatRightArrow", 58), "RS", 286)],
["ReadbackTable", 233, (("FatRightArrow", 58), "RS", 286)],
["ReadbackTable", 234, (("GrammarType", 3), "RSN", 277), (("Defaults", 296), "RSN", 277)],
["ReadbackTable", 235, (("GrammarType", 3), "RSN", 277), (("Defaults", 296), "RSN", 277)],
["ReadbackTable", 236, (("Production", 11), "RSN", 284), (("Macro", 7), "RSN", 284), (("Defaults", 296), "RSN", 284), (("GrammarType", 3), "RSN", 284)],
["ReadbackTable", 237, (("GrammarType", 3), "RSN", 277), (("Defaults", 296), "RSN", 277)],
["ReadbackTable", 238, (("attribute", 4), "RS", 252)],
["ReadbackTable", 239, (("Name", 43), "RSN", 254)],
["ReadbackTable", 240, (("OpenRound", 35), "RS", 272), (("OpenCurly", 16), "RS", 272), (("RepetitionOption", 49), "RSN", 272), (("Equals", 17), "RS", 272), (("RightArrow", 23), "RS", 272), (("And", 52), "RS", 272), (("Or", 56), "RS", 272), (("Minus", 48), "RS", 272)],
["ReadbackTable", 241, (("Name", 43), "RSN", 258)],
["ReadbackTable", 242, (("terminal", 13), "RS", 262)],
["ReadbackTable", 243, (("nonterminal", 14), "RS", 263)],
["ReadbackTable", 244, (("OpenCurly", 36), "RS", 275), (("OpenRound", 35), "RS", 275), (("Equals", 17), "RS", 275), (("RightArrow", 23), "RS", 275)],
["ReadbackTable", 245, (("OpenRound", 35), "RS", 283), (("OpenCurly", 16), "RS", 283), (("RepetitionOption", 49), "RSN", 283), (("Equals", 17), "RS", 283), (("RightArrow", 23), "RS", 283), (("And", 52), "RS", 283), (("Or", 56), "RS", 283), (("Minus", 48), "RS", 283)],
["ReadbackTable", 246, (("OpenRound", 35), "RS", 272), (("OpenCurly", 16), "RS", 272), (("RepetitionOption", 49), "RSN", 272), (("Equals", 17), "RS", 272), (("RightArrow", 23), "RS", 272), (("And", 52), "RS", 272), (("Or", 56), "RS", 272), (("Minus", 48), "RS", 272)],
["ReadbackTable", 247, (("OpenCurly", 36), "RS", 270), (("OpenRound", 35), "RS", 270), (("Equals", 17), "RS", 270), (("Minus", 48), "RS", 270), (("RightArrow", 23), "RS", 270)],
["ReadbackTable", 248, (("OpenRound", 35), "RS", 272), (("OpenCurly", 16), "RS", 272), (("RepetitionOption", 49), "RSN", 272), (("Equals", 17), "RS", 272), (("RightArrow", 23), "RS", 272), (("And", 52), "RS", 272), (("Or", 56), "RS", 272), (("Minus", 48), "RS", 272)],
["ReadbackTable", 249, (("OpenRound", 35), "RS", 285), (("OpenCurly", 16), "RS", 285), (("And", 52), "RS", 285), (("Equals", 17), "RS", 285), (("RightArrow", 23), "RS", 285), (("Minus", 48), "RS", 285)],
["ReadbackTable", 250, (("OpenRound", 35), "RS", 276), (("OpenCurly", 16), "RS", 276), (("RepetitionOption", 49), "RSN", 276), (("Equals", 17), "RS", 276), (("RightArrow", 23), "RS", 276), (("FatRightArrow", 58), "RS", 276), (("Or", 56), "RS", 276), (("Minus", 48), "RS", 276), (("And", 52), "RS", 276)],
["ReadbackTable", 251, (("RightArrow", 23), "RS", 264)],
["ReadbackTable", 252, (("GrammarType", 3), "RSN", 277)],
["ReadbackTable", 253, (("Defaults", 296), "RSN", 277)],
["ReadbackTable", 254, (("Defaults", 296), "RSN", 282)],
["ReadbackTable", 255, (("GrammarType", 3), "RSN", 282)],
["ReadbackTable", 256, (("Macro", 7), "RSN", 282)],
["ReadbackTable", 257, (("Production", 11), "RSN", 282)],
["ReadbackTable", 258, (("Defaults", 296), "RSN", 271)],
["ReadbackTable", 259, (("GrammarType", 3), "RSN", 271)],
["ReadbackTable", 260, (("Macro", 7), "RSN", 271)],
["ReadbackTable", 261, (("Production", 11), "RSN", 271)],
["ReadbackTable", 262, (("attribute", 42), "RS", 265)],
["ReadbackTable", 263, (("attribute", 42), "RS", 267)],
["ReadbackTable", 264, (("LeftPart", 9), "RSN", 287), (("RightPart", 22), "RSN", 287)],
["ReadbackTable", 265, (("Defaults", 296), "RSN", 277)],
["ReadbackTable", 266, (("GrammarType", 3), "RSN", 277)],
["ReadbackTable", 267, (("Defaults", 296), "RSN", 277)],
["ReadbackTable", 268, (("GrammarType", 3), "RSN", 277)],
["ReduceTable", 269, "Rules", (3, "RSN", 326), (326, "RSN", 326), (296, "RSN", 326)],
["ReduceTable", 270, "AndExpression", (16, "RSN", 29), (17, "RSN", 29), (23, "RSN", 29), (35, "RSN", 29), (36, "RSN", 29), (48, "RSN", 314), (314, "RSN", 314)],
["ReduceTable", 271, "Macro", (3, "RSN", 7), (7, "RSN", 7), (11, "RSN", 7), (296, "RSN", 7)],
["ReduceTable", 272, "Secondary", (16, "RSN", 31), (17, "RSN", 31), (23, "RSN", 31), (30, "RSN", 31), (35, "RSN", 31), (36, "RSN", 31), (48, "RSN", 31), (49, "RSN", 31), (52, "RSN", 31), (56, "RSN", 31)],
["ReduceTable", 273, "Attribute", (50, "RSN", 60), (60, "RSN", 60)],
["ReduceTable", 274, "RepetitionOption", (30, "RSN", 49), (49, "RSN", 49), (16, "RSN", 30), (17, "RSN", 30), (23, "RSN", 30), (35, "RSN", 30), (36, "RSN", 30), (48, "RSN", 30), (52, "RSN", 30), (56, "RSN", 30)],
["ReduceTable", 275, "Expression", (35, "RSN", 53), (36, "RSN", 54), (23, "RSN", 44), (17, "RSN", 40), (16, "RSN", 34)],
["ReduceTable", 276, "SemanticAction", (16, "RSN", 300), (17, "RSN", 300), (23, "RSN", 300), (30, "RSN", 300), (35, "RSN", 300), (36, "RSN", 300), (48, "RSN", 300), (49, "RSN", 300), (52, "RSN", 300), (56, "RSN", 300), (300, "RSN", 300), (58, "RSN", 322), (322, "RSN", 322)],
["ReduceTable", 277, "Defaults", (3, "RSN", 296), (296, "RSN", 296)],
["ReduceTable", 278, "Name", (7, "RSN", 18), (296, "RSN", 43), (57, "RSN", 111), (62, "RSN", 111), (111, "RSN", 111), (11, "RSN", 25), (3, "RSN", 6), (27, "RSN", 46), (46, "RSN", 46), (16, "RSN", 82), (17, "RSN", 82), (23, "RSN", 82), (30, "RSN", 82), (35, "RSN", 82), (36, "RSN", 82), (48, "RSN", 82), (49, "RSN", 82), (52, "RSN", 82), (56, "RSN", 82), (82, "RSN", 82), (58, "RSN", 323), (323, "RSN", 323), (10, "RSN", 24), (24, "RSN", 24), (5, "RSN", 15), (28, "RSN", 47), (47, "RSN", 47), (8, "RSN", 19), (12, "RSN", 26), (26, "RSN", 26)],
["ReduceTable", 279, "Concatenation", (56, "RSN", 61), (16, "RSN", 38), (17, "RSN", 38), (23, "RSN", 38), (35, "RSN", 38), (36, "RSN", 38), (48, "RSN", 38), (52, "RSN", 38)],
["ReduceTable", 280, "RightParts", (9, "RSN", 21)],
["ReduceTable", 281, "SemanticActionParameter", (57, "RSN", 62), (62, "RSN", 62)],
["ReduceTable", 282, "LeftPart", (3, "RSN", 9), (7, "RSN", 9), (11, "RSN", 9), (296, "RSN", 9)],
["ReduceTable", 283, "Primary", (23, "RSN", 45), (35, "RSN", 45), (17, "RSN", 41), (30, "RSN", 41), (36, "RSN", 55), (48, "RSN", 55), (52, "RSN", 55), (56, "RSN", 55), (49, "RSN", 59), (16, "RSN", 37)],
["ReduceTable", 284, "Production", (3, "RSN", 11), (7, "RSN", 11), (11, "RSN", 11), (296, "RSN", 11)],
["ReduceTable", 285, "Alternation", (16, "RSN", 33), (17, "RSN", 33), (23, "RSN", 33), (35, "RSN", 33), (36, "RSN", 33), (48, "RSN", 33), (52, "RSN", 317), (317, "RSN", 317)],
["ReduceTable", 286, "TreeBuildingOptions", (58, "RSN", 321), (321, "RSN", 321)],
["ReduceTable", 287, "RightPart", (9, "RSN", 22), (22, "RSN", 22)],
["ReduceTable", 288, "GrammarType", (1, "RSN", 3)],
["ReduceTable", 289, "Byte", (16, "RSN", 32), (17, "RSN", 32), (23, "RSN", 32), (30, "RSN", 32), (35, "RSN", 32), (36, "RSN", 32), (48, "RSN", 32), (49, "RSN", 32), (52, "RSN", 32), (56, "RSN", 32), (51, "RSN", 316), (316, "RSN", 316), (57, "RSN", 113), (62, "RSN", 113), (113, "RSN", 113)],
["SemanticTable", 290, "processTypeNow", ["scanner"], 65],
["SemanticTable", 291, "processTypeNow", ["superScanner"], 66],
["SemanticTable", 292, "processTypeNow", ["parser"], 69],
["SemanticTable", 293, "processTypeNow", ["superScanner"], 70],
["SemanticTable", 294, "buildTree", ["walkLeftPart"], 71],
["SemanticTable", 295, "buildTree", ["walkGrammar"], 72],
["SemanticTable", 296, "processAndDiscardDefaultsNow", [], 20],
["SemanticTable", 297, "buildTree", ["walkEpsilon"], 77],
["SemanticTable", 298, "buildTree", ["walkOr"], 85],
["SemanticTable", 299, "buildTree", ["walkOutput"], 88],
["SemanticTable", 300, "buildTree", ["walkNonTreeBuildingSemanticAction"], 90],
["SemanticTable", 301, "buildTree", ["walkSemanticAction"], 91],
["SemanticTable", 302, "buildTree", ["walkOptimize"], 92],
["SemanticTable", 303, "buildTree", ["walkProduction"], 93],
["SemanticTable", 304, "buildTree", ["walkKeywords"], 94],
["SemanticTable", 305, "buildTree", ["walkAttributeDefaults"], 95],
["SemanticTable", 306, "buildTree", ["walkConcatenation"], 96],
["SemanticTable", 307, "buildTree", ["walkLeftPartWithLookahead"], 106],
["SemanticTable", 308, "buildTree", ["walkPlus"], 108],
["SemanticTable", 309, "buildTree", ["walkQuestionMark"], 109],
["SemanticTable", 310, "buildTree", ["walkStar"], 110],
["SemanticTable", 311, "buildTree", ["walkMacro"], 114],
["SemanticTable", 312, "buildTree", ["walkAttributeTerminalDefaults"], 115],
["SemanticTable", 313, "buildTree", ["walkAttributeNonterminalDefaults"], 116],
["SemanticTable", 314, "buildTree", ["walkMinus"], 117],
["SemanticTable", 315, "buildTree", ["walkAttributes"], 118],
["SemanticTable", 316, "buildTree", ["walkDotDot"], 119],
["SemanticTable", 317, "buildTree", ["walkAnd"], 120],
["SemanticTable", 318, "buildTree", ["walkLook"], 121],
["SemanticTable", 319, "buildTree", ["walkOr"], 122],
["SemanticTable", 320, "buildTree", ["walkSemanticAction"], 123],
["SemanticTable", 321, "buildTree", ["walkConcatenation"], 124],
["SemanticTable", 322, "buildTree", ["walkTreeBuildingSemanticAction"], 125],
["SemanticTable", 323, "buildTree", ["walkBuildTreeOrTokenFromName"], 126],
["SemanticTable", 324, "buildTree", ["walkBuildTreeFromLeftIndex"], 127],
["SemanticTable", 325, "buildTree", ["walkBuildTreeFromRightIndex"], 128],
["AcceptTable", 326]]

    var parrserTables: [Any] =
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
