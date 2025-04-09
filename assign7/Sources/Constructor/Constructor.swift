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

        if Grammar.activeGrammar!.isParser() {
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

        mergeStates()

        replaceSemanticTransitions()

        finalizeReduceTables()

        if Grammar.activeGrammar!.isParser() {
            // remove initial readahead state from parsers (left goalpost transition)
            readaheadStates.remove(at: 0)
        }

        optimize()

        renumber()

        if Grammar.activeGrammar!.isParser() {
            outputParserTables()
        } else {
            outputScannerTables()
        }
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

            localDown.do {
                up.add(Pair($2, raState), and: $1, and: Pair($0, raState))
            }

            raState.items.append(contentsOf: raState.initialItems)
            raState.items.append(contentsOf: localDown.allTo())

            right.from(raState.items) { M, localRight in
                let candidate = ReadaheadState(localRight.allTo())
                var successor = readaheadStates.first { state in
                    state.initialItems.allSatisfy { candidate.initialItems.contains($0) }
                }
                if successor == nil {
                    readaheadStates.append(candidate)
                    successor = candidate
                }
                raState.addTransition(
                    Transition(label: Label(label: M), goto: successor!))
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
        // mergeStates()
        // renumber()
        while i < readaheadStates.count {
            let raState = readaheadStates[i]
            let finalItems = raState.items.filter { $0.isFinal }
            let partition = finalItems.partitionUsing { $0.leftPart }

            for (key, value) in partition {
                var new_state: FiniteStateMachineState = FiniteStateMachineState()

                if Grammar.activeGrammar!.isScanner() {
                    new_state = readaheadStates[0]
                } else if Grammar.activeGrammar!.isGoal(key) {
                    new_state = acceptState
                } else {
                    new_state = ReadbackState(
                        initialItems: finalItems.map {
                            Pair($0, raState)
                        })

                    new_state.isInitial = true

                    readbackStates.append(new_state as! ReadbackState)
                }

                Grammar.activeGrammar!.productionFor(key).followSet.do {
                    raState.addTransition(
                        Transition(
                            label: Label(name: $0).asLook(),  // this doesn't work for scanner (printablity is lost)
                            goto: new_state
                        ))
                }

                Grammar.activeGrammar!.productionFor(key).unprintableFollowSet.do {
                    raState.addTransition(
                        Transition(
                            label: Label(name: $0, printable: false).asLook(),
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

            visible_left.from(rbState.items) { Mp, local_left in
                let candidate = ReadbackState(initialItems: local_left.allTo())
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

    func lookbackFor(_ items: [Pair]) -> Set<Pair> {
        var result = up.performOnce(items)

        var i = 0

        while i < result.count {
            let pair = result[i]
            let up_items = up.performStar([pair])
            let left_items = invisible_left.performStar([pair])

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
        readaheadStates.do { raState in
            for sem_transition in raState.transitions where sem_transition.hasAction() {
                buildSemanticState(raState, transition: sem_transition)
            }
            raState.transitions.removeAll { $0.hasAction() }
        }

        // var dead_states: [SemanticState] = []

        // semanticStates.do { state in
        //     if !dead_states.contains(state) {
        //         let matched_states = semanticStates.filter {
        //             $0.label == state.label && $0.goto == state.goto && $0 != state
        //         }
        //         dead_states.append(contentsOf: matched_states)
        //         matched_states.do {
        //             replaceState($0, with: state)
        //         }
        //     }
        // }

        // semanticStates.removeAll { dead_states.contains($0) }

    }

    func buildSemanticState(_ state: ReadaheadState, transition: Transition) {
        // get follow set for goto
        let follow: Pair = Grammar.activeGrammar!.getFollow(transition.goto as! ReadaheadState)

        // build semantic state based on transition
        var new_state = SemanticState(transition.label, goto: transition.goto)
        let id_state = semanticStates.first(where: {
            $0.label == new_state.label && $0.goto == new_state.goto
        })

        if id_state != nil {
            new_state = id_state!
        } else {
            semanticStates.append(new_state)
        }

        // add follow to transitions from state to new semantic state
        (follow.first() as! [String]).do { name in
            if state.transitions.first(where: { $0.label.terseDescription == name }) == nil {
                state.addTransition(Transition(label: Label(name: name).asLook(), goto: new_state))
            }
        }

        (follow.second() as! [String]).do { name in
            if state.transitions.first(where: { $0.label.terseDescription == name }) == nil {
                state.addTransition(
                    Transition(label: Label(name: name, printable: false).asLook(), goto: new_state)
                )
            }
        }
    }

    func finalizeReduceTables() {

        readaheadStates.do { raState in
            raState.transitionsDo { transition in
                if Grammar.activeGrammar!.isNonterminal(transition.label.name!) {
                    if reduceStates[transition.label.name!]!.restarts[transition.goto] == nil {
                        reduceStates[transition.label.name!]!.restarts[transition.goto] = []
                    }

                    reduceStates[transition.label.name!]!.restarts[transition.goto]!.append(raState)
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
                    stackable_states.append(transition.goto)
                }
            }
        }

        // build invisible_left for readahead states
        let ra_invisible_left = Relation<FiniteStateMachineState, Label>()
        readaheadStates.do { raState in
            raState.transitionsDo {
                if $0.label.isInvisible() {
                    ra_invisible_left.add($0.goto, and: $0.label, and: raState)
                }
            }
        }

        semanticStates.do {
            ra_invisible_left.add($0.goto, and: $0.label, and: $0)
        }

        for rdState in reduceStates {
            for (key, value) in rdState.value.restarts {

                var i = 0

                while i < value.count {
                    let state = value[i]
                    i += 1

                    let alt_restarts = ra_invisible_left.performStar([state]).filter {
                        stackable_states.contains($0)
                    }

                    if state as? SemanticState != nil {
                        print(alt_restarts)
                    }

                    rdState.value.restarts[key]!.removeAll { $0 == state }

                    rdState.value.restarts[key]!.appendIfAbsent(alt_restarts)
                }
            }
        }
    }

    func optimize() {
        eliminateStates()
        readbackToShift()
    }

    func conflictCheck(){
        for rdState in reduceStates{
            for (key,value) in rdState.value.restarts {
                if(value.count > 1){
                    print("REDUCE ALTERNATE CONFLICT IN \(rdState.key)")
                }
            }
        }
    }

    func mergeStates() {
        var dead_states = mergeStateArray(readaheadStates)
        readaheadStates.removeAll { dead_states.contains($0) }
        dead_states = mergeStateArray(readbackStates)
        readbackStates.removeAll { dead_states.contains($0) }
    }

    private func mergeStateArray(_ states: [FiniteStateMachineState]) -> [FiniteStateMachineState] {
        var dead_states: [FiniteStateMachineState] = []
        states.do { state1 in
            if !dead_states.contains(state1) {
                let matched_states = states.filter { state2 in
                    state2.transitions.allSatisfy { transition in
                        state1.transitions.contains(where: {
                            $0.label == transition.label && $0.goto == transition.goto
                        })
                    }
                        && state1.transitions.allSatisfy { transition in
                            state2.transitions.contains(where: {
                                $0.label == transition.label && $0.goto == transition.goto
                            })
                        } && state1 != state2
                }
                dead_states.append(contentsOf: matched_states)
                matched_states.do { replaceState($0, with: state1) }
            }
        }

        return dead_states
    }

    func eliminateStates() {
        var dead_states: [FiniteStateMachineState] = []

        readaheadStates.do { raState in
            if (raState.transitions.allSatisfy {
                $0.goto == raState.transitions[0].goto && !$0.label.isVisible()
            }) && !raState.transitions.isEmpty {
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
            if rbState.transitions.partitionUsing { $0.goto }.count == 1
                && rbState.transitions[0].goto != rbState
            {
                let new_state = ShiftState(1, rbState.transitions[0].goto)
                shiftStates.append(new_state)

                replaceState(
                    rbState, with: new_state)
                dead_states.append(rbState)
            }
        }

        readbackStates.removeAll { dead_states.contains($0) }

        dead_states.removeAll()

        shiftStates.do { state in
            if !dead_states.contains(state) {
                shiftStates.filter {
                    $0 != state && $0.shiftVal == state.shiftVal && $0.goto == state.goto
                }.do {
                    replaceState($0, with: state)
                    dead_states.append($0)
                }
            }
        }

        shiftStates.removeAll { dead_states.contains($0) }

        // merge consecutive shift states
        mergeShifts()
    }

    func mergeShifts() {
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

                if key == old {
                    new_key = new_state
                }

                var new_value = value
                new_value.removeAll { $0 == old }
                if new_value.count < value.count || new_key != key {
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
        var tables: String = ""

        print("\n\n\n~~~~~~~~~~~~~~ BEGIN SCANNER TABLES ~~~~~~~~~~~~~~\n\n\n")

        tables.append("[")
        readaheadStates.do { raState in
            tables.append(
                "[\"ScannerReadaheadTable\", \(raState.stateNumber), "
            )

            let printable_transitions: [Transition]? = raState.transitions.partitionUsing {
                $0.label.isPrintable
            }[true]
            let unprintable_transition: [Transition]? = raState.transitions.partitionUsing {
                $0.label.isPrintable
            }[false]

            if printable_transitions != nil {
                for (attribute, attribute_val) in printable_transitions!.partitionUsing(separator: {
                    $0.label.attributes
                }) {
                    for (goto, goto_val) in attribute_val.partitionUsing { $0.goto.stateNumber } {
                        var label_map = goto_val.map {
                            ($0.label.terseDescription == "\"" || $0.label.terseDescription == "\\")
                                ? "\\\($0.label.terseDescription)" : $0.label.terseDescription
                        }
                        label_map.sort()
                        tables.append(
                            "(\"\(label_map.joined())\", \"\(attribute)\", \(goto)), "
                        )
                    }
                }
            }

            if unprintable_transition != nil {
                for (attribute, attribute_val) in unprintable_transition!.partitionUsing(
                    separator: {
                        $0.label.attributes
                    })
                {
                    for (goto, goto_val) in attribute_val.partitionUsing { $0.goto.stateNumber } {
                        var label_map = goto_val.map { $0.label.terseDescription }
                        label_map.sort()
                        tables.append(
                            "([\(label_map.joined(separator: ", "))], \"\(attribute)\", \(goto)), "
                        )
                    }
                }
            }

            tables.append("],\n")
        }

        semanticStates.do { smState in
            tables.append(
                "[\"SemanticTable\", \(smState.stateNumber), \"\(smState.label.action)\", [\(smState.label.parameters.map{"\"\($0)\""}.joined(separator: ", "))], \(smState.goto.stateNumber)],\n"
            )
        }
        tables.append("]")

        do {
            try tables.write(
                to: URL(fileURLWithPath: "Sources/Constructor/bootstrapScannerTables.txt"),
                atomically: true, encoding: String.Encoding.utf8)
        } catch {
            print("scanner table write failed")
            // failed to write file – bad permissions, bad filename, missing permissions, or more likely it can't be converted to the encoding
        }
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

        let lookahead_fsm = walkTree((tree as! Tree).child(1)) as! FiniteStateMachine

        return_val.lookahead = lookahead_fsm.printableTransitionNames()

        return_val.unprintableLookahed = lookahead_fsm.unprintableTransitionNames()

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

        if symbol.last! == ":" {
            symbol.removeLast()
        }

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
        if symbolOnly { return Character((tree as! Token).symbol) }
        return FiniteStateMachine.forCharacter(Character((tree as! Token).symbol))
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
        for i in 1..<(tree as! Tree).children.count {
            return_val = infix(
                return_val, (walkTree((tree as! Tree).child(i)) as! FiniteStateMachine))
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

var scannerTables: [Any] = [["ScannerReadaheadTable", 1, ("/", "R", 7), ("#", "R", 11), ("'", "R", 12), ("\"", "R", 5), ("$", "R", 6), ("}", "RK", 27), (")", "RK", 26), ("{", "RK", 37), ("[", "RK", 32), ("]", "RK", 33), ("|", "RK", 31), ("=", "RK", 8), ("ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 2), ("*", "RK", 28), ("+", "RK", 35), ("-", "RK", 9), (".", "RK", 10), ("?", "RK", 25), ("(", "RK", 24), ("0123456789", "RK", 4), ("&", "RK", 36), ([10, 12, 13, 32, 9], "R", 3), ([256], "LK", 21), ],
["ScannerReadaheadTable", 2, ("0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 2), ("!#$%&'()*+,-./;<=>?@[\"\\]^{|}~", "LK", 22), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 22), ],
["ScannerReadaheadTable", 3, ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 1), ([10, 12, 13, 32, 9], "R", 3), ([147, 148, 256, 96], "LK", 1), ],
["ScannerReadaheadTable", 4, ("0123456789", "RK", 4), ("!#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 23), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 23), ],
["ScannerReadaheadTable", 5, ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "RK", 5), ("\"", "R", 13), ([10, 12, 13, 147, 148, 32, 9, 96], "RK", 5), ([256], "LK", 39), ],
["ScannerReadaheadTable", 6, ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "RK", 40), ([10, 12, 13, 147, 148, 32, 9, 96], "RK", 40), ],
["ScannerReadaheadTable", 7, ("/", "R", 14), ("!#$%&'()*+,-.0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 41), ([10, 12, 13, 32, 9], "L", 41), ([147, 148, 256, 96], "LK", 41), ],
["ScannerReadaheadTable", 8, (">", "RK", 42), ("!#$%&'()*+,-./0123456789:;<=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 29), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 29), ],
["ScannerReadaheadTable", 9, (">", "RK", 43), ("!#$%&'()*+,-./0123456789:;<=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 30), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 30), ],
["ScannerReadaheadTable", 10, (".", "RK", 44), ("!#$%&'()*+,-/0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 34), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 34), ],
["ScannerReadaheadTable", 11, ("ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 15), ("'", "R", 17), ("\"", "R", 16), ],
["ScannerReadaheadTable", 12, ("!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "RK", 12), ("'", "R", 18), ([10, 12, 13, 147, 148, 32, 9, 96], "RK", 12), ([256], "LK", 46), ],
["ScannerReadaheadTable", 13, ("\"", "RK", 5), ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 38), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 38), ],
["ScannerReadaheadTable", 14, ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "R", 14), ([10, 12, 13], "R", 1), ([147, 148, 32, 9, 96], "R", 14), ([256], "LK", 1), ],
["ScannerReadaheadTable", 15, ("0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 15), ("!#$%&'()*+,-./;<=>?@[\"\\]^{|}~", "LK", 45), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 45), ],
["ScannerReadaheadTable", 16, ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "RK", 16), ("\"", "R", 19), ([10, 12, 13, 147, 148, 32, 9, 96], "RK", 16), ([256], "LK", 49), ],
["ScannerReadaheadTable", 17, ("!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "RK", 17), ("'", "R", 20), ([10, 12, 13, 147, 148, 32, 9, 96], "RK", 17), ([256], "LK", 50), ],
["ScannerReadaheadTable", 18, ("'", "RK", 12), ("!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 47), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 47), ],
["ScannerReadaheadTable", 19, ("\"", "RK", 16), ("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 48), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 48), ],
["ScannerReadaheadTable", 20, ("'", "RK", 17), ("!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\"\\]^_abcdefghijklmnopqrstuvwxyz{|}~", "LK", 51), ([10, 12, 13, 147, 148, 256, 32, 96], "LK", 51), ],
["SemanticTable", 21, "buildToken", ["-|"], 1],
["SemanticTable", 22, "buildToken", ["walkIdentifier"], 1],
["SemanticTable", 23, "buildToken", ["walkInteger"], 1],
["SemanticTable", 24, "buildToken", ["OpenRound"], 1],
["SemanticTable", 25, "buildToken", ["QuestionMark"], 1],
["SemanticTable", 26, "buildToken", ["CloseRound"], 1],
["SemanticTable", 27, "buildToken", ["CloseCurly"], 1],
["SemanticTable", 28, "buildToken", ["Star"], 1],
["SemanticTable", 29, "buildToken", ["Equals"], 1],
["SemanticTable", 30, "buildToken", ["Minus"], 1],
["SemanticTable", 31, "buildToken", ["Or"], 1],
["SemanticTable", 32, "buildToken", ["OpenSquare"], 1],
["SemanticTable", 33, "buildToken", ["CloseSquare"], 1],
["SemanticTable", 34, "buildToken", ["Dot"], 1],
["SemanticTable", 35, "buildToken", ["Plus"], 1],
["SemanticTable", 36, "buildToken", ["And"], 1],
["SemanticTable", 37, "buildToken", ["OpenCurly"], 1],
["SemanticTable", 38, "buildToken", ["walkString"], 1],
["SemanticTable", 39, "syntaxError", ["missing end quote for double quoted string"], 38],
["SemanticTable", 40, "buildToken", ["walkCharacter"], 1],
["SemanticTable", 41, "syntaxError", ["// is a comment, / alone is not valid"], 1],
["SemanticTable", 42, "buildToken", ["FatRightArrow"], 1],
["SemanticTable", 43, "buildToken", ["RightArrow"], 1],
["SemanticTable", 44, "buildToken", ["DotDot"], 1],
["SemanticTable", 45, "buildToken", ["walkSymbol"], 1],
["SemanticTable", 46, "syntaxError", ["missing end quote for single quoted string"], 47],
["SemanticTable", 47, "buildToken", ["walkString"], 1],
["SemanticTable", 48, "buildToken", ["walkSymbol"], 1],
["SemanticTable", 49, "syntaxError", ["missing end quote for double quoted string"], 48],
["SemanticTable", 50, "syntaxError", ["missing end quote for single quoted string"], 51],
["SemanticTable", 51, "buildToken", ["walkSymbol"], 1],
]

var parserTables: [Any] = [["keywords","stack", "noStack", "read", "look", "node", "noNode", "keep", "noKeep", "parser", "scanner", "super", "superScanner", "attribute", "defaults", "keywords", "output", "optimize", "terminal", "nonterminal"],
["ReadaheadTable", 1, ("GrammarType", "RSN", 2), ("superScanner", "RS", 143), ("super", "RS", 3), ("parser", "RS", 144), ("scanner", "RS", 145)],
["ReadaheadTable", 2, ("optimize", "RS", 4), ("Defaults", "RSN", 146), ("attribute", "RS", 5), ("Production", "RSN", 6), ("output", "RS", 7), ("keywords", "RS", 8), ("walkString", "RSN", 63), ("Macro", "RSN", 6), ("Name", "RSN", 9), ("Rules", "RSN", 179), ("walkIdentifier", "RSN", 63), ("LeftPart", "RSN", 10)],
["ReadaheadTable", 3, ("scanner", "RS", 149)],
["ReadaheadTable", 4, ("Name", "RSN", 11), ("walkString", "RSN", 63), ("walkIdentifier", "RSN", 63)],
["ReadaheadTable", 5, ("defaults", "RS", 12), ("nonterminal", "RS", 13), ("terminal", "RS", 14)],
["ReadaheadTable", 6, ("Production", "RSN", 6), ("walkString", "RSN", 63), ("Macro", "RSN", 6), ("walkIdentifier", "RSN", 63), ("Name", "RSN", 9), ("LeftPart", "RSN", 10), ("-|", "L", 147)],
["ReadaheadTable", 7, ("Name", "RSN", 15), ("walkString", "RSN", 63), ("walkIdentifier", "RSN", 63)],
["ReadaheadTable", 8, ("walkIdentifier", "RSN", 63), ("walkString", "RSN", 63), ("Name", "RSN", 16)],
["ReadaheadTable", 9, ("Equals", "RS", 17), ("OpenCurly", "RS", 18), ("RightArrow", "L", 148)],
["ReadaheadTable", 10, ("RightArrow", "RS", 19), ("RightParts", "RSN", 20), ("RightPart", "RSN", 21)],
["ReadaheadTable", 11, ("Dot", "RS", 152)],
["ReadaheadTable", 12, ("Name", "RSN", 22), ("walkIdentifier", "RSN", 63), ("walkString", "RSN", 63)],
["ReadaheadTable", 13, ("defaults", "RS", 23)],
["ReadaheadTable", 14, ("defaults", "RS", 24)],
["ReadaheadTable", 15, ("Dot", "RS", 153)],
["ReadaheadTable", 16, ("walkIdentifier", "RSN", 63), ("Dot", "RS", 154), ("walkString", "RSN", 63), ("Name", "RSN", 16)],
["ReadaheadTable", 17, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("AndExpression", "RSN", 28), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 32), ("Expression", "RSN", 33), ("SemanticAction", "RSN", 156), ("OpenRound", "RS", 34), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 18, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("AndExpression", "RSN", 28), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 32), ("Expression", "RSN", 36), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 19, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("AndExpression", "RSN", 28), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 32), ("Expression", "RSN", 37), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 20, ("Dot", "RS", 157)],
["ReadaheadTable", 21, ("RightArrow", "RS", 19), ("RightPart", "RSN", 21), ("Dot", "L", 151)],
["ReadaheadTable", 22, ("walkString", "RSN", 63), ("walkIdentifier", "RSN", 63), ("Dot", "RS", 158), ("Name", "RSN", 22)],
["ReadaheadTable", 23, ("Name", "RSN", 38), ("walkIdentifier", "RSN", 63), ("walkString", "RSN", 63)],
["ReadaheadTable", 24, ("Name", "RSN", 39), ("walkIdentifier", "RSN", 63), ("walkString", "RSN", 63)],
["ReadaheadTable", 25, ("DotDot", "RS", 40), ("OpenRound", "L", 67), ("OpenCurly", "L", 67), ("walkIdentifier", "L", 67), ("walkString", "L", 67), ("walkCharacter", "L", 67), ("walkInteger", "L", 67), ("walkSymbol", "L", 67), ("Star", "L", 67), ("QuestionMark", "L", 67), ("Plus", "L", 67), ("OpenSquare", "L", 67), ("Or", "L", 67), ("CloseCurly", "L", 67), ("Minus", "L", 67), ("And", "L", 67), ("Dot", "L", 67), ("RightArrow", "L", 67), ("FatRightArrow", "L", 67), ("CloseRound", "L", 67)],
["ReadaheadTable", 26, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("AndExpression", "RSN", 28), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 32), ("Expression", "RSN", 41), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 27, ("OpenSquare", "RS", 42), ("OpenRound", "L", 155), ("OpenCurly", "L", 155), ("walkIdentifier", "L", 155), ("walkString", "L", 155), ("walkCharacter", "L", 155), ("walkInteger", "L", 155), ("walkSymbol", "L", 155), ("Star", "L", 155), ("QuestionMark", "L", 155), ("Plus", "L", 155), ("Or", "L", 155), ("RightArrow", "L", 155), ("CloseCurly", "L", 155), ("Minus", "L", 155), ("And", "L", 155), ("Dot", "L", 155), ("FatRightArrow", "L", 155), ("CloseRound", "L", 155)],
["ReadaheadTable", 28, ("Minus", "RS", 43), ("CloseCurly", "L", 116), ("Dot", "L", 116), ("RightArrow", "L", 116), ("FatRightArrow", "L", 116), ("CloseRound", "L", 116)],
["ReadaheadTable", 29, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("RepetitionOption", "RSN", 44), ("walkIdentifier", "RSN", 63), ("walkInteger", "RSN", 70), ("walkCharacter", "RSN", 70), ("Secondary", "RSN", 31), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("Or", "L", 68), ("CloseCurly", "L", 68), ("Minus", "L", 68), ("And", "L", 68), ("Dot", "L", 68), ("RightArrow", "L", 68), ("FatRightArrow", "L", 68), ("CloseRound", "L", 68)],
["ReadaheadTable", 30, ("Or", "RS", 45), ("CloseCurly", "L", 69), ("Minus", "L", 69), ("And", "L", 69), ("Dot", "L", 69), ("RightArrow", "L", 69), ("FatRightArrow", "L", 69), ("CloseRound", "L", 69)],
["ReadaheadTable", 31, ("OpenSquare", "RS", 46), ("OpenRound", "L", 76), ("OpenCurly", "L", 76), ("walkIdentifier", "L", 76), ("walkString", "L", 76), ("walkCharacter", "L", 76), ("walkInteger", "L", 76), ("walkSymbol", "L", 76), ("Star", "L", 76), ("QuestionMark", "L", 76), ("Plus", "L", 76), ("Or", "L", 76), ("CloseCurly", "L", 76), ("Minus", "L", 76), ("And", "L", 76), ("Dot", "L", 76), ("RightArrow", "L", 76), ("FatRightArrow", "L", 76), ("CloseRound", "L", 76)],
["ReadaheadTable", 32, ("And", "RS", 47), ("CloseCurly", "L", 117), ("Minus", "L", 117), ("Dot", "L", 117), ("RightArrow", "L", 117), ("FatRightArrow", "L", 117), ("CloseRound", "L", 117)],
["ReadaheadTable", 33, ("Dot", "RS", 160)],
["ReadaheadTable", 34, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("AndExpression", "RSN", 28), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 32), ("Expression", "RSN", 48), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 35, ("QuestionMark", "RS", 161), ("Plus", "RS", 162), ("Star", "RS", 163), ("OpenRound", "L", 103), ("OpenCurly", "L", 103), ("walkIdentifier", "L", 103), ("walkString", "L", 103), ("walkCharacter", "L", 103), ("walkInteger", "L", 103), ("walkSymbol", "L", 103), ("Or", "L", 103), ("CloseCurly", "L", 103), ("Minus", "L", 103), ("And", "L", 103), ("Dot", "L", 103), ("RightArrow", "L", 103), ("FatRightArrow", "L", 103), ("CloseRound", "L", 103)],
["ReadaheadTable", 36, ("CloseCurly", "RS", 164)],
["ReadaheadTable", 37, ("FatRightArrow", "RS", 49), ("RightArrow", "L", 71), ("Dot", "L", 71)],
["ReadaheadTable", 38, ("Dot", "RS", 165), ("walkIdentifier", "RSN", 63), ("walkString", "RSN", 63), ("Name", "RSN", 38)],
["ReadaheadTable", 39, ("Dot", "RS", 166), ("walkIdentifier", "RSN", 63), ("walkString", "RSN", 63), ("Name", "RSN", 39)],
["ReadaheadTable", 40, ("walkInteger", "RSN", 70), ("Byte", "RSN", 167), ("walkCharacter", "RSN", 70)],
["ReadaheadTable", 41, ("CloseCurly", "RS", 168)],
["ReadaheadTable", 42, ("SemanticActionParameter", "RSN", 42), ("walkSymbol", "RSN", 79), ("Byte", "RSN", 79), ("walkInteger", "RSN", 70), ("CloseSquare", "RS", 169), ("walkString", "RSN", 63), ("walkIdentifier", "RSN", 63), ("Name", "RSN", 79), ("walkCharacter", "RSN", 70)],
["ReadaheadTable", 43, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("AndExpression", "RSN", 170), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 32), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 44, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("RepetitionOption", "RSN", 44), ("walkIdentifier", "RSN", 63), ("walkInteger", "RSN", 70), ("walkCharacter", "RSN", 70), ("Secondary", "RSN", 31), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("Or", "L", 159), ("CloseCurly", "L", 159), ("Minus", "L", 159), ("And", "L", 159), ("Dot", "L", 159), ("RightArrow", "L", 159), ("FatRightArrow", "L", 159), ("CloseRound", "L", 159)],
["ReadaheadTable", 45, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("walkInteger", "RSN", 70), ("walkCharacter", "RSN", 70), ("Concatenation", "RSN", 50), ("Secondary", "RSN", 31), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35)],
["ReadaheadTable", 46, ("noStack", "RSN", 81), ("Attribute", "RSN", 46), ("stack", "RSN", 81), ("read", "RSN", 81), ("look", "RSN", 81), ("CloseSquare", "RS", 172), ("noKeep", "RSN", 81), ("keep", "RSN", 81), ("noNode", "RSN", 81), ("node", "RSN", 81)],
["ReadaheadTable", 47, ("Byte", "RSN", 25), ("Name", "RSN", 67), ("OpenCurly", "RS", 26), ("walkSymbol", "RSN", 27), ("RepetitionOption", "RSN", 29), ("walkIdentifier", "RSN", 63), ("Concatenation", "RSN", 30), ("walkCharacter", "RSN", 70), ("walkInteger", "RSN", 70), ("Secondary", "RSN", 31), ("Alternation", "RSN", 173), ("OpenRound", "RS", 34), ("SemanticAction", "RSN", 156), ("walkString", "RSN", 63), ("Primary", "RSN", 35), ("CloseCurly", "L", 150), ("Minus", "L", 150), ("And", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("CloseRound", "L", 150)],
["ReadaheadTable", 48, ("CloseRound", "RS", 83)],
["ReadaheadTable", 49, ("Plus", "RS", 51), ("walkSymbol", "RSN", 27), ("walkInteger", "RSN", 174), ("SemanticAction", "RSN", 175), ("TreeBuildingOptions", "RSN", 176), ("walkString", "RSN", 63), ("Minus", "RS", 52), ("Name", "RSN", 177), ("walkIdentifier", "RSN", 63)],
["ReadaheadTable", 50, ("Or", "RS", 45), ("CloseCurly", "L", 171), ("Minus", "L", 171), ("And", "L", 171), ("Dot", "L", 171), ("RightArrow", "L", 171), ("FatRightArrow", "L", 171), ("CloseRound", "L", 171)],
["ReadaheadTable", 51, ("walkInteger", "RSN", 174)],
["ReadaheadTable", 52, ("walkInteger", "RSN", 178)],
["ReadbackTable", 53, (("Production", 6), "RSN", 53), (("Macro", 6), "RSN", 53), (("Defaults", 146), "L", 136), (("GrammarType", 2), "L", 136)],
["ReadbackTable", 54, (("RightPart", 21), "RSN", 54), (("LeftPart", 10), "L", 140)],
["ReadbackTable", 55, (("RepetitionOption", 29), "RSN", 131), (("RepetitionOption", 44), "RSN", 55)],
["ReadbackTable", 56, (("OpenSquare", 42), "RS", 115), (("SemanticActionParameter", 42), "RSN", 56)],
["ReadbackTable", 57, (("OpenSquare", 46), "RS", 76), (("Attribute", 46), "RSN", 57)],
["ReadbackTable", 58, (("Name", 16), "RSN", 58), (("keywords", 8), "RS", 138)],
["ReadbackTable", 59, (("Name", 22), "RSN", 59), (("defaults", 12), "RS", 111)],
["ReadbackTable", 60, (("defaults", 23), "RS", 120), (("Name", 38), "RSN", 60)],
["ReadbackTable", 61, (("Name", 39), "RSN", 61), (("defaults", 24), "RS", 120)],
["ReadbackTable", 62, (("Concatenation", 30), "RSN", 122), (("Concatenation", 50), "RSN", 108)],
["ShiftbackTable", 63, 1, 134],
["ShiftbackTable", 64, 1, 133],
["ShiftbackTable", 65, 1, 53],
["ShiftbackTable", 66, 2, 133],
["ShiftbackTable", 67, 1, 123],
["ShiftbackTable", 68, 1, 131],
["ShiftbackTable", 69, 1, 122],
["ShiftbackTable", 70, 1, 142],
["ShiftbackTable", 71, 2, 132],
["ShiftbackTable", 72, 1, 54],
["ShiftbackTable", 73, 2, 111],
["ShiftbackTable", 74, 2, 111],
["ShiftbackTable", 75, 2, 58],
["ShiftbackTable", 76, 1, 129],
["ShiftbackTable", 77, 2, 112],
["ShiftbackTable", 78, 2, 59],
["ShiftbackTable", 79, 1, 139],
["ShiftbackTable", 80, 1, 55],
["ShiftbackTable", 81, 1, 126],
["ShiftbackTable", 82, 2, 113],
["ShiftbackTable", 83, 2, 67],
["ShiftbackTable", 84, 2, 130],
["ShiftbackTable", 85, 2, 114],
["ShiftbackTable", 86, 2, 60],
["ShiftbackTable", 87, 2, 61],
["ShiftbackTable", 88, 2, 67],
["ShiftbackTable", 89, 2, 67],
["ShiftbackTable", 90, 1, 56],
["ShiftbackTable", 91, 2, 116],
["ShiftbackTable", 92, 2, 62],
["ShiftbackTable", 93, 1, 57],
["ShiftbackTable", 94, 2, 117],
["ShiftbackTable", 95, 1, 124],
["ShiftbackTable", 96, 2, 71],
["ShiftbackTable", 97, 2, 124],
["ShiftbackTable", 98, 1, 58],
["ShiftbackTable", 99, 2, 135],
["ShiftbackTable", 100, 1, 59],
["ShiftbackTable", 101, 2, 118],
["ShiftbackTable", 102, 2, 123],
["ShiftbackTable", 103, 1, 130],
["ShiftbackTable", 104, 2, 119],
["ShiftbackTable", 105, 1, 60],
["ShiftbackTable", 106, 1, 61],
["ShiftbackTable", 107, 2, 141],
["ShiftbackTable", 108, 1, 62],
["ShiftbackTable", 109, 2, 137],
["ShiftbackTable", 110, 3, 132],
["ShiftbackTable", 111, 1, 138],
["ShiftbackTable", 112, 1, 135],
["ShiftbackTable", 113, 2, 127],
["ShiftbackTable", 114, 2, 128],
["ShiftbackTable", 115, 1, 125],
["ShiftbackTable", 116, 1, 141],
["ShiftbackTable", 117, 1, 137],
["ShiftbackTable", 118, 1, 127],
["ShiftbackTable", 119, 1, 128],
["ShiftbackTable", 120, 2, 138],
["ShiftbackTable", 121, 1, 132],
["ReduceTable", 122, "Alternation", (47, "RSN", 173), (173, "RSN", 173), (17, "RSN", 32), (18, "RSN", 32), (19, "RSN", 32), (26, "RSN", 32), (34, "RSN", 32), (43, "RSN", 32)],
["ReduceTable", 123, "Secondary", (17, "RSN", 31), (18, "RSN", 31), (19, "RSN", 31), (26, "RSN", 31), (29, "RSN", 31), (34, "RSN", 31), (43, "RSN", 31), (44, "RSN", 31), (45, "RSN", 31), (47, "RSN", 31)],
["ReduceTable", 124, "TreeBuildingOptions", (49, "RSN", 176), (176, "RSN", 176)],
["ReduceTable", 125, "SemanticAction", (49, "RSN", 175), (175, "RSN", 175), (17, "RSN", 156), (18, "RSN", 156), (19, "RSN", 156), (26, "RSN", 156), (29, "RSN", 156), (34, "RSN", 156), (43, "RSN", 156), (44, "RSN", 156), (45, "RSN", 156), (47, "RSN", 156), (156, "RSN", 156)],
["ReduceTable", 126, "Attribute", (46, "RSN", 46)],
["ReduceTable", 127, "Macro", (2, "RSN", 6), (6, "RSN", 6), (146, "RSN", 6)],
["ReduceTable", 128, "LeftPart", (2, "RSN", 10), (6, "RSN", 10), (146, "RSN", 10)],
["ReduceTable", 129, "Primary", (17, "RSN", 35), (18, "RSN", 35), (19, "RSN", 35), (26, "RSN", 35), (29, "RSN", 35), (34, "RSN", 35), (43, "RSN", 35), (44, "RSN", 35), (45, "RSN", 35), (47, "RSN", 35)],
["ReduceTable", 130, "RepetitionOption", (17, "RSN", 29), (18, "RSN", 29), (19, "RSN", 29), (26, "RSN", 29), (34, "RSN", 29), (43, "RSN", 29), (45, "RSN", 29), (47, "RSN", 29), (29, "RSN", 44), (44, "RSN", 44)],
["ReduceTable", 131, "Concatenation", (45, "RSN", 50), (17, "RSN", 30), (18, "RSN", 30), (19, "RSN", 30), (26, "RSN", 30), (34, "RSN", 30), (43, "RSN", 30), (47, "RSN", 30)],
["ReduceTable", 132, "RightPart", (10, "RSN", 21), (21, "RSN", 21)],
["ReduceTable", 133, "GrammarType", (1, "RSN", 2)],
["ReduceTable", 134, "Name", (8, "RSN", 16), (16, "RSN", 16), (12, "RSN", 22), (22, "RSN", 22), (2, "RSN", 9), (6, "RSN", 9), (146, "RSN", 9), (4, "RSN", 11), (24, "RSN", 39), (39, "RSN", 39), (49, "RSN", 177), (177, "RSN", 177), (23, "RSN", 38), (38, "RSN", 38), (17, "RSN", 67), (18, "RSN", 67), (19, "RSN", 67), (26, "RSN", 67), (29, "RSN", 67), (34, "RSN", 67), (43, "RSN", 67), (44, "RSN", 67), (45, "RSN", 67), (47, "RSN", 67), (67, "RSN", 67), (7, "RSN", 15), (42, "RSN", 79), (79, "RSN", 79)],
["ReduceTable", 135, "Production", (2, "RSN", 6), (6, "RSN", 6), (146, "RSN", 6)],
["ReduceTable", 136, "Rules", (2, "RSN", 179), (146, "RSN", 179), (179, "RSN", 179)],
["ReduceTable", 137, "AndExpression", (43, "RSN", 170), (170, "RSN", 170), (17, "RSN", 28), (18, "RSN", 28), (19, "RSN", 28), (26, "RSN", 28), (34, "RSN", 28)],
["ReduceTable", 138, "Defaults", (2, "RSN", 146), (146, "RSN", 146)],
["ReduceTable", 139, "SemanticActionParameter", (42, "RSN", 42)],
["ReduceTable", 140, "RightParts", (10, "RSN", 20)],
["ReduceTable", 141, "Expression", (18, "RSN", 36), (26, "RSN", 41), (17, "RSN", 33), (34, "RSN", 48), (19, "RSN", 37)],
["ReduceTable", 142, "Byte", (42, "RSN", 79), (79, "RSN", 79), (40, "RSN", 167), (167, "RSN", 167), (17, "RSN", 25), (18, "RSN", 25), (19, "RSN", 25), (26, "RSN", 25), (29, "RSN", 25), (34, "RSN", 25), (43, "RSN", 25), (44, "RSN", 25), (45, "RSN", 25), (47, "RSN", 25)],
["SemanticTable", 143, "processTypeNow", ["superScanner"], 64],
["SemanticTable", 144, "processTypeNow", ["parser"], 64],
["SemanticTable", 145, "processTypeNow", ["scanner"], 64],
["SemanticTable", 146, "processAndDiscardDefaultsNow", [], 2],
["SemanticTable", 147, "buildTree", ["walkGrammar"], 65],
["SemanticTable", 148, "buildTree", ["walkLeftPart"], 119],
["SemanticTable", 149, "processTypeNow", ["superScanner"], 66],
["SemanticTable", 150, "buildTree", ["walkEpsilon"], 122],
["SemanticTable", 151, "buildTree", ["walkOr"], 72],
["SemanticTable", 152, "buildTree", ["walkOptimize"], 73],
["SemanticTable", 153, "buildTree", ["walkOutput"], 74],
["SemanticTable", 154, "buildTree", ["walkKeywords"], 75],
["SemanticTable", 155, "buildTree", ["walkSemanticAction"], 115],
["SemanticTable", 156, "buildTree", ["walkNonTreeBuildingSemanticAction"], 76],
["SemanticTable", 157, "buildTree", ["walkProduction"], 77],
["SemanticTable", 158, "buildTree", ["walkAttributeDefaults"], 78],
["SemanticTable", 159, "buildTree", ["walkConcatenation"], 80],
["SemanticTable", 160, "buildTree", ["walkMacro"], 82],
["SemanticTable", 161, "buildTree", ["walkQuestionMark"], 84],
["SemanticTable", 162, "buildTree", ["walkPlus"], 84],
["SemanticTable", 163, "buildTree", ["walkStar"], 84],
["SemanticTable", 164, "buildTree", ["walkLeftPartWithLookahead"], 85],
["SemanticTable", 165, "buildTree", ["walkAttributeNonterminalDefaults"], 86],
["SemanticTable", 166, "buildTree", ["walkAttributeTerminalDefaults"], 87],
["SemanticTable", 167, "buildTree", ["walkDotDot"], 88],
["SemanticTable", 168, "buildTree", ["walkLook"], 89],
["SemanticTable", 169, "buildTree", ["walkSemanticAction"], 90],
["SemanticTable", 170, "buildTree", ["walkMinus"], 91],
["SemanticTable", 171, "buildTree", ["walkOr"], 92],
["SemanticTable", 172, "buildTree", ["walkAttributes"], 93],
["SemanticTable", 173, "buildTree", ["walkAnd"], 94],
["SemanticTable", 174, "buildTree", ["walkBuildTreeFromLeftIndex"], 97],
["SemanticTable", 175, "buildTree", ["walkTreeBuildingSemanticAction"], 95],
["SemanticTable", 176, "buildTree", ["walkConcatenation"], 96],
["SemanticTable", 177, "buildTree", ["walkBuildTreeOrTokenFromName"], 95],
["SemanticTable", 178, "buildTree", ["walkBuildTreeFromRightIndex"], 97],
["AcceptTable", 179]]
}