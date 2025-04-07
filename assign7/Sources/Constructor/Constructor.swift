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
        // print("\n\n\n\(tree)\n")
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

        // print(Grammar.activeGrammar!.productions)

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

        finalizeReduceTables()

        replaceSemanticTransitions()

        if Grammar.activeGrammar!.isParser() {
            // remove initial readahead state from parsers (left goalpost transition)
            readaheadStates.remove(at: 0)
        }

        optimize()

        renumber()

        // print(Grammar.activeGrammar!.productionFor("DoubleQuotedStringOrSymbol"))

        // printStates()
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
            // print(i)
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
                var successor = readaheadStates.first { state in
                    state.initialItems.allSatisfy{candidate.initialItems.contains($0)}
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
            // print("\n")
            // print(raState.terseDescription)
            // print(partition)
            // print()

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

            rbState.items.append(contentsOf: more_items)

            visible_left.from(rbState.items) { Mp, local_left in
                let candidate = ReadbackState(initialItems: local_left.allTo())
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
        readaheadStates.do {
            for sem_transition in $0.transitions where sem_transition.hasAction() {
                buildSemanticState($0, transition: sem_transition)
                $0.transitions.removeAll { transition in transition == sem_transition }
            }
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

        (follow.second() as! [String]).do {name in
            if state.transitions.first(where: { $0.label.terseDescription == name }) == nil {
                state.addTransition(Transition(label: Label(name: name, printable: false).asLook(), goto: new_state))
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

                while i < value.count {
                    let state = value[i]
                    i += 1

                    // print(raState.terseDescription)
                    // print(
                    //     ra_invisible_left.performStar([raState]).filter {
                    //         stackable_states.contains($0)
                    //     }.map { $0.terseDescription }.joined(separator: ", "))

                    if stackable_states.contains(state) {
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
        readbackToShift()
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
            return_val = infix(return_val, (walkTree((tree as! Tree).child(i)) as! FiniteStateMachine))
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
        // print(tree)
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
["ReadaheadTable", 1, ("superScanner", "RS", 143), ("scanner", "RS", 144), ("GrammarType", "RSN", 2), ("parser", "RS", 145), ("super", "RS", 3)],
["ReadaheadTable", 2, ("attribute", "RS", 4), ("walkIdentifier", "RSN", 64), ("Name", "RSN", 5), ("output", "RS", 6), ("walkString", "RSN", 64), ("Defaults", "RSN", 147), ("Production", "RSN", 7), ("optimize", "RS", 8), ("keywords", "RS", 9), ("Macro", "RSN", 7), ("LeftPart", "RSN", 10), ("Rules", "RSN", 179)],
["ReadaheadTable", 3, ("scanner", "RS", 149)],
["ReadaheadTable", 4, ("nonterminal", "RS", 11), ("terminal", "RS", 12), ("defaults", "RS", 13)],
["ReadaheadTable", 5, ("Equals", "RS", 14), ("OpenCurly", "RS", 15), ("RightArrow", "L", 146)],
["ReadaheadTable", 6, ("Name", "RSN", 16), ("walkIdentifier", "RSN", 64), ("walkString", "RSN", 64)],
["ReadaheadTable", 7, ("walkIdentifier", "RSN", 64), ("Macro", "RSN", 7), ("Name", "RSN", 5), ("walkString", "RSN", 64), ("Production", "RSN", 7), ("LeftPart", "RSN", 10), ("-|", "L", 148)],
["ReadaheadTable", 8, ("Name", "RSN", 17), ("walkIdentifier", "RSN", 64), ("walkString", "RSN", 64)],
["ReadaheadTable", 9, ("Name", "RSN", 18), ("walkIdentifier", "RSN", 64), ("walkString", "RSN", 64)],
["ReadaheadTable", 10, ("RightPart", "RSN", 19), ("RightParts", "RSN", 20), ("RightArrow", "RS", 21)],
["ReadaheadTable", 11, ("defaults", "RS", 22)],
["ReadaheadTable", 12, ("defaults", "RS", 23)],
["ReadaheadTable", 13, ("Name", "RSN", 24), ("walkIdentifier", "RSN", 64), ("walkString", "RSN", 64)],
["ReadaheadTable", 14, ("AndExpression", "RSN", 25), ("Secondary", "RSN", 26), ("Alternation", "RSN", 27), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("walkSymbol", "RSN", 30), ("Expression", "RSN", 31), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 15, ("AndExpression", "RSN", 25), ("Secondary", "RSN", 26), ("Alternation", "RSN", 27), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("walkSymbol", "RSN", 30), ("Expression", "RSN", 36), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 16, ("Dot", "RS", 154)],
["ReadaheadTable", 17, ("Dot", "RS", 155)],
["ReadaheadTable", 18, ("Dot", "RS", 156), ("walkString", "RSN", 64), ("Name", "RSN", 18), ("walkIdentifier", "RSN", 64)],
["ReadaheadTable", 19, ("RightPart", "RSN", 19), ("RightArrow", "RS", 21), ("Dot", "L", 151)],
["ReadaheadTable", 20, ("Dot", "RS", 157)],
["ReadaheadTable", 21, ("AndExpression", "RSN", 25), ("Secondary", "RSN", 26), ("Alternation", "RSN", 27), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("Expression", "RSN", 37), ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 22, ("walkString", "RSN", 64), ("Name", "RSN", 38), ("walkIdentifier", "RSN", 64)],
["ReadaheadTable", 23, ("walkString", "RSN", 64), ("Name", "RSN", 39), ("walkIdentifier", "RSN", 64)],
["ReadaheadTable", 24, ("Dot", "RS", 158), ("walkString", "RSN", 64), ("Name", "RSN", 24), ("walkIdentifier", "RSN", 64)],
["ReadaheadTable", 25, ("Minus", "RS", 40), ("CloseRound", "L", 115), ("CloseCurly", "L", 115), ("Dot", "L", 115), ("RightArrow", "L", 115), ("FatRightArrow", "L", 115)],
["ReadaheadTable", 26, ("OpenSquare", "RS", 41), ("OpenRound", "L", 73), ("OpenCurly", "L", 73), ("walkIdentifier", "L", 73), ("walkString", "L", 73), ("walkCharacter", "L", 73), ("walkInteger", "L", 73), ("walkSymbol", "L", 73), ("Star", "L", 73), ("QuestionMark", "L", 73), ("Plus", "L", 73), ("And", "L", 73), ("Or", "L", 73), ("CloseRound", "L", 73), ("CloseCurly", "L", 73), ("Dot", "L", 73), ("RightArrow", "L", 73), ("FatRightArrow", "L", 73), ("Minus", "L", 73)],
["ReadaheadTable", 27, ("And", "RS", 42), ("CloseRound", "L", 116), ("CloseCurly", "L", 116), ("Dot", "L", 116), ("RightArrow", "L", 116), ("FatRightArrow", "L", 116), ("Minus", "L", 116)],
["ReadaheadTable", 28, ("Secondary", "RSN", 26), ("RepetitionOption", "RSN", 43), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("walkSymbol", "RSN", 30), ("OpenCurly", "RS", 29), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 67), ("Or", "L", 67), ("CloseRound", "L", 67), ("CloseCurly", "L", 67), ("Dot", "L", 67), ("RightArrow", "L", 67), ("FatRightArrow", "L", 67), ("Minus", "L", 67)],
["ReadaheadTable", 29, ("AndExpression", "RSN", 25), ("Secondary", "RSN", 26), ("Alternation", "RSN", 27), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("Expression", "RSN", 44), ("OpenCurly", "RS", 29), ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 30, ("OpenSquare", "RS", 45), ("OpenRound", "L", 152), ("OpenCurly", "L", 152), ("walkIdentifier", "L", 152), ("walkString", "L", 152), ("walkCharacter", "L", 152), ("walkInteger", "L", 152), ("walkSymbol", "L", 152), ("Star", "L", 152), ("QuestionMark", "L", 152), ("Plus", "L", 152), ("RightArrow", "L", 152), ("Dot", "L", 152), ("And", "L", 152), ("Or", "L", 152), ("CloseRound", "L", 152), ("CloseCurly", "L", 152), ("FatRightArrow", "L", 152), ("Minus", "L", 152)],
["ReadaheadTable", 31, ("Dot", "RS", 160)],
["ReadaheadTable", 32, ("QuestionMark", "RS", 161), ("Plus", "RS", 162), ("Star", "RS", 163), ("OpenRound", "L", 102), ("OpenCurly", "L", 102), ("walkIdentifier", "L", 102), ("walkString", "L", 102), ("walkCharacter", "L", 102), ("walkInteger", "L", 102), ("walkSymbol", "L", 102), ("And", "L", 102), ("Or", "L", 102), ("CloseRound", "L", 102), ("CloseCurly", "L", 102), ("Dot", "L", 102), ("RightArrow", "L", 102), ("FatRightArrow", "L", 102), ("Minus", "L", 102)],
["ReadaheadTable", 33, ("DotDot", "RS", 46), ("OpenRound", "L", 70), ("OpenCurly", "L", 70), ("walkIdentifier", "L", 70), ("walkString", "L", 70), ("walkCharacter", "L", 70), ("walkInteger", "L", 70), ("walkSymbol", "L", 70), ("Star", "L", 70), ("QuestionMark", "L", 70), ("Plus", "L", 70), ("OpenSquare", "L", 70), ("And", "L", 70), ("Or", "L", 70), ("CloseRound", "L", 70), ("CloseCurly", "L", 70), ("Dot", "L", 70), ("RightArrow", "L", 70), ("FatRightArrow", "L", 70), ("Minus", "L", 70)],
["ReadaheadTable", 34, ("Or", "RS", 47), ("And", "L", 69), ("CloseRound", "L", 69), ("CloseCurly", "L", 69), ("Dot", "L", 69), ("RightArrow", "L", 69), ("FatRightArrow", "L", 69), ("Minus", "L", 69)],
["ReadaheadTable", 35, ("AndExpression", "RSN", 25), ("Secondary", "RSN", 26), ("Alternation", "RSN", 27), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("Expression", "RSN", 48), ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 36, ("CloseCurly", "RS", 164)],
["ReadaheadTable", 37, ("FatRightArrow", "RS", 49), ("RightArrow", "L", 72), ("Dot", "L", 72)],
["ReadaheadTable", 38, ("Name", "RSN", 38), ("Dot", "RS", 165), ("walkIdentifier", "RSN", 64), ("walkString", "RSN", 64)],
["ReadaheadTable", 39, ("Name", "RSN", 39), ("Dot", "RS", 166), ("walkIdentifier", "RSN", 64), ("walkString", "RSN", 64)],
["ReadaheadTable", 40, ("AndExpression", "RSN", 167), ("Secondary", "RSN", 26), ("Alternation", "RSN", 27), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 41, ("Attribute", "RSN", 41), ("noKeep", "RSN", 79), ("stack", "RSN", 79), ("noNode", "RSN", 79), ("look", "RSN", 79), ("read", "RSN", 79), ("CloseSquare", "RS", 168), ("node", "RSN", 79), ("noStack", "RSN", 79), ("keep", "RSN", 79)],
["ReadaheadTable", 42, ("Secondary", "RSN", 26), ("Alternation", "RSN", 169), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 34), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 150), ("CloseRound", "L", 150), ("CloseCurly", "L", 150), ("Dot", "L", 150), ("RightArrow", "L", 150), ("FatRightArrow", "L", 150), ("Minus", "L", 150)],
["ReadaheadTable", 43, ("Secondary", "RSN", 26), ("RepetitionOption", "RSN", 43), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("walkSymbol", "RSN", 30), ("OpenCurly", "RS", 29), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153), ("And", "L", 159), ("Or", "L", 159), ("CloseRound", "L", 159), ("CloseCurly", "L", 159), ("Dot", "L", 159), ("RightArrow", "L", 159), ("FatRightArrow", "L", 159), ("Minus", "L", 159)],
["ReadaheadTable", 44, ("CloseCurly", "RS", 170)],
["ReadaheadTable", 45, ("SemanticActionParameter", "RSN", 45), ("walkCharacter", "RSN", 68), ("walkSymbol", "RSN", 81), ("walkIdentifier", "RSN", 64), ("Name", "RSN", 81), ("walkString", "RSN", 64), ("Byte", "RSN", 81), ("CloseSquare", "RS", 171), ("walkInteger", "RSN", 68)],
["ReadaheadTable", 46, ("Byte", "RSN", 172), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68)],
["ReadaheadTable", 47, ("Secondary", "RSN", 26), ("RepetitionOption", "RSN", 28), ("walkString", "RSN", 64), ("walkInteger", "RSN", 68), ("walkCharacter", "RSN", 68), ("OpenCurly", "RS", 29), ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("Primary", "RSN", 32), ("Byte", "RSN", 33), ("Concatenation", "RSN", 50), ("Name", "RSN", 70), ("OpenRound", "RS", 35), ("SemanticAction", "RSN", 153)],
["ReadaheadTable", 48, ("CloseRound", "RS", 84)],
["ReadaheadTable", 49, ("walkSymbol", "RSN", 30), ("walkIdentifier", "RSN", 64), ("TreeBuildingOptions", "RSN", 174), ("Name", "RSN", 175), ("walkString", "RSN", 64), ("Plus", "RS", 51), ("SemanticAction", "RSN", 176), ("Minus", "RS", 52), ("walkInteger", "RSN", 177)],
["ReadaheadTable", 50, ("Or", "RS", 47), ("And", "L", 173), ("CloseRound", "L", 173), ("CloseCurly", "L", 173), ("Dot", "L", 173), ("RightArrow", "L", 173), ("FatRightArrow", "L", 173), ("Minus", "L", 173)],
["ReadaheadTable", 51, ("walkInteger", "RSN", 177)],
["ReadaheadTable", 52, ("walkInteger", "RSN", 178)],
["ReadbackTable", 53, (("Macro", 7), "RSN", 53), (("Production", 7), "RSN", 53), (("GrammarType", 2), "L", 138), (("Defaults", 147), "L", 138)],
["ReadbackTable", 54, (("RightPart", 19), "RSN", 54), (("LeftPart", 10), "L", 122)],
["ReadbackTable", 55, (("RepetitionOption", 28), "RSN", 123), (("RepetitionOption", 43), "RSN", 55)],
["ReadbackTable", 56, (("OpenSquare", 41), "RS", 73), (("Attribute", 41), "RSN", 56)],
["ReadbackTable", 57, (("SemanticActionParameter", 45), "RSN", 57), (("OpenSquare", 45), "RS", 117)],
["ReadbackTable", 58, (("Name", 18), "RSN", 58), (("keywords", 9), "RS", 135)],
["ReadbackTable", 59, (("defaults", 13), "RS", 111), (("Name", 24), "RSN", 59)],
["ReadbackTable", 60, (("defaults", 22), "RS", 120), (("Name", 38), "RSN", 60)],
["ReadbackTable", 61, (("Name", 39), "RSN", 61), (("defaults", 23), "RS", 120)],
["ReadbackTable", 62, (("Concatenation", 34), "RSN", 124), (("Concatenation", 50), "RSN", 109)],
["ShiftbackTable", 63, 1, 139],
["ShiftbackTable", 64, 1, 126],
["ShiftbackTable", 65, 1, 53],
["ShiftbackTable", 66, 2, 139],
["ShiftbackTable", 67, 1, 123],
["ShiftbackTable", 68, 1, 132],
["ShiftbackTable", 69, 1, 124],
["ShiftbackTable", 70, 1, 130],
["ShiftbackTable", 71, 1, 54],
["ShiftbackTable", 72, 2, 134],
["ShiftbackTable", 73, 1, 129],
["ShiftbackTable", 74, 2, 111],
["ShiftbackTable", 75, 2, 111],
["ShiftbackTable", 76, 2, 58],
["ShiftbackTable", 77, 2, 112],
["ShiftbackTable", 78, 2, 59],
["ShiftbackTable", 79, 1, 127],
["ShiftbackTable", 80, 1, 55],
["ShiftbackTable", 81, 1, 141],
["ShiftbackTable", 82, 2, 113],
["ShiftbackTable", 83, 2, 142],
["ShiftbackTable", 84, 2, 70],
["ShiftbackTable", 85, 2, 114],
["ShiftbackTable", 86, 2, 60],
["ShiftbackTable", 87, 2, 61],
["ShiftbackTable", 88, 2, 115],
["ShiftbackTable", 89, 1, 56],
["ShiftbackTable", 90, 2, 116],
["ShiftbackTable", 91, 2, 70],
["ShiftbackTable", 92, 1, 57],
["ShiftbackTable", 93, 2, 70],
["ShiftbackTable", 94, 2, 62],
["ShiftbackTable", 95, 2, 72],
["ShiftbackTable", 96, 1, 131],
["ShiftbackTable", 97, 2, 131],
["ShiftbackTable", 98, 1, 58],
["ShiftbackTable", 99, 2, 137],
["ShiftbackTable", 100, 1, 59],
["ShiftbackTable", 101, 2, 118],
["ShiftbackTable", 102, 1, 142],
["ShiftbackTable", 103, 2, 130],
["ShiftbackTable", 104, 2, 119],
["ShiftbackTable", 105, 1, 60],
["ShiftbackTable", 106, 1, 61],
["ShiftbackTable", 107, 2, 136],
["ShiftbackTable", 108, 2, 128],
["ShiftbackTable", 109, 1, 62],
["ShiftbackTable", 110, 3, 134],
["ShiftbackTable", 111, 1, 135],
["ShiftbackTable", 112, 1, 137],
["ShiftbackTable", 113, 2, 140],
["ShiftbackTable", 114, 2, 133],
["ShiftbackTable", 115, 1, 136],
["ShiftbackTable", 116, 1, 128],
["ShiftbackTable", 117, 1, 125],
["ShiftbackTable", 118, 1, 140],
["ShiftbackTable", 119, 1, 133],
["ShiftbackTable", 120, 2, 135],
["ShiftbackTable", 121, 1, 134],
["ReduceTable", 122, "RightParts", (10, "RSN", 20)],
["ReduceTable", 123, "Concatenation", (14, "RSN", 34), (15, "RSN", 34), (21, "RSN", 34), (29, "RSN", 34), (35, "RSN", 34), (40, "RSN", 34), (42, "RSN", 34), (47, "RSN", 50)],
["ReduceTable", 124, "Alternation", (42, "RSN", 169), (169, "RSN", 169), (14, "RSN", 27), (15, "RSN", 27), (21, "RSN", 27), (29, "RSN", 27), (35, "RSN", 27), (40, "RSN", 27)],
["ReduceTable", 125, "SemanticAction", (14, "RSN", 153), (15, "RSN", 153), (21, "RSN", 153), (28, "RSN", 153), (29, "RSN", 153), (35, "RSN", 153), (40, "RSN", 153), (42, "RSN", 153), (43, "RSN", 153), (47, "RSN", 153), (153, "RSN", 153), (49, "RSN", 176), (176, "RSN", 176)],
["ReduceTable", 126, "Name", (49, "RSN", 175), (175, "RSN", 175), (6, "RSN", 16), (23, "RSN", 39), (39, "RSN", 39), (9, "RSN", 18), (18, "RSN", 18), (14, "RSN", 70), (15, "RSN", 70), (21, "RSN", 70), (28, "RSN", 70), (29, "RSN", 70), (35, "RSN", 70), (40, "RSN", 70), (42, "RSN", 70), (43, "RSN", 70), (47, "RSN", 70), (70, "RSN", 70), (8, "RSN", 17), (22, "RSN", 38), (38, "RSN", 38), (13, "RSN", 24), (24, "RSN", 24), (45, "RSN", 81), (81, "RSN", 81), (2, "RSN", 5), (7, "RSN", 5)],
["ReduceTable", 127, "Attribute", (41, "RSN", 41)],
["ReduceTable", 128, "AndExpression", (14, "RSN", 25), (15, "RSN", 25), (21, "RSN", 25), (29, "RSN", 25), (35, "RSN", 25), (40, "RSN", 167), (167, "RSN", 167)],
["ReduceTable", 129, "Primary", (14, "RSN", 32), (15, "RSN", 32), (21, "RSN", 32), (28, "RSN", 32), (29, "RSN", 32), (35, "RSN", 32), (40, "RSN", 32), (42, "RSN", 32), (43, "RSN", 32), (47, "RSN", 32)],
["ReduceTable", 130, "Secondary", (14, "RSN", 26), (15, "RSN", 26), (21, "RSN", 26), (28, "RSN", 26), (29, "RSN", 26), (35, "RSN", 26), (40, "RSN", 26), (42, "RSN", 26), (43, "RSN", 26), (47, "RSN", 26)],
["ReduceTable", 131, "TreeBuildingOptions", (49, "RSN", 174), (174, "RSN", 174)],
["ReduceTable", 132, "Byte", (46, "RSN", 172), (172, "RSN", 172), (14, "RSN", 33), (15, "RSN", 33), (21, "RSN", 33), (28, "RSN", 33), (29, "RSN", 33), (35, "RSN", 33), (40, "RSN", 33), (42, "RSN", 33), (43, "RSN", 33), (47, "RSN", 33), (45, "RSN", 81), (81, "RSN", 81)],
["ReduceTable", 133, "LeftPart", (2, "RSN", 10), (7, "RSN", 10)],
["ReduceTable", 134, "RightPart", (10, "RSN", 19), (19, "RSN", 19)],
["ReduceTable", 135, "Defaults", (2, "RSN", 147), (147, "RSN", 147)],
["ReduceTable", 136, "Expression", (29, "RSN", 44), (15, "RSN", 36), (35, "RSN", 48), (14, "RSN", 31), (21, "RSN", 37)],
["ReduceTable", 137, "Production", (2, "RSN", 7), (7, "RSN", 7)],
["ReduceTable", 138, "Rules", (2, "RSN", 179), (179, "RSN", 179)],
["ReduceTable", 139, "GrammarType", (1, "RSN", 2)],
["ReduceTable", 140, "Macro", (2, "RSN", 7), (7, "RSN", 7)],
["ReduceTable", 141, "SemanticActionParameter", (45, "RSN", 45)],
["ReduceTable", 142, "RepetitionOption", (14, "RSN", 28), (15, "RSN", 28), (21, "RSN", 28), (29, "RSN", 28), (35, "RSN", 28), (40, "RSN", 28), (42, "RSN", 28), (47, "RSN", 28), (28, "RSN", 43), (43, "RSN", 43)],
["SemanticTable", 143, "processTypeNow", ["superScanner"], 63],
["SemanticTable", 144, "processTypeNow", ["scanner"], 63],
["SemanticTable", 145, "processTypeNow", ["parser"], 63],
["SemanticTable", 146, "buildTree", ["walkLeftPart"], 119],
["SemanticTable", 147, "processAndDiscardDefaultsNow", [], 2],
["SemanticTable", 148, "buildTree", ["walkGrammar"], 65],
["SemanticTable", 149, "processTypeNow", ["superScanner"], 66],
["SemanticTable", 150, "buildTree", ["walkEpsilon"], 124],
["SemanticTable", 151, "buildTree", ["walkOr"], 71],
["SemanticTable", 152, "buildTree", ["walkSemanticAction"], 117],
["SemanticTable", 153, "buildTree", ["walkNonTreeBuildingSemanticAction"], 73],
["SemanticTable", 154, "buildTree", ["walkOutput"], 74],
["SemanticTable", 155, "buildTree", ["walkOptimize"], 75],
["SemanticTable", 156, "buildTree", ["walkKeywords"], 76],
["SemanticTable", 157, "buildTree", ["walkProduction"], 77],
["SemanticTable", 158, "buildTree", ["walkAttributeDefaults"], 78],
["SemanticTable", 159, "buildTree", ["walkConcatenation"], 80],
["SemanticTable", 160, "buildTree", ["walkMacro"], 82],
["SemanticTable", 161, "buildTree", ["walkQuestionMark"], 83],
["SemanticTable", 162, "buildTree", ["walkPlus"], 83],
["SemanticTable", 163, "buildTree", ["walkStar"], 83],
["SemanticTable", 164, "buildTree", ["walkLeftPartWithLookahead"], 85],
["SemanticTable", 165, "buildTree", ["walkAttributeNonterminalDefaults"], 86],
["SemanticTable", 166, "buildTree", ["walkAttributeTerminalDefaults"], 87],
["SemanticTable", 167, "buildTree", ["walkMinus"], 88],
["SemanticTable", 168, "buildTree", ["walkAttributes"], 89],
["SemanticTable", 169, "buildTree", ["walkAnd"], 90],
["SemanticTable", 170, "buildTree", ["walkLook"], 91],
["SemanticTable", 171, "buildTree", ["walkSemanticAction"], 92],
["SemanticTable", 172, "buildTree", ["walkDotDot"], 93],
["SemanticTable", 173, "buildTree", ["walkOr"], 94],
["SemanticTable", 174, "buildTree", ["walkConcatenation"], 95],
["SemanticTable", 175, "buildTree", ["walkBuildTreeOrTokenFromName"], 96],
["SemanticTable", 176, "buildTree", ["walkTreeBuildingSemanticAction"], 96],
["SemanticTable", 177, "buildTree", ["walkBuildTreeFromLeftIndex"], 97],
["SemanticTable", 178, "buildTree", ["walkBuildTreeFromRightIndex"], 97],
["AcceptTable", 179]]
    
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