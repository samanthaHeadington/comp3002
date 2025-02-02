//
//  FSMLibrary.swift
//  FSMBuilder
//
//  Created by Wilf Lalonde on 2023-01-24.
//

import Foundation

postfix operator ^  // used in place of ?, since swift does not allow ? postfix operators
postfix operator +
postfix operator *
infix operator ..
public class FiniteStateMachine: CustomStringConvertible {
    var states: [FiniteStateMachineState]

    init() { states = [] }

    init(fsm: FiniteStateMachine) {
        states = fsm.states.map { return FiniteStateMachineState(state: $0) }

        for i in 0..<states.count {
            for j in 0..<states[i].transitions.count {
                states[i].transitions[j].goto =
                    states[fsm.states[i].transitions[j].goto.stateNumber]
            }
        }
    }

    init(states: [FiniteStateMachineState]) {
        self.states = states

        renumber()
    }

    func override(_ attributes: [String]) {
        for state in states {
            for transition in state.transitions {
                transition.override(attributes)
            }
        }
    }

    func override(_ attributes: AttributeList) {
        for state in states {
            for transition in state.transitions {
                transition.override(AttributeList(attributes: attributes))
            }
        }
    }

    func renumber() {
        for (index, state) in states.enumerated() {
            state.stateNumber = index
        }
    }

    static func empty() -> FiniteStateMachine {
        var return_val = FiniteStateMachine()

        return_val.states.append(FiniteStateMachineState())
        return_val.states[0].isInitial = true
        return_val.states[0].isFinal = true

        return return_val
    }

    static func | (lhs: FiniteStateMachine, rhs: FiniteStateMachine) -> FiniteStateMachine {
        for state in rhs.states {
            lhs.states.append(state)
        }

        lhs.renumber()

        return lhs
    }

    static postfix func ^ (lhs: FiniteStateMachine) -> FiniteStateMachine {
        return lhs | empty()
    }

    static postfix func + (lhs: FiniteStateMachine) -> FiniteStateMachine {
        for i_state in lhs.states where i_state.isInitial {
            for f_state in lhs.states where f_state.isFinal {
                f_state.addTransitions(i_state.transitions)
            }
        }

        return lhs
    }

    static postfix func * (lhs: FiniteStateMachine) -> FiniteStateMachine {
        return (lhs+)^
    }

    static func .. (lhs: FiniteStateMachine, rhs: FiniteStateMachine) -> FiniteStateMachine {
        let lhs_recognizes_e = lhs.canRecognizeE()
        let rhs_recognizes_e = rhs.canRecognizeE()

        // lhs.states.printWithNewLines()
        // print("debug rhs \(rhs)")

        for rhs_i_state in rhs.states where rhs_i_state.isInitial {
            for lhs_f_state in lhs.states where lhs_f_state.isFinal {
                lhs_f_state.addTransitions(rhs_i_state.transitions)
            }
        }

        if !rhs_recognizes_e {
            for state in lhs.states where state.isFinal {
                state.isFinal = false
            }
        }

        if !lhs_recognizes_e {
            for state in rhs.states where state.isInitial {
                state.isInitial = false
            }
        }

        // lhs.states.printWithNewLines()
        // rhs.states.printWithNewLines()

        rhs.states.do {
            // print("debug rhs state: \($0)")
            lhs.addState($0)
        }

        // print("debug \(lhs)");

        lhs.reduce()

        return lhs
    }

    func reduce() {
        renumber()  // this function will fail if there are duplicate state numbers, so they're renumbered here for safety

        var as_relation = Relation<Int, String>(from: getAsTriples())

        let reachable_from_initial = as_relation.performStar(
            states.filter { $0.isInitial }.map { $0.stateNumber })

        as_relation.invert()

        let can_reach_final = as_relation.performStar(
            states.filter { $0.isFinal }.map { $0.stateNumber })

        let is_useful_state: [Bool] = states.map { $0.stateNumber }.map {
            can_reach_final.contains($0) && reachable_from_initial.contains($0)
        }

        for i in (0..<states.count).reversed() {
            if !is_useful_state[i] {
                states.remove(at: i)
            }
        }

        for state in states {
            for i in (0..<state.transitions.count).reversed() {
                if !is_useful_state[state.transitions[i].goto.stateNumber] {
                    state.transitions.remove(at: i)
                }
            }
        }

        renumber()
    }

    func getAsTriples() -> [(Int, String, Int)] {
        var return_val: [(Int, String, Int)] = []

        for state in states {
            state.getAsTriples(&return_val)
        }

        return return_val
    }

    static func orAll(fsm_list: inout [FiniteStateMachine]) {
        for i in 1..<fsm_list.count {
            fsm_list[0] = (fsm_list[0]) | (fsm_list[i])
        }
    }

    func canRecognizeE() -> Bool {
        for state in states where state.isInitial {
            if state.isFinal {
                return true
            }
        }

        return false
    }

    static func - (lhs: FiniteStateMachine, rhs: FiniteStateMachine) -> FiniteStateMachine {
        return buildDualState(lhs, rhs) { state in
            state.left.contains { $0.isFinal } && !state.right.contains { $0.isFinal }
        }
    }

    static func & (lhs: FiniteStateMachine, rhs: FiniteStateMachine) -> FiniteStateMachine {
        return buildDualState(lhs, rhs) { state in
            state.left.contains { $0.isFinal } && state.right.contains { $0.isFinal }
        }
    }

    private static func buildDualState(
        _ lhs: FiniteStateMachine, _ rhs: FiniteStateMachine,
        _ isFinal: (DualFiniteStateMachineState) -> Bool
    ) -> FiniteStateMachine {
        // print(lhs)
        // print(rhs)
        print("\n\n\n")

        var return_val = FiniteStateMachine();

        var initial_state = DualFiniteStateMachineState()
        initial_state.isInitial = true

        var dual_states: Set<DualFiniteStateMachineState> = Set<DualFiniteStateMachineState>([
            initial_state
        ])

        return_val.addState(initial_state)

        for state in lhs.states where state.isInitial {
            initial_state.left.insert(state)
        }
        for state in rhs.states where state.isInitial {
            initial_state.right.insert(state)
        }

        var i: Int = 0
        while i < return_val.states.count && i < 20{
            return_val.states[i].isFinal = isFinal(return_val.states[i] as! DualFiniteStateMachineState);

            // print("\n\((return_val.states[i] as! DualFiniteStateMachineState).dual_description)\n")

            Array((return_val.states[i] as! DualFiniteStateMachineState).getLabels()).do {
                return_val.states[i].addTransition(Transition(label: $0))
            }

            return_val.states[i].transitions.do {
                // print("\n\($0)\n")

                var successor: DualFiniteStateMachineState = (return_val.states[i] as! DualFiniteStateMachineState).getSuccessor($0.label)

                //if the successor is equal to an existing dual state, replace it with the existing version
                if dual_states.contains(successor) {
                    successor = dual_states[dual_states.firstIndex(of: successor)!]
                } else {
                    dual_states.insert(successor)
                    return_val.states.append(successor)
                }

                $0.goto = successor
            }

            i += 1;
        }

        return_val.reduce();

        print(return_val);

        return return_val
    }

    static func forAction(_ action: String, parameters: [Any], isRootBuilding: Bool)
        -> FiniteStateMachine
    {
        var transition = Transition(
            action: action, parameters: parameters, isRootBuilding: isRootBuilding)

        return fromTransition(transition)
    }

    static func forIdentifier(_ identifer: String) -> FiniteStateMachine {
        return fromTransition(Transition(name: identifer))
    }

    static func forSymbol(_ symbol: String) -> FiniteStateMachine {
        return fromTransition(Transition(name: symbol))
    }

    static func forString(_ string: String) -> FiniteStateMachine {
        return (Grammar.activeGrammar!.isScanner())
            ? forStringScanner(string) : forStringParser(string)
    }

    private static func forStringScanner(_ string: String) -> FiniteStateMachine {
        return fromTransitions(string.map { Transition(name: $0) } as! [Transition])
    }

    private static func forStringParser(_ string: String) -> FiniteStateMachine {
        var return_val = fromTransition(Transition(name: string[0]))

        for i in 1..<string.count {
            return_val = return_val .. fromTransition(Transition(name: string[i]))
        }

        return return_val
    }

    static func forCharacter(_ character: String) -> FiniteStateMachine {
        var name = character
        name.insert(contentsOf: "$", at: name.startIndex)
        return fromTransition(Transition(name: name))
    }

    static func forInteger(_ integer: String) -> FiniteStateMachine {
        return fromTransition(Transition(name: integer))
    }

    static func fromTransitions(_ transitions: [Transition]) -> FiniteStateMachine {
        var return_val = fromTransition(transitions[0])

        return_val.states[0].addTransitions(Array(transitions[1..<transitions.count]))

        return return_val
    }

    static func fromTransition(_ transition: Transition) -> FiniteStateMachine {
        var return_val = FiniteStateMachine()

        return_val.addState(FiniteStateMachineState())
        return_val.addState(FiniteStateMachineState())

        return_val.renumber()

        return_val.states[0].isInitial = true
        return_val.states[1].isFinal = true

        return_val.states[0].addTransition(transition)
        transition.goto = return_val.states[1]

        return return_val
    }

    func addState(_ state: FiniteStateMachineState) {
        states.appendIfIdenticalAbsent(state)
    }

    public var description: String {
        return states.map { return String(describing: $0) }.joined(separator: "\n")
    }
}

public class FiniteStateMachineState: CustomStringConvertible, Hashable {
    var stateNumber: Int = 0
    var isInitial: Bool = false
    var isFinal: Bool = false
    var transitions: [Transition]

    init() {
        transitions = []
    }

    public static func == (lhs: FiniteStateMachineState, rhs: FiniteStateMachineState) -> Bool {
        return lhs === rhs
    }

    init(state: FiniteStateMachineState) {
        stateNumber = state.stateNumber
        isInitial = state.isInitial
        isFinal = state.isFinal
        transitions = state.transitions.map { return Transition(transition: $0) }
    }

    public var description: String {
        return "State \(stateNumber)" + ((isInitial) ? " initial;" : "")
            + ((isFinal) ? " final;" : "")
            + ((transitions.isEmpty)
                ? ""
                : "\n\(transitions.map {return String(describing: $0)}.joined(separator: "\n"))")
    }

    func addTransitions(_ transitions: [Transition]) {
        self.transitions.appendIfAbsent(transitions)
    }

    func addTransition(_ transition: Transition) {
        transitions.appendIfAbsent(transition)
    }

    func getAsTriples(_ arr: inout [(Int, String, Int)]) {
        for transition in transitions {
            arr.append((stateNumber, transition.identifier(), transition.goto.stateNumber))
        }
    }

    func getSuccessor(_ label: Label) -> Set<FiniteStateMachineState> {
        var return_val = Set<FiniteStateMachineState>()

        transitions.do {
            if $0.label == label {
                return_val.insert($0.goto)
            }
        }

        return return_val
    }

    public func transitionsDo(_ operation: (inout Transition) -> Void) {
        for var transition in transitions {
            operation(&transition)
        }
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(stateNumber)
        hasher.combine(isInitial)
        hasher.combine(isFinal)
        hasher.combine(transitions.count)
    }
}

public class DualFiniteStateMachineState: FiniteStateMachineState {
    var left: Set<FiniteStateMachineState>
    var right: Set<FiniteStateMachineState>

    override init() {
        left = Set<FiniteStateMachineState>()
        right = Set<FiniteStateMachineState>()
        super.init()
    }

    public func leftHasFinal() -> Bool {
        return left.contains { $0.isFinal }
    }

    public func rightHasFinal() -> Bool {
        return right.contains { $0.isFinal }
    }

    public func getLabels() -> Set<Label> {
        var return_val = Set<Label>()

        for state in left.union(right) {
            state.transitions.do {
                return_val.insert($0.label)
            }
        }

        return return_val
    }

    public func getSuccessor(_ label: Label) -> DualFiniteStateMachineState {
        var return_val = DualFiniteStateMachineState()

        for state in left {
            Array(state.getSuccessor(label)).do {
                return_val.left.insert($0)
            }
        }

        for state in right {
            Array(state.getSuccessor(label)).do {
                return_val.left.insert($0)
            }
        }

        return return_val
    }

    public static func == (lhs: DualFiniteStateMachineState, rhs: DualFiniteStateMachineState)
        -> Bool
    {
        return lhs.left == rhs.left && lhs.right == rhs.right
    }

    public var dual_description: String {
        return "Left:\n" + left.map { "\($0)" }.joined<String>(separator: "\n") + "\n\nRight:\n"
            + right.map { "\($0)" }.joined<String>(separator: "\n")
    }
}

public class Transition: CustomStringConvertible, Hashable {
    var label: Label
    var goto: FiniteStateMachineState = FiniteStateMachineState()

    init(name: String) {
        label = Label(name: name)
    }

    init(action: String, parameters: [Any], isRootBuilding: Bool) {
        label = Label(action: action, parameters: parameters, isRootBuilding: isRootBuilding)
    }

    convenience init(transition: Transition) {
        self.init(label: transition.label)
    }

    init(label: Label) {
        self.label = label
        goto = FiniteStateMachineState()  // temp endpoint
    }

    func setAttributes(_ attributes: AttributeList) {
        label.setAttributes(attributes)
    }

    func contents() -> Any {
        return label.contents()
    }

    func identifier() -> String {
        return label.identifier()
    }

    func hasAttributes() -> Bool { return label.hasAttributes() }
    func hasAction() -> Bool { return label.hasAction() }

    func override(_ attributes: [String]) {
        if self.hasAction() { return }
        label.attributes.override(attributes)
    }

    func override(_ attributes: AttributeList) {
        if self.hasAction() { return }
        setAttributes(attributes)
    }


    public var description: String {
        return "\(label)\ngoto \(goto.stateNumber)"
    }

    public static func == (lhs: Transition, rhs: Transition) -> Bool {
        return lhs.label == rhs.label
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(label)
    }

}

public class Label: Hashable, CustomStringConvertible {
    var name: String = ""
    var attributes: AttributeList = AttributeList()
    var action: String = ""
    var parameters: [Any] = []
    var isRootBuilding: Bool = false

    init(name: String) {
        self.name = name
        attributes = AttributeList(attributes: Grammar.defaultsFor(name))
    }

    init(action: String, parameters: [Any], isRootBuilding: Bool) {
        self.action = action
        self.isRootBuilding = isRootBuilding
        self.parameters = parameters
    }

    public func setAttributes(_ attributes: AttributeList) {
        self.attributes = attributes
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(name)
        hasher.combine(action)
        hasher.combine(attributes)
        hasher.combine(parameters.count)
        hasher.combine(isRootBuilding)
    }

    func hasAttributes() -> Bool { return name != "" }
    func hasAction() -> Bool { return action != "" }

    func contents() -> Any {
        return (hasAction()) ? parameters : attributes
    }
    func identifier() -> String {
        return (hasAction()) ? action : name
    }

    public var description: String {
        return
            ((hasAttributes())
            ? "    \(name) \"\(attributes)\""
            : "    \(action) \"\(parameters)\" \n" + "    isRootBuilding: \(isRootBuilding)")
    }

    public static func == (lhs: Label, rhs: Label) -> Bool {
        return lhs.name == rhs.name && lhs.action == rhs.action && lhs.attributes == rhs.attributes
            && lhs.isRootBuilding == rhs.isRootBuilding
            && lhs.parameters.count == rhs.parameters.count; // TODO : Fix parameter equality
    }
}

//
public class AttributeList: CustomStringConvertible, Hashable {
    var isRead: Bool = false
    var isStack: Bool = false
    var isKeep: Bool = false
    var isNode: Bool = false

    init() {}

    init(attributes: AttributeList) {
        isRead = attributes.isRead
        isStack = attributes.isStack
        isKeep = attributes.isKeep
        isNode = attributes.isNode
    }

    func set(_ attributes: [String]) -> AttributeList {
        for string in attributes {
            if string == "read" { isRead = true }
            if string == "look" { isRead = false }
            if string == "stack" { isStack = true }
            if string == "noStack" { isStack = false }
            if string == "keep" { isKeep = true }
            if string == "noKeep" { isKeep = false }
            if string == "node" { isNode = true }
            if string == "noNode" { isNode = false }
        }
        return self
    }

    static func fromString(_ attributes: String) -> AttributeList {
        //Convert from the description notation below to an attribute list.
        var attributeList: AttributeList = AttributeList()
        attributeList.isRead = attributes.contains("R")  //"R" versus "L"
        attributeList.isStack = attributes.contains("S")  //"S" versus no "S"
        attributeList.isKeep = attributes.contains("K")  //"K" versus no "K"
        attributeList.isNode = attributes.contains("N")  //"N" versus no "N"
        return attributeList
    }

    public var description: String {
        if !isRead { return "L" }
        return ("R") + (isStack ? "S" : "") + (isKeep ? "K" : "") + (isNode ? "N" : "")
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(isRead)
        hasher.combine(isStack)
        hasher.combine(isKeep)
        hasher.combine(isNode)
    }

    public func override(_ attributes: [String]) {
        set(attributes)
    }

    public static func == (lhs: AttributeList, rhs: AttributeList) -> Bool {
        return lhs.isRead == rhs.isRead && lhs.isStack == rhs.isStack && lhs.isKeep == rhs.isKeep
            && lhs.isNode == rhs.isNode
    }
}
