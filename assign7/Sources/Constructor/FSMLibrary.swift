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

    func transitionsDo(_ operation: (inout Transition) -> Transition) {
        states.do {
            $0.transitionsDo { transition in
                operation(&transition)
            }
        }
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

    func initialStates() -> [FiniteStateMachineState] {
        return states.filter { $0.isInitial }
    }

    func renumber() {
        renumberFrom(0)
    }

    func renumberFrom(_ start: Int) {
        for (index, state) in states.enumerated() {
            state.stateNumber = index + start
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

        rhs.states.do {
            lhs.addState($0)
        }

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

        states.do {
            for i in (0..<$0.transitions.count).reversed() {
                if !is_useful_state[$0.transitions[i].goto.stateNumber] {
                    $0.transitions.remove(at: i)
                }
            }
        }

        renumber()
    }

    func transitionNames() -> [String] {
        var return_val: [String] = []
        states.do {
            $0.transitions.do { transition in
                return_val.append(transition.label.terseDescription)
            }
        }

        return return_val
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

        var initial_state = DualFiniteStateMachineState()
        initial_state.isInitial = true

        for state in lhs.states where state.isInitial {
            initial_state.left.insert(state)
        }
        for state in rhs.states where state.isInitial {
            initial_state.right.insert(state)
        }

        var state_queue: [DualFiniteStateMachineState] = [initial_state]
        var i: Int = 0

        while i < state_queue.count {
            state_queue[i].isFinal = isFinal(state_queue[i])

            var existing_val: DualFiniteStateMachineState?

            for (key, value) in state_queue[i].getSuccessors() {
                existing_val = state_queue.first {
                    DualFiniteStateMachineState.equal(lhs: value, rhs: $0)
                }
                if existing_val != nil {
                    state_queue[i].addTransition(
                        Transition(
                            label: key, goto: existing_val!))
                } else {
                    state_queue[i].addTransition(Transition(label: key, goto: value))
                    state_queue.append(value)
                }
            }

            i += 1
        }

        var return_val = FiniteStateMachine(states: state_queue)

        return_val.reduce()

        return return_val
    }

    static func forAction(_ action: String, parameters: [AnyHashable], isRootBuilding: Bool)
        -> FiniteStateMachine
    {
        return fromTransition(
            Transition(
                action: action, parameters: parameters, isRootBuilding: isRootBuilding))
    }

    static func forString(_ string: String) -> FiniteStateMachine {
        return (Grammar.activeGrammar!.isScanner())
            ? forStringScanner(string) : forStringParser(string)
    }

    private static func forStringScanner(_ string: String) -> FiniteStateMachine {
        return fromTransitions(string.map { Transition(name: $0) } as! [Transition])
    }

    private static func forStringParser(_ string: String) -> FiniteStateMachine {
        return fromTransition(Transition(name: string))
    }

    static func forCharacter(_ character: Character) -> FiniteStateMachine {
        return fromTransition(Transition(name: String(character)))
    }

    private static func intAsString(_ integer: Int) -> String {
        return (integer > 32 && integer < 127)
            ? String(Character(UnicodeScalar(integer)!)) : String(integer)
    }

    static func forInteger(_ integer: Int) -> FiniteStateMachine {
        if integer < 33 || integer > 126 {
            return fromTransition(
                Transition(label: Label(name: intAsString(integer), printable: false)))
        } else {
            return fromTransition(
                Transition(
                    name: intAsString(integer)))
        }
    }

    static func forDotDot(_ start: Int, _ end: Int) -> FiniteStateMachine {
        var dotdot_string = ""
        for i in start...end {
            dotdot_string.append(intAsString(i))
        }
        return forString(dotdot_string)
    }

    static func fromTransitions(_ transitions: [Transition]) -> FiniteStateMachine {
        var return_val = fromTransition(transitions[0])

        return_val.states[0].addTransitions(Array(transitions[1..<transitions.count]))
        return_val.states[0].transitionsDo {
            $0.goto = return_val.states[1]
        }

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

    func transitionsDo(_ operation: (Transition) -> Void) {
        states.do { $0.transitions.do(operation) }
    }

    func addState(_ state: FiniteStateMachineState) {
        states.appendIfIdenticalAbsent(state)
    }

    public var description: String {
        return states.map { return String(describing: $0) }.joined(separator: "\n")
    }
}

public class FiniteStateMachineState: Relatable {
    var stateNumber: Int = 0
    var isInitial: Bool = false
    var isFinal: Bool = false
    var transitions: [Transition]
    var leftPart: String = ""

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
        leftPart = state.leftPart
    }

    public var description: String {
        return terseDescription + ((isInitial) ? " initial;" : "")
            + ((isFinal) ? " final;" : "")
            + ((transitions.isEmpty)
                ? ""
                : "\n\(transitions.map {return String(describing: $0)}.joined(separator: "\n"))")
    }

    public var terseDescription: String {
        return "State \(stateNumber)"
    }

    func addTransitions(_ transitions: [Transition]) {
        self.transitions.appendIfAbsent(transitions)
    }

    func addTransition(_ transition: Transition) {
        transitions.appendIfAbsent(transition)
    }

    func getAsTriples(_ arr: inout [(Int, String, Int)]) {
        for transition in transitions {
            arr.append((stateNumber, transition.description, transition.goto.stateNumber))
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
        hasher.combine(ObjectIdentifier(self))
    }
}

public class ReadaheadState: FiniteStateMachineState {
    var items: [FiniteStateMachineState]

    public init(_ items: [FiniteStateMachineState]) {
        self.items = items
        super.init()
    }

    override public var terseDescription: String {
        return "ReadaheadState \(stateNumber) \(items.map{$0.stateNumber})"
    }
}

public class ReadbackState: FiniteStateMachineState {
    var items: [Pair] = []

    public init(items: [Pair]) {
        self.items.append(contentsOf: items)
        super.init()
    }

    override public var terseDescription: String {
        return
            "Readback \(stateNumber) \(items.map{"(\(($0.first() as! FiniteStateMachineState).stateNumber), ReadaheadState \(($0.second() as! FiniteStateMachineState).stateNumber))"})"
    }
}

public class ShiftState: FiniteStateMachineState {
    var shiftVal: Int
    var goto: FiniteStateMachineState

    init(_ shiftVal: Int, _ goto: FiniteStateMachineState) {
        self.shiftVal = shiftVal
        self.goto = goto
        super.init()
    }

    override public var terseDescription: String {
        return "Shift \(stateNumber) by \(shiftVal), goto \(goto.stateNumber)"
    }
}

public class ReduceState: FiniteStateMachineState {
    var nonterminal: String
    var restarts: [FiniteStateMachineState: [FiniteStateMachineState]] = [:]

    init(_ nonterminal: String) {
        self.nonterminal = nonterminal
        super.init()
    }

    override public var terseDescription: String {
        return "Reduce to \(nonterminal)"  //: (\(restarts.map{"\($0.key): [\($0.value.map{state in "\(state.terseDescription)"}.joined(separator: ", "))]"}))"
    }

}

public class SemanticState: FiniteStateMachineState {
    var label: Label
    var goto: FiniteStateMachineState

    init(_ label: Label, goto: FiniteStateMachineState) {
        self.label = label
        self.goto = goto

        super.init()
    }

    override public var terseDescription: String {
        return "SemanticState \(stateNumber) \n\(label) \ngoto \(goto.stateNumber)"
    }
}

public class AcceptState: FiniteStateMachineState {
    override public var terseDescription: String {
        return "AcceptState \(stateNumber)"
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

    public func getSuccessors() -> [Label: DualFiniteStateMachineState] {
        var return_val = [Label: DualFiniteStateMachineState]()
        var left_successors = [Label: [FiniteStateMachineState]]()
        var right_successors = [Label: [FiniteStateMachineState]]()

        for state in left {
            state.transitionsDo {
                if left_successors[$0.label] != nil {
                    left_successors[$0.label]!.append($0.goto)
                } else {
                    left_successors[$0.label] = [$0.goto]
                }
            }
        }

        for state in right {
            state.transitionsDo {
                if right_successors[$0.label] != nil {
                    right_successors[$0.label]!.append($0.goto)
                } else {
                    right_successors[$0.label] = [$0.goto]
                }
            }
        }

        for (key, value) in left_successors {
            return_val[key] = DualFiniteStateMachineState()
            for state in value {
                return_val[key]!.left.insert(state)
            }
        }

        for (key, value) in right_successors {
            if return_val[key] == nil {
                return_val[key] = DualFiniteStateMachineState()
            }
            value.do {
                return_val[key]!.right.insert($0)
            }
        }

        return return_val
    }

    public override func hash(into hasher: inout Hasher) {
        for state in left {
            hasher.combine(state)
        }
        hasher.combine(left.count)
        hasher.combine(right.count)
    }

    public static func equal(lhs: DualFiniteStateMachineState, rhs: DualFiniteStateMachineState)
        -> Bool
    {
        return lhs.left == rhs.left && lhs.right == rhs.right
    }

    public var dual_description: String {
        return "Left:\n" + left.map { "\($0)" }.joined<String>(separator: "\n") + "\n\nRight:\n"
            + right.map { "\($0)" }.joined<String>(separator: "\n")
    }
}

public class Transition: Relatable {
    var label: Label
    var goto: FiniteStateMachineState = FiniteStateMachineState()

    init(name: String) {
        label = Label(name: name)
    }

    init(action: String, parameters: [AnyHashable], isRootBuilding: Bool) {
        label = Label(action: action, parameters: parameters, isRootBuilding: isRootBuilding)
    }

    convenience init(transition: Transition) {
        self.init(label: transition.label)
    }

    init(label: Label, goto: FiniteStateMachineState) {
        self.label = Label(label: label)
        self.goto = goto  // temp endpoint
    }

    convenience init(label: Label) {
        self.init(label: label, goto: FiniteStateMachineState())
    }

    func setAttributes(_ attributes: AttributeList) {
        label.setAttributes(attributes)
    }

    func contents() -> Any {
        return label.contents()
    }

    var terseDescription: String {
        return label.terseDescription
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
        return "    \(label)\ngoto \(goto.stateNumber)"
    }

    public static func == (lhs: Transition, rhs: Transition) -> Bool {
        return lhs.label == rhs.label
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(label)
        hasher.combine(ObjectIdentifier(goto))
    }

}

public class Label: Relatable, Comparable {
    public static func < (lhs: Label, rhs: Label) -> Bool {
        return lhs.terseDescription < rhs.terseDescription
    }

    var name: String?
    var attributes: AttributeList = AttributeList()
    var action: String = ""
    var parameters: [AnyHashable] = []
    var isRootBuilding: Bool = false
    var predecessor: FiniteStateMachineState?
    var isPrintable: Bool = true

    convenience init(name: String) {
        self.init(name: name, printable: true)
    }

    init(name: String, printable: Bool) {
        self.name = name
        attributes = AttributeList(attributes: Grammar.defaultsFor(String(name)))
        self.isPrintable = printable
    }

    init(label: Label, predecessor: FiniteStateMachineState?) {
        name = label.name
        attributes = AttributeList(attributes: label.attributes)
        action = label.action
        parameters.append(contentsOf: label.parameters)
        isRootBuilding = label.isRootBuilding
        isPrintable = label.isPrintable
        if predecessor != nil {
            self.predecessor = predecessor
        } else {
            self.predecessor = label.predecessor
        }
    }

    convenience init(label: Label) {
        self.init(label: label, predecessor: nil)
    }

    init(action: String, parameters: [AnyHashable], isRootBuilding: Bool) {
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
        hasher.combine(parameters)
        hasher.combine(isRootBuilding)
        hasher.combine(isPrintable)
    }

    func hasAttributes() -> Bool { return name != nil }
    func hasAction() -> Bool { return action != "" }
    func isVisible() -> Bool { return hasAttributes() && attributes.isRead }
    func isInvisible() -> Bool { return hasAction() || !attributes.isRead }
    func printable() -> Bool { return isPrintable }

    func contents() -> Any {
        return (hasAction()) ? parameters : attributes
    }
    var terseDescription: String {
        return "\((hasAttributes()) ? name! : action)"
    }

    func asLook() -> Label {
        let new_label = Label(label: self)
        new_label.attributes.override(["look"])
        return new_label
    }

    public var description: String {
        return "\(terseDescription) "
            + ((hasAttributes())
                ? "\"\(attributes)\""
                : "\"\(parameters.map{String(describing: $0)})\" \n"
                    + "    isRootBuilding: \(isRootBuilding)")
            + ((predecessor != nil) ? ", \(predecessor!.stateNumber)" : "")
    }

    public static func == (lhs: Label, rhs: Label) -> Bool {
        return lhs.name == rhs.name && lhs.attributes == rhs.attributes
            && rhs.action == lhs.action
            && lhs.isRootBuilding == rhs.isRootBuilding
            && lhs.isPrintable == rhs.isPrintable
            && Set(lhs.parameters) == Set(rhs.parameters)
    }
}

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
