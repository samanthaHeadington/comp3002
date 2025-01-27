//
//  FSMLibrary.swift
//  FSMBuilder
//
//  Created by Wilf Lalonde on 2023-01-24.
//

import Foundation

public class FiniteStateMachine : CustomStringConvertible {
    var states: Array<FiniteStateMachineState>
    
    init () {states = []}

    init (fsm: FiniteStateMachine){
        states = fsm.states.map{return FiniteStateMachineState(state: $0)};

        for i in 0..<states.count{
            for j in 0..<states[i].transitions.count{
                states[i].transitions[j].goto = states[fsm.states[i].transitions[j].goto.stateNumber];
            }
        }
    }

    func override (_ attributes: Array<String>) {
	for state in states {
	    for transition in state.transitions {
		transition.override (attributes)
            }
        }
    }

    func override (_ attributes: AttributeList) {
        for state in states {
            for transition in state.transitions {
                transition.attributes = AttributeList(attributes: attributes)
            }
        }
    }

    func renumber () {
       for (index, state) in states.enumerated() {
          state.stateNumber = index
       }
    }

    static func forAction(_ action : String, parameters : Array<Any>, isRootBuilding : Bool) -> FiniteStateMachine{
        var transition = Transition(action: action, parameters: parameters, isRootBuilding : isRootBuilding);

        return fromTransition(transition);
    }

    static func forIdentifier(_ identifer : String) -> FiniteStateMachine{
        return fromTransition(Transition(name: identifer));
    }

    static func forSymbol(_ symbol : String) -> FiniteStateMachine{
        return fromTransition(Transition(name: symbol));
    }

    static func forString(_ string : String) -> FiniteStateMachine{
        return fromTransition(Transition(name: string));
    }

    static func forCharacter(_ character : String) -> FiniteStateMachine{
        var name = character;
        name.insert(contentsOf: "$", at: name.startIndex);
        return fromTransition(Transition(name: name))
    }

    static func forInteger(_ integer : String) -> FiniteStateMachine{
        return fromTransition(Transition(name: integer));
    }

    static func fromTransition(_ transition : Transition) -> FiniteStateMachine{
        var return_val = FiniteStateMachine();

        return_val.states.append(FiniteStateMachineState());
        return_val.states.append(FiniteStateMachineState());

        return_val.renumber();

        return_val.states[0].transitions.append(transition);
        transition.goto = return_val.states[1];

        return_val.override(Grammar.defaultsFor(transition.name));

        return return_val;
    }

    public var description: String {
        return states.map {return String(describing: $0)}.joined(separator: "\n");
    }
}

public class FiniteStateMachineState : CustomStringConvertible {
    var stateNumber: Int = 0
    var isInitial: Bool = false
    var isFinal: Bool = false
    var transitions: Array<Transition>
    
    init(){
        transitions = [];
    }

    init(state: FiniteStateMachineState){
        stateNumber = state.stateNumber;
        isInitial = state.isInitial;
        isFinal = state.isFinal;
        transitions = state.transitions.map{return Transition(transition: $0)};
    }

    public var description : String {
        return "State \(stateNumber)" +
            ((isInitial) ? "initial; " : "") +
            ((isFinal) ? "final;" : "") +
            "\n\(transitions.map {return String(describing: $0)}.joined(separator: "\n"))";
    }
}

public class Transition : CustomStringConvertible{
    var name: String = ""
    var attributes: AttributeList = AttributeList ().set (["look", "noStack", "noKeep", "noNode"])
    var action: String = ""
    var parameters: Array<Any> = []
    var isRootBuilding: Bool = false
    var goto: FiniteStateMachineState = FiniteStateMachineState();

    init(name : String){
        self.name = name;
    }

    init(action : String, parameters : Array<Any>, isRootBuilding : Bool){
        self.action = action;
        self.isRootBuilding = isRootBuilding;
        self.parameters = parameters;
    }

    init(transition: Transition){
        name = transition.name;
        attributes = AttributeList(attributes: transition.attributes);
        action = transition.action;
        parameters = transition.parameters;
        isRootBuilding = transition.isRootBuilding;
        goto = FiniteStateMachineState(); // temp endpoint
    }
    
    func hasAttributes () -> Bool {return name != ""}
    func hasAction () -> Bool {return action != ""}

    func override (_ attributes: Array<String>) {
        if self.hasAction () {return}
        self.attributes.override (attributes)
    }

    public var description: String{
        return ((action == "") ? 
            "    \(name) \"\(attributes)\""
        :
            "    \(action) \"\(parameters)\"") +
        "\ngoto \(goto.stateNumber)";
    }

}
//
public class AttributeList : CustomStringConvertible{
    var isRead: Bool = false
    var isStack: Bool = false
    var isKeep: Bool = false
    var isNode: Bool = false

    init(){}

    init(attributes: AttributeList){
        isRead = attributes.isRead;
        isStack = attributes.isStack;
        isKeep = attributes.isKeep;
        isNode = attributes.isNode;
    }
    
    func set (_ attributes: Array<String>) -> AttributeList {
        for string in attributes {
            if (string == "read") {isRead = true;}
            if (string == "look") {isRead = false;}
            if (string == "stack") {isStack = true;}
            if (string == "noStack") {isStack = false;}
            if (string == "keep") {isKeep = true;}
            if (string == "noKeep") {isKeep = false;}
            if (string == "node") {isNode = true;}
            if (string == "noNode") {isNode = false;}
        }
        return self
    }

    static func fromString (_ attributes: String) -> AttributeList {
	//Convert from the description notation below to an attribute list.
        var attributeList: AttributeList = AttributeList();
        attributeList.isRead = attributes.contains("R") //"R" versus "L" 
        attributeList.isStack = attributes.contains("S") //"S" versus no "S"
        attributeList.isKeep = attributes.contains("K") //"K" versus no "K"
        attributeList.isNode = attributes.contains("N")  //"N" versus no "N"
        return attributeList
    }
	
    public var description: String {
        if (!isRead) {return "L"};
        return ("R") + (isStack ? "S" : "") + (isKeep ? "K" : "") + (isNode ? "N" : "")
    }

    public func override (_ attributes: Array<String>) {
	    set (attributes)
    }
}
