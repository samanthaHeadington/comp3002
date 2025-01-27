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

    func renumber () {
       for (index, state) in states.enumerated() {
          state.stateNumber = index
       }
    }

    func addState(_ state: FiniteStateMachineState){
        state.stateNumber = states.count;
        states.append(state);
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

    init (state_number: Int) {
        stateNumber = state_number;
        transitions = []
    }

    init(state: FiniteStateMachineState){
        stateNumber = state.stateNumber;
        isInitial = state.isInitial;
        isFinal = state.isFinal;
        transitions = state.transitions.map{return Transition(transition: $0)};
    }

    func addTransition(name : String, attributes : AttributeList, goto : FiniteStateMachineState? = nil){
        transitions.append(Transition(name: name, goto: (goto == nil) ? self : goto!));
        transitions[transitions.count - 1].attributes = attributes;
    }

    func addSemanticAction(action : String, parameters : Array<Any>){
        transitions.append(Transition(action: action, parameters: parameters))
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
    var goto: FiniteStateMachineState

    init(name : String, goto : FiniteStateMachineState){
        self.name = name;
        self.goto = goto;
    }

    init(action : String, parameters : Array<Any>){
        self.action = action;
        goto = FiniteStateMachineState();
    }

    init(transition: Transition){
        name = transition.name;
        attributes = AttributeList(attributes: transition.attributes);
        action = transition.action;
        parameters = transition.parameters;
        isRootBuilding = transition.isRootBuilding;
        goto = FiniteStateMachineState(state_number: 0); // temp endpoint
    }
    
    func hasAttributes () -> Bool {return name != ""}
    func hasAction () -> Bool {return action != ""}

    func override (_ attributes: Array<String>) {
        if self.hasAction () {return}
        self.attributes.override (attributes)
    }

    public var description: String{
        return (action == "") ? 
            "    \(name) \"\(attributes)\"\n" +
            "goto \(goto.stateNumber)"
        :
            "    \(action) \"\(parameters)\"";
    }

}

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
