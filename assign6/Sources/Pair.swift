//
//  Pair.swift
//
//  Created by Wilf Lalonde on 2023-03-09.
//

import Foundation

public class Pair : Relatable {
    public static func < (lhs: Pair, rhs: Pair) -> Bool {
        return true
    }

    var item1 : Any = 0
    var item2: Any = 0
    
    //Creating...
    
    init (_ labelOrState: Any, _ readaheadState: Any) {item1 = labelOrState; item2 = readaheadState}
    //init (_ labelOrState: AnyObject, _ readaheadState: AnyObject) {item1 = labelOrState; item2 = readaheadState}
    
    //Get/set methods...
    
    func first () -> Any {return item1}
    func second () -> Any {return item2}
    
    //Comparing
    
    public static func == (pair1: Pair, pair2: Pair) -> Bool {
        let labelPair1 = pair1.isLabelPair ()
        let labelPair2 = pair2.isLabelPair ()
        if labelPair1 != labelPair2 {return false}
        
        if (labelPair1) {
            let label1 = pair1.item1 as? Label
            let label2 = pair2.item1 as? Label
            if label1 != label2 {return false}
        } else {
            let fsm1 = pair1.item1 as? FiniteStateMachineState
            let fsm2 = pair2.item1 as? FiniteStateMachineState
            if fsm1 !== fsm2 {return false}
        }
        
        let fsm1 = pair1.item2 as? FiniteStateMachineState
        let fsm2 = pair2.item2 as? FiniteStateMachineState
        if fsm1 !== fsm2 {return false}
        
        return true
    }
    
    public func hash(into hasher: inout Hasher) {
        if isLabelPair () {
            let label1 = item1 as? Label
            let fsm2 = item2 as? FiniteStateMachineState
            hasher.combine(label1); hasher.combine(fsm2)
        } else {
            let fsm1 = item1 as? FiniteStateMachineState
            let fsm2 = item2 as? FiniteStateMachineState
            hasher.combine(fsm1); hasher.combine(fsm2)
        }
    }
    
    //Querying...
    func comment () -> String {
        return """
        A pair is either a state pair where the first item is a right part state
        and the second item is a readahead state, or a label pair where the first item is a standard label
        and the second item is a readahead state. For a label pair, the description method will put the 
        standard label and the second item together to make it look like a two component label
        """
    }
    
    func isLabelPair () -> Bool {
        if let label = item1 as? Label {return true}
        return false
    }
    func isStatePair() -> Bool {
        if let fsm = item1 as? FiniteStateMachineState {return true}
        return false
    }
    
    func nonterminal () -> String {
        if (isStatePair ()) {return (item1 as? FiniteStateMachineState)!.leftPart}
        error ("Label pairs cannot refer to nonterminals")
        return ""
    }
    
    func isInitial () -> Bool {
        if (isStatePair ()) {return (item1 as? FiniteStateMachineState)!.isInitial}
        error ("Label pairs cannot be initial")
        return false
    }
    
    func isFinal () -> Bool {
        if (isStatePair ()) {return (item1 as? FiniteStateMachineState)!.isFinal}
        error ("Label pairs cannot be final")
        return false
    }

    func isVisible () -> Bool {
       //Semantic actions and look transitions are invisible; otherwise visible.
       if (isLabelPair ()) {error ("State pairs cannot be visible/invisible")}
       let label = item1 as! Label
       if (label.hasAction ()) {return false}
       //So if its not an action, it's a symbol with attributes.
       return label.attributes.isRead  //read is visible, look is invisible
    }
    
    //Printing...
    
    public var terseDescription: String {
        if (self.isStatePair ()) {
            return "[" + "\n(item1.stateNumber)" + ", " + "\n(item2.stateNumber)]"
        } else {
            return "[" + "\n(item1)" + ", " + "\n(item2.stateNumber)]"
        }
    }
    
    //Converting...
    
    func asLook () -> Pair {
        if self.isLabelPair() {
            var pair: Pair = Pair (item1, item2)
            if let label = pair.item1 as? Label {
                label.attributes = AttributeList ().set (["look", "noStack", "noKeep", "noNode"])
                return pair
            }
        }
        error ("State pairs cannot be looks")
        return self
    }

    public var description: String{
        "\(item1) \(item2)"
    }
    
}
 
