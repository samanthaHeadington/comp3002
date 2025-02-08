//
//  FSMBuilder.swift
//  FSMBuilder
//
//  Created by Wilf Lalonde on 2023-01-24.
//

import Foundation

typealias treeClosure = (VirtualTree) -> Any  //Ultimately FSM

public final class FSMBuilder: Translator {
    var parser: Parser?
    var tree: VirtualTree? = nil
    var fsmMap: [String: FiniteStateMachine] = [:]  //Ultimately FSM
    var symbolOnly: Bool = false  // when symbolOnly is true, the builder stops constructing FSMs and starts returning token symbols instead

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
        // case "walkTreeOrTokenFromName":
        // return walkTreeOr
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
            error("Attempt to perform unknown walkTree routine \(action)")
            return 0
        }
    }

    public func canPerformAction(_ action: String) -> Bool {
        if action == "processTypeNow" { return true }
        return false
    }

    public func performAction(_ action: String, _ parameters: [Any]) {
        if action == "processTypeNow" {
            processTypeNow(parameters)
        }
    }

    static public func example1(parserFSM parserFSMs: String, scannerFSMs: String) -> String {  //Returns a string to please ContentView
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

        let builder = FSMBuilder()
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
                var text = parserFSMs
                print(text)
                builder.process(text)
                text = scannerFSMs
                builder.process(text)
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
                fsmMap[name] = fsm as! FiniteStateMachine
            }
            Grammar.activeGrammar!.addMacro(name, fsm)
            index += 2
        }

        // print("\n\n\n ~~~ POST LOOP FSMs ~~~");

        // // moved print to post loop to check that copied fsms are not altered
        // for (key, value) in fsmMap{
        //     print ("FSM for \(key) = \n\(value)")
        // }

        return 0
    }

    func walkIdentifier(_ tree: VirtualTree) -> Any {

        var return_val: FiniteStateMachine

        let symbol: String = (tree as! Token).symbol

        if symbolOnly {
            return symbol
        }

        if fsmMap[symbol] != nil {
            return_val = FiniteStateMachine(fsm: fsmMap[symbol]!)
        } else {
            return_val = FiniteStateMachine.forString(symbol)
        }

        return return_val
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
        for i in 1..<t.children.count {
            attributes.append((t.child(i) as! Token).symbol)
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
        return walkSemanticAction(
            constructSemanticTree(
                tree, action: (Grammar.activeGrammar!.isScanner()) ? "buildToken" : "buildTree"),
            treeBuilding: true)
    }

    func walkBuildTreeFromLeftIndex(_ tree: VirtualTree) -> Any {
        return walkSemanticAction(
            constructSemanticTree(tree, action: "buildTreeFromIndex"), treeBuilding: true)
    }

    func walkBuildTreeFromRightIndex(_ tree: VirtualTree) -> Any {
        var semantic_tree = constructSemanticTree(tree, action: "buildTreeFromIndex")

        semantic_tree.children.doWithoutFirst {
            ($0 as! Token).symbol.insert(
                contentsOf: "-", at: ($0 as! Token).label.startIndex)
        }

        return walkSemanticAction(semantic_tree, treeBuilding: true)
    }

    func constructSemanticTree(_ tree: VirtualTree, action: String) -> Tree {
        var semantic_tree: Tree = Tree(
            label: "walkSemanticAction", children: (tree as! Tree).children)

        semantic_tree.children.insert(Token(label: "walkSymbol", symbol: action), at: 0)

        return semantic_tree
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
        symbolOnly = true  // enter symbol only mode

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

    var scannerTables: [Any] =
        [
            [
                "ScannerReadaheadTable", 1, ("'", "R", 9), ("]", "RK", 36), ("/", "R", 10),
                ("{", "RK", 37), ("}", "RK", 38), ("\"", "R", 11), ("$", "R", 12), ([256], "L", 21),
                ("?", "RK", 32), ("ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 6),
                ("(", "RK", 23), (")", "RK", 24), ("*", "RK", 25), ("+", "RK", 26), ("-", "RK", 2),
                ("&", "RK", 22), (".", "RK", 3), ([9, 10, 12, 13, 32], "R", 7),
                ("0123456789", "RK", 4), (";", "RK", 30), ("=", "RK", 5), ("[", "RK", 34),
                ("#", "R", 8), ("|", "RK", 35),
            ],
            [
                "ScannerReadaheadTable", 2, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 27),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<[]{}()^;#:.$'\"",
                    "L", 27
                ), (">", "RK", 39),
            ],
            [
                "ScannerReadaheadTable", 3, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 28),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:$'\"",
                    "L", 28
                ), (".", "RK", 40),
            ],
            [
                "ScannerReadaheadTable", 4, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 29),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!,+-/\\*~=@%&?|<>[]{}()^;#:.$'\"",
                    "L", 29
                ), ("0123456789", "RK", 4),
            ],
            [
                "ScannerReadaheadTable", 5, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 31),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<[]{}()^;#:.$'\"",
                    "L", 31
                ), (">", "RK", 41),
            ],
            [
                "ScannerReadaheadTable", 6, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 33),
                ("!,+-/\\*~=@%&?|<>[]{}()^;#.$'\"", "L", 33),
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
                "ScannerReadaheadTable", 9, ([256], "LK", 43), ("'", "R", 16),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 9),
                (
                    "!\"#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 9
                ),
            ],
            [
                "ScannerReadaheadTable", 10, ([9, 10, 12, 13, 32], "L", 45),
                ([96, 147, 148, 256], "LK", 45),
                (
                    "=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_\\abcdefghijklmnopqrstuvwxyz{|}~!\"#$%&'()*+,-.0123456789:;<",
                    "LK", 45
                ), ("/", "R", 17),
            ],
            [
                "ScannerReadaheadTable", 11, ([256], "LK", 46), ("\"", "R", 18),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 11),
                (
                    "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 11
                ),
            ],
            [
                "ScannerReadaheadTable", 12, ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 47),
                (
                    "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 47
                ),
            ],
            [
                "ScannerReadaheadTable", 13, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 42),
                ("!,+-/\\*~=@%&?|<>[]{}()^;#.$'\"", "L", 42),
                ("0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", "RK", 13),
            ],
            [
                "ScannerReadaheadTable", 14, ([256], "LK", 48), ("\"", "R", 19),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 14),
                (
                    "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 14
                ),
            ],
            [
                "ScannerReadaheadTable", 15, ([256], "LK", 49), ("'", "R", 20),
                ([9, 10, 12, 13, 32, 96, 147, 148], "RK", 15),
                (
                    "!\"#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz{|}~",
                    "RK", 15
                ),
            ],
            [
                "ScannerReadaheadTable", 16, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 44),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$\"",
                    "L", 44
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
                "ScannerReadaheadTable", 18, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 44),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$'",
                    "L", 44
                ), ("\"", "RK", 11),
            ],
            [
                "ScannerReadaheadTable", 19, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 42),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$'",
                    "L", 42
                ), ("\"", "RK", 14),
            ],
            [
                "ScannerReadaheadTable", 20, ([9, 10, 12, 13, 32, 96, 147, 148, 256], "L", 42),
                (
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789!,+-/\\*~=@%&?|<>[]{}()^;#:.$\"",
                    "L", 42
                ), ("'", "RK", 15),
            ],
            ["SemanticTable", 21, "buildToken", ["EndOfFile"], 1],
            ["SemanticTable", 22, "buildToken", ["&"], 1],
            ["SemanticTable", 23, "buildToken", ["("], 1],
            ["SemanticTable", 24, "buildToken", [")"], 1],
            ["SemanticTable", 25, "buildToken", ["*"], 1],
            ["SemanticTable", 26, "buildToken", ["+"], 1],
            ["SemanticTable", 27, "buildToken", ["-"], 1],
            ["SemanticTable", 28, "buildToken", ["."], 1],
            ["SemanticTable", 29, "buildToken", ["walkInteger"], 1],
            ["SemanticTable", 30, "buildToken", [";"], 1],
            ["SemanticTable", 31, "buildToken", ["="], 1],
            ["SemanticTable", 32, "buildToken", ["?"], 1],
            ["SemanticTable", 33, "buildToken", ["walkIdentifier"], 1],
            ["SemanticTable", 34, "buildToken", ["["], 1],
            ["SemanticTable", 35, "buildToken", ["|"], 1],
            ["SemanticTable", 36, "buildToken", ["]"], 1],
            ["SemanticTable", 37, "buildToken", ["{"], 1],
            ["SemanticTable", 38, "buildToken", ["}"], 1],
            ["SemanticTable", 39, "buildToken", ["->"], 1],
            ["SemanticTable", 40, "buildToken", [".."], 1],
            ["SemanticTable", 41, "buildToken", ["=>"], 1],
            ["SemanticTable", 42, "buildToken", ["walkSymbol"], 1],
            [
                "SemanticTable", 43, "syntaxError", ["missing end quote for single quoted string"],
                44,
            ],
            ["SemanticTable", 44, "buildToken", ["walkString"], 1],
            ["SemanticTable", 45, "syntaxError", ["// is a comment, / alone is not valid"], 1],
            [
                "SemanticTable", 46, "syntaxError", ["missing end quote for double quoted string"],
                44,
            ],
            ["SemanticTable", 47, "buildToken", ["walkCharacter"], 1],
            [
                "SemanticTable", 48, "syntaxError", ["missing end quote for double quoted string"],
                42,
            ],
            [
                "SemanticTable", 49, "syntaxError", ["missing end quote for single quoted string"],
                42,
            ],
        ]

    var parserTables: [Any] =
        [
            [
                "keywords", "stack", "noStack", "read", "look", "node", "noNode", "keep", "noKeep",
                "parser", "scanner",
            ],
            [
                "ReadaheadTable", 1, ("parser", "RS", 103), ("scanner", "RS", 104),
                ("GrammarType", "RSN", 2), ("ListOfFiniteStateMachines", "RSN", 105),
            ],
            [
                "ReadaheadTable", 2, ("walkString", "RSN", 39), ("Name", "RSN", 3),
                ("walkIdentifier", "RSN", 39), ("EndOfFile", "L", 31),
            ],
            ["ReadaheadTable", 3, ("=", "RS", 4)],
            [
                "ReadaheadTable", 4, ("Primary", "RSN", 5), ("walkString", "RSN", 39),
                ("Alternation", "RSN", 6), ("Byte", "RSN", 7), ("FiniteStateMachine", "RSN", 8),
                ("walkSymbol", "RSN", 9), ("walkInteger", "RSN", 43), ("SemanticAction", "RSN", 48),
                ("(", "RS", 10), ("walkCharacter", "RSN", 43), ("{", "RS", 11),
                ("walkIdentifier", "RSN", 39), ("Expression", "RSN", 12),
                ("Concatenation", "RSN", 13), ("RepetitionOption", "RSN", 14), ("Name", "RSN", 42),
                (")", "L", 86), ("}", "L", 86), ("=>", "L", 86), (";", "L", 86),
            ],
            [
                "ReadaheadTable", 5, ("[", "RS", 15), ("*", "L", 40), ("?", "L", 40),
                ("+", "L", 40), ("&", "L", 40), ("-", "L", 40), ("(", "L", 40), ("{", "L", 40),
                ("walkIdentifier", "L", 40), ("walkString", "L", 40), ("walkSymbol", "L", 40),
                ("walkCharacter", "L", 40), ("walkInteger", "L", 40), ("|", "L", 40),
                (")", "L", 40), ("}", "L", 40), ("=>", "L", 40), (";", "L", 40),
            ],
            ["ReadaheadTable", 6, ("=>", "RS", 16), (";", "L", 41)],
            [
                "ReadaheadTable", 7, ("..", "RS", 17), ("[", "L", 42), ("*", "L", 42),
                ("?", "L", 42), ("+", "L", 42), ("&", "L", 42), ("-", "L", 42), ("(", "L", 42),
                ("{", "L", 42), ("walkIdentifier", "L", 42), ("walkString", "L", 42),
                ("walkSymbol", "L", 42), ("walkCharacter", "L", 42), ("walkInteger", "L", 42),
                ("|", "L", 42), (")", "L", 42), ("}", "L", 42), ("=>", "L", 42), (";", "L", 42),
            ],
            ["ReadaheadTable", 8, (";", "RS", 18)],
            [
                "ReadaheadTable", 9, ("[", "RS", 19), ("*", "L", 47), ("?", "L", 47),
                ("+", "L", 47), ("&", "L", 47), ("-", "L", 47), ("(", "L", 47), ("{", "L", 47),
                ("walkIdentifier", "L", 47), ("walkString", "L", 47), ("walkSymbol", "L", 47),
                ("walkCharacter", "L", 47), ("walkInteger", "L", 47), (";", "L", 47),
                ("|", "L", 47), (")", "L", 47), ("}", "L", 47), ("=>", "L", 47),
            ],
            [
                "ReadaheadTable", 10, ("Primary", "RSN", 5), ("walkString", "RSN", 39),
                ("Alternation", "RSN", 20), ("Byte", "RSN", 7), ("walkSymbol", "RSN", 9),
                ("walkInteger", "RSN", 43), ("SemanticAction", "RSN", 48), ("(", "RS", 10),
                ("walkCharacter", "RSN", 43), ("{", "RS", 11), ("walkIdentifier", "RSN", 39),
                ("Expression", "RSN", 12), ("Concatenation", "RSN", 13),
                ("RepetitionOption", "RSN", 14), ("Name", "RSN", 42), (")", "L", 86),
                ("}", "L", 86), ("=>", "L", 86), (";", "L", 86),
            ],
            [
                "ReadaheadTable", 11, ("Primary", "RSN", 5), ("Alternation", "RSN", 21),
                ("walkString", "RSN", 39), ("Byte", "RSN", 7), ("walkSymbol", "RSN", 9),
                ("walkInteger", "RSN", 43), ("(", "RS", 10), ("SemanticAction", "RSN", 48),
                ("walkCharacter", "RSN", 43), ("{", "RS", 11), ("walkIdentifier", "RSN", 39),
                ("Expression", "RSN", 12), ("Concatenation", "RSN", 13),
                ("RepetitionOption", "RSN", 14), ("Name", "RSN", 42), (")", "L", 86),
                ("}", "L", 86), ("=>", "L", 86), (";", "L", 86),
            ],
            [
                "ReadaheadTable", 12, ("*", "RS", 52), ("?", "RS", 53), ("+", "RS", 54),
                ("&", "RS", 22), ("-", "RS", 23), ("(", "L", 44), ("{", "L", 44),
                ("walkIdentifier", "L", 44), ("walkString", "L", 44), ("walkSymbol", "L", 44),
                ("walkCharacter", "L", 44), ("walkInteger", "L", 44), ("|", "L", 44),
                (")", "L", 44), ("}", "L", 44), ("=>", "L", 44), (";", "L", 44),
            ],
            [
                "ReadaheadTable", 13, ("|", "RS", 24), (")", "L", 45), ("}", "L", 45),
                ("=>", "L", 45), (";", "L", 45),
            ],
            [
                "ReadaheadTable", 14, ("Primary", "RSN", 5), ("walkString", "RSN", 39),
                ("Byte", "RSN", 7), ("walkSymbol", "RSN", 9), ("walkInteger", "RSN", 43),
                ("SemanticAction", "RSN", 48), ("(", "RS", 10), ("walkCharacter", "RSN", 43),
                ("{", "RS", 11), ("walkIdentifier", "RSN", 39), ("Expression", "RSN", 12),
                ("RepetitionOption", "RSN", 25), ("Name", "RSN", 42), ("|", "L", 46),
                (")", "L", 46), ("}", "L", 46), ("=>", "L", 46), (";", "L", 46),
            ],
            [
                "ReadaheadTable", 15, ("Attribute", "RSN", 26), ("keep", "RSN", 49),
                ("noNode", "RSN", 49), ("noStack", "RSN", 49), ("]", "RS", 56), ("read", "RSN", 49),
                ("look", "RSN", 49), ("stack", "RSN", 49), ("node", "RSN", 49),
                ("noKeep", "RSN", 49),
            ],
            [
                "ReadaheadTable", 16, ("walkString", "RSN", 39), ("-", "RS", 27),
                ("walkSymbol", "RSN", 9), ("Name", "RSN", 57), ("walkIdentifier", "RSN", 39),
                ("TreeBuildingOptions", "RSN", 58), ("+", "RS", 28), ("walkInteger", "RSN", 59),
                ("SemanticAction", "RSN", 60),
            ],
            [
                "ReadaheadTable", 17, ("Byte", "RSN", 61), ("walkInteger", "RSN", 43),
                ("walkCharacter", "RSN", 43),
            ],
            [
                "ReadaheadTable", 18, ("walkString", "RSN", 39), ("Name", "RSN", 3),
                ("walkIdentifier", "RSN", 39), ("EndOfFile", "L", 31),
            ],
            [
                "ReadaheadTable", 19, ("walkString", "RSN", 39), ("walkSymbol", "RSN", 50),
                ("Name", "RSN", 50), ("walkIdentifier", "RSN", 39), ("Byte", "RSN", 50),
                ("walkCharacter", "RSN", 43), ("]", "RS", 62),
                ("SemanticActionParameter", "RSN", 29), ("walkInteger", "RSN", 43),
            ],
            ["ReadaheadTable", 20, (")", "RS", 51)],
            ["ReadaheadTable", 21, ("}", "RS", 63)],
            [
                "ReadaheadTable", 22, ("walkSymbol", "RSN", 9), ("walkString", "RSN", 39),
                ("Primary", "RSN", 5), ("Expression", "RSN", 64), ("Name", "RSN", 42),
                ("Byte", "RSN", 7), ("walkIdentifier", "RSN", 39), ("{", "RS", 11),
                ("walkCharacter", "RSN", 43), ("SemanticAction", "RSN", 48),
                ("walkInteger", "RSN", 43), ("(", "RS", 10),
            ],
            [
                "ReadaheadTable", 23, ("walkSymbol", "RSN", 9), ("walkString", "RSN", 39),
                ("Primary", "RSN", 5), ("Expression", "RSN", 65), ("Name", "RSN", 42),
                ("Byte", "RSN", 7), ("walkIdentifier", "RSN", 39), ("{", "RS", 11),
                ("walkCharacter", "RSN", 43), ("SemanticAction", "RSN", 48),
                ("walkInteger", "RSN", 43), ("(", "RS", 10),
            ],
            [
                "ReadaheadTable", 24, ("Primary", "RSN", 5), ("walkString", "RSN", 39),
                ("Byte", "RSN", 7), ("walkSymbol", "RSN", 9), ("walkInteger", "RSN", 43),
                ("SemanticAction", "RSN", 48), ("(", "RS", 10), ("walkCharacter", "RSN", 43),
                ("{", "RS", 11), ("walkIdentifier", "RSN", 39), ("Expression", "RSN", 12),
                ("Concatenation", "RSN", 30), ("RepetitionOption", "RSN", 14), ("Name", "RSN", 42),
            ],
            [
                "ReadaheadTable", 25, ("Primary", "RSN", 5), ("walkString", "RSN", 39),
                ("Byte", "RSN", 7), ("walkSymbol", "RSN", 9), ("walkInteger", "RSN", 43),
                ("SemanticAction", "RSN", 48), ("(", "RS", 10), ("walkCharacter", "RSN", 43),
                ("{", "RS", 11), ("walkIdentifier", "RSN", 39), ("Expression", "RSN", 12),
                ("RepetitionOption", "RSN", 25), ("Name", "RSN", 42), ("|", "L", 55),
                (")", "L", 55), ("}", "L", 55), ("=>", "L", 55), (";", "L", 55),
            ],
            [
                "ReadaheadTable", 26, ("Attribute", "RSN", 26), ("keep", "RSN", 49),
                ("noNode", "RSN", 49), ("noStack", "RSN", 49), ("]", "RS", 56), ("read", "RSN", 49),
                ("look", "RSN", 49), ("stack", "RSN", 49), ("node", "RSN", 49),
                ("noKeep", "RSN", 49),
            ],
            ["ReadaheadTable", 27, ("walkInteger", "RSN", 67)],
            ["ReadaheadTable", 28, ("walkInteger", "RSN", 59)],
            [
                "ReadaheadTable", 29, ("walkString", "RSN", 39), ("walkSymbol", "RSN", 50),
                ("SemanticActionParameter", "RSN", 29), ("Byte", "RSN", 50), ("]", "RS", 62),
                ("Name", "RSN", 50), ("walkIdentifier", "RSN", 39), ("walkCharacter", "RSN", 43),
                ("walkInteger", "RSN", 43),
            ],
            [
                "ReadaheadTable", 30, ("|", "RS", 24), (")", "L", 66), ("}", "L", 66),
                ("=>", "L", 66), (";", "L", 66),
            ],
            ["ReadbackTable", 31, (("GrammarType", 2), "RSN", 85), ((";", 18), "RS", 68)],
            [
                "ReadbackTable", 32, (("RepetitionOption", 25), "RSN", 32),
                (("RepetitionOption", 14), "RSN", 92),
            ],
            ["ReadbackTable", 33, (("[", 15), "RS", 70), (("Attribute", 26), "RSN", 33)],
            ["ReadbackTable", 34, (("+", 28), "RS", 95), (("=>", 16), "L", 95)],
            [
                "ReadbackTable", 35, (("[", 19), "RS", 47),
                (("SemanticActionParameter", 29), "RSN", 35),
            ],
            [
                "ReadbackTable", 36, (("Concatenation", 30), "RSN", 69),
                (("Concatenation", 13), "RSN", 101),
            ],
            ["ReadbackTable", 37, (("GrammarType", 2), "RSN", 85), ((";", 18), "RS", 68)],
            ["ShiftbackTable", 38, 1, 79],
            ["ShiftbackTable", 39, 1, 75],
            ["ShiftbackTable", 40, 1, 71],
            ["ShiftbackTable", 41, 1, 83],
            ["ShiftbackTable", 42, 1, 80],
            ["ShiftbackTable", 43, 1, 82],
            ["ShiftbackTable", 44, 1, 74],
            ["ShiftbackTable", 45, 1, 81],
            ["ShiftbackTable", 46, 1, 72],
            ["ShiftbackTable", 47, 1, 87],
            ["ShiftbackTable", 48, 1, 88],
            ["ShiftbackTable", 49, 1, 77],
            ["ShiftbackTable", 50, 1, 76],
            ["ShiftbackTable", 51, 3, 80],
            ["ShiftbackTable", 52, 2, 89],
            ["ShiftbackTable", 53, 2, 90],
            ["ShiftbackTable", 54, 2, 91],
            ["ShiftbackTable", 55, 1, 32],
            ["ShiftbackTable", 56, 1, 33],
            ["ShiftbackTable", 57, 1, 94],
            ["ShiftbackTable", 58, 3, 92],
            ["ShiftbackTable", 59, 1, 34],
            ["ShiftbackTable", 60, 1, 96],
            ["ShiftbackTable", 61, 3, 97],
            ["ShiftbackTable", 62, 1, 35],
            ["ShiftbackTable", 63, 3, 98],
            ["ShiftbackTable", 64, 3, 99],
            ["ShiftbackTable", 65, 3, 100],
            ["ShiftbackTable", 66, 2, 36],
            ["ShiftbackTable", 67, 2, 102],
            ["ShiftbackTable", 68, 3, 37],
            ["ShiftbackTable", 69, 1, 36],
            ["ShiftbackTable", 70, 1, 93],
            [
                "ReduceTable", 71, "Expression", (4, "RSN", 12), (10, "RSN", 12), (11, "RSN", 12),
                (14, "RSN", 12), (22, "RSN", 64), (23, "RSN", 65), (24, "RSN", 12), (25, "RSN", 12),
            ],
            [
                "ReduceTable", 72, "Concatenation", (4, "RSN", 13), (10, "RSN", 13),
                (11, "RSN", 13), (24, "RSN", 30),
            ],
            ["ReduceTable", 73, "ListOfFiniteStateMachines", (1, "RSN", 105)],
            [
                "ReduceTable", 74, "RepetitionOption", (4, "RSN", 14), (10, "RSN", 14),
                (11, "RSN", 14), (14, "RSN", 25), (24, "RSN", 14), (25, "RSN", 25),
            ],
            [
                "ReduceTable", 75, "Name", (2, "RSN", 3), (4, "RSN", 42), (10, "RSN", 42),
                (11, "RSN", 42), (14, "RSN", 42), (16, "RSN", 57), (18, "RSN", 3), (19, "RSN", 50),
                (22, "RSN", 42), (23, "RSN", 42), (24, "RSN", 42), (25, "RSN", 42), (29, "RSN", 50),
            ],
            ["ReduceTable", 76, "SemanticActionParameter", (19, "RSN", 29), (29, "RSN", 29)],
            ["ReduceTable", 77, "Attribute", (15, "RSN", 26), (26, "RSN", 26)],
            ["ReduceTable", 78, "TreeBuildingOptions", (16, "RSN", 58)],
            ["ReduceTable", 79, "GrammarType", (1, "RSN", 2)],
            [
                "ReduceTable", 80, "Primary", (4, "RSN", 5), (10, "RSN", 5), (11, "RSN", 5),
                (14, "RSN", 5), (22, "RSN", 5), (23, "RSN", 5), (24, "RSN", 5), (25, "RSN", 5),
            ],
            ["ReduceTable", 81, "Alternation", (4, "RSN", 6), (10, "RSN", 20), (11, "RSN", 21)],
            [
                "ReduceTable", 82, "Byte", (4, "RSN", 7), (10, "RSN", 7), (11, "RSN", 7),
                (14, "RSN", 7), (17, "RSN", 61), (19, "RSN", 50), (22, "RSN", 7), (23, "RSN", 7),
                (24, "RSN", 7), (25, "RSN", 7), (29, "RSN", 50),
            ],
            ["ReduceTable", 83, "FiniteStateMachine", (4, "RSN", 8)],
            [
                "ReduceTable", 84, "SemanticAction", (4, "RSN", 48), (10, "RSN", 48),
                (11, "RSN", 48), (14, "RSN", 48), (16, "RSN", 60), (22, "RSN", 48), (23, "RSN", 48),
                (24, "RSN", 48), (25, "RSN", 48),
            ],
            ["SemanticTable", 85, "buildTree", ["walkList"], 73],
            ["SemanticTable", 86, "buildTree", ["walkEpsilon"], 81],
            ["SemanticTable", 87, "buildTree", ["walkSemanticAction"], 84],
            ["SemanticTable", 88, "buildTree", ["walkNonTreeBuildingSemanticAction"], 71],
            ["SemanticTable", 89, "buildTree", ["walkStar"], 74],
            ["SemanticTable", 90, "buildTree", ["walkQuestionMark"], 74],
            ["SemanticTable", 91, "buildTree", ["walkPlus"], 74],
            ["SemanticTable", 92, "buildTree", ["walkConcatenation"], 72],
            ["SemanticTable", 93, "buildTree", ["walkAttributes"], 71],
            ["SemanticTable", 94, "buildTree", ["walkBuildTreeOrTokenFromName"], 78],
            ["SemanticTable", 95, "buildTree", ["walkBuildTreeFromLeftIndex"], 78],
            ["SemanticTable", 96, "buildTree", ["walkTreeBuildingSemanticAction"], 78],
            ["SemanticTable", 97, "buildTree", ["walkDotDot"], 80],
            ["SemanticTable", 98, "buildTree", ["walkLook"], 80],
            ["SemanticTable", 99, "buildTree", ["walkAnd"], 74],
            ["SemanticTable", 100, "buildTree", ["walkMinus"], 74],
            ["SemanticTable", 101, "buildTree", ["walkOr"], 81],
            ["SemanticTable", 102, "buildTree", ["walkBuildTreeFromRightIndex"], 78],
            ["SemanticTable", 103, "processTypeNow", ["parser"], 38],
            ["SemanticTable", 104, "processTypeNow", ["scanner"], 38],
            ["AcceptTable", 105],
        ]
}
