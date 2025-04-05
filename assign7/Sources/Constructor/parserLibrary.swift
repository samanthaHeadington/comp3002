//
//  ParserLibrary.swift
//  Constructor
//
//  Created by Wilf Lalonde on 2022-12-19
//  and Jeeheon Kim (for Wilf Lalonde) on 2022-1-10
//

import Foundation

//debug, error, Error, MyError, TransducerError
func debug (_ string: String) {
    //print ("debug: " + string)
}
func error (_ string: String) {
    print ("error: " + string)
}
func report (_ string: String) {
    print ("report: " + string)
}

enum MyError: Error {
    case runtimeError(String)
}

enum TransducerError: Error {
    case invalidRawTable
    case transducerError // invalid transducer to perform the given task
    case designError(String)
    case lexicalError
    case invalidAction
}

//VirtualTree, Tree, Token

protocol VirtualTree : CustomStringConvertible {
    var label: String { get set }
    var description: String { get }
    func description (tabs: Int) -> String
}

public class Tree: VirtualTree {
    var label: String
    var children: [VirtualTree] = []
  
    init() {
        (label, children) = ("Unknown", [])
    }

    init(label: String, children: [VirtualTree]) {
        (self.label, self .children) = (label, children)
    }

    func child(_ index: Int) -> VirtualTree{
        return children[index];
    }

    func addChild(aTree: VirtualTree) {
        children.append(aTree)
    }

    public var description: String {
        return self.description (tabs: 0)
    }
    public func description (tabs: Int ) -> String {
        var string: String = "\n"
        if tabs > 0 {
            for _ in 1...tabs {
                string += "\t"
            }
        }
        string += "\(label)";
        for child in children {
            string += child.description (tabs: tabs+1)
        }
        return string
    }
}

public class Token: VirtualTree, Hashable, Equatable {
    var label: String
    var symbol: String
    nonisolated(unsafe) static let sharedEmpty = Token(label: "Unknown", symbol: "")

    init(label: String, symbol: String) {
      (self.label, self.symbol) = (label, symbol)
    }

    public static func == (token1: Token, token2: Token) -> Bool {
      return token1.label == token2.label && token1.symbol == token2.symbol
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(label)
      hasher.combine(symbol)
    }

    public var description: String {
        return "Token (label: \(label) symbol: \(symbol))"
    }
    public func description (tabs: Int ) -> String {
        var string: String = "\n"
        if tabs > 0 {
            for _ in 1...tabs {
                string += "\t"
            }
        }
        string += description;
        return string
    }
}

//=======================================================================================

//Allows converting Strings/Substring to ascii; i.e., print"Hello".asciiValues) // [72, 101, 108, 108, 111]
extension StringProtocol {
    var asciiValues: [UInt8] { compactMap(\.asciiValue) }
}

//Allows local pairs as dictionary keys.
struct ParserPair<T: Hashable, U: Hashable>: Hashable {
    //Swift will automatically generates the hash(into:) method, which combines the hashes of first
    //and second to produce a hash value for the Pair instance.
    let first: T
    let second: U
    
    init(_ pair: (T, U)) {
        (first, second) = pair
    }
}

//All table types...
enum TableType: String {
    case
    // Scanner tables
    ScannerReadaheadTable,

    // parser tables
    ReadaheadTable, ReadbackTable, ShiftbackTable, ReduceTable, AcceptTable,

    // Both
    SemanticTable
}

//=======================================================================================

protocol Table {
    var transducer: Transducer? { get set }
    var tableNumber: Int { get set }
    var tableType: TableType { get }
    func run() throws -> Int?
    func registerTable(rawTable: Array<Any>) -> Void
  }

protocol TableWithTransitionsWithIntKey {
    var transitions: [Int: (String, Int)] { get set }
}

protocol TableWithTransitionsWithStringKey {
    var transitions: [String: (String, Int)] { get set }
}

protocol TableWithTransitionsWithPairKey {
    var transitions: [ParserPair<String, Int>: (String, Int)] { get set }
}

class TableFactory {
    // Factory that makes different types of table based on the parameter
    static func create(type: String) throws -> Table {
        switch TableType(rawValue: type) {
            case .ScannerReadaheadTable:
                return ScannerReadaheadTable()
            case .SemanticTable:
                return SemanticTable()
            case .ReadaheadTable:
                return ReadaheadTable()
            case .ReadbackTable:
                return ReadbackTable()
            case .ShiftbackTable:
                return ShiftbackTable()
            case .ReduceTable:
                return ReduceTable()
            case .AcceptTable:
                return AcceptTable()
        default:
            throw TransducerError.invalidRawTable  // If you enter this, add more cases
        }
    }
}

class ScannerReadaheadTable: Table, TableWithTransitionsWithIntKey {
    var tableType = TableType.ScannerReadaheadTable
    var tableNumber: Int = -1
    var transducer: Transducer?
    var transitions = [Int: (String, Int)]()
    init() {}
    
    func run() throws -> Int? {
        let scanner = self.transducer as! Scanner
        let character = scanner.peekInput(); debug("\t peeked at `\(String (describing: character))`")
        let charAsInt =
            character == nil
            ? 256
            : Int(character!.asciiValue!); debug("\t\t with ascii `\(String (describing: charAsInt))`")
        let pair = transitions[charAsInt]; debug("\t\t transition for \(String (describing: charAsInt)) -> \(String (describing: pair)) ")
        if pair != nil {
            let (attributes, goto) = pair!
            let isRead = attributes.contains("R")
            let isKeep = attributes.contains("K")
            if !isRead { return goto }
            if isKeep {
                scanner.keptCharacters += String(character!)
            }
            scanner.discardInput()
            return goto
        }
        throw TransducerError.lexicalError
    }
    
    func registerTable (rawTable: Array<Any>) {
        //Any is really [(Any, String, Int)]; i.e., an array of triples where
        //a triple is an integer collection or character collection, attributes, and goto. The integers
        //are unprintable characters or the end of file integer 256, the attributes may include R for read
        //(versus L for look) and K for keep. It's particularly important here to store the data so that
        //given a character or 256, you can perform a fast lookup to find the associated attributes
        //and goto. Hence we convert everything to an integer.
        transitions = [Int: (String, Int)]()
        let data = rawTable as? [(Any, String, Int)]
        for triple in data! {
            let item = triple.0
            let (attributes, goto) = (triple.1, triple.2) as (String, Int)
            if item is String {//e.g., "abc"
                let string = item as! String
                for character: Character in string {
                    let asInteger = Int (character.asciiValue!)
                    transitions [asInteger] = (attributes, goto)
                }
            } else {//e.g., [10,20,30]
                let integers = item as! Array<Int>
                for integer in integers {
                    transitions [integer] = (attributes, goto)
                }
            }
        }
    }
}

class SemanticTable: Table {
    var tableType = TableType.SemanticTable
    var tableNumber: Int = -1
    var transducer: Transducer?
    var action: String = ""
    var parameters: [Any] = []
    var goto: Int = -1
    
    init() {}
    
    func registerTable(rawTable: Array<Any>) -> Void {
        //Any is really [(String, Array<Any>, Int)] consisting of (action, parameters, goto)
        (action, parameters, goto) = (rawTable [0] as! String, rawTable [1] as! [Any],
                                      rawTable [2] as! Int)
    }
    
    func run() throws -> Int? {
        //Note: Get the parser or the scanner to perform the action if
	//it can (it's a transducer); otherwise, get the sponser to do it.
        //Guarantees an error message if action is missing.
        debug("\t semantic table has to run \(action)(\(parameters))")
        if self.transducer!.canPerformAction(action) {
            self.transducer!.performAction(action, parameters)
        } else if transducer!.sponsor!.canPerformAction(action) {
            self.transducer!.sponsor!.performAction(action, parameters)
        } else {
	    error ("Semantic action \"\(action)\" needs to be added to " +
        	"canPerformAction in \(type(of: transducer!.sponsor!))")
        }
        return goto
    }
}

class ReadaheadTable: Table, TableWithTransitionsWithStringKey {
    var tableType = TableType.ReadaheadTable
    var tableNumber: Int = -1
    var transducer: Transducer?
    var transitions = [String: (String, Int)]()
    init() {}

    //func registerTable(rawTable: Array<(String, String, Int)>) -> Void {
    func registerTable(rawTable: Array<Any>) -> Void {
        transitions = [String: (String, Int)]()
        for triple in rawTable {
            let (item, attributes, goto) = triple as! (String, String, Int)
            transitions[item] = (attributes, goto)
        }
      }

    func run() throws -> Int? {
        // Peek at the next token label
        let parser = self.transducer as! Parser
        let token = parser.peekScannerToken()
        let tokenLabel = token.label
        let transition = transitions[tokenLabel]
        guard transition != nil else {
            error("Syntax error: `\(tokenLabel)` not allowed\n") //Smalltalk version has better error message...
            return nil
        }

        let (attributes, goto) = transition!
    
        let isRead = attributes.contains("R") //read versus "L" (look)
        let isNode = attributes.contains("N")  //node (virtual tree) versus no "N"
        let isStack = attributes.contains("S") //stack versus no "S"
        if !isRead {
            return goto
        }
        parser.discardScannerToken()
        if isStack {
            parser.tokenStack.append(token)
            parser.tableNumberStack.append(goto)
            parser.treeStack.append(isNode ? token : nil)
            parser.right = parser.tokenStack.count - 1
            parser.left = parser.right + 1
        }
    debug("\tleft: \(parser.left), right: \(parser.right)")
    debug("\t\ttransition for \(tokenLabel) to \(goto)")
    return goto
  }
}

class ReadbackTable: Table, TableWithTransitionsWithPairKey {
    var tableType = TableType.ReadbackTable
    var tableNumber: Int = -1
    var transducer: Transducer?
    var transitions: [ParserPair<String, Int>: (String, Int)] = [:]
    init() {}
    
    //Array<Any> is really Array<((String, Int), String, Int)>; i.e., a pair, attributes and goto.
    func registerTable(rawTable: Array<Any>) -> Void {
        transitions = [:]
        for triple in rawTable {
            let (pair, attributes, goto) = triple as! ((String, Int), String, Int)
            transitions[ParserPair(pair)] = (attributes, goto)
        }
    }

    func run() throws -> Int? {
        //Peek at the next token label
        let parser = self.transducer as! Parser
        //Readback considers the token label in the token stack and the state number in the table number stack

        //Pick up the pair a (a string) and b (an integer).
        let a = parser.tokenStack[parser.left - 1].label as String?
        let b = parser.tableNumberStack[parser.left - 1] as Int
        let pair = ParserPair((a!,b))
        print(pair)
        if transitions[pair] == nil {
            throw TransducerError.designError("incorrect Readback tables")
        }
        let (attributes, goto) = transitions[pair]!

        let isRead = attributes.contains("R")
        if isRead {
            parser.left = parser.left - 1
        }
        debug("\tleft: \(parser.left), right: \(parser.right)")
        return goto
    }
}

class ShiftbackTable: Table {
    //Shifts Parser's left by `shift`
    var tableType = TableType.ShiftbackTable
    var tableNumber: Int = -1
    var transducer: Transducer?
    var shift: Int?; var goto: Int?
    init() {}
    
    //func registerTable(rawTable: (Int, Int)) -> Void {
    func registerTable(rawTable: Array<Any>) -> Void {
        //Array<Any> is really (Int, Int); i.e., shift and goto.
        (shift, goto) = (rawTable[0] as? Int, rawTable[1] as? Int)
    }

    func run() throws -> Int? {
        let parser = self.transducer as! Parser

        //Adjust left by the amount specified and return the goto table
        parser.left = parser.left - shift!
        debug("\tleft: \(parser.left), right: \(parser.right)")

        return goto
    }
}

class ReduceTable: Table, TableWithTransitionsWithIntKey {
    var tableType = TableType.ReduceTable
    var tableNumber: Int = -1
    //var transducer: Parser?
    var transducer: Transducer?
    var nonterminal: String = ""
    var transitions: [Int : (String, Int)] = [:]
    init() {}
    
    func registerTable(rawTable: Array<Any>) -> Void {
        // Store the nonterminal and triple collection where a triple
        // contains table number, attributes, destination table number.
        
        let nonterminal: String = rawTable[0] as! String
        transitions = [Int : (String, Int)]()
        for triple in rawTable.dropFirst () {
            let (state, attributes, goto) = triple as! (Int, String, Int)
            transitions[state] = (attributes, goto)
        }
        self.nonterminal = nonterminal
    }

    func run() throws -> Int? {
        // Reduce to A: A is the nonterminal to reduce to.
        // Pick up the new tree and simulate a readahead of A as a token
        // where the new tree is associated with A
        debug ("Reduce to \(nonterminal)")
        let parser = self.transducer as! Parser //Eliminate unwrapping once and for all
        var tree: VirtualTree? = nil

        if let routine = parser.delayedRoutine { //Run if not nil.
	          tree = routine (); parser.delayedRoutine = nil
        } else {
            // Capture that one subtree (if any) in the stacks
            debug("\tArray of size (\(parser.treeStack.count)) - [\(parser.left)~\(parser.right)]")
            let left = parser.left; let right = parser.right
            let treeStackWithNils = left <= right ? parser.treeStack[left...right] : []
            let children = treeStackWithNils.compactMap { $0 } // treeStack without nils
            
            if children.count == 0 {
                tree = nil
            } else if children.count == 1 {
                tree = children.first
            } else {
                throw TransducerError.designError("incorrect reduce table; more than one child in tree")
            }
        }
        debug ("Reduce to \(nonterminal) building tree \(String(describing: tree))")
            
        // Clear the stacks between [left, right]
        let removeCount = parser.right - parser.left + 1
        parser.tokenStack.removeLast(removeCount)
        parser.tableNumberStack.removeLast(removeCount)
        parser.treeStack.removeLast(removeCount)
            
        // Use the top table number on the stack (`from table #`) and locate the pair (attr, `to table #`)
        // Case1: If you cannot find it, then it is a design error
        // Case2: If done, return the other table #
        // Case3: Use the attributes as you did for a Readaheadtable
        //        + create a new token using the nonterminal as a symbol
        //        + adjust `left` and `right`
        let tableNumber = parser.tableNumberStack.last!
        print ("Looking for \(tableNumber) + \(nonterminal) -> ?")
            
        if let transition = self.transitions[tableNumber] {
            let (attributes, goto) = transition
            let isStack = attributes.contains("S")
            let isNode = attributes.contains("N")
            if isStack {
                let newToken = Token(label: nonterminal, symbol: nonterminal)
                parser.tokenStack.append(newToken)
                parser.tableNumberStack.append(goto)
                parser.treeStack.append(isNode ? tree : nil)
                debug("\ttoken added to the stack is <\(nonterminal): \(nonterminal)>")
            }
            parser.right = parser.treeStack.count - 1
            parser.left = parser.right + 1
            debug("\tleft: \(parser.left), right: \(parser.right)")
                
            return goto
        } else {
            throw TransducerError.designError("Missing reduce table entry")
        }
    }
}

class AcceptTable: Table {
    var tableType = TableType.AcceptTable
    var tableNumber: Int = -1
    var transducer: Transducer?
    init() {}

    func registerTable(rawTable: Array<Any>) -> Void {
        //Do nothing
    }

    func run() -> Int? {
        return nil
    }
}

//=======================================================================================

//Translator, Transducer, Screener, Scanner, Parser

public protocol Translator {
    func canPerformAction(_ :String) -> Bool
    func performAction(_ :String, _ :[Any]) -> Void
}

//=======================================================================================

public class Transducer : Translator {
    var sponsor: Translator? = nil
    var tables: [Table?] = [nil]
    
    public func canPerformAction(_ :String) -> Bool {return false}
    public func performAction(_ :String, _ :[Any]) -> Void {}
}

//=======================================================================================

public final class Screener: Transducer {
    var keywords: [String] = [String]()

    override public init() {}
    public init(keywords: [String]) {
        self.keywords = keywords
    }
    
    func screen(_ token: Token) -> Token {
        //A proper screener would replace the symbol part by a unique string
        //so that future tests for equality could be done via identity instead
        
        //Older name is #Identifier and newer name is #walkIdentifier
        if !(token.label == "Identifier" || token.label == "walkIdentifier") {
            // if not #Identifier nor #walkIdentifier
            return token
        }
        
        //An identifer in the list of keywords is converted to that keyword.
        if !self.keywords.contains(token.symbol) {
            return token
        }

        let newToken = Token(label: token.symbol, symbol: token.symbol)
        return newToken
    }
}

//=======================================================================================

// Scanner: Scan text and output tokens
public final class Scanner: Transducer {
    private var input: String?
    private var token: Token?
    var keptCharacters: String = ""
    
    public override func canPerformAction(_ action :String) -> Bool {
        return action == "buildToken"
    }
    public override func performAction(_ action: String, _ parameters: [Any]) -> Void {
        switch (action) {
        case "buildToken":
            let label = parameters [0] as! String
            buildToken (label: label)
        default:
            error ("Attempt to perform unknown action \(action)")
        }
    }

    var start: String.Index?
    var current: String.Index?

    nonisolated(unsafe) static let sharedEmpty = Scanner()

    private func isAtEnd() -> Bool {
        if let input = input {
            return current == input.endIndex
        }
        return true // if there's no code at all, just say that it has reached the end
    }

    func scanTokens(_ text: String) {
        // They will be retrieved on demand... rather than all at once.
        input = text
        start = text.startIndex
        current = start
        discardToken()
    }


    private func buildToken(label: String) {
        // Create a token with the supplied label and keptCharacters in the scanner
        // the result would be used by `peekToken`
        // Reset keptCharacters so the process can repeat
        self.token = Token(label: label, symbol: keptCharacters)
        debug("\tbuildToken \(label): \(keptCharacters)")
        keptCharacters = ""
    }

    func discardInput() {
        current = input!.index(after: current!)
    }

    func discardToken() {
        // Set token to nil
        // Execute tables in a loop starting with table1 until token is no longer nil
            // Somewhere during this execution, the semantic action buildToken
            // will execute putting something into variable 'token'
        debug("----> Running discardToken")

        var index = 1
        token = nil

        while token == nil {
            let table = tables[index]!
      
            do {
                debug("Scanner \(table.tableType) #\(index) is running...\n")
                index = try table.run()!
            } catch {
                print("Syntax error encountered in table \(index).")
                return
            }
        }
        debug("----> Token \(token!) has been created... exiting discardToken")
    }
    
    func registerTables(rawTables: Array<Any>) ->  Void {
        // Transform raw scanner tables (tableType tableNumber ...) to appropriate table objects
        for index in (0...rawTables.count-1) {
            let rawTable = rawTables[index] as! Array<Any>
            let tableNumber = rawTable [1] as! Int
            let type = rawTable[0] as! String
            do {
                var table = try TableFactory.create(type: type)
                let subArray = Array(rawTable[2...])
                table.registerTable(rawTable: subArray)
                table.tableNumber = tableNumber
                table.transducer = self
                if (tables.count != tableNumber) {
                    error ("Table numbered \(tableNumber) incorrectly inserted into table \(tables.count).")
                }
                tables.append(table)
            } catch {
                print ("DEBUG catch entered")
                return
            }
        }
    }

    func peekInput() -> Character? {
        // Peeks at next input and returns a character or nil.
        if current == input!.endIndex {
            return nil
        } else {
            return input![current!]
        }
    }

    func peekToken() -> Token {
        if token == nil {
            report("Scanner/peekToken reported that the token is nil")
            return Token.sharedEmpty
        }
        return token!
    }
}

//=======================================================================================

public final class Parser: Transducer {
    var scanner: Scanner = Scanner()
    var screener: Screener = Screener()
    var keywords: [String] = []
    var tokenStack: [Token] = [
        Token(label: "|-", symbol: "|-")
    ]
    var tableNumberStack: [Int] = [1]
    var treeStack: [VirtualTree?] = [nil]
    var left: Int = 0   // left, right has to do with a treeStack
    var right: Int = 0
    var newTree: VirtualTree? = nil
    var delayedRoutine: (() -> VirtualTree)? = nil

    
    public override func canPerformAction(_ action :String) -> Bool {
        return action == "buildTree" || action == "buildTreeFromIndex"
    }
    public override func performAction(_ action: String, _ parameters: [Any]) -> Void {
        switch (action) {
        case "buildTree":
            let label = parameters [0] as! String
            delayedRoutine = delayedBuildTree(label)
        case "buildTreeFromIndex":
            let index = parameters [0] as! Int
            delayedRoutine = delayedBuildTreeFromIndex(index)
        default:
            error ("Attempt to perform unknown action \(action)")
        }
    }
        
    init(sponsor: Translator?, parserTables: Array<Any>, scannerTables: Array<Any>) {
        super.init()
        // Set up the user of the scanner and parser
        self.sponsor = sponsor
        self.scanner.sponsor = sponsor
        
        // Give the keywords to the screener and the remaining tables to the parser
        let keywords = (parserTables [0] as! Array<String>).dropFirst ()
        self.screener = Screener(keywords: Array (keywords))
        self.registerTables(rawTables: Array(parserTables[1...]))
        
        // Give the entire set of scanner tables to the scanner
        self.scanner.registerTables(rawTables: scannerTables)
    }
    
    private func registerTables(rawTables: Array<Any>) -> Void {
        for index in 0...rawTables.count-1 {
            let rawTable = rawTables[index] as! Array<Any>
            let type = rawTable[0] as! String
                
            // Get the class name for the type, make a new instance, and then add it to tables.
            // Also, tell it that as a parser, I'm the transducer
            do {
                let tableNumber = rawTable [1] as! Int
                var table = try TableFactory.create(type: type)
                let subArray = Array(rawTable[2...])
                table.registerTable(rawTable: subArray)
                table.tableNumber = tableNumber
                table.transducer = self
                if (tables.count != tableNumber) {
                    error ("Table numbered \(tableNumber) incorrectly inserted into table \(tables.count).")
                }
                tables.append(table)
            } catch {
                print("Sanitize Raw tables - error in your raw data"); return
            }
        }
    }
        
    func parse(_ text: String) -> VirtualTree? {
        //To parse the text, you have to give it to the scanner (say via scan: text).
        //It in turn should have the first token set up for you. Next you need
        //to execute tables starting from table 1 until you reach an Accept table.
        //Once that happen, the tree that was built should be on top of the tree
        //stack. Just return it. If you look at the Accept table, you will see
        //that of all the tables, it is the only one that returns nil. You can
        //use that information to stop your loop.
             
        scanner.scanTokens(text)
        var index: Int = 1
        var table = tables[index]!
        var i = 1
            
        while table.tableType != .AcceptTable {
            print(table)
            print(scanner.peekToken())
            print("Table index: \(index)")
            print("RemoveCount: \(right - left + 1)")
            print("Tables walked: \(i)\n")
            i += 1

            // if i == 20{break}

            do {
                debug("Parser \(table.tableType) #\(index) is running...\n")
                index = try table.run()! // The result returned is the next table to run
            } catch {
                print("Syntax error : Table run failed") //See Smalltalk version for more detailed error message
                break // no point in infinite looping here
            }

            do{
                debug("\tgoing to #\(index)...\n")
                table = tables[index]!
            } catch {
                print("Syntax error : Bad index")
                break
            }

            // if i == 265{break}
        }
            
        return treeStack.last!
    }
    
    func buildTree(_ rootLabel: String) {
        //Pick up the children from the tree stack between left and right inclusively
        //(provided they are not nil) and build a tree with the given label
        //Store it in instance newTree so a reduce table can use it
                    
        let children = left <= right ? treeStack[left...right].compactMap{ $0 } : [] // Picks up each non-nil object
        self.newTree = Tree(label: rootLabel, children: children)
    }
    
    func buildTreeFromIndex(_ index: Int) {
        //Index is (+1,+2,+3... => label is in the token relative to the left-end; i.e., to the right of left end.
        //Index is (-1,-2,-3... => label is in the token relative to the right-end; i.e., to the left of right end.
        //Pick up the children from the tree stack between left and right inclusively
        //(provided they are not nil) and build a tree with the label in the token specified by index.
        //Store it in instance newTree so a reduce table can use it
            
        let children = left <= right ? treeStack[left...right].compactMap{ $0 } : [] // Picks up each non-nil object
        let label = tokenStack[index > 0 ? left+index-1 : right+index+1].symbol
        self.newTree = Tree(label: label, children: children)
    }

    func delayedBuildTree (_ rootNode: String) -> (() -> VirtualTree) {
	      return { self.buildTree(rootNode); return self.newTree! }
    }

    func delayedBuildTreeFromIndex (_ index: Int) -> (() -> VirtualTree) {
	      return { self.buildTreeFromIndex(index); return self.newTree! }
    }

    func discardScannerToken() -> Void {
        scanner.discardToken()
    }
        
    public func peekScannerToken() -> Token {
        let token = screener.screen(scanner.peekToken())
        debug("\t\tpeekScannerToken for \(token)")
        return token
    }
}

