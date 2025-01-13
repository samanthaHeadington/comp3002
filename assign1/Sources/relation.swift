class Utilities { 
 static func className(of element: Any?) -> String { 
  //Usage: Utilities.className(of element)
  if let element = element { 
   return String(describing: type(of: element)) 
  } else { return "nil" } 
 } 
}

//hashable wrapper class for Item, Relationship, Item tuples
//swift doesn't allow extensions for tuples
class HashableTuple<Item: Relatable, Relationship: Relatable>: Hashable{
    var triple: (Item, Relationship, Item);
    init(_ triple: (Item, Relationship, Item)){
        self.triple = triple;
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(triple.0);
        hasher.combine(triple.1);
        hasher.combine(triple.2);
    }
    static func ==(lhs: HashableTuple, rhs: HashableTuple) -> Bool{
        return (lhs.triple.0 == rhs.triple.0) && (lhs.triple.1 == rhs.triple.1) && (lhs.triple.2 == rhs.triple.2);
    }
}
 
protocol Relatable: Hashable, Comparable, CustomStringConvertible {
    var terseDescription: String { get } 
} 

extension Relatable { 
    var terseDescription: String { return description }
}

class Relation<Item: Relatable, Relationship: Relatable> : CustomStringConvertible{
    var triples: Set<HashableTuple<Item, Relationship>> //Set of tuples
 
    //Initializers for constructors.
    init() {//Empty
        //USAGE: let relation1 = Relation<Int,String> ()
        self.triples = Set()
    }
    
    init(from triples: [(Item, Relationship, Item)]) {//Array of tuples
        //USAGE: let relation2 = Relation<Int,String> (from: [(10, "<", 20), (10, "<", 30)])
        self.triples = Set ()
        for triple: (Item, Relationship, Item) in triples{
            self.triples.insert(HashableTuple<Item, Relationship>(triple));
        }
    }
    
    init(from triples: Set <HashableTuple<Item, Relationship>>) {//Set of tuples
        //Usage: let relation3 = Relation<Int,String> (from: relation2.triples)
        self.triples = triples
    }
    var description: String { 
        //Output format: Relation(from [(a1 b1 c1) (a2 b2 c2) …]).
        let triplesDescription = triples.map {
            "(\($0.triple.0.terseDescription) \($0.triple.1.terseDescription) \($0.triple.2.terseDescription))" } 
        return "Relation(from: [\(triplesDescription.joined(separator: ", "))])" 
    }  

    func `do`(_ operation: (Item, Relationship, Item) -> Void){
        for triple: HashableTuple<Item, Relationship> in triples{
            operation(triple.triple.0, triple.triple.1, triple.triple.2);
        }
    }

    func `do`(_ operation: ((Item, Relationship, Item)) -> Void){
        for triple: HashableTuple<Item, Relationship> in triples{
            operation(triple.triple);
        }
    }

    //The 'from(_, relationsDo: closure)' method partitions the triples that start with froms and iterates over them. 
    func from(_ froms: [Item], relationsDo: (Relationship, Relation) -> Void){ 
        //For you to fill in the code. Note that froms is NOT a keyword but relationsDo: is a keyword.

        //appends all relations where the from item is in froms to relations_from
        var relations_from: [HashableTuple<Item, Relationship>] = [];
        for triple: HashableTuple<Item, Relationship> in triples{
            if(froms.contains(triple.triple.0)){
                relations_from.append(triple);
            }
        }

        //partititions relations_from by relationship
        let from_map: [AnyHashable : [HashableTuple<Item, Relationship>]] = relations_from.partitionUsing { return $0.triple.1; };

        //forced cast to convert mapping from [AnyHashable : [HashableTuple<Item, Relationship>]] to [Relationship : [HashableTuple<Item, Relationship>]]
        //this is necessary for input to relationsDo()
        for mapping: (key: Relationship, value: [HashableTuple<Item, Relationship>]) in from_map as! [Relationship : [HashableTuple<Item, Relationship>]]{
            let subrelation: Relation<Item, Relationship> = Relation();
            for triple: HashableTuple<Item, Relationship> in mapping.value{
                subrelation.addTriple(triple.triple);
            }
            relationsDo(mapping.key, subrelation);
        }
    }

    func allFrom() -> [Item]{
        var result: [Item] = [];
        for triple: HashableTuple<Item, Relationship> in triples{
            result.append(triple.triple.0);
        }
        return result;
    }

    func allRelationships() -> [Relationship]{
        var result: [Relationship] = [];
        for triple: HashableTuple<Item, Relationship> in triples{
            result.append(triple.triple.1);
        }
        return result;
    }

    func allTo() -> [Item]{
        var result: [Item] = [];
        for triple: HashableTuple<Item, Relationship> in triples{
            result.append(triple.triple.2);
        }
        return result;
    }

    func add(_ from: Item, and relation: Relationship, and to: Item){
        triples.insert(HashableTuple<Item, Relationship>((from, relation, to)));
    }

    func addTriple(_ new_triple: (Item, Relationship, Item)){
        triples.insert(HashableTuple<Item, Relationship>(new_triple));
    }

    static func example1 ()  -> Void {
        //Relation.example1 ()
        //First, build a relation.
        var relation: Relation<Int,String> = Relation<Int, String>(from: [(2, "<", 3), (1, "=", 1), 
        (3, ">", 1), (2, "<", 4), (1, "<", 5), (5, "<", 6), (2, "<", 5)]);

        //Second, show that the entire relation can be printed…
        print ("\nLet relation = \(relation)")

        //Third, show that the 3-parameter do: works…  print ("\nOne triple per line, version1 of relation is")
        relation.do {a,b,c in print ("\n(\(a.terseDescription) \(b.terseDescription) \(c.terseDescription))")}

        //Fourth, show that the 1-parameter do: works…  print ("\nOne triple per line, version2 of relation is")
        relation.do {triple in print ("\n(\(triple.0.terseDescription) \(triple.1.terseDescription) \(triple.2.terseDescription)) ")}
    }

    static func example2 ()  -> Void {
        //Relation.example2()
        //First, build a relation.
        var relation: Relation<Int,String> = Relation<Int, String>(from: [(2, "<", 3), (1, "=", 1), 
        (3, ">", 1), (2, "<", 4), (1, "<", 5), (5, "<", 6), (2, "<", 5)]);

        //Second, print the relation…"
        print ("\nLet relation = \(relation)")

        //Third, show that from:relationsDo: works…  print ("\nStarting from {1 2 3},'.
        relation.from ([1, 2, 3,]) {relationship, subrelation in 
            print ("\nThe class of the subrelation is \(Utilities.className(of: subrelation))");
            print ("\nThere is a relationship \(relationship) with subrelation")
            subrelation.do {triple in print ("\n         \(triple)")}
        }

        //Without debugging information and for different from sets.
        for fromCollection in [[1, 2, 3], [1, 2], [2], []] {
        relation.from (fromCollection) {relationship, subrelation in
            print ("\nThere is a relationship \(relationship) with subrelation")
            subrelation.do {triple in print ("\n         \(triple) ")}}
        }
    }

    //tests allFrom, allRelationships and allTo, proper set functionality and proper insertion functionality
    static func example3(){
        var relation: Relation<Int,String> = Relation<Int, String>(from: [(2, "<", 3), (1, "=", 1), 
            (3, ">", 1), (2, "<", 4), (1, "<", 5), (5, "<", 6), (2, "<", 5)]);

        print("Let relation = \(relation)");
        print("Has relations from \(relation.allFrom())");
        print("Has relationships \(relation.allRelationships())")
        print("Has relations to \(relation.allTo())");

        print(#"Relation contains (2, "<", 3) is (true) "#, relation.triples.contains(HashableTuple<Int, String>((2, "<", 3))));
        print(#"Relation contains (2, "<", 1) is (false) "#, relation.triples.contains(HashableTuple<Int, String>((2, "<", 1))));

        print(#"Test insert (1, "=", 1), should fail"#);
        relation.add(1, and: "=", and: 1);
        print("\(relation)");
        print(#"Test insert (2, "=", 2), should succeed"#);
        relation.add(2, and: "=", and: 2);
        print("\(relation)");
        print(#"Test insert (2, "=", 2) as tuple, should fail"#);
        relation.addTriple((2, "=", 2));
        print("\(relation)");
        print(#"Test insert (3, "=", 3) as tuple, should succeed"#);
        relation.addTriple((3, "=", 3));
        print("\(relation)");

    }

    static func testing(){
        example1();
        example2();
        example3();
    }
}