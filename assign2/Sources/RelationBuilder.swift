class RelationBuilder<Item: Relatable, Relationship: Relatable> : CustomStringConvertible{
    var description: String {
        return "Right \(right.description)\nDown \(down.description)";
    }

    var right: Relation<Item, Relationship>;
    var down: Relation<Item, Relationship>;

    init(right: [(Item, Relationship, Item)], down: [(Item, Relationship, Item)]){
        self.right = Relation<Item, Relationship>(from: right);
        self.down = Relation<Item, Relationship>(from: down);
    }

    // downRelationsDo is an optional closure, either relationsDo is used for but relationships, or relationsDo is used for right and downRelationsDo is used for down
    func from(_ froms: [Item], relationsDo: (Relationship, Relation<Item, Relationship>) -> Void, downRelationsDo: ((Relationship, Relation<Item, Relationship>) -> Void)? = nil){
        right.from(froms, relationsDo: relationsDo);
        down.from(froms, relationsDo: (downRelationsDo != nil) ? downRelationsDo! : relationsDo);
    }

    static func example1(){
        let relation_builder: RelationBuilder<Int, String> = RelationBuilder<Int, String>(
            right: [(1, "|-", 2), (2, "G", 3), (3, "-|", 4), (5, "A", 6), (6, "c", 6), (7, "a", 7), (7, "b", 8)],
            down: [(2, "G", 5), (5, "A", 7)]
        );

        print("\(relation_builder)");

        relation_builder.from([2,5,7,8], relationsDo: { relationship, relation in
            print("Maps to \(relation.allTo()) under \(relationship) right");
        }, downRelationsDo: {relationship, relation in
            print("Maps to \(relation.allTo()) under \(relationship) down");
        })

        relation_builder.from([2,5,7,8]) { relationship, relation in
            print("Maps to \(relation.allTo()) under \(relationship)");
        }
    }
}