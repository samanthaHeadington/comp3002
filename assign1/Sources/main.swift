func a1(){
    var arr: [Any?] = [nil, true, false, "hi", "h", 10, 10.2, [10, 20]]; 
    for elt: Any? in arr{
        if(elt == nil){
            print(elt as Any, ": nil");
        }else{
            print(elt!, ": ", type(of: elt!))
        }
    }
}

class Thing{
    var name: String?;
    init(name: String){
        self.name = name;
    }
}

class Thingy{
    var name: String?;
    init(name: String){
        self.name = name;
    }
}


func a2(){
    let a: Int = 10, b: Int = 20, c: Int = 10;
    // var bools: [Bool] = [a == b, a === b, a == c, a === c];
    // let testEquality1 = [1,2,[3,3]] == [1,2,[3,3]];
    // let testEquality2: Bool = [1,2,[3,3]] as [Any] == [1,2,[3,3]] as [Any];
    // let testEquality3: Bool = [1,2,[3,3]] as [Equatable] == [1,2,[3,3]] as [Equatable];
    let d: Thing = Thing(name: "test"), e: Thing = Thing(name: "test");
    let testEquality4: Bool = d === e;
    let f: Thing = d;
    let g: Thingy = Thingy(name: "test");
    let testEquality5: Bool = d === g;
    print(testEquality4, testEquality5);
}

func a3(){
    let str: String = "test";
    // print(str[1]);
    print(str[str.index(str.startIndex, offsetBy: 1)]);
    print(Array(str)[1]);
}

func a4(){
    let arr: [Int] = [1,2,3,4,5];
    print(arr.collect{$0.squared()});
    print(arr.select{$0.odd()});

    print(arr.map{$0.squared()});
    print(arr.filter{$0.odd()});
}

func a5(){
    print([1,2,3,4,5,6,7,8,9].partitionUsing{$0.odd()});
    print(["Hi", "all", "guys", "and", "gals"].partitionUsing{Array($0)[0]});
    print([10.5, "all", nil, [1,2], "any"].partitionUsing{
        if($0 == nil){
            return "Undefined"
        }
        return "\(type(of: $0!))";
    })
}

func a6(){
    var arr: [Int] = [1,2];
    print(arr);
    arr.appendIfAbsent(1);
    print("After appendIfAbsent(1)", arr);
    arr.appendIfAbsent(3);
    print("After appendIfAbsent(3)", arr);
    arr.appendIfAbsent([2,4]);
    print("After appendIfAbsent([2,4])", arr);

    var things: [Thing] = [Thing(name: "1"), Thing(name: "2")];
    print(things);
    arr.appendIfIdenticalAbsent();
    print("After appendIfAbsent(1)", arr);
    arr.appendIfAbsent(3);
    print("After appendIfAbsent(3)", arr);
    arr.appendIfAbsent([2,4]);
    print("After appendIfAbsent([2,4])", arr);


    print("Result of hashable setEqual for [3,1,2,3], [3,1,2] is", [3,1,2].setEqual(hashable_arr: [3,1,2,3]));
    print("Result of comparable setEqual for [3,1,2,3], [3,1,2] is", [3,1,2].setEqual(comparable_arr: [3,1,2,3]));
    print("Result of equatable setEqual for [3,1,2,3], [3,1,2] is", [3,1,2].setEqual(equatable_arr: [3,1,2,3]));

    print("Result of hashable setEqual for [1,2,3,1], [4,1,2,2] is", [4,1,2,2].setEqual(hashable_arr: [1,2,3,1]));
    print("Result of comparable setEqual for [1,2,3,1], [4,1,2,2] is", [4,1,2,2].setEqual(comparable_arr: [1,2,3,1]));
    print("Result of equatable setEqual for [1,2,3,1], [4,1,2,2] is", [4,1,2,2].setEqual(equatable_arr: [1,2,3,1]));

}

a1();
a2();
a3();
a4();
a5();
a6();
Truck.example1();
Relation<Int, String>.example1();
Relation<Int, String>.example2();