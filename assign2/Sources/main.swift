RelationBuilder<Int, String>.example1();

let translator: SampleTranslator = SampleTranslator();
var user_input: String?;
var eval_result: Any;
while(user_input != "quit"){
    print("Evalute: ")
    user_input = readLine();
    print("Compile: \(translator.compile(text: user_input!))")
    
    eval_result = translator.evaluate(text: user_input!)!;
    if(eval_result is Int){
        print("Result: \(eval_result)")
    }else{
        print("Result: \((eval_result as! (Int, [String: Int])).0)\n" + "\((eval_result as! (Int, [String: Int])).1)")
    }
}