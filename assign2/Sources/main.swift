RelationBuilder<Int, String>.example1();

let translator: SampleTranslator = SampleTranslator();
var user_input: String?;
while(user_input != "quit"){
    print("Evalute: ")
    user_input = readLine();
    print("Compile: \(translator.compile(text: user_input!))")
    print("Result: \(translator.evaluate(text: user_input!)!)");
}