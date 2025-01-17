var translator: SampleTranslator = SampleTranslator();
var user_input: String?;
while(user_input != "exit"){
    print("Evalute: ")
    user_input = readLine();
    print(translator.evaluate(text: user_input!)!);
    print(translator.variableDictionary);
}