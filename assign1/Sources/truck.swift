class Truck{
    var driver: String?;
    var passengers: [String] = [];
    var load: [String] = [];

    func driver(_ name: String){
        driver = name;
    }
    func addPassenger(_ name: String){
        passengers.append(name);
    }
    func addLoad(_ item: String){
        load.append(item);
    }
    func driverDo(_ operation: (String) -> Void){
        if(driver == nil){
            return;
        }
        operation(driver!);
    }
    func passengersDo(_ operation: (String) -> Void){
        passengers.do(operation: operation);
    }
    func loadDo(_ operation: (String) -> Void){
        load.do(operation: operation);
    }
    func `do`(_ operation: (String) -> Void){
        driverDo(operation);
        passengersDo(operation);
        loadDo(operation);
    }

    static func example1 (){
        //Truck.example1
        var aTruck: Truck = Truck ()
        aTruck.driver ("Jim")
        aTruck.addPassenger ("Tom")
        aTruck.addPassenger ("Frank")
        aTruck.addLoad ("Wheelbarrow")
        aTruck.addLoad ("Ladder")
        aTruck.addLoad ("Cement")
        aTruck.driverDo { (_ driver: String) -> Void in
            print ("\nThe driver is \(driver)")
        }
        aTruck.passengersDo { (_ passenger: String) -> Void in
            print ("\nOne passenger is \(passenger)");
        }
        aTruck.loadDo { (_ load: String) -> Void in
            print ("\nThe back of the truck contains a \(load)");
        }
        //Method do sequences over everything in the truck. The compiler   //won’t allow you to compile the following until func `do` is defined (even                     //if it contains no code.
        aTruck.do { (_ anObject: String) -> Void in
            print ("\nIn the truck, there is a \(anObject)");
        }
    }
}