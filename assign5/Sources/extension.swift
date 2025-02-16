extension Array {
    func `do`(_ operation: ((Element) -> Void)) {
        for elt: Element in self {
            operation(elt)
        }
    }
    func doWithoutFirst(operation: ((Element) -> Void)) {
        for i in 1..<count {
            operation(self[i])
        }
    }
    func firstSatisfying(_ predicate: (Element) -> Bool) -> Element? where Element: Equatable {
        for elt in self {
            if predicate(elt) {
                return elt
            }
        }
        return nil
    }
    func descriptionWithNewLines() -> String{
        var return_val = "["
        for elt in self {
            return_val.append("\(elt),\n")
        }
        return_val.append("]")
        return return_val
    }
    func partitionUsing<Key: Hashable>(separator: ((Element) -> Key)) -> [Key: [Element]] {
        var result: [Key: [Element]] = [:]
        for elt: Element in self {
            let key: Key = separator(elt)
            if result[key] == nil {
                result[key] = []
            }
            result[key]!.append(elt)
        }
        return result
    }
    func collect(operation: ((Element) -> Element)) -> [Element] {
        var result: [Element] = [Element]()
        for elt: Element in self {
            result.append(operation(elt))
        }
        return result
    }
    func select(predicate: ((Element) -> Bool)) -> [Element] {
        var result: [Element] = [Element]()
        for elt: Element in self {
            if predicate(elt) {
                result.append(elt)
            }
        }
        return result
    }
    mutating func appendIfAbsent(_ obj: Element) -> Bool where Element: Equatable {
        if self.contains(obj) {
            return false
        }
        self.append(obj)
        return true
    }
    mutating func appendIfAbsent(_ arr: [Element]) where Element: Equatable {
        for elt: Element in arr {
            appendIfAbsent(elt)
        }
    }
    mutating func appendIfIdenticalAbsent(_ obj: Element) where Element: AnyObject {
        for elt: Element in self {
            if elt === obj {
                return
            }
        }
        self.append(obj)
    }
    mutating func appendIfIdenticalAbsent(_ arr: [Element]) where Element: AnyObject {
        for elt: Element in arr {
            appendIfIdenticalAbsent(elt)
        }
    }
    mutating func appendIfAbsent(_ obj: Element, _ operation_if_added: () -> Void)
    where Element: Equatable {
        if appendIfAbsent(obj) {
            operation_if_added()
        }
    }
    func setEqual(hashable_arr: [Element]) -> Bool where Element: Hashable {
        let self_set: Set<Element> = Set(self)
        for elt: Element in hashable_arr {
            if !self_set.contains(elt) {
                return false
            }
        }
        return true
    }
    //alternate version of setEqual, less efficient but works for non-hashable elements but comparable elements
    func setEqual(comparable_arr: [Element]) -> Bool where Element: Comparable {
        var self_sorted: [Element] = self
        var arr_sorted: [Element] = comparable_arr
        self_sorted.sort()
        arr_sorted.sort()
        var j: Int = 0
        var i: Int = 0
        while true {
            if i >= self_sorted.count && j >= arr_sorted.count {
                return true
            }

            if arr_sorted[j] != self_sorted[i] {
                return false
            }

            i = (i == self_sorted.count) ? i : i + 1
            j = (j == arr_sorted.count) ? j : j + 1

            while i < self_sorted.count && self_sorted[i] == self_sorted[i - 1] {
                i += 1
            }
            while j < arr_sorted.count && arr_sorted[j] == arr_sorted[j - 1] {
                j += 1
            }
        }
    }
    //alternate version of setEqual, minimally efficient but works for non-hashable elements and non-comparable elements
    func setEqual(equatable_arr: [Element]) -> Bool where Element: Equatable {
        for elt: Element in equatable_arr {
            if !self.contains(elt) {
                return false
            }
        }
        return true
    }

}

extension Int {
    func squared() -> Int {
        return self * self
    }
    func odd() -> Bool {
        return self % 2 == 1
    }
    func fromAscii() -> String {
        return (self > 31 && self < 127) ? String(UnicodeScalar(self)!) : String(self)
    }
}

extension String {
    subscript(index: Int) -> String {
        return String(self[self.index(startIndex, offsetBy: index)])
    }

    func map(_ operation: (String) -> Any) -> [Any] {
        var return_val: [Any] = []

        for i in 0..<count {
            return_val.append(operation(self[i]))
        }

        return return_val
    }
}
