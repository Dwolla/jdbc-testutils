$version: "2.0"
namespace com.dwolla.test

string MySmithy4sNewtype
integer Smithy4sPrimitiveNewtype

list MyList {
    member: String
}

map MyMap {
    key: String
    value: Integer
}

structure MyStructure {
    foo: String

    @required
    baz: Integer

    greeting: String = "Hello"
}

union MyUnion {
    i32: Integer
    string: String,
}

enum MyEnum {
    DIAMOND
    CLUB
    HEART
    SPADE
}

structure MyRecursive {
    foo: String
    recurse: MyRecursive
}
