error_chain! {
    foreign_links {
        Fmt(::std::fmt::Error);
        Io(::std::io::Error);
    }

    errors {
        EoF {
            description("end of file")
            display("end of file reached")
        }
        UnexpectedType {
            description("unexpected type")
            display("unexpected type")
        }
        Parser {
            description("Parser error")
            display("Parser error")
        }
        InvalidArguments(t: String) {
            description("invalid arguments")
            display("invalid argument: {}", t)
        }
        NotAFunction {
            description("not a function")
            display("is not a function")
        }
        RuntimeAssertion {
            description("runtime assertion")
            display("runtime assertion")
        }
        NotEnoughArguments(e: usize, a: usize) {
            description("Not Enough Arguments")
            display("not enough arguments: expected: {}, actual: {}", e, a)
        }
        NotImplemented {
            description("not implemeneted error")
            display("Not implemented")
        }
    }
}
