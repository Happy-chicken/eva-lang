#include "src/EvaLLVM.h"

int main() {
    EvaLLVM eva;
    std::string program = R"(
        (class Point null 
            (begin 
                (var x 0)
                (var y 0)
                (def constructor (self x y)
                    (begin
                        0))


                (def calc (self) 0)

            )
        )

        (var p (new Point 10 20))
        (print "p.x = %d" (prop p x))
    )";// Not escaping a string
    eva.exec(program);
    return 0;
}

// local var
// (var a 10)
// (begin
//     (var a "str")
//     (print "a=%s" a)
// )
// (print "VERSION=%d" VERSION)

// if-else
// (var x 42)
// (if (== x 42)
//     (if (> x 42)
//     (set x 1)
//     (set x 3)
//     )
// (set x 2)
// )
// (print "%d" x)

// (def fun ((x string)) -> string x)
// //(square 2)
// (var (a string) (fun "sample"))
// (print "%s" a)