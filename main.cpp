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
                        (set (prop self x) x)
                        (set (prop self y) y)
                        ))


                (def calc (self) 0)

            )
        )

        (class Point3D Point 
            (begin 
                (var z 0)
                (def constructor (self x y z)
                    (begin
                        (set (prop self x) x)
                        (set (prop self y) y)
                        (set (prop self z) z)
                        ))

                (def calc (self) 1)
            )

        )

        (var p1 (new Point 10 20))
        (var p2 (new Point3D 10 20 30))
        (print "p1.x = %d" (prop p1 x))
        (print "p2.x = %d" (prop p2 x))
        (print "p2.z = %d" (prop p2 z))
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