#include "./include/EvaIRGener.h"

int main() {
    EvaLLVM eva;
    std::string program = R"(
        // (class Point null 
        //     (begin 
        //         (var x 0)
        //         (var y 0)
        //         (def constructor (self x y)
        //             (begin
        //                 (set (prop self x) x)
        //                 (set (prop self y) y)
        //                 ))


        //         (def calc (self x) (print "calc %d in Point\n" x))
        //         (def calc2 (self x) (print "calc2 %d  in Point\n" x))

        //     )
        // )

        // (class Point3D Point 
        //     (begin 
        //         (var z 0)
        //         (def constructor (self x y z)
        //             (begin
        //                 (set (prop self x) x)
        //                 (set (prop self y) y)
        //                 (set (prop self z) z)
        //                 ))

        //         (def calc (self x) (print "calc %d in Point3D\n" x))
        //     )

        //)

        // (def check ((obj Point))
        //     (begin
        //         ((method obj calc) obj 1)
        //     )
        // )

        // (var p1 (new Point 10 20))
        // (var p2 (new Point3D 10 20 30))

        // ((method p1 calc) p1 1)
        // ((method p1 calc2) p2 2)
        // ((method (super Point3D) calc) p2 3)
        
        // (check p1)
        // (check p2)
        // (print "p1.x = %d" (prop p1 x))
        // (print "p2.x = %d" (prop p2 x))
        // (print "p2.z = %d" (prop p2 z))

        (class Transformer null
            (begin 
                (var factor 0)
                (def constructor (self factor) -> Transformer
                    (begin
                        (set (prop self factor) factor)
                        self
                    )
                )

                (def __call__ (self v)
                    (* (prop self factor) v)
                )
            )
        )
        
        (var transform (new Transformer 5))
        (print "(transform 10) = %d\n" (transform 10))
        
        (def calculate (x (modify Transformer)) 
            (modify x)
        )
        (print "(calculate 10 transform) = %d\n" (calculate 10 transform))

        (list a (1 2 3))
        (var b 2)
        (while (>= b 1)
            (begin
                (print "a=%d\n" (get a b))
                (set b (- b 1))
            )
        )
        (print "a=%d\n" -1)
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