import compiler.cgen as c
import compiler.builtins as b

print(
    c.Module([
        c.Include("string", True),
        c.Include("iostream", True),
        c.FunctionBody(
            c.FunctionDeclaration(c.Value("int", "main"), []),
            c.Block([
                c.Initializer(c.Value("std::string", "x"), c.String("Hey there")),
                c.Initializer(c.Value("float", "z"), c.Float(3.5)),
                b.Print("x"),
                c.Statement('std::cout << x << std::endl')
            ])
        )
    ])
)