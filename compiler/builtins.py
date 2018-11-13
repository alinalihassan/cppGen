from compiler.cgen import Generable

class Print(Generable):
    def __init__(self, text):
        self.text = text

    def generate(self):
        yield "std::cout << %s;" % self.text

class Println(Generable):
    def __init__(self, text):
        self.text = text

    def generate(self):
        yield "std::cout << %s << '\n';" % self.text

