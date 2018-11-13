"""Generator for C/C++."""

import struct as _struct
import hashlib

class Generable(object):
    def __str__(self):
        """Return a single string (possibly containing newlines) representing
        this code construct."""
        return "\n".join(l.rstrip() for l in self.generate())

    def generate(self, with_semicolon=True):
        """Generate (i.e. yield) the lines making up this code construct."""

        raise NotImplementedError


# {{{ declarators

class Declarator(Generable):
    def generate(self, with_semicolon=True):
        tp_lines, tp_decl = self.get_decl_pair()
        tp_lines = list(tp_lines)
        for line in tp_lines[:-1]:
            yield line
        sc = ";"
        if not with_semicolon:
            sc = ""
        if tp_decl is None:
            yield "%s%s" % (tp_lines[-1], sc)
        else:
            yield "%s %s%s" % (tp_lines[-1], tp_decl, sc)

    def get_decl_pair(self):
        """Return a tuple ``(type_lines, rhs)``.

        *type_lines* is a non-empty list of lines (most often just a
        single one) describing the type of this declarator. *rhs* is the right-
        hand side that actually contains the function/array/constness notation
        making up the bulk of the declarator syntax.
        """

    def inline(self, with_semicolon=True):
        """Return the declarator as a single line."""
        tp_lines, tp_decl = self.get_decl_pair()
        tp_lines = " ".join(tp_lines)
        if tp_decl is None:
            return tp_lines
        else:
            return "%s %s" % (tp_lines, tp_decl)

class Value(Declarator):
    """A simple declarator: *typename* and *name* are given as strings."""

    def __init__(self, typename, name):
        self.typename = typename
        self.name = name

    def get_decl_pair(self):
        return [self.typename], self.name

    def struct_maker_code(self, data):
        raise RuntimeError("named-type values can't be put into structs")

    def struct_format(self):
        raise RuntimeError("named-type values have no struct format")

    def default_value(self):
        return 0


class NestedDeclarator(Declarator):
    def __init__(self, subdecl):
        self.subdecl = subdecl

    @property
    def name(self):
        return self.subdecl.name

    def struct_format(self):
        return self.subdecl.struct_format()

    def alignment_requirement(self):
        return self.subdecl.alignment_requirement()

    def struct_maker_code(self, data):
        return self.subdecl.struct_maker_code(data)

    def get_decl_pair(self):
        return self.subdecl.get_decl_pair()


class DeclSpecifier(NestedDeclarator):
    def __init__(self, subdecl, spec, sep=' '):
        NestedDeclarator.__init__(self, subdecl)
        self.spec = spec
        self.sep = sep

    def get_decl_pair(self):
        def add_spec(sub_it):
            it = iter(sub_it)
            try:
                yield "%s%s%s" % (self.spec, self.sep, next(it))
            except StopIteration:
                pass

            for line in it:
                yield line

        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return add_spec(sub_tp), sub_decl


class NamespaceQualifier(DeclSpecifier):
    def __init__(self, namespace, subdecl):
        DeclSpecifier.__init__(self, subdecl, namespace, '::')


class Typedef(DeclSpecifier):
    def __init__(self, subdecl):
        DeclSpecifier.__init__(self, subdecl, "typedef")


class Static(DeclSpecifier):
    def __init__(self, subdecl):
        DeclSpecifier.__init__(self, subdecl, "static")


class Const(NestedDeclarator):
    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return sub_tp, ("const %s" % sub_decl)


class Extern(DeclSpecifier):
    def __init__(self, language, subdecl):
        self.language = language
        super(Extern, self).__init__(subdecl, "extern \"%s\"" % language)


class TemplateSpecializer(NestedDeclarator):
    def __init__(self, specializer, subdecl):
        self.specializer = specializer
        NestedDeclarator.__init__(self, subdecl)

    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        sub_tp[-1] = sub_tp[-1] + '<%s>' % self.specializer
        return sub_tp, sub_decl


class MaybeUnused(NestedDeclarator):
    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return sub_tp, ("%s __attribute__ ((unused))" % sub_decl)


class AlignedAttribute(NestedDeclarator):
    def __init__(self, align_bytes, subdecl):
        super(AlignedAttribute, self).__init__(subdecl)
        self.align_bytes = align_bytes

    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return sub_tp, ("%s __attribute__ ((aligned (%d)))"
                % (sub_decl, self.align_bytes))


class Pointer(NestedDeclarator):
    def __init__(self, subdecl):
        NestedDeclarator.__init__(self, subdecl)

    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return sub_tp, ("*%s" % sub_decl)

    def struct_maker_code(self, data):
        raise NotImplementedError

    def struct_format(self):
        return "P"

    def alignment_requirement(self):
        return _struct.calcsize(self.struct_format())


class RestrictPointer(Pointer):
    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return sub_tp, ("*__restrict__ %s" % sub_decl)


class Reference(Pointer):
    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        return sub_tp, ("&%s" % sub_decl)


class ArrayOf(NestedDeclarator):
    def __init__(self, subdecl, count=None):
        NestedDeclarator.__init__(self, subdecl)
        self.count = count

    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()
        if self.count is None:
            count_str = ""
        else:
            count_str = str(self.count)
        return sub_tp, ("%s[%s]" % (sub_decl, count_str))

    def struct_maker_code(self, name):
        return ", ".join("%s[%d]" % (name, i) for i in range(self.count))

    def struct_format(self):
        if self.count is None:
            return "P"
        else:
            return "%d%s" % (self.count, self.subdecl.struct_format())

    def alignment_requirement(self):
        return self.subdecl.alignment_requirement()

    def default_value(self):
        return self.count*[self.subdecl.default_value()]


class FunctionDeclaration(NestedDeclarator):
    def __init__(self, subdecl, arg_decls):
        NestedDeclarator.__init__(self, subdecl)
        self.arg_decls = arg_decls

    def get_decl_pair(self):
        sub_tp, sub_decl = self.subdecl.get_decl_pair()

        return sub_tp, ("%s(%s)" % (
            sub_decl,
            ", ".join(ad.inline() for ad in self.arg_decls)))

    def struct_maker_code(self, data):
        raise RuntimeError("function pointers can't be put into structs")

    def struct_format(self):
        raise RuntimeError("function pointers have no struct format")

# }}}


# {{{ struct-like

class Struct(Declarator):
    """A structure declarator."""

    def __init__(self, tpname, fields, declname=None):
        """Initialize the structure declarator.
        *tpname* is the name of the structure, while *declname* is the
        name used for the declarator.
        *fields* is a list of :class:`Declarator` instances.
        """
        self.tpname = tpname
        self.fields = fields
        self.declname = declname

    def get_decl_pair(self):
        def get_tp():
            if self.tpname is not None:
                yield "struct %s" % self.tpname
            else:
                yield "struct"
            yield "{"
            for f in self.fields:
                for f_line in f.generate():
                    yield "  " + f_line
            yield "} " + self.struct_attributes()
        return get_tp(), self.declname

    def alignment_requirement(self):
        return max(f.alignment_requirement() for f in self.fields)

    def struct_attributes(self):
        return ""


class GenerableStruct(Struct):
    def __init__(self, tpname, fields, declname=None):
        """Initialize a structure declarator.
        *tpname* is the name of the structure, while *declname* is the
        name used for the declarator.
        *fields* is a list of :class:`Declarator` instances.
        """

        format = "".join(f.struct_format() for f in fields)
        bytes = _struct.calcsize(format)

        Struct.__init__(self, tpname, fields, declname)

        self.format = format
        self.bytes = bytes

        assert _struct.calcsize(self.format) == self.bytes

    def make(self, **kwargs):
        """Build a binary, packed representation of *self* in a
        :class:`str` instance with members set to the values specified
        in *kwargs*.
        """
        return self._maker()(_struct.pack, **kwargs)

    def make_with_defaults(self, **kwargs):
        """Build a binary, packed representation of *self* in a
        :class:`str` instance with members set to the values specified
        in *kwargs*.

        Unlike :meth:`make`, not all members have to occur in *kwargs*.
        """
        return self._maker(with_defaults=True)(_struct.pack, **kwargs)

    def _maker(self, with_defaults=False):
        def format_arg(f):
            if with_defaults:
                return "%s=%s" % (f.name, repr(f.default_value()))
            else:
                return f.name

        code = "lambda pack, %s: pack(%s, %s)" % (
                ", ".join(format_arg(f) for f in self.fields),
                repr(self.struct_format()),
                ", ".join(f.struct_maker_code(f.name) for f in self.fields))
        return eval(code)

    def struct_format(self):
        """Return the format of the struct as digested by the :mod:`struct`
        module.
        """
        return self.format

    def __len__(self):
        """Return the number of bytes occupied by this struct."""
        return self.bytes
# }}}

# {{{
class Enum(Declarator):
    """A structure declarator."""

    def __init__(self, tpname, fields):
        """Initialize the structure declarator.
        *tpname* is the name of the structure, while *declname* is the
        name used for the declarator.
        *fields* is a list of :class:`Declarator` instances.
        """
        self.tpname = tpname
        self.fields = fields

    def get_decl_pair(self):
        def get_tp():
            if self.tpname is not None:
                yield "enum %s" % self.tpname
            else:
                yield "struct"
            yield "{"
            for f in self.fields:
                for f_line in f.generate():
                    yield "  " + f_line
            yield "} " + self.struct_attributes()
        return get_tp()

    def alignment_requirement(self):
        return max(f.alignment_requirement() for f in self.fields)

    def struct_attributes(self):
        return ""
# }}}

# {{{ template

class Template(NestedDeclarator):
    def __init__(self, template_spec, subdecl):
        self.template_spec = template_spec
        self.subdecl = subdecl

    def generate(self, with_semicolon=False):
        yield "template <%s>" % self.template_spec
        for i in self.subdecl.generate(with_semicolon):
            yield i

# }}}


# {{{ control flow/statement stuff

class If(Generable):
    def __init__(self, condition, then_, else_=None):
        self.condition = condition

        assert isinstance(then_, Generable)
        if else_ is not None:
            assert isinstance(else_, Generable)

        self.then_ = then_
        self.else_ = else_

    def generate(self, with_semicolon=True):
        cond_str = str(self.condition)
        condition_lines = cond_str.split("\n")
        if len(condition_lines) > 1:
            yield "if ("
            for l in condition_lines:
                yield "    "+l
            yield "  )"
        else:
            yield "if (%s)" % self.condition

        if isinstance(self.then_, Block):
            for line in self.then_.generate():
                yield line
        else:
            for line in self.then_.generate():
                yield "  "+line

        if self.else_ is not None:
            yield "else"
            if isinstance(self.else_, Block):
                for line in self.else_.generate():
                    yield line
            else:
                for line in self.else_.generate():
                    yield "  "+line


class Loop(Generable):
    def __init__(self, body):
        self.body = body

    def generate(self):
        if self.intro_line() is not None:
            yield self.intro_line()

        if isinstance(self.body, Block):
            for line in self.body.generate():
                yield line
        else:
            for line in self.body.generate():
                yield "  "+line

        if self.outro_line() is not None:
            yield self.outro_line()

    def outro_line(self):
        return None


class CustomLoop(Loop):
    def __init__(self, intro_line, body, outro_line=None):
        self.intro_line_ = intro_line
        self.body = body
        self.outro_line_ = outro_line

    def intro_line(self):
        return self.intro_line_

    def outro_line(self):
        return self.outro_line_


class While(Loop):
    def __init__(self, condition, body):
        self.condition = condition
        assert isinstance(body, Generable)
        self.body = body

    def intro_line(self):
        return "while (%s)" % self.condition


class For(Loop):
    def __init__(self, start, condition, update, body):
        self.start = start
        self.condition = condition
        self.update = update

        assert isinstance(body, Generable)
        self.body = body

    def intro_line(self):
        return "for (%s; %s; %s)" % (self.start, self.condition, self.update)


class DoWhile(Loop):
    def __init__(self, condition, body):
        self.condition = condition
        assert isinstance(body, Generable)
        self.body = body

    def intro_line(self):
        return "do"

    def outro_line(self):
        return "while (%s);" % self.condition


def make_multiple_ifs(conditions_and_blocks, base=None):
    if base == "last":
        _, base = conditions_and_blocks[-1]
        conditions_and_blocks = conditions_and_blocks[:-1]

    for cond, block in conditions_and_blocks[::-1]:
        base = If(cond, block, base)
    return base

# }}}


# {{{ simple statements

class Define(Generable):
    def __init__(self, symbol, value):
        self.symbol = symbol
        self.value = value

    def generate(self):
        yield "#define %s %s" % (self.symbol, self.value)


class Include(Generable):
    def __init__(self, filename, system=True):
        self.filename = filename
        self.system = system

    def generate(self):
        if self.system:
            yield "#include <%s>" % self.filename
        else:
            yield "#include \"%s\"" % self.filename

class Pragma(Generable):
    def __init__(self, value):
        self.value = value

    def generate(self):
        yield "#pragma %s" % (self.value)


class Statement(Generable):
    def __init__(self, text):
        self.text = text

    def generate(self):
        yield self.text+";"


class ExpressionStatement(Generable):
    def __init__(self, expr):
        self.expr = expr

    def generate(self):
        yield str(self.expr)+";"


class Assign(Generable):
    def __init__(self, lvalue, rvalue):
        self.lvalue = lvalue
        self.rvalue = rvalue

    def generate(self):
        yield "%s = %s;" % (self.lvalue, self.rvalue)


class Line(Generable):
    def __init__(self, text=""):
        self.text = text

    def generate(self):
        yield self.text


class Comment(Generable):
    def __init__(self, text, skip_space=False):
        self.text = text
        if skip_space:
            self.fmt_str = "/*%s*/"
        else:
            self.fmt_str = "/* %s */"

    def generate(self):
        yield self.fmt_str % self.text


class MultilineComment(Generable):
    def __init__(self, text, skip_space=False):
        self.text = text
        self.skip_space = skip_space

    def generate(self):
        yield '/**'
        if self.skip_space is True:
            line_begin, comment_end = '*', '*/'
        else:
            line_begin, comment_end = ' * ', ' */'
        for line in self.text.splitlines():
            yield line_begin + line
        yield comment_end


class LineComment(Generable):
    def __init__(self, text):
        assert "\n" not in text
        self.text = text

    def generate(self):
        yield "// %s" % self.text


def add_comment(comment, stmt):
    if comment is None:
        return stmt

    if isinstance(stmt, Block):
        result = Block([Comment(comment), Line()])
        result.extend(stmt.contents)
        return result
    else:
        return Block([Comment(comment), Line(), stmt])

# }}}


# {{{ initializers

class Initializer(Generable):
    def __init__(self, vdecl, data):
        self.vdecl = vdecl
        self.data = data

    def generate(self):
        tp_lines, tp_decl = self.vdecl.get_decl_pair()
        tp_lines = list(tp_lines)
        for line in tp_lines[:-1]:
            yield line
        if isinstance(self.data, str) and "\n" in self.data:
            data_lines = self.data.split("\n")
            yield "%s %s =" % (tp_lines[-1], tp_decl)
            for i, l in enumerate(data_lines):
                if i == len(data_lines)-1:
                    yield "  %s;" % l
                else:
                    yield "  %s" % l
        else:
            yield "%s %s = %s;" % (tp_lines[-1], tp_decl, self.data)


class InlineInitializer(Initializer):
    """
    Class to represent Initializers (int i=0) without the semicolon in the end
    (e.g. in a for statement)
    Usage: same as cgen.Initializer
    Result: same as cgen.Initializer except for the lack of a semi-colon at the end
    """
    def generate(self):
        result = super(InlineInitializer, self).generate()
        for v in result:
            if v.endswith(';'):
                yield v[:-1]
            else:
                yield v


def Constant(vdecl, data):
    return Initializer(Const(vdecl), data)


class ArrayInitializer(Generable):
    def __init__(self, vdecl, data):
        self.vdecl = vdecl
        self.data = data

    def generate(self):
        for v_line in self.vdecl.generate(with_semicolon=False):
            yield v_line
        yield "  = { %s };" % (", ".join(str(item) for item in self.data))


class FunctionBody(Generable):
    def __init__(self, fdecl, body):
        """Initialize a function definition. *fdecl* is expected to be
        a :class:`FunctionDeclaration` instance, while *body* is a
        :class:`Block`.
        """

        self.fdecl = fdecl
        self.body = body

    def generate(self):
        for f_line in self.fdecl.generate(with_semicolon=False):
            yield f_line
        for b_line in self.body.generate():
            yield b_line

# }}}


# {{{ block

class Block(Generable):
    def __init__(self, contents=[]):
        if(isinstance(contents, Block)):
            contents = contents.contents
        self.contents = contents[:]

        for item in contents:
            assert isinstance(item, Generable)

    def generate(self):
        yield "{"
        for item in self.contents:
            for item_line in item.generate():
                yield "  " + item_line
        yield "}"

    def append(self, data):
        self.contents.append(data)

    def extend(self, data):
        self.contents.extend(data)

    def insert(self, i, data):
        self.contents.insert(i, data)

    def extend_log_block(self, descr, data):
        self.contents.append(Comment(descr))
        self.contents.extend(data)
        self.contents.append(Line())


def block_if_necessary(contents):
    if len(contents) == 1:
        return contents[0]
    else:
        return Block(contents)


class LiteralLines(Generable):
    def __init__(self, text):
        # accommodate pyopencl syntax highlighting
        text = text.lstrip("//CL//")

        if not text.startswith("\n"):
            raise ValueError("expected newline as first character "
                    "in literal lines")

        lines = text.split("\n")
        while lines[0].strip() == "":
            lines.pop(0)
        while lines[-1].strip() == "":
            lines.pop(-1)

        if lines:
            base_indent = 0
            while lines[0][base_indent] in " \t":
                base_indent += 1

            for line in lines[1:]:
                if line[:base_indent].strip():
                    raise ValueError("inconsistent indentation")

        self.lines = [line[base_indent:] for line in lines]

    def generate(self):
        for line in self.lines:
            yield line


class LiteralBlock(LiteralLines):
    def generate(self):
        yield "{"
        for line in self.lines:
            yield "  " + line
        yield "}"


class Collection(Block):
    def generate(self):
        for c in self.contents:
            for line in c.generate():
                yield line


Module = Collection


class IfDef(Module):
    """
    Class to represent IfDef-Else-EndIf construct for the C preprocessor.
    :param condition: the condition in IfDef
    :param iflines: the block of code inside the if [an array of type Generable]
    :param elselines: the block of code inside the else [an array of type Generable]
    """
    def __init__(self, condition, iflines, elselines):
        ifdef_line = Line('#ifdef %s' % condition)
        if len(elselines):
            elselines.insert(0, Line('#else'))
        endif_line = Line('#endif')
        lines = [ifdef_line]+iflines+elselines+[endif_line]
        super(IfDef, self).__init__(lines)


class IfNDef(Module):
    """
    Class to represent IfNDef-Else-EndIf construct for the C preprocessor.
    :param condition: the condition in IfNDef
    :param ifndeflines: the block of code inside the if not [an array of type Generable]
    :param elselines: the block of code inside the else [an array of type Generable]
    """
    def __init__(self, condition, ifndeflines, elselines):
        ifndefdef_line = Line('#ifndef %s' % condition)
        if len(elselines):
            elselines.insert(0, Line('#else'))
        lines = [ifndefdef_line]+ifndeflines+elselines+[Line('#endif')]
        super(IfNDef, self).__init__(lines)


class PrivateNamespace(Block):
    def get_namespace_name(self):
        checksum = hashlib.md5()

        for c in self.contents:
            for line in c.generate():
                checksum.update(line.encode('utf-8'))

        return "private_namespace_"+checksum.hexdigest()

    def generate(self):
        yield "namespace "+self.get_namespace_name()
        yield "{"
        for item in self.contents:
            for item_line in item.generate():
                yield "  " + item_line
        yield "}"
        yield ""
        yield "using namespace %s;" % self.get_namespace_name()

# }}}


# {{{ literals

class Literal(Generable):
    def __init__(self, value):
        self.value = value

    def generate(self, with_semicolon=False):
        yield self.value

class String(Generable):
    def __init__(self, value):
        self.value = value #TODO: Check the value 
    def generate(self, with_semicolon=False):
        yield "std::string{\"%s\"}" % self.value
        return 

class Int(Generable):
    def __init__(self, value):
        self.value = value  #TODO: Check the value 
    def generate(self, with_semicolon=False):
        yield "int{%s}" % self.value
        return

class Float(Generable):
    def __init__(self, value):
        self.value = value  #TODO: Check the value 
    def generate(self, with_semicolon=False):
        yield "float{%s}" % self.value
        return

# }}}