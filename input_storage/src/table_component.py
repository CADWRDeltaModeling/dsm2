"""@package table_component Module for defining user-defined data types.

   TableComponent is the whole user object, for instance a channel.

   The various Field subclasses (CharField, DoubleField, IntField ...) are for
   defining individual fields within the TableComponent. For instance, if TableComponent
   defines a channel, it might have an IntField representing channel number
   and a DoubleField representing friction.
"""



class Field(object):
    """ Superclass representing a single field of a user-defined data type.
        This is an abstract class that contains defaults that are shared across
        most data types. For client code, you should use subclasses such as DoubleField
        or CharField
    """

    def __init__(self,name,type):
        self.name = name
        self.type = type

    def c_arg(self,input = True):
        arg = "%s & a_%s" % (self.type,self.name)
        if (input):
            arg="const "+arg
        return arg

    def fc_arg(self,input = True):
        arg = "%s * a_%s" % (self.type,self.name)
        if (input):
            arg="const "+arg
        return arg

    def stringlen_arg(self,input = True):
        return None

    def stringlen_assign(self):
        return None

    def fortran_pointer(self):
        return "*"

    def initializer(self,style):
        if not style in ("arg","copy","default"): raise IllegalArgumentException("Style argument not understood")
        if style == "arg": val = "a_" + self.name
        if style == "copy": val = "other." + self.name
        if style == "default": val = "%s" % self.default()
        return "%s(%s)" % (self.name,val)

    def identifier_type(self):
        return "const %s&" % self.type

    def identifier_assign(self,ndx):
        return self.assign(other="identifier.get<%s>()" % ndx)

    def constructor(self,copy):
        return None

    def equaler(self):
        return "this->%s=rhs.%s;" % (self.name,self.name)

    def member(self):
        return "%s %s;" % (self.type,self.name)

    def simple_assign(self,name,other):
        return "%s=%s;" % (name,other)

    def assign(self,prefix1="",prefix2="",other=None):
        if (not other): other = self.name
        other = prefix2+other
        return self.simple_assign("%s%s" % (prefix1,self.name), \
                                  "%s" % other)

    def default(self):
        return -901

    def pad(self):
        return None

    def hdf_type(self):
        raise NotImplementedError("HDF type not defined")

    def output_width(self):
        return len(self.name)+2

    def output_format(self):
        return \
        """ setw(%s)
            << setfill(\' \')
            << left
            << obj.%s
        """ % (self.output_width(), self.name)

    def instream_format(self):
        raise NotImplementedError

class CharField(Field):
    """Character field within a client data type"""

    def __init__(self,name,size,format_width):
        """ Construct a character field
             name name of field
            size fixed dimension in memory of character array
            format_width suggested formatting width when writing as a table
        """
        Field.__init__(self,name,"char[%s]" % size)
        self.size = size
        self.format_width=format_width

    def pad(self,arg=None):
        if (not arg):arg=self.name
        return "if (strlen(%s) < %s)fill(%s+strlen(%s),%s+%s,' ');" % (arg,self.size,arg,arg,arg,self.size)

    def c_arg(self,input = True):
        arg = " char a_%s[%s]" % (self.name,self.size)
        if (input):
            arg="const "+arg
        return arg

    def fc_arg(self,input = True):
        return self.c_arg(input)

    def fortran_type(self):
        return "character(len=%s)" % self.size

    def initializer(self,style):
        return None

    def identifier_type(self):
        return "const std::string"

    def identifier_assign(self,ndx):
        return self.assign(other="identifier.get<%s>().c_str()" % ndx)

    def simple_assign(self,name,other):
        return "memcpy(%s,%s,%s);" % (name,other,self.size)

    def constructor(self,style):
        if not style in ("arg","copy","default"): raise IllegalArgumentException("Style argument not understood")
        if (style == "arg" or style == "copy"):
            if style == "arg": val = "a_" + self.name
            if style == "copy": val = "other." + self.name
            return "memcpy(%s,%s,%s);" % (self.name,val,self.size)
        if style == "default":
            blank = "fill_n(%s,%s,\'\\0\');" % (self.name,self.size)
            return blank  #+ "\n" + copy

    def fortran_pointer(self):
        return ""

    def equaler(self):
        return "strcpy(this->%s,rhs.%s);" % (self.name,self.name)

    def member(self):
        return "char %s[%s];" % (self.name,self.size)

    def default(self):
        return "\"\""

    def hdf_type(self):
        return "string_type(%s)" % self.size

    def stringlen_arg(self, input):
        arg = "%s %s_len" % ("int",self.name)
        if (input):
            arg="const "+arg
        return arg

    def stringlen_assign(self):
        return "%s_len=(int)strlen(a_%s);" % (self.name,self.name)

    def output_width(self):
        return max( len(self.name)+2, self.size)

    def output_format(self):
        return \
        """
            setw(max(4+%s,(int)(4+strlen(obj.%s))))
            << setfill(\' \')
            << left
            << quote_spaces(obj.%s, %s)
        """ % (self.format_width, self.name, self.name,self.size)

    def input_code(self):
        return \
   """
   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }
   if(beg->size()<= %s)
   {
        strcpy(obj.%s, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width %s):" + (*beg));
   }
   """ % (self.size, self.name,self.size)




class DoubleField(Field):
    """Double precision float field within a client data type"""

    def __init__(self,name,width,precision=4):
        """ Create double field with the given name, printing width, decimal precision"""
        Field.__init__(self,name,"double")
        if (width < len(name) + 2):
            raise ValueError("Specified width too short: must be > length of name+2 ")
        self.width = width
        self.precision = precision

    def default(self):
        return -901.

    def hdf_type(self):
        return "H5T_NATIVE_DOUBLE"

    def output_format(self):
        return \
        """
            setw(%s)
            << setfill(\' \')
            << setprecision(%s)
            << left
            << obj.%s
        """ % (self.width,self.precision,self.name)

    def fortran_type(self):
        return "real(8)"

    def input_code(self):
        return \
        """
        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        obj.%s = strtod((beg++)->c_str(),NULL);
         """ % (self.name)




class BoolField(Field):
    """Boolean field within a client data type [untested and possibly incomplete]"""

    def __init__(self,name):
        Field.__init__(self,name,"int")

    def default(self):
        return 0

    def hdf_type(self):
        return "H5T_NATIVE_INT"

    def fortran_type(self):
        return "logical"



class IntField(Field):
    """Integer field within a client data type"""

    def __init__(self,name):
        Field.__init__(self,name,"int32_t")

    def hdf_type(self):
        return "H5T_NATIVE_INT"

    def input_code(self):
        return \
        """
        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.%s;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert %s to correct data type:"+tempstr);
        }
        """ % (self.name,self.name)

    def fortran_type(self):
        return "integer"



class TableComponent(object):
    """ Definition of the user data type for a table.

    This is the class that the user must define.
    """

    def __init__(self,name,members,identifiers,parent=None,parent_id=None):
        self.members = members
        self.name = name
        self.identifiers = identifiers
        self.parent=parent
        self.parent_id=parent_id
        if (parent and not parent_id):
            raise ValueError("If parent table specified, its identifier(s) must also be specified")

    def get_member(self,name):
        for m in self.members:
            if m.name == name: return m
        raise ValueError("Member %s not found" % name)
