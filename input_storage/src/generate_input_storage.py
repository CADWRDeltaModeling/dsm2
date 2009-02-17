"""Module for creating code in C++ to read and store user-defined data structures.

   This script can be used by a driver that defines the user data types. The script then
   converts the data type description into C++ code.
"""

import string
import os.path
indir = os.path.join(os.path.dirname(__file__),"template")
include_lines = []
input_map_lines = []
include_defs = []
prioritize_buffer_lines=[]
clear_buffer_lines=[]
fortran_include_lines=[]

class Field(object):
    """ Superclass representing attributes of a single field of a user-defined data object."""

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
    def __init__(self,name,size,format_width):
        Field.__init__(self,name,"char[%s]" % size)
        self.size = size
        self.format_width=format_width
    
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

    def simple_assign(self,name,other):
        return "strcpy(%s,%s);" % (name,other)        
            
    def constructor(self,style):
        if not style in ("arg","copy","default"): raise IllegalArgumentException("Style argument not understood")
        if (style == "arg" or style == "copy"):
            if style == "arg": val = "a_" + self.name
            if style == "copy": val = "other." + self.name
            return "strcpy(%s,%s);" % (self.name,val)
        if style == "default":
            blank = "fill_n(%s,%s,\'\\0\');" % (self.name,self.size)
            #if self.default == "": return blank
            #copy = "strcpy(%s,%s);" % (self.name,self.default())
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
            setw(max(%s,(int)strlen(obj.%s)))
            << setfill(\' \')
            << left
            << obj.%s  
        """ % (self.format_width, self.name, self.name)

    def input_code(self):
        return \
        """
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
    def __init__(self,name,width,precision=4):
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
        obj.%s = strtod((beg++)->c_str(),NULL);
         """ % (self.name)




class BoolField(Field):
    def __init__(self,name):
        Field.__init__(self,name,"int")

    def default(self):
        return 0

    def hdf_type(self):
        return "H5T_NATIVE_INT"

    def fortran_type(self):
        return "logical"


        
class IntField(Field):
    def __init__(self,name):
        Field.__init__(self,name,"int")

    def hdf_type(self):
        return "H5T_NATIVE_INT"

    def input_code(self):
        return \
        """
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
    
def referencify(typename, const=False):
    if ("char" in typename):
        ref = typename.replace("char","char(&)")
    else:
        ref = typename + "&"
    if const: return "const %s" % ref
    return ref


def fortran_declaration(field, intent):
    return "%s, intent(%s) :: %s" % (field.fortran_type(), intent, field.name)


# todo: this may be wrong for child tables? need to delete the unused one first. On other hand, this may be harmless?
def compare_items(component):
    if  component.layered:
        compareitems = "(this->identifier() < other.identifier()) || (this->identifier() == other.identifier() && this->layer > other.layer)"
    else:
        compareitems="(this->identifier() < other.identifier())"
    return compareitems

def prioritize(component):
    if (component.parent == None):
       priority =  \
    """
    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    // Eliminate duplicates. Because of prior ordering, 
    // this will eliminate lower layers
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<@TABLEOBJ>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<@TABLEOBJ>())), buffer().end());
    """
    #
    else:
        priority = \
    """ 
    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    // remove if my version != parent version that is used
    buffer().erase(remove_if(buffer().begin(),
                           buffer().end(),
                           not1(mem_fun_ref(&@TABLEOBJ::parent_valid))),buffer().end());
    """
    priority = priority.replace("@TABLEOBJ",component.name)
    return priority

def ensure_line_in_file(include_line,filename):
    # add .h file to the master .h file that includes all user data types from this script
    do_append = True
    try:
        infile=open(filename,"r")
        includes=infile.readlines()
        if include_line in includes:
            do_append = False
        infile.close()
    except:
        pass  # file does not exist (probably)
    
    if do_append:
        outfile=open(filename,"a")
        outfile.write(include_line)
        outfile.close()




## Generate the C++ code. This is the high level, principle routine of this module.
#
#  The component must be defined by the calling code
def prep_component(component,outdir):
    c_signature = string.join([x.c_arg(False) for x in component.members],",")
    stringlen_output_args = string.join([x.stringlen_arg(False) for x in component.members if x.stringlen_arg(False)],",")
    fortran_c_output_signature=string.join([x.fc_arg(False) for x in component.members],",")

    if (len(stringlen_output_args) > 0):
        fortran_c_output_signature+=(", \n              %s"  % stringlen_output_args)

        
    c_input_signature = string.join([x.c_arg(True) for x in component.members],",")
    stringlen_input_args = string.join([x.stringlen_arg(True) for x in component.members if x.stringlen_arg(True)],",")
    fortran_c_input_signature=string.join([x.fc_arg(True) for x in component.members],",")
    if (len(stringlen_input_args) > 0):
        fortran_c_input_signature+=(", \n              %s"  % stringlen_input_args)


    c_call = string.join([x.name for x in component.members],",")
    strlenassign = string.join(["%s" % x.stringlen_assign() for x in component.members if x.stringlen_assign()],"\n        ")
    construct = string.join([x.constructor("arg") for x in component.members if x.constructor("arg")],"\n    ")
    copyconstruct = string.join([x.constructor("copy") for x in component.members if x.constructor("copy")],"\n    ")
    default_construct = string.join([x.constructor("default") for x in component.members if x.constructor("default")],"\n    ")
    init = string.join([x.initializer("arg") for x in component.members if x.initializer("arg")],",\n    ").strip("\n, ")
    if (init): init += ","
    
    copyinit= string.join([x.initializer("copy") for x in component.members if x.initializer("copy")],",\n    ").strip("\n, ")
    if (copyinit): copyinit += ","
    
    equalop= string.join([x.equaler() for x in component.members if x.equaler()],"\n    ")
    members = string.join([x.member() for x in component.members if x.member()],"\n  ")
    c_pass_through_call =  string.join([x.fortran_pointer()+"a_"+\
                                        x.name for x in component.members],",")
    buffer_query =  string.join([x.assign(x.fortran_pointer()+"a_","obj.") for x in component.members],"\n    ")
    #offsets = string.join(["HOFFSET( %s, %s)" % (component.name, x.name) for x in component.members],",\n            ")
    offsets = string.join([" ((char*)&default_struct.%s - (char*)&default_struct)" \
                          % x.name for x in component.members],",\n            ")
    default_member_data = string.join( ["%s" % x.default() for x in component.members],",")
    default_member_init = string.join( [x.initializer("default") for x in component.members if x.initializer("default")],",\n    ").strip("\n, ")
    if (default_member_init): default_member_init += ","
    quoted_members = string.join(["\""+x.name+"\"" for x in component.members],",")
    hdftypes = string.join([x.hdf_type() for x in component.members],",")
    membersizes=string.join(["sizeof( default_struct.%s )" % x.name for x in component.members],",\n         ")

    identifiers = string.join([component.get_member(x).name for x in component.identifiers],",")
    identifiertypes = string.join([referencify(component.get_member(x).type,True) for x in component.identifiers],",")
    identifiereq = string.join([component.get_member(x).assign(other="identifier.get<%s>()" % i) for x,i in zip(component.identifiers, range(len(component.identifiers)))],"\n      ")

    outstreamformat = string.join([x.output_format() for x in component.members],"<<")
    instreamformat = string.join([x.input_code() for x in component.members], "\n")


    fortran_signature = string.join([x.name for x in component.members], ",")
    fortran_decl_in = "      "  + string.join([fortran_declaration(x,"in") for x in component.members], "\n       ")
    fortran_decl_out = "      "  + string.join([fortran_declaration(x,"out") for x in component.members], "\n       ")


    if component.parent: 
        parent = component.parent
        parentid = string.join(component.parent_id,",")
        headerparent = "#include \"input_storage_%s.h\"" % parent
    else:
        parent = component.name
        parentid = identifiers
        headerparent = ""

    
    compareitems = compare_items(component)
    prioritizecode = prioritize(component)

    
    def do_txt_replace(txt):
        txt = txt.replace("@TABLEOBJ",component.name)
        txt = txt.replace("@NFIELDS","%s" % len(component.members))


        txt = txt.replace("@C_INPUT_SIGNATURE",c_input_signature)
        txt = txt.replace("@C_SIGNATURE",c_signature)

        txt = txt.replace("@C_CALL",c_call)
        txt = txt.replace("@C_PASS_THROUGH_CALL",c_pass_through_call)
        txt = txt.replace("@STRLENASSIGN",strlenassign)

        txt = txt.replace("@INIT",init)
        txt = txt.replace("@CONSTRUCT",construct)
        txt = txt.replace("@COPYINIT",copyinit)
        txt = txt.replace("@COPYCONSTRUCT",copyconstruct)
        txt = txt.replace("@EQUALOP",equalop)
        txt = txt.replace("@MEMBERS",members)
        txt = txt.replace("@BUFFER_QUERY",buffer_query)
        txt = txt.replace("@OFFSETS",offsets)
        txt = txt.replace("@DEFAULT_MEMBER_DATA",default_member_data)
        txt = txt.replace("@DEFAULT_MEMBER_INIT",default_member_init)
        txt = txt.replace("@DEFAULTCONSTRUCT",default_construct)
        txt = txt.replace("@QUOTED_MEMBERS",quoted_members)
        txt = txt.replace("@HDFTYPES",hdftypes)
        txt = txt.replace("@SIZES",membersizes)
        txt = txt.replace("@FORTRAN_C_INPUT_SIGNATURE",fortran_c_input_signature) 
        txt = txt.replace("@FORTRAN_C_OUTPUT_SIGNATURE",fortran_c_output_signature)
        txt = txt.replace("@FORTRAN_SIGNATURE",fortran_signature)
        txt = txt.replace("@FORTRAN_DECL_IN",fortran_decl_in)
        txt = txt.replace("@FORTRAN_DECL_OUT",fortran_decl_out)
        
        txt = txt.replace("@IDENTIFIERTYPES",identifiertypes)
        txt = txt.replace("@IDENTIFIERS",identifiers)
        txt = txt.replace("@IDENTIFIEREQ",identifiereq)

        txt = txt.replace("@COMPARETABLEITEM",compareitems)
        txt = txt.replace("@PRIORITIZE",prioritizecode)
        txt = txt.replace("@ZPARENTIDENTIFIERS",parentid)
        txt = txt.replace("@HEADERPARENT",headerparent)
        txt = txt.replace("@PARENT",parent)

        txt = txt.replace("@OUTSTREAMFMT",outstreamformat)
        txt = txt.replace("@INSTREAMFMT",instreamformat)
        return txt

    # perform object-specific replacements to create C++ .cpp file
    infile=open(os.path.join(indir,"input_storage_template.cpp"),"r")
    outfile=open(os.path.join(outdir,"input_storage_%s.cpp"% component.name),"w")
    txt = infile.read()
    txt = do_txt_replace(txt)
    outfile.write(txt)
    infile.close()
    outfile.close()
    # perform object-specific replacements to create C++ .h file    
    infile=open(os.path.join(indir,"input_storage_template.h"),"r")
    outfile=open(os.path.join(outdir,"input_storage_%s.h" % component.name),"w")
    txt = infile.read()
    txt = do_txt_replace(txt)
    outfile.write(txt)
    infile.close()
    outfile.close()

    # add .h file to the master .h file that includes all user data types from this script
    do_append = True
    include_lines.append("#include \"input_storage_%s.h\"\n" % component.name)
    input_map_lines.append(  "   InputStatePtr %sPtr(new ItemInputState<%s>());\n    inputMap[\"%s\"] = %sPtr;\n" \
                             % (component.name,component.name,component.name.upper(),component.name))

    
    infilename = os.path.join(outdir,"input_storage.h")
    #ensure_line_in_file(include_line,infilename)

    # perform object-specific replacements to create FORTRAN .f90 file for this object   
    infile = open(os.path.join(indir,"fortran_binding_template.f90"),"r")
    txt = infile.read()
    infile.close()
    fortfile = "%s_input_storage.fi" % component.name
    outfile = open(os.path.join(outdir,fortfile),"w")
    txt = do_txt_replace(txt)
    outfile.write(txt)
    outfile.close()
    clear_buffer_lines.append("HDFTableManager<%s>::instance().buffer().clear();" % component.name)
    prioritize_buffer_lines.append("HDFTableManager<%s>::instance().prioritize_buffer();" % component.name)                                  
    fortran_include_lines.append("include \"%s\"" % fortfile)
 
    # add the FORTRAN .f90 file for this object as an include to the main module
    
    

def define_include_block(name,valid_keywords):
    include_defs.append( (name, valid_keywords))

def define_text_sub(name,outdir):
    textsub = name.lower()
    f=open(os.path.join(indir,"text_parser_template.cpp"),'r')
    txt = f.read()
    f.close()
    txt=txt.replace("$TEXTSUBTYPE",textsub)
    f=open(os.path.join(outdir,"text_parser.cpp"),'w')
    f.write(txt)


def process_include_defs():
    include_code = "" 
    for block in include_defs:
        blockName = block[0]
        contextName = "%sContextItems" % blockName
        include_code += "    vector<string> %s;\n" % (contextName)
        for item in block[1]:
            include_code += "    %s.push_back(\"%s\");\n" % (contextName,item.upper());
        include_code += "    InputStatePtr %sPtr(new InsertFileState(%s));\n" % (blockName, contextName)
        include_code += "    inputMap[\"%s\"] = %sPtr;\n" % (blockName.upper(), blockName)
    return include_code



def finalize(outdir):
    f=open(os.path.join(outdir,"input_storage.h"),'w')
    f.write(string.join(include_lines,"\n"))
    f.close
    f=open(os.path.join(indir,"input_state_map_template.cpp"),"r")
    txt = f.read()
    f.close()
    maplines = string.join(input_map_lines,"\n")
    txt=txt.replace("// Item readers DO NOT ALTER THIS LINE AT ALL",maplines)
    includedefs = process_include_defs()
    txt=txt.replace("// Include definitions DO NOT ALTER THIS LINE AT ALL",includedefs)
    f=open(os.path.join(outdir,"input_state_map.cpp"),"w")
    f.write(txt)
    f.close()

    f=open(os.path.join(indir,"buffer_actions_template.cpp"),"r")
    txt=f.read()
    f.close()
    txt=txt.replace("// Clear all buffers DO NOT ALTER THIS LINE AT ALL",string.join(clear_buffer_lines,"\n"))
    txt=txt.replace("// Prioritize all buffers DO NOT ALTER THIS LINE AT ALL",string.join(prioritize_buffer_lines,"\n"))
    f=open(os.path.join(outdir,"buffer_actions.cpp"),"w")
    f.write(txt)
    f.close()

    f=open(os.path.join(indir,"input_storage_fortran_template.f90"),"r")
    txt=f.read()
    f.close()
    txt=txt.replace("// Fortran Include Files DO NOT ALTER THIS LINE AT ALL",string.join(fortran_include_lines,"\n       "))
    f=open(os.path.join(outdir,"input_storage_fortran.f90"),"w")
    f.write(txt)
    f.close()

