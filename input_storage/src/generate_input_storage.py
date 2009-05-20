"""Module for creating code in C++ to read and store user-defined data structures.

   This script can be used by a driver that defines the user data types. The script then
   converts the data type description into C++ code.
"""

import string
import os.path
import shutil
from table_component import *
indir = os.path.join(os.path.dirname(__file__),"template")
include_lines = []
input_map_lines = []
include_defs = []
prioritize_buffer_lines=[]
clear_buffer_lines=[]
fortran_include_lines=[]
write_text_buffer_lines=[]
write_text_buffer_cond_lines=[]
write_hdf5_buffer_cond_lines=[]
read_hdf5_buffer_cond_lines=[]
all_components=[]
write_hdf5_buffer_lines=[]
profiles={}
include_block_assign={}
    
def referencify(typename, const=False):
    if ("char" in typename):
        ref = typename.replace("char","char(&)")
    else:
        ref = typename + "&"
    if const: return "const %s" % ref
    return ref


def fortran_declaration(field, intent):
    return "%s, intent(%s) :: %s" % (field.fortran_type(), intent, field.name)


# todo: this may be wrong for child tables? need to delete the unused one first?
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
    vector<@TABLEOBJ>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
    if ( dupl != buffer().end())
    {   
        string message = "Duplicate identifiers in the same input layer:";
        stringstream messagestrm;
        messagestrm << message << endl << *dupl << " (" << (*dupl).objectName() <<")" << endl;
        messagestrm << "Layer: " << LayerManager::instance().layerName((*dupl).layer);
        throw runtime_error(messagestrm.str());
    }
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
    all_components.append(component)
    c_signature = string.join([x.c_arg(False) for x in component.members],",")
    stringlen_output_args = string.join([x.stringlen_arg(False) for x in component.members if x.stringlen_arg(False)],",")
    fortran_c_output_signature=string.join([x.fc_arg(False) for x in component.members],",")
    fortran_c_output_signature += ", int * ierror"
    if (len(stringlen_output_args) > 0):
        fortran_c_output_signature+=(", \n              %s"  % stringlen_output_args)

        
    c_input_signature = string.join([x.c_arg(True) for x in component.members],",")
    stringlen_input_args = string.join([x.stringlen_arg(True) for x in component.members if x.stringlen_arg(True)],",")
    fortran_c_input_signature=string.join([x.fc_arg(True) for x in component.members],",")
    fortran_c_input_signature += ", int * ierror"

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
    buffer_query =  string.join([x.assign(x.fortran_pointer()+"a_","obj.") for x in component.members],"\n    ")+"\n    "
    buffer_query += string.join([x.pad("a_"+x.name) for x in component.members if x.pad()],"\n    ")
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
    identifiertypes = string.join([component.get_member(x).identifier_type() for x in component.identifiers],",")
    identifier_assign = string.join([component.get_member(x).identifier_assign(i) \
           for x,i in zip(component.identifiers, range(len(component.identifiers)))],"\n      ")

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
        txt = txt.replace("@IDENTIFIERASSIGN",identifier_assign)

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
    prioritize_buffer_lines.append("HDFTableManager<%s>::instance().prioritize_buffer();\n     if(*ierror != 0) return;" % component.name)
    write_buffer_line="%s_write_buffer_to_text_f(file,append,ierror,filelen);\n     if(*ierror != 0) return;" % (component.name)
    conditional_write_buffer_line=("if(buffer_name == \"%s\"){" % component.name) + write_buffer_line + "}"
    write_text_buffer_lines.append(write_buffer_line)
    write_text_buffer_cond_lines.append(conditional_write_buffer_line)
    write_buffer_hdf5_line = "%s_write_buffer_to_hdf5_f(file_id,ierror);\n     if(*ierror != 0) return;" % component.name
    conditional_write_hdf5_buffer_line=("if(buffer_name == \"%s\"){" % component.name) + write_buffer_hdf5_line + "}"    
    write_hdf5_buffer_lines.append(write_buffer_hdf5_line)
    write_hdf5_buffer_cond_lines.append(conditional_write_hdf5_buffer_line)

    read_buffer_hdf5_line = "%s_read_buffer_from_hdf5_f(file_id,ierror);\n     if(*ierror != 0) return;" % component.name
    conditional_read_hdf5_buffer_line=("if(buffer_name == \"%s\"){" % component.name) + read_buffer_hdf5_line + "}"
    read_hdf5_buffer_cond_lines.append(conditional_read_hdf5_buffer_line)

    fortran_include_lines.append("include \"%s\"" % fortfile)
 
    # add the FORTRAN .f90 file for this object as an include to the main module


def define_include_block(name,valid_keywords):
    include_defs.append( (name, valid_keywords))
    for table in valid_keywords:
        include_block_assign[table]=name

def define_text_sub(name,outdir):
    textsub = name.lower()
    f=open(os.path.join(indir,"text_parser_template.cpp"),'r')
    txt = f.read()
    f.close()
    txt=txt.replace("$TEXTSUBTYPE",textsub)
    f=open(os.path.join(outdir,"text_parser.cpp"),'w')
    f.write(txt)

    
def define_profile(profile,contents):
    profiles[profile]=contents

    
def process_profiles():
    profile_code = ""
    for prof in profiles.keys():
        contentcode = ""
        contentcode += string.join(["out.push_back(\"%s\");" % member.upper() for member in profiles[prof] ],"\n        ")
        p = \
    """
    if(name =="%s")
    {
        %s
    }
    """ % (prof,contentcode)
        profile_code += p
    return profile_code
    

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
    profilelines = process_profiles()
    txt=txt.replace("// Profile definitions DO NOT ALTER THIS LINE AT ALL", profilelines)
    f=open(os.path.join(outdir,"input_state_map.cpp"),"w")
    f.write(txt)
    f.close()

    f=open(os.path.join(indir,"buffer_actions_template.cpp"),"r")
    txt=f.read()
    f.close()
    txt=txt.replace("// Clear all buffers DO NOT ALTER THIS LINE AT ALL",string.join(clear_buffer_lines,"\n"))
    txt=txt.replace("// Prioritize all buffers DO NOT ALTER THIS LINE AT ALL",string.join(prioritize_buffer_lines,"\n"))
    txt=txt.replace("// Write text all buffers DO NOT ALTER THIS LINE AT ALL",string.join(write_text_buffer_lines,"\n"))
    txt=txt.replace("// Write text one buffer DO NOT ALTER THIS LINE AT ALL",string.join(write_text_buffer_cond_lines,"\n"))
    txt=txt.replace("// Write hdf5 one buffer DO NOT ALTER THIS LINE AT ALL",string.join(write_hdf5_buffer_cond_lines,"\n")) 
    txt=txt.replace("// Read hdf5 one buffer DO NOT ALTER THIS LINE AT ALL",string.join(read_hdf5_buffer_cond_lines,"\n"))    
    txt=txt.replace("// Write hdf5 all buffers DO NOT ALTER THIS LINE AT ALL",string.join(write_hdf5_buffer_lines,"\n"))

    
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
    
    f=open(os.path.join(indir,"component_template.py"),"r")
    txt=f.read()
    f.close()
    componentlines=[]
    memberlines=[]
    for c in all_components:
        memberline="\"%s\":[" % c.name
        members=["\""+m.name+"\"" for m in c.members]
        memberline+=string.join(members,",")
        memberline+="]"
        memberlines.append(memberline)
        componentlines.append("\"%s\"" % c.name)
    subtxt="def component_order():\n    return["+string.join(componentlines,",\\\n      ")+"]\n\n\n"
    subtxt+="def component_members():\n    return {" + string.join(memberlines,",\\\n      ")+"}\n\n\n"
    include_assign=[]
    for key in include_block_assign:
        include_assign.append("    \""+key+"\":\""+include_block_assign[key]+"\"")
    subtxt+="def include_block():\n    return {\\\n" + string.join(include_assign,",\n")+"}\n"
    txt=txt.replace("@COMPONENTSCRIPT",subtxt)
    f=open(os.path.join(outdir,"component.py"),"w")
    f.write(txt)
    f.close()
    shutil.copy(os.path.join(indir,"userDefineLangTemplate.xml"),os.path.join(outdir,"."))
    process_profiles()
    
    
    

