import pyodbc

class DBConnect(object):
  def __init__(self,dsn,usr=None,psw=None):
    self.cnn = None
    self.dsn = dsn
    #print dsn,usr,psw
    if usr:
      self.uid=usr
    else:
      self.uid=raw_input("dbase user name>")
    if psw:
       self.pwd=psw
    else:
      import getpass
      self.pwd=getpass.win_getpass("password>")
    self.open()

  def open(self):
    try:
      cnnstr = "DSN=%s;UID=%s;PWD=%s" % (self.dsn,self.uid,self.pwd)
      self.cnn = pyodbc.connect(cnnstr)
    except NameError,e:
      print 'error ', e, 'undefined'
    return
  #
  def commit(self): self.cnn.commit()

  def getCursor(self):
    assert(self.cnn)
    return self.cnn.cursor()

  def givePermission(self,layername, newuser):
    # Generate primary key for permission entry
    # Autocommit is false: will commit the permission chore here as
    # a single transaction
    pk=self.generatePrimaryID(1,autocommit=False)[0]
    print "primary key created: %s" % pk
    subqry="SELECT %s,%s,layer_id,%s FROM layer_definition WHERE NAME = %s" \
      % (pk,"\'"+newuser+"\'","\'06/01/2005\'","\'"+layername+"\'")
    qry = "INSERT INTO permissions (PERMISSION_ID,USER_NAME," \
           "COMPONENT_SET_ID,EXPIRATION_DATE) " + subqry + ";"
    print qry
    cur = self.cnn.cursor()
    cur.execute(qry)
    self.cnn.commit()
    cur.close()   #todo: is this a good idea?

  def checkPermission(self,uid,layerId):
      cur = self.cnn.cursor()
      #print 'checkPermission uid,layer:',uid,layer
      # eid Note LCASE is used for ACCESS and LOWER is used for Firebird
      qry="SELECT layer_id \
             FROM permissions p, layer_description l \
            WHERE p.component_set_id=l.layer_id \
              AND l.name LIKE '" + layer + "' \
              AND p.user_name = '"+uid+"'"
      #print 'checkPermission qry:',qry
      cur.execute(qry)
      #print 'checkPermission cursor:',cursor
      items=cur.fetchall()
      cur.close()
      print 'checkPermission items: <',items,'>'
      if len(items) > 0:
          return items
      else:
          return None

  def generatePrimaryID(self,nalloc,autocommit=True,limit=1000):
      if nalloc > limit or nalloc <1:
          raise "Maximum number of primary ids exceeded or fewer than one requested (%s requested)" % nalloc
      cur = self.cnn.cursor()
      qry="SELECT id_id,new_id FROM primary_key_generation;"
      cur.execute
      #qry="SELECT (gen_id(GLOBAL_GEN, %s)) FROM rdb$database;" % nalloc
      print qry
      cur.execute(qry)
      ids=cur.fetchall()
      if (len(ids) > 1):
          raise "primary_key_generation table should have one row. " \
                "Please notify the DSM2 dba immediately"
      current_id=ids[0][1]
      next_id=current_id+nalloc
      print "Executing primary key update. "
      ok=cur.execute("UPDATE primary_key_generation SET new_id = %s WHERE new_id = %s;"
         %  (current_id+nalloc,current_id))
      if ok==1:
        if autocommit: self.cnn.commit()
        ids=range(current_id+1,next_id+1)
        return ids
      elif ok==0:
        raise "Database changed during transaction, primary key no good."
      else:
        raise "primary_key_generation table should have one row. " \
              "Please notify DSM2 dba immediately"
