#ifndef HDFTABLEMANAGER_H
#define HDFTABLEMANAGER_H
#include "TableDescription.h"

/** Controls a buffer of objects of type <T> and a TableDescription 
    describing the fields of the <T> object. 

    The table manager is a singleton. Its buffer is used to 
    read data in or out of text/HDF5 storage. When you 
    read data in, it will be placed into the buffer, which can be directly accessed
    in C++ and indirectly queried from C or FORTRAN. The buffer is also "priority aware".
    The member function prioritize_buffer() will handle items whose identifier has been
    redefined on a higher priority layer. The TableDescription holds information on the table
    used for HDF5 storage using the High Level Table API.
*/
template<typename T> 
class HDFTableManager {
public:

  static HDFTableManager<T>& instance() {
    static HDFTableManager<T> _instance;
    return _instance;
  }
  typedef std::vector<T> buffer_type;

  /** Default object used for fill values on unspecified fields*/
  T& default_fill(){ return m_default_fill;}

  /** TableDescription associated with buffered table*/
  TableDescription description;

  /** Enforce the layer-based priories on the buffer.
      If the buffer contains items whose identifer has been redefined on 
      multiple layers, the items will be overwritten or discarded in accordance with
      the \ref layer scheme.
  */
  void prioritize_buffer();
  
  /** Reference the buffer used to store the table entries*/
  std::vector<T> & buffer()
    {
      return m_buffer;
    }


 private:
  T m_default_fill;
  std::vector<T> m_buffer;

  /* more (non-static, singleton - enforcing) functions here */
  HDFTableManager();

  ~HDFTableManager()
  {
     m_buffer.clear();
  }; // destructor hidden
};

#endif
