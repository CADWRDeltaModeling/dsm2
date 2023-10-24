#ifndef TABLEITEMFUNCTORS_H__
#define TABLEITEMFUNCTORS_H__

template<typename T>
class identifier_compare : public binary_function<T,T,bool>
{
public:
    bool operator()(const T& first, const T& second)
{
   return first.identifier() < second.identifier();
}
};


template<typename T>
class identifier_compare1 : public binary_function<typename T::identifier_type, T,bool>
{
public:
  bool operator()(const T & first, const typename T::identifier_type & second)
{
  return first.identifier() < second;
}
};

template<typename T>
class identifier_compare2 : public binary_function<typename T::identifier_type, T,bool>
{
public:
   bool operator()(const typename T::identifier_type &first, const T & second)
{
   return first < second.identifier();
}
};

template<typename T>
class identifier_compare3 : public binary_function<typename T::identifier_type, T,bool>
{
public:
   bool operator()(const typename T::identifier_type &first, const typename T::identifier_type &second)
{
   return first < second;
}
};

template<typename T>
class identifier_equal: public binary_function<T,T,bool>
{
public:
    bool operator()(const T& first, const T& second)
{
   return first.identifier() == second.identifier();
}
};


template<typename T>
class identifier_equal1 : public binary_function<T, typename T::identifier_type, bool>{
public:
    bool operator()(const T& first, const typename T::identifier_type &second)
{
   return second == first.identifier();
}
};

template<typename T>
class identifier_equal2 : public binary_function<typename T::identifier_type, T,bool>
{
public:
  bool operator()(typename T::identifier_type & first, const T & second)
{
  return first == second.identifier();
}
};



template<typename T>
class entry_used : public unary_function<T, bool>{
public:
    bool operator()(const T& entry) const
    {
        return entry.used;
    }
};


#endif
