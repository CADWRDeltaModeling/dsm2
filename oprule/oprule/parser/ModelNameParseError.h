#ifndef oprule_parser_MODELNAMEPARSEERROR_H_INCLUDED_
#define oprule_parser_MODELNAMEPARSEERROR_H_INCLUDED_

#include <stdexcept>
#include <iostream>
#include <string>

namespace oprule{
namespace parser{

class MissingIdentifier : public std::logic_error {
  public:
	MissingIdentifier(const std::string& msg = "") :  std::logic_error(msg) {}
	virtual ~MissingIdentifier(){}
};


class InvalidIdentifier : public std::logic_error {
public:
    InvalidIdentifier(const std::string& msg = "") : std::logic_error(msg) {}
	virtual ~InvalidIdentifier(){};
};

class ModelNameNotFound : public std::logic_error {
public:
    ModelNameNotFound(const std::string& msg = "") : std::logic_error(msg) {}
	virtual ~ModelNameNotFound(){}
};

}} //namespace

#endif //include guard